{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |Consensus queries against the multi-version runner.
module Concordium.Queries where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Common.Version
import Concordium.Genesis.Data
import Concordium.GlobalState.Instance
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Block (absoluteToLocalBlockHeight, localToAbsoluteBlockHeight)
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.Types.Parameters
import Concordium.Types.Queries
import Concordium.Types.SeedState
import qualified Concordium.Wasm as Wasm

import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types.InvokeContract as InvokeContract

import Concordium.Afgjort.Finalize.Types (FinalizationCommittee (..), PartyInfo (..))
import Concordium.Afgjort.Monad
import Concordium.Birk.Bake
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule (toAccountReleaseSummary)
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Statistics
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Types
import Concordium.ID.Types
import Concordium.Kontrol
import Concordium.Kontrol.BestBlock
import Concordium.MultiVersion
import Concordium.Skov as Skov (
    SkovQueryMonad (getBlocksAtHeight),
    evalSkovT,
 )

-- |Input to block based queries, i.e., queries which query the state of an
-- entity in a given block.
data BlockHashInput
    = -- |Best block.
      BHIBest
    | -- |Last finalized block
      BHILastFinal
      -- |Given block hash
    | BHIGiven BlockHash

-- |Run a query against a specific skov version.
liftSkovQuery ::
    MultiVersionRunner gsconf finconf ->
    EVersionedConfiguration gsconf finconf ->
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      VersionedSkovM gsconf finconf pv a
    ) ->
    IO a
liftSkovQuery mvr (EVersionedConfiguration vc) a = do
    st <- readIORef (vcState vc)
    runMVR (evalSkovT a (mvrSkovHandlers vc mvr) (vcContext vc) st) mvr

-- |Run a query against the latest skov version.
liftSkovQueryLatest ::
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      VersionedSkovM gsconf finconf pv a
    ) ->
    MVR gsconf finconf a
liftSkovQueryLatest a = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    liftSkovQuery mvr (Vec.last versions) a

-- |Run a query at a specific genesis index. The genesis index is assumed to be valid.
liftSkovQueryAtGenesisIndex ::
    GenesisIndex ->
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      VersionedSkovM gsconf finconf pv a
    ) ->
    MVR gsconf finconf a
liftSkovQueryAtGenesisIndex genIndex a = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    liftSkovQuery mvr (versions Vec.! fromIntegral genIndex) a

-- |Run a function at each genesis index from the latest, until it returns a 'Just' result.
atLatestSuccessfulVersion ::
    (EVersionedConfiguration gsconf finconf -> IO (Maybe a)) ->
    MultiVersionRunner gsconf finconf ->
    IO (Maybe a)
atLatestSuccessfulVersion a mvr = do
    versions <- readIORef (mvVersions mvr)
    let tryAt (i :: Int)
            | i < 0 = return Nothing
            | otherwise = do
                r <- a (versions Vec.! i)
                case r of
                    Just _ -> return r
                    Nothing -> tryAt (i - 1)
    tryAt (Vec.length versions - 1)

-- |Try a query against the latest skov version, working back
-- to earlier versions until we obtain a result or run out of
-- versions to check.
liftSkovQueryLatestResult ::
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      VersionedSkovM gsconf finconf pv (Maybe a)
    ) ->
    MVR gsconf finconf (Maybe a)
liftSkovQueryLatestResult a = MVR $ \mvr ->
    atLatestSuccessfulVersion (\vc -> liftSkovQuery mvr vc a) mvr

-- |Try a block based query on the latest skov version, working
-- backwards until we find the specified block or run out of
-- versions.
liftSkovQueryBlock ::
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      BlockPointerType (VersionedSkovM gsconf finconf pv) ->
      VersionedSkovM gsconf finconf pv a
    ) ->
    BlockHash ->
    MVR gsconf finconf (Maybe a)
liftSkovQueryBlock a bh =
    MVR $ \mvr ->
        atLatestSuccessfulVersion
            (\vc -> liftSkovQuery mvr vc (mapM a =<< resolveBlock bh))
            mvr

-- |Try a block based query on the latest skov version, working
-- backwards until we find the specified block or run out of
-- versions.
liftSkovQueryBlock' ::
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      BlockPointerType (VersionedSkovM gsconf finconf pv) ->
      VersionedSkovM gsconf finconf pv a
    ) ->
    BlockHashInput ->
    MVR gsconf finconf (Maybe a)
liftSkovQueryBlock' a bhi = do
  case bhi of
    BHIGiven bh ->
      MVR $ \mvr ->
              atLatestSuccessfulVersion
                (\vc -> liftSkovQuery mvr vc (mapM a =<< resolveBlock bh))
                mvr
    other -> liftSkovQueryLatest $ do
      bp <- case other of
             BHIBest -> bestBlock
             BHILastFinal -> lastFinalizedBlock
      Just <$> a bp

-- |Try a block based query on the latest skov version, working
-- backwards until we find the specified block or run out of
-- versions.  This version also passes the version configuration
-- to the query function.
liftSkovQueryBlockAndVersion ::
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovM gsconf finconf pv),
        FinalizationMonad (VersionedSkovM gsconf finconf pv)
      ) =>
      VersionedConfiguration gsconf finconf pv ->
      BlockPointerType (VersionedSkovM gsconf finconf pv) ->
      VersionedSkovM gsconf finconf pv a
    ) ->
    BlockHash ->
    MVR gsconf finconf (Maybe a)
liftSkovQueryBlockAndVersion a bh = MVR $ \mvr ->
    atLatestSuccessfulVersion
        ( \(EVersionedConfiguration vc) -> do
            st <- readIORef (vcState vc)
            runMVR
                ( evalSkovT
                    (mapM (a vc) =<< resolveBlock bh)
                    (mvrSkovHandlers vc mvr)
                    (vcContext vc)
                    st
                )
                mvr
        )
        mvr

-- |Retrieve the consensus status.
getConsensusStatus :: MVR gsconf finconf ConsensusStatus
getConsensusStatus = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    (csGenesisBlock, csGenesisTime) <- liftSkovQuery mvr (Vec.head versions) $ do
        genesis <- genesisBlock
        genTime <- getGenesisTime
        return (getHash genesis :: BlockHash, timestampToUTCTime genTime)
    -- while this case statement might look strange, it is needed because EVersionedConfiguration
    -- is an existential type and in the subsequent computation we need access to the implicit parameter `pv`
    -- the protocol version.
    case Vec.last versions of
        evc@(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) -> do
            liftSkovQuery mvr evc $ do
                let absoluteHeight = localToAbsoluteBlockHeight (vcGenesisHeight vc) . bpHeight
                bb <- bestBlock
                let csBestBlock = getHash bb
                let csBestBlockHeight = absoluteHeight bb
                csCurrentEraGenesisBlock <- getHash <$> genesisBlock
                csCurrentEraGenesisTime <- timestampToUTCTime <$> getGenesisTime
                genData <- getGenesisData
                let csSlotDuration = gdSlotDuration genData
                let csEpochDuration = fromIntegral (gdEpochLength genData) * csSlotDuration
                lfb <- lastFinalizedBlock
                let csLastFinalizedBlock = getHash lfb
                let csLastFinalizedBlockHeight = absoluteHeight lfb
                let csGenesisIndex = vcIndex vc
                let csProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
                stats <- getConsensusStatistics
                let csBlocksReceivedCount = stats ^. blocksReceivedCount
                    csBlockLastReceivedTime = stats ^. blockLastReceived
                    csBlockReceiveLatencyEMA = stats ^. blockReceiveLatencyEMA
                    csBlockReceiveLatencyEMSD = sqrt $ stats ^. blockReceiveLatencyEMVar
                    csBlockReceivePeriodEMA = stats ^. blockReceivePeriodEMA
                    csBlockReceivePeriodEMSD = sqrt <$> stats ^. blockReceivePeriodEMVar
                    csBlocksVerifiedCount = stats ^. blocksVerifiedCount
                    csBlockLastArrivedTime = stats ^. blockLastArrive
                    csBlockArriveLatencyEMA = stats ^. blockArriveLatencyEMA
                    csBlockArriveLatencyEMSD = sqrt $ stats ^. blockArriveLatencyEMVar
                    csBlockArrivePeriodEMA = stats ^. blockArrivePeriodEMA
                    csBlockArrivePeriodEMSD = sqrt <$> stats ^. blockArrivePeriodEMVar
                    csTransactionsPerBlockEMA = stats ^. transactionsPerBlockEMA
                    csTransactionsPerBlockEMSD = sqrt $ stats ^. transactionsPerBlockEMVar
                    csFinalizationCount = stats ^. finalizationCount
                    csLastFinalizedTime = stats ^. lastFinalizedTime
                    csFinalizationPeriodEMA = stats ^. finalizationPeriodEMA
                    csFinalizationPeriodEMSD = sqrt <$> stats ^. finalizationPeriodEMVar
                return ConsensusStatus{..}

-- * Queries against latest version

-- ** Blocks

-- |Returns a recursive structure representing the branches of the tree
-- from the last finalized block, inclusive.
getBranches :: MVR gsconf finconf Branch
getBranches = liftSkovQueryLatest $ do
    brs <- branchesFromTop
    brt <- foldM up Map.empty brs
    lastFin <- lastFinalizedBlock
    return $ Branch (getHash lastFin) (Map.findWithDefault [] (getHash lastFin :: BlockHash) brt)
  where
    up childrenMap =
        foldrM
            ( \b ma -> do
                parent <- bpParent b
                return $
                    ( at (getHash parent) . non []
                        %~ (Branch (getHash b) (Map.findWithDefault [] (getHash b) childrenMap) :)
                    )
                        ma
            )
            Map.empty

-- |Get a list of block hashes at a particular absolute height.
-- This traverses versions from the latest to the earliest, which is probably
-- fine for most practical cases.
getBlocksAtHeight ::
    -- |Height at which to get blocks
    BlockHeight ->
    -- |Base genesis index to query from
    GenesisIndex ->
    -- |Whether to restrict to the specified genesis index
    Bool ->
    MVR gsconf finconf [BlockHash]
getBlocksAtHeight basedHeight baseGI False = MVR $ \mvr -> do
    baseGenHeight <-
        if baseGI == 0
            then return (Just 0)
            else do
                versions <- readIORef (mvVersions mvr)
                return $
                    (\(EVersionedConfiguration vc) -> vcGenesisHeight vc)
                        <$> (versions Vec.!? fromIntegral baseGI)
    case baseGenHeight of
        Nothing -> return [] -- This occurs if the genesis index is invalid
        Just genHeight -> do
            let height = localToAbsoluteBlockHeight genHeight basedHeight
            -- The default case should never be needed, since 'absoluteToLocalBlockHeight' won't fail
            -- at a genesis block height of 0, which should be the case for the initial genesis.
            fromMaybe []
                <$> atLatestSuccessfulVersion
                    ( \evc@(EVersionedConfiguration vc) ->
                        forM (absoluteToLocalBlockHeight (vcGenesisHeight vc) height) $ \localHeight ->
                            liftSkovQuery mvr evc (map getHash <$> Skov.getBlocksAtHeight localHeight)
                    )
                    mvr
getBlocksAtHeight height baseGI True = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    case versions Vec.!? fromIntegral baseGI of
        Nothing -> return []
        Just evc -> liftSkovQuery mvr evc (map getHash <$> Skov.getBlocksAtHeight height)

-- ** Accounts

-- |Get a list of non-finalized transaction hashes for a given account.
getAccountNonFinalizedTransactions :: AccountAddress -> MVR gsconf finconf [TransactionHash]
getAccountNonFinalizedTransactions acct = liftSkovQueryLatest $ queryNonFinalizedTransactions . accountAddressEmbed $ acct

-- |Return the best guess as to what the next account nonce should be.
-- If all account transactions are finalized then this information is reliable.
-- Otherwise this is the best guess, assuming all other transactions will be
-- committed to blocks and eventually finalized.
getNextAccountNonce :: AccountAddress -> MVR gsconf finconf NextAccountNonce
getNextAccountNonce acct = liftSkovQueryLatest $ do
    (nanNonce, nanAllFinal) <- queryNextAccountNonce . accountAddressEmbed $ acct
    return NextAccountNonce{..}

-- * Queries against latest version that produces a result

-- ** Block indexed

-- |Get the basic info about a particular block.
getBlockInfo :: BlockHash -> MVR gsconf finconf (Maybe BlockInfo)
getBlockInfo bh =
    liftSkovQueryBlockAndVersion
        ( \vc bp -> do
            let biBlockHash = getHash bp
            biBlockParent <-
                if blockSlot bp == 0 && vcIndex vc /= 0
                    then do
                        -- The block is the genesis block of a non-initial chain, so we use the
                        -- hash of the last finalized block of the previous chain as the parent block.
                        -- Since the genesis index is non-zero, we know that there will be a previous
                        -- chain, and that it will be shut down with the last finalized block being
                        -- terminal.
                        lift $
                            liftSkovQueryAtGenesisIndex (vcIndex vc - 1) $
                                getHash <$> lastFinalizedBlock
                    else getHash <$> bpParent bp
            biBlockLastFinalized <- getHash <$> bpLastFinalized bp
            let biBlockHeight = localToAbsoluteBlockHeight (vcGenesisHeight vc) (bpHeight bp)
            let biGenesisIndex = vcIndex vc
            let biEraBlockHeight = bpHeight bp
            let biBlockReceiveTime = bpReceiveTime bp
            let biBlockArriveTime = bpArriveTime bp
            let biBlockSlot = blockSlot bp
            biBlockSlotTime <- getSlotTime biBlockSlot
            let biBlockBaker = blockBaker <$> blockFields bp
            biFinalized <- isFinalized bh
            let biTransactionCount = bpTransactionCount bp
            let biTransactionEnergyCost = bpTransactionsEnergyCost bp
            let biTransactionsSize = bpTransactionsSize bp
            let biBlockStateHash = blockStateHash bp
            return BlockInfo{..}
        )
        bh

-- |Get a detailed summary of a particular block including:
--   * The transaction outcomes in the block (including special transactions)
--   * Details of any finalization record in the block
--   * The state of the chain parameters and any pending updates
getBlockSummary :: forall gsconf finconf. BlockHash -> MVR gsconf finconf (Maybe BlockSummary)
getBlockSummary = liftSkovQueryBlock getBlockSummarySkovM
  where
    getBlockSummarySkovM ::
        forall pv.
        SkovMonad (VersionedSkovM gsconf finconf pv) =>
        BlockPointerType (VersionedSkovM gsconf finconf pv) ->
        VersionedSkovM gsconf finconf pv BlockSummary
    getBlockSummarySkovM bp = do
        bs <- blockState bp
        bsTransactionSummaries <- BS.getOutcomes bs
        bsSpecialEvents <- BS.getSpecialOutcomes bs
        bsFinalizationData <- case blockFinalizationData <$> blockFields bp of
            Just (BlockFinalizationData FinalizationRecord{..}) -> do
                -- Get the finalization committee by examining the previous finalized block
                fsFinalizers <-
                    blockAtFinIndex (finalizationIndex - 1) >>= \case
                        Nothing -> return Vec.empty
                        Just prevFin -> do
                            com <- getFinalizationCommittee prevFin
                            let signers = Set.fromList (finalizationProofParties finalizationProof)
                                fromPartyInfo i PartyInfo{..} =
                                    FinalizationSummaryParty
                                        { fspBakerId = partyBakerId,
                                          fspWeight = fromIntegral partyWeight,
                                          fspSigned = Set.member (fromIntegral i) signers
                                        }
                            return $ Vec.imap fromPartyInfo (parties com)
                let fsFinalizationBlockPointer = finalizationBlockPointer
                    fsFinalizationIndex = finalizationIndex
                    fsFinalizationDelay = finalizationDelay
                return $ Just FinalizationSummary{..}
            _ -> return Nothing
        bsUpdates <- BS.getUpdates bs
        let bsProtocolVersion = protocolVersion @pv
        return BlockSummary{..}

-- |Get the total amount of GTU in existence and status of the reward accounts.
getRewardStatus :: BlockHash -> MVR gsconf finconf (Maybe RewardStatus)
getRewardStatus = liftSkovQueryBlock $ \bp -> do
    reward <- BS.getRewardStatus =<< blockState bp
    gd <- getGenesisData
    let epochToUTC e = timestampToUTCTime $ 
            addDuration (gdGenesisTime gd) (fromIntegral e * fromIntegral (gdEpochLength gd) * gdSlotDuration gd)
    return $ epochToUTC <$> reward

-- |Get the birk parameters that applied when a given block was baked.
getBlockBirkParameters :: BlockHash -> MVR gsconf finconf (Maybe BlockBirkParameters)
getBlockBirkParameters = liftSkovQueryBlock $ \bp -> do
    bs <- blockState bp
    bbpElectionDifficulty <- BS.getCurrentElectionDifficulty bs
    bbpElectionNonce <- currentLeadershipElectionNonce <$> BS.getSeedState bs
    FullBakers{..} <- BS.getCurrentEpochBakers bs
    let resolveBaker FullBakerInfo{_theBakerInfo = BakerInfo{..}, ..} = do
            let bsBakerId = _bakerIdentity
            let bsBakerLotteryPower = fromIntegral _bakerStake / fromIntegral bakerTotalStake
            -- This should never return Nothing
            bacct <- BS.getBakerAccount bs _bakerIdentity
            bsBakerAccount <- mapM BS.getAccountCanonicalAddress bacct
            return BakerSummary{..}
    bbpBakers <- mapM resolveBaker fullBakerInfos
    return BlockBirkParameters{..}

-- |Get the cryptographic parameters of the chain at a given block.
-- The result is versioned (which will currently always be version 0).
getCryptographicParameters :: BlockHash -> MVR gsconf finconf (Maybe (Versioned CryptographicParameters))
getCryptographicParameters = liftSkovQueryBlock $ \bp -> do
    bs <- blockState bp
    Versioned 0 <$> BS.getCryptographicParameters bs

-- |Get all of the identity providers registered in the system as of a given block.
getAllIdentityProviders :: BlockHash -> MVR gsconf finconf (Maybe [IpInfo])
getAllIdentityProviders = liftSkovQueryBlock $ BS.getAllIdentityProviders <=< blockState

-- |Get all of the anonymity revokers registered in the system as of a given block.
getAllAnonymityRevokers :: BlockHash -> MVR gsconf finconf (Maybe [ArInfo])
getAllAnonymityRevokers = liftSkovQueryBlock $ BS.getAllAnonymityRevokers <=< blockState

-- |Get the ancestors of a block (including itself) up to a maximum
-- length.
getAncestors :: BlockHash -> BlockHeight -> MVR gsconf finconf (Maybe [BlockHash])
getAncestors blockHash count =
    liftSkovQueryBlock
        ( \bp -> do
            map bpHash <$> iterateForM bpParent (fromIntegral $ min count (1 + bpHeight bp)) bp
        )
        blockHash
  where
    iterateForM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
    iterateForM f steps initial = reverse <$> go [] steps initial
      where
        go acc n a
            | n <= 0 = return acc
            | otherwise = do
                a' <- f a
                go (a : acc) (n - 1) a'

-- |Get a list of all accounts in the block state.
getAccountList :: BlockHash -> MVR gsconf finconf (Maybe [AccountAddress])
getAccountList = liftSkovQueryBlock $ BS.getAccountList <=< blockState

-- |Get a list of all smart contract instances in the block state.
getInstanceList :: BlockHash -> MVR gsconf finconf (Maybe [ContractAddress])
getInstanceList =
    liftSkovQueryBlock $
        BS.getContractInstanceList <=< blockState

-- |Get the list of modules present as of a given block.
getModuleList :: BlockHash -> MVR gsconf finconf (Maybe [ModuleRef])
getModuleList = liftSkovQueryBlock $ BS.getModuleList <=< blockState

-- |Get the details of an account in the block state.
-- The account can be given via an address, an account index or a credential registration id.
-- In the latter case we lookup the account the credential is associated with, even if it was
-- removed from the account.
getAccountInfo ::
    BlockHashInput ->
    AccountIdentifier ->
    MVR gsconf finconf (Maybe AccountInfo)
getAccountInfo blockHashInput acct =
    join
        <$> liftSkovQueryBlock'
            ( \bp -> do
                bs <- blockState bp
                macc <- case acct of 
                                AccAddress addr -> BS.getAccount bs addr
                                AccIndex idx -> BS.getAccountByIndex bs idx
                                CredRegID crid -> BS.getAccountByCredId bs crid
                forM macc $ \(aiAccountIndex, acc) -> do
                    aiAccountNonce <- BS.getAccountNonce acc
                    aiAccountAmount <- BS.getAccountAmount acc
                    aiAccountReleaseSchedule <- toAccountReleaseSummary <$> BS.getAccountReleaseSchedule acc
                    aiAccountCredentials <- fmap (Versioned 0) <$> BS.getAccountCredentials acc
                    aiAccountThreshold <- aiThreshold <$> BS.getAccountVerificationKeys acc
                    aiAccountEncryptedAmount <- BS.getAccountEncryptedAmount acc
                    aiAccountEncryptionKey <- BS.getAccountEncryptionKey acc
                    gd <- getGenesisData
                    let convEpoch e =
                            timestampToUTCTime $
                                addDuration
                                    (gdGenesisTime gd)
                                    (fromIntegral e * fromIntegral (gdEpochLength gd) * gdSlotDuration gd)
                    aiStakingInfo <- toAccountStakingInfo convEpoch <$> BS.getAccountStake acc
                    aiAccountAddress <- BS.getAccountCanonicalAddress acc
                    return AccountInfo{..}
            )
            blockHashInput

-- |Get the details of a smart contract instance in the block state.
getInstanceInfo :: BlockHash -> ContractAddress -> MVR gsconf finconf (Maybe Wasm.InstanceInfo)
getInstanceInfo blockHash caddr =
    join
        <$> liftSkovQueryBlock
            ( \bp -> do
                bs <- blockState bp
                mkII =<< BS.getContractInstance bs caddr
            )
            blockHash
    where mkII Nothing = return Nothing
          mkII (Just (BS.InstanceInfoV0 BS.InstanceInfoV{..})) = do
            iiModel <- BS.thawContractState iiState
            return (Just (Wasm.InstanceInfoV0{
              Wasm.iiOwner = instanceOwner iiParameters,
              Wasm.iiAmount = iiBalance,
              Wasm.iiMethods = instanceReceiveFuns iiParameters,
              Wasm.iiName = instanceInitName iiParameters,
              Wasm.iiSourceModule = GSWasm.miModuleRef (instanceModuleInterface iiParameters),
              ..
              }))
          mkII (Just (BS.InstanceInfoV1 BS.InstanceInfoV{..})) = do
            return (Just (Wasm.InstanceInfoV1{
              Wasm.iiOwner = instanceOwner iiParameters,
              Wasm.iiAmount = iiBalance,
              Wasm.iiMethods = instanceReceiveFuns iiParameters,
              Wasm.iiName = instanceInitName iiParameters,
              Wasm.iiSourceModule = GSWasm.miModuleRef (instanceModuleInterface iiParameters)
              }))

-- |Get the source of a module as it was deployed to the chain.
getModuleSource :: BlockHash -> ModuleRef -> MVR gsconf finconf (Maybe Wasm.WasmModule)
getModuleSource blockHash modRef =
    join
        <$> liftSkovQueryBlock
            ( \bp -> do
                bs <- blockState bp
                BS.getModule bs modRef
            )
            blockHash

-- |Get the status of a particular delegation pool.
getPoolStatus :: forall gsconf finconf. BlockHash -> Maybe BakerId -> MVR gsconf finconf (Maybe PoolStatus)
getPoolStatus blockHash mbid =
    join
        <$> liftSkovQueryBlock poolStatus blockHash
  where
    poolStatus ::
        forall pv.
        ( SkovMonad (VersionedSkovM gsconf finconf pv)
        ) =>
        BlockPointerType (VersionedSkovM gsconf finconf pv) ->
        VersionedSkovM gsconf finconf pv (Maybe PoolStatus)
    poolStatus bp = case protocolVersion @pv of
        SP1 -> return Nothing
        SP2 -> return Nothing
        SP3 -> return Nothing
        SP4 -> do
            bs <- blockState bp
            BS.getPoolStatus bs mbid

-- |Get a list of all registered baker IDs in the specified block.
getRegisteredBakers :: forall gsconf finconf. BlockHash -> MVR gsconf finconf (Maybe [BakerId])
getRegisteredBakers = liftSkovQueryBlock (BS.getActiveBakers <=< blockState)

-- ** Transaction indexed

-- |Get the status of a transaction specified by its hash.
getTransactionStatus :: TransactionHash -> MVR gsconf finconf (Maybe TransactionStatus)
getTransactionStatus trHash =
    liftSkovQueryLatestResult $
        queryTransactionStatus trHash >>= \case
            Nothing -> return Nothing
            Just TT.Received{} -> return $ Just Received
            Just TT.Committed{..} -> do
                outcomes <- forM (HM.toList tsResults) $ \(bh, idx) ->
                    resolveBlock bh >>= \case
                        Nothing -> return (bh, Nothing) -- should not happen
                        Just bp -> do
                            bs <- blockState bp
                            outcome <- BS.getTransactionOutcome bs idx
                            return (bh, outcome)
                return $ Just $ Committed (Map.fromList outcomes)
            Just TT.Finalized{..} ->
                resolveBlock tsBlockHash >>= \case
                    Nothing -> return Nothing -- should not happen
                    Just bp -> do
                        bs <- blockState bp
                        outcome <- BS.getTransactionOutcome bs tsFinResult
                        return $ Just $ Finalized tsBlockHash outcome

-- |Get the status of a transaction within a particular block.
--
-- Note that, since this does not acquire the write lock, it is possible that
-- 'queryTransactionStatus' reports that the transaction is finalized even if it does not occur in
-- any block currently visible in the state.
getTransactionStatusInBlock :: TransactionHash -> BlockHash -> MVR gsconf finconf BlockTransactionStatus
getTransactionStatusInBlock trHash blockHash =
    fromMaybe BTSNotInBlock
        <$> liftSkovQueryLatestResult
            ( queryTransactionStatus trHash >>= \case
                Nothing ->
                    resolveBlock blockHash >>= \case
                        -- If the block is unknown in this skov version, then try earlier versions.
                        Nothing -> return Nothing
                        -- If the block is known, then we can return BTSNotInBlock already.
                        Just _ -> return $ Just BTSNotInBlock
                Just TT.Received{} -> return $ Just BTSReceived
                Just TT.Committed{..} -> case HM.lookup blockHash tsResults of
                    Nothing -> return $ Just BTSNotInBlock
                    Just idx ->
                        resolveBlock blockHash >>= \case
                            Nothing -> return $ Just BTSNotInBlock -- should not happen
                            Just bp -> do
                                bs <- blockState bp
                                outcome <- BS.getTransactionOutcome bs idx
                                return $ Just $ BTSCommitted outcome
                Just TT.Finalized{..} ->
                    if tsBlockHash == blockHash
                        then
                            resolveBlock blockHash >>= \case
                                Nothing -> return $ Just BTSNotInBlock -- unlikely but possible
                                Just bp -> do
                                    bs <- blockState bp
                                    outcome <- BS.getTransactionOutcome bs tsFinResult
                                    return $ Just $ BTSFinalized outcome
                        else return $ Just BTSNotInBlock
            )

-- * Smart contract invocations
invokeContract :: BlockHash -> InvokeContract.ContractContext -> MVR gsconf finconf (Maybe InvokeContract.InvokeContractResult)
invokeContract bh cctx =
    liftSkovQueryBlockAndVersion
    (\(_ :: VersionedConfiguration gsconf finconf pv) bp -> do
        bs <- blockState bp
        cm <- ChainMetadata <$> getSlotTimestamp (blockSlot bp)
        InvokeContract.invokeContract cctx cm bs)
    bh
    


-- * Miscellaneous

-- |Check whether the node is currently a member of the finalization committee.
checkIsCurrentFinalizer :: MVR gsconf finconf Bool
checkIsCurrentFinalizer = liftSkovQueryLatest isFinalizationCommitteeMember

-- |Check whether consensus has been shut down
checkIsShutDown :: MVR gsconf finconf Bool
checkIsShutDown = liftSkovQueryLatest isShutDown

-- |Result of a baker status query.
data BakerStatus
    = -- |The baker is a member of the current committee
      ActiveInComittee
    | -- |The account has a baker, but it is not yet in the committee
      AddedButNotActiveInCommittee
    | -- |The baker id does not correspond with a current baker
      NotInCommittee
    | -- |The baker may exist, but the keys do not match
      AddedButWrongKeys
    deriving (Eq, Ord, Show)

-- |Determine the status of the baker with respect to the current best block.
getBakerStatusBestBlock :: MVR gsconf finconf (BakerStatus, Maybe BakerId)
getBakerStatusBestBlock =
    asks mvBaker >>= \case
        Nothing -> return (NotInCommittee, Nothing)
        Just Baker{bakerIdentity = bakerIdent} -> liftSkovQueryLatest $ do
            bb <- bestBlock
            bs <- queryBlockState bb
            bakers <- BS.getCurrentEpochBakers bs
            bakerStatus <- case fullBaker bakers (bakerId bakerIdent) of
                Just fbinfo
                    -- Current baker with valid keys
                    | validateBakerKeys (fbinfo ^. bakerInfo) bakerIdent ->
                        return ActiveInComittee
                    -- Current baker, but invalid keys
                    | otherwise -> return AddedButWrongKeys
                Nothing ->
                    -- Not a current baker
                    BS.getBakerAccount bs (bakerId bakerIdent) >>= \case
                        Just acc ->
                            -- Account is valid
                            BS.getAccountBaker acc >>= \case
                                -- Account has no registered baker
                                Nothing -> return NotInCommittee
                                Just ab
                                    -- Registered baker with valid keys
                                    | validateBakerKeys (ab ^. accountBakerInfo . bakerInfo) bakerIdent ->
                                        return AddedButNotActiveInCommittee
                                    -- Registered baker with invalid keys
                                    | otherwise -> return AddedButWrongKeys
                        Nothing -> return NotInCommittee
            return (bakerStatus, Just $ bakerId bakerIdent)
