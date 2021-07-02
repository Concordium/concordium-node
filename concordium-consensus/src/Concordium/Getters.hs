{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.Getters where

import Lens.Micro.Platform hiding ((.=))

import Concordium.Kontrol.BestBlock
import Concordium.Skov as Skov
import qualified Data.HashMap.Strict as HM

import Control.Monad.State.Class

import Concordium.Common.Version
import Concordium.ID.Types (CredentialRegistrationID, aiThreshold)
import qualified Concordium.Scheduler.Types as AT
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockMonads
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Statistics as Stat
import qualified Concordium.GlobalState.Parameters as Parameters
import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.Types as T
import Concordium.Types.Accounts
import Concordium.Types.Queries
import qualified Concordium.Wasm as Wasm
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block hiding (PendingBlock)
import Concordium.Types.HashableTo
import Concordium.Types.Instance
import Concordium.GlobalState.Finalization
import Concordium.Types.SeedState
import qualified Concordium.Types.Queries as Q

import Concordium.Afgjort.Finalize(FinalizationStateLenses(..), FinalizationCurrentRound(..))
import Concordium.Afgjort.Finalize.Types(FinalizationCommittee(..), PartyInfo(..))
import Concordium.Birk.Bake (BakerIdentity(..), validateBakerKeys)
import Concordium.Kontrol (getFinalizationCommittee)

import Control.Concurrent.MVar
import Data.IORef
import Text.Read hiding (get, String)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Vector as Vector
import Control.Monad
import Data.Foldable (foldrM)

class SkovQueryMonad (SkovStateProtocolVersion z) m => SkovStateQueryable z m | z -> m where
    type SkovStateProtocolVersion z :: ProtocolVersion
    runStateQuery :: z -> m a -> IO a

instance (SkovConfiguration c, SkovQueryMonad (SkovProtocolVersion c) (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, IORef (SkovState c)) (SkovT () c IO) where
    type SkovStateProtocolVersion (SkovContext c, IORef (SkovState c)) = SkovProtocolVersion c
    runStateQuery (ctx, st) a = readIORef st >>= evalSkovT a () ctx

instance (SkovConfiguration c, SkovQueryMonad (SkovProtocolVersion c) (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, MVar (SkovState c)) (SkovT () c IO) where
    type SkovStateProtocolVersion (SkovContext c, MVar (SkovState c)) = SkovProtocolVersion c
    runStateQuery (ctx, st) a = readMVar st >>= evalSkovT a () ctx

hsh :: (HashableTo BlockHash a) => a -> String
hsh x = show (getHash x :: BlockHash)

getBestBlockState :: (BlockPointerMonad m, SkovQueryMonad pv m) => m (BlockState m)
getBestBlockState = queryBlockState =<< bestBlock

getLastFinalState :: SkovQueryMonad pv m => m (BlockState m)
getLastFinalState = queryBlockState =<< lastFinalizedBlock

-- |Get the status of a transaction specified by its hash.
getTransactionStatus :: SkovStateQueryable z m => AT.TransactionHash -> z -> IO (Maybe TransactionStatus)
getTransactionStatus hash sfsRef = runStateQuery sfsRef $
  queryTransactionStatus hash >>= \case
            Nothing -> return Nothing
            Just TT.Received{} -> return $ Just Received
            Just TT.Committed{..} -> do
                outcomes <- forM (HM.toList tsResults) $ \(bh, idx) ->
                    resolveBlock bh >>= \case
                        Nothing -> return (bh, Nothing) -- should not happen
                        Just bp -> do
                            bs <- queryBlockState bp
                            outcome <- BS.getTransactionOutcome bs idx
                            return (bh, outcome)
                return $ Just $ Committed (Map.fromList outcomes)
            Just TT.Finalized{..} ->
                resolveBlock tsBlockHash >>= \case
                    Nothing -> return Nothing -- should not happen
                    Just bp -> do
                        bs <- queryBlockState bp
                        outcome <- BS.getTransactionOutcome bs tsFinResult
                        return $ Just $ Finalized tsBlockHash outcome

-- |Get the status of a transaction within a particular block.
--
-- Note that, since this does not acquire the write lock, it is possible that
-- 'queryTransactionStatus' reports that the transaction is finalized even if it does not occur in
-- any block currently visible in the state.
getTransactionStatusInBlock :: SkovStateQueryable z m => AT.TransactionHash -> BlockHash -> z -> IO (Maybe BlockTransactionStatus)
getTransactionStatusInBlock txHash blockHash sfsRef = runStateQuery sfsRef $
  queryTransactionStatus txHash >>= \case
    Nothing -> return Nothing
    Just TT.Received{} -> return $ Just BTSReceived
    Just TT.Committed{..} -> case HM.lookup blockHash tsResults of
        Nothing -> return $ Just BTSNotInBlock
        Just idx ->
            resolveBlock blockHash >>= \case
                Nothing -> return $ Just BTSNotInBlock -- should not happen
                Just bp -> do
                    bs <- queryBlockState bp
                    outcome <- BS.getTransactionOutcome bs idx
                    return $ Just $ BTSCommitted outcome
    Just TT.Finalized{..} ->
        if tsBlockHash == blockHash
            then
                resolveBlock blockHash >>= \case
                    Nothing -> return $ Just BTSNotInBlock -- unlikely but possible
                    Just bp -> do
                        bs <- queryBlockState bp
                        outcome <- BS.getTransactionOutcome bs tsFinResult
                        return $ Just $ BTSFinalized outcome
            else return $ Just BTSNotInBlock

-- |Get a list of non-finalized transaction hashes for a given account.
getAccountNonFinalizedTransactions :: SkovStateQueryable z m => AccountAddress -> z -> IO [TransactionHash]
getAccountNonFinalizedTransactions addr sfsRef = runStateQuery sfsRef $
    queryNonFinalizedTransactions addr

-- |Return the best guess as to what the next account nonce should be.
-- If all account transactions are finalized then this information is reliable.
-- Otherwise this is the best guess, assuming all other transactions will be
-- committed to blocks and eventually finalized.
-- The 'Bool' indicates whether all transactions are finalized.
getNextAccountNonce :: SkovStateQueryable z m => AccountAddress -> z -> IO NextAccountNonce
getNextAccountNonce addr sfsRef = runStateQuery sfsRef $ do
    (nanNonce, nanAllFinal) <- queryNextAccountNonce addr
    return NextAccountNonce{..}

-- |Get a detailed summary of a particular block including:
--   * The transaction outcomes in the block (including special transactions)
--   * Details of any finalization record in the block
--   * The state of the chain parameters and any pending updates
getBlockSummary :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe BlockSummary)
getBlockSummary hash sfsRef = runStateQuery sfsRef $
  resolveBlock hash >>= \case
    Nothing -> return Nothing
    Just bp -> do
      bs <- queryBlockState bp
      bsTransactionSummaries <- BS.getOutcomes bs
      bsSpecialEvents <- BS.getSpecialOutcomes bs
      bsFinalizationData <- case blockFinalizationData <$> blockFields bp of
          Just (BlockFinalizationData FinalizationRecord{..}) -> do
              -- Get the finalization committee by examining the previous finalized block
              fsFinalizers <-
                  blockAtFinIndex (finalizationIndex - 1) >>= \case
                      Nothing -> return Vector.empty
                      Just prevFin -> do
                          com <- getFinalizationCommittee prevFin
                          let signers = S.fromList (finalizationProofParties finalizationProof)
                              fromPartyInfo i PartyInfo{..} =
                                  FinalizationSummaryParty
                                      { fspBakerId = partyBakerId,
                                        fspWeight = fromIntegral partyWeight,
                                        fspSigned = S.member (fromIntegral i) signers
                                      }
                          return $ Vector.imap fromPartyInfo (parties com)
              let fsFinalizationBlockPointer = finalizationBlockPointer
                  fsFinalizationIndex = finalizationIndex
                  fsFinalizationDelay = finalizationDelay
              return $ Just FinalizationSummary{..}
          _ -> return Nothing
      bsUpdates <- BS.getUpdates bs
      return (Just BlockSummary{..})

-- |Run a query on a block state.
withBlockState :: SkovQueryMonad pv m => BlockHash -> (BlockState m -> m a) -> m (Maybe a)
withBlockState hash f = withBlockStateMaybe hash (fmap Just . f)

-- |Run a query that might fail on a block state.
withBlockStateMaybe :: SkovQueryMonad pv m => BlockHash -> (BlockState m -> m (Maybe a)) -> m (Maybe a)
withBlockStateMaybe hash f =
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> f =<< queryBlockState bp


-- |Get a list of all accounts in the block state.
getAccountList :: SkovStateQueryable z m => BlockHash -> z -> IO (Maybe [AccountAddress])
getAccountList hash sfsRef = runStateQuery sfsRef $ withBlockState hash BS.getAccountList

-- |Get a list of all smart contract instances in the block state.
getInstanceList :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe [ContractAddress])
getInstanceList hash sfsRef = runStateQuery sfsRef $ withBlockState hash $
  fmap (fmap iaddress) . BS.getContractInstanceList

-- |Get the details of an account in the block state.
-- The account can be given either via an address, or via a credential registration id.
-- In the latter case we lookup the account the credential is associated with, even if it was
-- removed from the account.
getAccountInfo :: (SkovStateQueryable z m) => BlockHash -> z -> Either CredentialRegistrationID AccountAddress  -> IO (Maybe AccountInfo)
getAccountInfo hash sfsRef pointer = runStateQuery sfsRef $
  withBlockStateMaybe hash $ \bs -> do
                macc <- either (BS.getAccountByCredId bs) (BS.getAccount bs) pointer
                forM macc $ \(aiAccountIndex, acc) -> do
                    aiAccountNonce <- BS.getAccountNonce acc
                    aiAccountAmount <- BS.getAccountAmount acc
                    aiAccountReleaseSchedule <- BS.getAccountReleaseSchedule acc
                    aiAccountCredentials <- fmap (Versioned 0) <$> BS.getAccountCredentials acc
                    aiAccountThreshold <- aiThreshold <$> BS.getAccountVerificationKeys acc
                    aiAccountEncryptedAmount <- BS.getAccountEncryptedAmount acc
                    aiAccountEncryptionKey <- BS.getAccountEncryptionKey acc
                    aiBaker <- BS.getAccountBaker acc
                    return AccountInfo{..}

-- |Get the details of a smart contract instance in the block state.
getContractInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AT.ContractAddress -> IO (Maybe Instance)
getContractInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateMaybe hash $ \st -> BS.getContractInstance st addr

-- |Get the total amount of GTU in existence and status of the reward accounts.
getRewardStatus :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe RewardStatus)
getRewardStatus hash sfsRef = runStateQuery sfsRef $
  withBlockState hash $ \st -> do
    reward <- BS.getRewardStatus st
    return $
        RewardStatus
            { rsTotalAmount = reward ^. AT.totalGTU,
              rsTotalEncryptedAmount = reward ^. AT.totalEncryptedGTU,
              rsBakingRewardAccount = reward ^. AT.bakingRewardAccount,
              rsFinalizationRewardAccount = reward ^. AT.finalizationRewardAccount,
              rsGasAccount = reward ^. AT.gasAccount
            }

-- |Get the birk parameters that applied when a given block was baked.
getBlockBirkParameters :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe BlockBirkParameters)
getBlockBirkParameters hash sfsRef = runStateQuery sfsRef $
  withBlockState hash $ \bs -> do
    bbpElectionDifficulty <- BS.getCurrentElectionDifficulty bs
    bbpElectionNonce <- currentLeadershipElectionNonce <$> BS.getSeedState bs
    FullBakers{..} <- BS.getCurrentEpochBakers bs
    let resolveBaker FullBakerInfo{_bakerInfo = BakerInfo{..}, ..} = do
            let bsBakerId = _bakerIdentity
            let bsBakerLotteryPower = fromIntegral _bakerStake / fromIntegral bakerTotalStake
            -- This should never return Nothing
            bacct <- BS.getBakerAccount bs _bakerIdentity
            bsBakerAccount <- mapM BS.getAccountAddress bacct
            return BakerSummary{..}
    bbpBakers <- mapM resolveBaker fullBakerInfos
    return BlockBirkParameters{..}

-- |Get the list of modules present as of a given block.
getModuleList :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe [ModuleRef])
getModuleList hash sfsRef = runStateQuery sfsRef $
  withBlockState hash BS.getModuleList

-- |Get the module source as it was deployed to the chain.
getModuleSource :: (SkovStateQueryable z m) => BlockHash -> z -> ModuleRef -> IO (Maybe Wasm.WasmModule)
getModuleSource hash sfsRef mhash = runStateQuery sfsRef $
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> do
            st <- queryBlockState bp
            BS.getModule st mhash

-- |Retrieve the consensus status.
getConsensusStatus :: (SkovStateQueryable z m, TS.TreeStateMonad pv m) => z -> IO ConsensusStatus
getConsensusStatus sfsRef = runStateQuery sfsRef $ do
        csGenesisBlock <- getHash <$> genesisBlock
        csGenesisTime <- timestampToUTCTime <$> getGenesisTime
        bb <- bestBlock
        let csBestBlock = getHash bb
        let csBestBlockHeight = bpHeight bb
        genData <- getGenesisData
        let csSlotDuration = Parameters.gdSlotDuration genData
        let csEpochDuration = fromIntegral (Parameters.gdEpochLength genData) * csSlotDuration
        lfb <- lastFinalizedBlock
        let csLastFinalizedBlock = getHash lfb
        let csLastFinalizedBlockHeight = bpHeight lfb
        stats <- TS.getConsensusStatistics
        let csBlocksReceivedCount = stats ^. Stat.blocksReceivedCount
            csBlockLastReceivedTime = stats ^. Stat.blockLastReceived
            csBlockReceiveLatencyEMA = stats ^. Stat.blockReceiveLatencyEMA
            csBlockReceiveLatencyEMSD = sqrt $ stats ^. Stat.blockReceiveLatencyEMVar
            csBlockReceivePeriodEMA = stats ^. Stat.blockReceivePeriodEMA
            csBlockReceivePeriodEMSD = sqrt <$> stats ^. Stat.blockReceivePeriodEMVar
            csBlocksVerifiedCount = stats ^. Stat.blocksVerifiedCount
            csBlockLastArrivedTime = stats ^. Stat.blockLastArrive
            csBlockArriveLatencyEMA = stats ^. Stat.blockArriveLatencyEMA
            csBlockArriveLatencyEMSD = sqrt $ stats ^. Stat.blockArriveLatencyEMVar
            csBlockArrivePeriodEMA = stats ^. Stat.blockArrivePeriodEMA
            csBlockArrivePeriodEMSD = sqrt <$> stats ^. Stat.blockArrivePeriodEMVar
            csTransactionsPerBlockEMA = stats ^. Stat.transactionsPerBlockEMA
            csTransactionsPerBlockEMSD = sqrt $ stats ^. Stat.transactionsPerBlockEMVar
            csFinalizationCount = stats ^. Stat.finalizationCount
            csLastFinalizedTime = stats ^. Stat.lastFinalizedTime
            csFinalizationPeriodEMA = stats ^. Stat.finalizationPeriodEMA
            csFinalizationPeriodEMSD = sqrt <$> stats ^. Stat.finalizationPeriodEMVar
        return ConsensusStatus{..}

-- |Get the cryptographic parameters of the chain at a given block.
-- The result is versioned (which will currently always be version 0).
getCryptographicParameters :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe (Versioned Parameters.CryptographicParameters))
getCryptographicParameters hash sfsRef = runStateQuery sfsRef $ do
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> do
            st <- queryBlockState bp
            Just . Versioned 0 <$> BS.getCryptographicParameters st

-- |Get the basic info about a particular block.
getBlockInfo :: (SkovStateQueryable z m, BlockPointerMonad m, HashableTo BlockHash (BlockPointerType m)) => z -> String -> IO (Maybe BlockInfo)
getBlockInfo sfsRef blockHash = case readMaybe blockHash of
        Nothing -> return Nothing
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Nothing
                    Just bp -> do
                        let biBlockHash = getHash bp
                        biBlockParent <- getHash <$> bpParent bp
                        biBlockLastFinalized <- getHash <$> bpLastFinalized bp
                        let biBlockHeight = bpHeight bp
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
                        return (Just BlockInfo{..})

-- |Get a list of block hashes at a particular height.
getBlocksAtHeight :: (SkovStateQueryable z m, HashableTo BlockHash (BlockPointerType m))
    => z -> BlockHeight -> IO [BlockHash]
getBlocksAtHeight sfsRef height = runStateQuery sfsRef $
    map getHash <$> Skov.getBlocksAtHeight height

-- |Get the ancestors of a block (including itself) up to a maximum
-- length.
getAncestors :: (SkovStateQueryable z m, BlockPointerMonad m)
             => z -> String -> BlockHeight -> IO (Maybe [BlockHash])
getAncestors sfsRef blockHash count = case readMaybe blockHash of
        Nothing -> return Nothing
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Nothing
                    Just bp ->
                      Just . map bpHash <$> iterateForM bpParent (fromIntegral $ min count (1 + bpHeight bp)) bp
   where
     iterateForM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
     iterateForM f steps initial = reverse <$> go [] steps initial
       where go acc n a | n <= 0 = return acc
                        | otherwise = do
                         a' <- f a
                         go (a:acc) (n-1) a'

-- |Returns a recursive structure representing the branches of the tree
-- from the last finalized block, inclusive.
getBranches :: forall pv z m. (SkovStateQueryable z m, TS.TreeStateMonad pv m)
            => z -> IO Branch
getBranches sfsRef = runStateQuery sfsRef $ do
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

-- |Determine the status of the baker with respect to the current best block.
bakerStatusBestBlock :: (BlockPointerMonad m, SkovStateQueryable z m)
    => BakerIdentity
    -> z
    -> IO Q.BakerStatus
bakerStatusBestBlock bid sfsRef = runStateQuery sfsRef $ do
        bb <- bestBlock
        bs <- queryBlockState bb
        bakers <- BS.getCurrentEpochBakers bs
        case fullBaker bakers (bakerId bid) of
          Just fbinfo
            | validateBakerKeys (fbinfo ^. bakerInfo) bid -> return Q.ActiveBaker
            | otherwise -> return Q.BadKeys
          Nothing -> do
              macc <- BS.getBakerAccount bs (bakerId bid)
              case macc of
                Just acc -> do
                  mab <- BS.getAccountBaker acc
                  case mab of
                    Nothing -> return Q.NoBaker
                    Just ab
                      | validateBakerKeys (ab ^. accountBakerInfo) bid -> return Q.InactiveBaker
                      | otherwise -> return Q.BadKeys
                Nothing -> return Q.NoBaker

-- |Check whether the node is currently a member of the finalization committee.
checkIsCurrentFinalizer :: (SkovStateQueryable z m, MonadState s m, FinalizationStateLenses s t) => z -> IO Bool
checkIsCurrentFinalizer sfsRef = runStateQuery sfsRef $ do
   fs <- use finState
   case fs ^. finCurrentRound of
     PassiveCurrentRound _ -> return False
     ActiveCurrentRound _ -> return True

-- |Get all of the identity providers registered in the system as of a given block.
getAllIdentityProviders :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe [AT.IpInfo])
getAllIdentityProviders hash sfsRef = runStateQuery sfsRef $
  withBlockState hash BS.getAllIdentityProviders

-- |Get all of the anonymity revokers registered in the system as of a given block.
getAllAnonymityRevokers :: (SkovStateQueryable z m) => BlockHash -> z -> IO (Maybe [AT.ArInfo])
getAllAnonymityRevokers hash sfsRef = runStateQuery sfsRef $
  withBlockState hash BS.getAllAnonymityRevokers
