{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Consensus queries against the multi-version runner.
module Concordium.Queries where

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Bool.Singletons
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Common.Version
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Instance
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Block (absoluteToLocalBlockHeight, localToAbsoluteBlockHeight)
import Concordium.Types.Conditionally
import Concordium.Types.Execution (TransactionSummary)
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.Types.Parameters
import Concordium.Types.Queries hiding (PassiveCommitteeInfo (..), bakerId)
import qualified Concordium.Types.Queries.KonsensusV1 as QueriesKonsensusV1
import Concordium.Types.SeedState
import Concordium.Types.Transactions
import qualified Concordium.Types.UpdateQueues as UQ
import qualified Concordium.Wasm as Wasm

import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types.InvokeContract as InvokeContract

import Concordium.Afgjort.Finalize.Types (FinalizationCommittee (..), PartyInfo (..), makeFinalizationCommittee)
import Concordium.Afgjort.Monad
import Concordium.Birk.Bake
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.CapitalDistribution (DelegatorCapital (..))
import Concordium.GlobalState.CooldownQueue
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Statistics
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.ID.Types
import qualified Concordium.KonsensusV1.Consensus as ConsensusV1
import qualified Concordium.KonsensusV1.SkovMonad as SkovV1
import qualified Concordium.KonsensusV1.TreeState.Implementation as SkovV1
import qualified Concordium.KonsensusV1.TreeState.Types as SkovV1
import qualified Concordium.KonsensusV1.Types as SkovV1
import Concordium.Kontrol
import Concordium.Kontrol.BestBlock
import Concordium.MultiVersion
import Concordium.Skov as Skov (
    SkovQueryMonad (getBlocksAtHeight),
    evalSkovT,
 )
import Concordium.Types.Option
import Control.Monad.State.Class
import Data.Time

-- | Type of a query that can be run against consensus version 0.
type QueryV0M finconf a =
    forall (pv :: ProtocolVersion).
    ( SkovMonad (VersionedSkovV0M finconf pv),
      FinalizationMonad (VersionedSkovV0M finconf pv)
    ) =>
    VersionedSkovV0M finconf pv a

-- | Type of a query that can be run against consensus version 1.
type QueryV1M finconf a =
    forall (pv :: ProtocolVersion).
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    VersionedSkovV1M finconf pv a

-- | Run a query against a specific skov version.
liftSkovQuery ::
    MultiVersionRunner finconf ->
    EVersionedConfiguration finconf ->
    -- | Query to run at version 0 consensus.
    QueryV0M finconf a ->
    -- | Query to run at version 1 consensus.
    QueryV1M finconf a ->
    IO a
liftSkovQuery mvr evc av0 av1 = liftSkovQueryWithVersion mvr evc (const av0) (const av1)

-- | Run a query against a specific skov version, passing in the versioned configuration as a
--  parameter.
liftSkovQueryWithVersion ::
    MultiVersionRunner finconf ->
    EVersionedConfiguration finconf ->
    -- | Query to run at version 0 consensus.
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovV0M finconf pv),
        FinalizationMonad (VersionedSkovV0M finconf pv)
      ) =>
      VersionedConfigurationV0 finconf pv ->
      VersionedSkovV0M finconf pv a
    ) ->
    -- | Query to run at version 1 consensus.
    ( forall (pv :: ProtocolVersion).
      (IsConsensusV1 pv, IsProtocolVersion pv) =>
      VersionedConfigurationV1 finconf pv ->
      VersionedSkovV1M finconf pv a
    ) ->
    IO a
liftSkovQueryWithVersion mvr (EVersionedConfigurationV0 vc) av0 _ = do
    st <- readIORef (vc0State vc)
    runMVR (evalSkovT (av0 vc) (mvrSkovHandlers vc mvr) (vc0Context vc) st) mvr
liftSkovQueryWithVersion mvr (EVersionedConfigurationV1 vc) _ av1 = do
    st <- readIORef (vc1State vc)
    runMVR (SkovV1.evalSkovT (av1 vc) (vc1Context vc) st) mvr

-- | Run a query against the latest skov version.
liftSkovQueryLatest ::
    -- | Query to run at consensus version 0.
    QueryV0M finconf a ->
    -- | Query to run at consensus version 1.
    QueryV1M finconf a ->
    MVR finconf a
liftSkovQueryLatest av0 av1 = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    liftSkovQuery mvr (Vec.last versions) av0 av1

-- | Run a query at a specific genesis index. The genesis index is assumed to be valid.
liftSkovQueryAtGenesisIndex ::
    GenesisIndex ->
    -- | Query to run at consensus version 0.
    QueryV0M finconf a ->
    -- | Query to run at consensus version 1.
    QueryV1M finconf a ->
    MVR finconf a
liftSkovQueryAtGenesisIndex genIndex av0 av1 = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    liftSkovQuery mvr (versions Vec.! fromIntegral genIndex) av0 av1

-- | Run a function at each genesis index from the latest, until it returns a 'Just' result.
atLatestSuccessfulVersion ::
    (EVersionedConfiguration finconf -> IO (Maybe a)) ->
    MultiVersionRunner finconf ->
    IO (Maybe a)
atLatestSuccessfulVersion a mvr = do
    versions <- readIORef (mvVersions mvr)
    let tryAt (i :: Int)
            | i < 0 = return Nothing
            | otherwise = do
                let version = versions Vec.! i
                r <- a version
                case r of
                    Just x -> return (Just x)
                    Nothing -> tryAt (i - 1)
    tryAt (Vec.length versions - 1)

-- | Try a query against the latest skov version, working back
--  to earlier versions until we obtain a result or run out of
--  versions to check.
liftSkovQueryLatestResult ::
    -- | Query to run at consensus version 0.
    QueryV0M finconf (Maybe a) ->
    -- | Query to run at consensus version 1.
    QueryV1M finconf (Maybe a) ->
    MVR finconf (Maybe a)
liftSkovQueryLatestResult av0 av1 = MVR $ \mvr ->
    atLatestSuccessfulVersion (\vc -> liftSkovQuery mvr vc av0 av1) mvr

-- | Try a block based query on the latest skov version, working
--  backwards until we find the specified block or run out of
--  versions.
liftSkovQueryBlock ::
    forall finconf a.
    -- | Query to run at consensus version 0.
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovV0M finconf pv),
        FinalizationMonad (VersionedSkovV0M finconf pv)
      ) =>
      BlockPointerType (VersionedSkovV0M finconf pv) ->
      VersionedSkovV0M finconf pv a
    ) ->
    -- | Query to run at consensus version 1.
    ( forall (pv :: ProtocolVersion).
      (IsConsensusV1 pv, IsProtocolVersion pv) =>
      SkovV1.BlockPointer pv ->
      VersionedSkovV1M finconf pv a
    ) ->
    BlockHash ->
    MVR finconf (Maybe a)
liftSkovQueryBlock av0 av1 bh =
    MVR $ \mvr ->
        atLatestSuccessfulVersion
            (\vc -> liftSkovQuery mvr vc withBlockV0 withBlockV1)
            mvr
  where
    withBlockV0 :: QueryV0M finconf (Maybe a)
    withBlockV0 = mapM av0 =<< resolveBlock bh
    withBlockV1 :: QueryV1M finconf (Maybe a)
    withBlockV1 = do
        status <- SkovV1.getBlockStatus bh =<< get
        case status of
            SkovV1.BlockAliveOrFinalized bp -> Just <$> av1 bp
            _ -> return Nothing

-- | Response for queries needing to resolve a BlockHashInput first.
data BHIQueryResponse a
    = -- | No block found for the provided identifier.
      BQRNoBlock
    | -- | Block and value found for the provided identifier.
      BQRBlock
        { -- | Hash of the block which the identifier was resolved to.
          bhiqrHash :: BlockHash,
          -- | Resulting value computed from the given block.
          bhiqrValue :: a
        }
    deriving (Show)

instance Functor BHIQueryResponse where
    fmap fn response = case response of
        BQRNoBlock -> BQRNoBlock
        BQRBlock bh a -> BQRBlock bh (fn a)

-- | Convert BHIQueryResponse to a Maybe.
responseToMaybe :: BHIQueryResponse a -> Maybe a
responseToMaybe response = case response of
    BQRNoBlock -> Nothing
    BQRBlock _ v -> Just v

-- | Try a 'BlockHashInput' based query on the latest skov version. If a specific
--  block hash is given we work backwards through consensus versions until we
--  find the specified block or run out of versions.
--  The return value contains the block hash used for the query and result, if it was able to resolve the BlockHashInput.
liftSkovQueryBHI ::
    forall finconf a.
    -- | Query to run at consensus version 0.
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovV0M finconf pv),
        FinalizationMonad (VersionedSkovV0M finconf pv),
        IsProtocolVersion pv
      ) =>
      BlockPointerType (VersionedSkovV0M finconf pv) ->
      VersionedSkovV0M finconf pv a
    ) ->
    -- | Query to run at consensus version 1.
    ( forall (pv :: ProtocolVersion).
      (IsConsensusV1 pv, IsProtocolVersion pv) =>
      SkovV1.BlockPointer pv ->
      VersionedSkovV1M finconf pv a
    ) ->
    BlockHashInput ->
    MVR finconf (BHIQueryResponse a)
liftSkovQueryBHI av1 av2 = liftSkovQueryBHIAndVersion (const av1) (\_ bp _ -> av2 bp)

-- | Try a 'BlockHashInput' based state query on the latest skov version. If a specific
--  block hash is given we work backwards through consensus versions until we
--  find the specified block or run out of versions.
--  The return value is the hash used for the query, and a result if it was found.
liftSkovQueryStateBHI ::
    forall finconf a.
    ( forall m.
      (BS.BlockStateQuery m, MonadProtocolVersion m) =>
      BlockState m ->
      m a
    ) ->
    BlockHashInput ->
    MVR finconf (BHIQueryResponse a)
liftSkovQueryStateBHI stateQuery =
    liftSkovQueryBHI
        (stateQuery <=< blockState)
        (stateQuery <=< blockState)

-- A helper function for getting the best block in consensus version 1. It is the block with the highest QC.
bestBlockConsensusV1 :: (MonadState (SkovV1.SkovData pv) m) => m (SkovV1.BlockPointer pv)
bestBlockConsensusV1 = SkovV1.cbQuorumBlock <$> use (SkovV1.roundStatus . SkovV1.rsHighestCertifiedBlock)

-- | Try a 'BlockHashInput' based query on the latest skov version, provided with the configuration.
--  If a specific block hash is given we work backwards through consensus versions until we
--  find the specified block or run out of versions.
--  The return value contains the block hash used for the query and result, if it was able to resolve the BlockHashInput.
liftSkovQueryBHIAndVersion ::
    forall finconf a.
    -- | Query to run at consensus version 0.
    ( forall (pv :: ProtocolVersion).
      ( SkovMonad (VersionedSkovV0M finconf pv),
        FinalizationMonad (VersionedSkovV0M finconf pv),
        IsProtocolVersion pv
      ) =>
      VersionedConfigurationV0 finconf pv ->
      BlockPointerType (VersionedSkovV0M finconf pv) ->
      VersionedSkovV0M finconf pv a
    ) ->
    -- | Query to run at consensus version 1.
    --  As well as the versioned configuration and block pointer, this takes a 'Bool' indicating
    --  if the block is finalized.
    ( forall (pv :: ProtocolVersion).
      (IsConsensusV1 pv, IsProtocolVersion pv) =>
      VersionedConfigurationV1 finconf pv ->
      SkovV1.BlockPointer pv ->
      Bool ->
      VersionedSkovV1M finconf pv a
    ) ->
    BlockHashInput ->
    MVR finconf (BHIQueryResponse a)
liftSkovQueryBHIAndVersion av0 av1 bhi = do
    case bhi of
        Given bh ->
            MVR $ \mvr -> do
                maybeValue <-
                    atLatestSuccessfulVersion
                        ( \vc ->
                            liftSkovQueryWithVersion
                                mvr
                                vc
                                -- consensus version 0
                                (\theVC -> mapM (av0 theVC) =<< resolveBlock bh)
                                -- consensus version 1
                                ( \theVC -> do
                                    status <- SkovV1.getBlockStatus bh =<< get
                                    case status of
                                        SkovV1.BlockAlive bp -> Just <$> av1 theVC bp False
                                        SkovV1.BlockFinalized bp -> Just <$> av1 theVC bp True
                                        _ -> return Nothing
                                )
                        )
                        mvr
                return $ case maybeValue of
                    Just v -> BQRBlock bh v
                    Nothing -> BQRNoBlock
        AtHeight heightInput -> do
            blocks <- case heightInput of
                Absolute abh -> Concordium.Queries.getBlocksAtHeight (fromIntegral abh) 0 False
                Relative{..} -> Concordium.Queries.getBlocksAtHeight rBlockHeight rGenesisIndex rRestrict
            case blocks of
                (Just ([bh], evc)) ->
                    MVR $ \mvr -> do
                        maybeValue <-
                            liftSkovQueryWithVersion
                                mvr
                                evc
                                -- consensus version 0
                                (\theVC -> mapM (av0 theVC) =<< resolveBlock bh)
                                -- consensus version 1
                                ( \theVC -> do
                                    status <- SkovV1.getBlockStatus bh =<< get
                                    case status of
                                        SkovV1.BlockAlive bp -> Just <$> av1 theVC bp False
                                        SkovV1.BlockFinalized bp -> Just <$> av1 theVC bp True
                                        _ -> return Nothing
                                )
                        return $ case maybeValue of
                            Just v -> BQRBlock bh v
                            Nothing -> BQRNoBlock
                _ -> return BQRNoBlock
        other -> do
            versions <- liftIO . readIORef =<< asks mvVersions
            let evc = Vec.last versions
            (bh, maybeValue) <-
                MVR $ \mvr ->
                    liftSkovQueryWithVersion
                        mvr
                        evc
                        ( \theVC -> do
                            -- consensus version 0
                            bp <- case other of
                                Best -> bestBlock
                                LastFinal -> lastFinalizedBlock
                            (bpHash bp,) . Just <$> av0 theVC bp
                        )
                        ( \theVC -> do
                            -- consensus version 1
                            (bp, finalized) <- case other of
                                Best -> (,False) <$> bestBlockConsensusV1
                                LastFinal -> (,True) <$> use SkovV1.lastFinalized
                            (getHash bp,) . Just <$> av1 theVC bp finalized
                        )
            return $ case maybeValue of
                Just v -> BQRBlock bh v
                Nothing -> BQRNoBlock

-- | Retrieve the consensus status.
getConsensusStatus :: forall finconf. MVR finconf ConsensusStatus
getConsensusStatus = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    genInfo <-
        liftSkovQuery
            mvr
            (Vec.head versions)
            ( do
                genesis <- genesisBlock
                genTime <- getGenesisTime
                return (getHash genesis :: BlockHash, timestampToUTCTime genTime)
            )
            ( do
                SkovV1.GenesisMetadata{..} <- use SkovV1.genesisMetadata
                return (gmFirstGenesisHash, timestampToUTCTime (BaseV1.genesisTime gmParameters))
            )
    -- while this case statement might look strange, it is needed because EVersionedConfiguration
    -- is an existential type and in the subsequent computation we need access to the implicit parameter `pv`
    -- the protocol version.
    let evc = Vec.last versions
    liftSkovQuery
        mvr
        evc
        (statusV0 genInfo evc)
        (statusV1 genInfo evc)
  where
    statusV0 ::
        forall (pv :: ProtocolVersion).
        (SkovMonad (VersionedSkovV0M finconf pv)) =>
        (BlockHash, UTCTime) ->
        EVersionedConfiguration finconf ->
        VersionedSkovV0M finconf pv ConsensusStatus
    statusV0 (csGenesisBlock, csGenesisTime) evc = do
        let absoluteHeight = localToAbsoluteBlockHeight (evcGenesisHeight evc) . bpHeight
        bb <- bestBlock
        let csBestBlock = getHash bb
        let csBestBlockHeight = absoluteHeight bb
        csCurrentEraGenesisBlock <- getHash <$> genesisBlock
        csCurrentEraGenesisTime <- timestampToUTCTime <$> getGenesisTime
        genData <- getGenesisData
        let slotDur = gdSlotDuration genData
        let csSlotDuration = Just slotDur
        let csEpochDuration = fromIntegral (gdEpochLength genData) * slotDur
        lfb <- lastFinalizedBlock
        let csLastFinalizedBlock = getHash lfb
        let csLastFinalizedBlockHeight = absoluteHeight lfb
        let csGenesisIndex = evcIndex evc
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
            csConcordiumBFTStatus = Nothing
        return ConsensusStatus{..}
    statusV1 ::
        forall (pv :: ProtocolVersion).
        (IsProtocolVersion pv, IsConsensusV1 pv) =>
        (BlockHash, UTCTime) ->
        EVersionedConfiguration finconf ->
        VersionedSkovV1M finconf pv ConsensusStatus
    statusV1 (csGenesisBlock, csGenesisTime) evc = do
        let absoluteHeight = localToAbsoluteBlockHeight (evcGenesisHeight evc) . SkovV1.blockHeight
        bb <- bestBlockConsensusV1
        let csBestBlock = getHash bb
        let csBestBlockHeight = absoluteHeight bb

        genMetadata <- use SkovV1.genesisMetadata
        let csCurrentEraGenesisBlock = SkovV1.gmCurrentGenesisHash genMetadata
        let csCurrentEraGenesisTime =
                timestampToUTCTime $
                    BaseV1.genesisTime $
                        SkovV1.gmParameters genMetadata
        let csSlotDuration = Nothing -- no slots in consensus version 1
        let csEpochDuration = BaseV1.genesisEpochDuration $ SkovV1.gmParameters genMetadata
        lfb <- use SkovV1.lastFinalized
        let csLastFinalizedBlock = getHash lfb
        let csLastFinalizedBlockHeight = absoluteHeight lfb
        let csGenesisIndex = evcIndex evc
        let csProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
        stats <- use SkovV1.statistics
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
        cbftsCurrentTimeoutDuration <- use (SkovV1.roundStatus . SkovV1.rsCurrentTimeout)
        cbftsCurrentRound <- use (SkovV1.roundStatus . SkovV1.rsCurrentRound)
        cbftsCurrentEpoch <- use (SkovV1.roundStatus . SkovV1.rsCurrentEpoch)
        ss <- BS.getSeedState (SkovV1.bpState lfb)
        let cbftsTriggerBlockTime = timestampToUTCTime (ss ^. triggerBlockTime)
        return ConsensusStatus{csConcordiumBFTStatus = Just ConcordiumBFTStatus{..}, ..}

-- | Retrieve the slot time of the last finalized block.
getLastFinalizedSlotTime :: MVR finconf Timestamp
getLastFinalizedSlotTime = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    liftSkovQuery
        mvr
        (Vec.last versions)
        ( do
            lfb <- lastFinalizedBlock
            utcTimeToTimestamp <$> getSlotTime (blockSlot lfb)
        )
        ( do
            lfb <- use SkovV1.lastFinalized
            return $ SkovV1.blockTimestamp lfb
        )

-- * Queries against latest version

-- ** Blocks

-- | Returns a recursive structure representing the branches of the tree
--  from the last finalized block, inclusive.
getBranches :: MVR finconf Branch
getBranches =
    liftSkovQueryLatest
        ( do
            brs <- branchesFromTop
            brt <- foldM up Map.empty brs
            lastFin <- lastFinalizedBlock
            return $
                Branch
                    (getHash lastFin)
                    (Map.findWithDefault [] (getHash lastFin :: BlockHash) brt)
        )
        ( do
            brs <- gets SkovV1.branchesFromTop
            brt <- foldM up Map.empty brs
            lastFin <- use SkovV1.lastFinalized
            return $
                Branch
                    (getHash lastFin)
                    (Map.findWithDefault [] (getHash lastFin :: BlockHash) brt)
        )
  where
    up childrenMap =
        foldrM
            ( \b ma -> do
                parent <- bpParent b
                return $
                    ( at (getHash parent)
                        . non []
                        %~ (Branch (getHash b) (Map.findWithDefault [] (getHash b) childrenMap) :)
                    )
                        ma
            )
            Map.empty

-- | Get a list of block hashes and configuration at a particular absolute height.
--  This traverses versions from the latest to the earliest, which is probably
--  fine for most practical cases.
getBlocksAtHeight ::
    -- | Height at which to get blocks
    BlockHeight ->
    -- | Base genesis index to query from
    GenesisIndex ->
    -- | Whether to restrict to the specified genesis index
    Bool ->
    MVR finconf (Maybe ([BlockHash], EVersionedConfiguration finconf))
getBlocksAtHeight basedHeight baseGI False = MVR $ \mvr -> do
    baseGenHeight <-
        if baseGI == 0
            then return (Just 0)
            else do
                versions <- readIORef (mvVersions mvr)
                return $
                    evcGenesisHeight
                        <$> (versions Vec.!? fromIntegral baseGI)
    case baseGenHeight of
        Nothing -> return Nothing -- This occurs if the genesis index is invalid
        Just genHeight -> do
            let height = localToAbsoluteBlockHeight genHeight basedHeight
            -- The default case should never be needed, since 'absoluteToLocalBlockHeight' won't fail
            -- at a genesis block height of 0, which should be the case for the initial genesis.
            atLatestSuccessfulVersion
                ( \evc ->
                    forM (absoluteToLocalBlockHeight (evcGenesisHeight evc) height) $ \localHeight ->
                        (,evc)
                            <$> liftSkovQuery
                                mvr
                                evc
                                (map getHash <$> Skov.getBlocksAtHeight localHeight)
                                (map getHash <$> (SkovV1.getBlocksAtHeight localHeight =<< get))
                )
                mvr
getBlocksAtHeight height baseGI True = MVR $ \mvr -> do
    versions <- readIORef (mvVersions mvr)
    forM (versions Vec.!? fromIntegral baseGI) $ \evc ->
        (,evc)
            <$> liftSkovQuery
                mvr
                evc
                (map getHash <$> Skov.getBlocksAtHeight height)
                (map getHash <$> (SkovV1.getBlocksAtHeight height =<< get))

-- | Retrieve the last finalized block height relative to the most recent genesis index. Used for
-- resuming out-of-band catchup.
getLastFinalizedBlockHeight :: MVR finconf BlockHeight
getLastFinalizedBlockHeight =
    liftSkovQueryLatest
        (bpHeight <$> lastFinalizedBlock)
        (use (SkovV1.lastFinalized . to SkovV1.blockHeight))

-- ** Accounts

-- | Get a list of non-finalized transaction hashes for a given account.
getAccountNonFinalizedTransactions :: AccountAddress -> MVR finconf [TransactionHash]
getAccountNonFinalizedTransactions acct =
    liftSkovQueryLatest
        -- consensus v0
        (queryNonFinalizedTransactions acctClass)
        -- consensus v1
        ( gets
            ( fmap getHash
                . concatMap (Map.keys . snd)
                . SkovV1.getNonFinalizedAccountTransactions acctClass minNonce
            )
        )
  where
    acctClass = accountAddressEmbed acct

-- | Return the best guess as to what the next account nonce should be.
--  If all account transactions are finalized then this information is reliable.
--  Otherwise this is the best guess, assuming all other transactions will be
--  committed to blocks and eventually finalized.
getNextAccountNonce :: AccountAddress -> MVR finconf NextAccountNonce
getNextAccountNonce accountAddress =
    liftSkovQueryLatest
        -- consensus v0
        ( do
            (nanNonce, nanAllFinal) <- queryNextAccountNonce acctEq
            return NextAccountNonce{..}
        )
        -- consensus v1
        ( do
            (nanNonce, nanAllFinal) <- SkovV1.getNextAccountNonce acctEq =<< get
            return NextAccountNonce{..}
        )
  where
    acctEq = accountAddressEmbed accountAddress

-- * Queries against latest version that produces a result

-- ** Block indexed

-- | Get the basic info about a particular block.
getBlockInfo :: BlockHashInput -> MVR finconf (BHIQueryResponse BlockInfo)
getBlockInfo =
    liftSkovQueryBHIAndVersion
        ( \(vc :: VersionedConfigurationV0 finconf pv) bp -> do
            let biBlockHash = getHash bp
            let biGenesisIndex = vc0Index vc
            biBlockParent <-
                if blockSlot bp == 0 && biGenesisIndex /= 0
                    then do
                        -- The block is the genesis block of a non-initial chain, so we use the
                        -- hash of the last finalized block of the previous chain as the parent block.
                        -- Since the genesis index is non-zero, we know that there will be a previous
                        -- chain, and that it will be shut down with the last finalized block being
                        -- terminal.
                        lift $
                            liftSkovQueryAtGenesisIndex
                                (biGenesisIndex - 1)
                                (getHash <$> lastFinalizedBlock)
                                (use (SkovV1.lastFinalized . to getHash))
                    else getHash <$> bpParent bp
            biBlockLastFinalized <- getHash <$> bpLastFinalized bp
            let biBlockHeight = localToAbsoluteBlockHeight (vc0GenesisHeight vc) (bpHeight bp)
            let biEraBlockHeight = bpHeight bp
            let biBlockReceiveTime = bpReceiveTime bp
            let biBlockArriveTime = bpArriveTime bp
            let biBlockSlot = Just $ blockSlot bp
            biBlockSlotTime <- getSlotTime $ blockSlot bp
            let biBlockBaker = blockBaker <$> blockFields bp
            biFinalized <- isFinalized (bpHash bp)
            let biTransactionCount = bpTransactionCount bp
            let biTransactionEnergyCost = bpTransactionsEnergyCost bp
            let biTransactionsSize = bpTransactionsSize bp
            let biBlockStateHash = bpBlockStateHash bp
            let biProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
            let biRound = Nothing
            let biEpoch = Nothing
            return BlockInfo{..}
        )
        ( \(vc :: VersionedConfigurationV1 finconf pv) bp biFinalized -> do
            let biBlockHash = getHash bp
            let biGenesisIndex = vc1Index vc
            biBlockParent <-
                if SkovV1.blockRound bp == 0 && biGenesisIndex /= 0
                    then do
                        -- The block is the genesis block of a non-initial chain, so we use the
                        -- hash of the last finalized block of the previous chain as the parent block.
                        -- Since the genesis index is non-zero, we know that there will be a previous
                        -- chain, and that it will be shut down with the last finalized block being
                        -- terminal.
                        lift $
                            liftSkovQueryAtGenesisIndex
                                (biGenesisIndex - 1)
                                (getHash <$> lastFinalizedBlock)
                                (use (SkovV1.lastFinalized . to getHash))
                    else getHash <$> bpParent bp
            biBlockLastFinalized <- getHash <$> bpLastFinalized bp
            let biBlockHeight = localToAbsoluteBlockHeight (vc1GenesisHeight vc) (SkovV1.blockHeight bp)
            let biEraBlockHeight = SkovV1.blockHeight bp
            let biBlockReceiveTime = SkovV1.blockReceiveTime bp
            let biBlockArriveTime = SkovV1.blockArriveTime bp
            let biBlockSlot = Nothing -- no slots in consensus version 1
            let biBlockSlotTime = timestampToUTCTime $ SkovV1.blockTimestamp bp
            let biBlockBaker = ofOption Nothing (Just . SkovV1.blockBaker) $ SkovV1.blockBakedData bp
            let biTransactionCount = SkovV1.blockTransactionCount bp
            let biTransactionEnergyCost = SkovV1.blockEnergyCost bp
            let biTransactionsSize = fromIntegral $ SkovV1.blockTransactionsSize bp
            let biBlockStateHash = getHash $ SkovV1.bpState bp
            let biProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
            let biRound = Just $ SkovV1.blockRound bp
            let biEpoch = Just $ SkovV1.blockEpoch bp
            return BlockInfo{..}
        )

-- | Get the block items of a block.
getBlockItems :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse [BlockItem])
getBlockItems = liftSkovQueryBHI (return . blockTransactions) (return . SkovV1.blockTransactions)

-- | Get the transaction outcomes in the block.
getBlockTransactionSummaries :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (Vec.Vector TransactionSummary))
getBlockTransactionSummaries = liftSkovQueryStateBHI BS.getOutcomes

-- | Get the transaction outcomes in the block.
getBlockSpecialEvents :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (Seq.Seq SpecialTransactionOutcome))
getBlockSpecialEvents = liftSkovQueryStateBHI BS.getSpecialOutcomes

-- | Get the pending updates at the end of a given block.
getBlockPendingUpdates :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse [(TransactionTime, PendingUpdateEffect)])
getBlockPendingUpdates = liftSkovQueryStateBHI query
  where
    query ::
        forall m.
        (MonadProtocolVersion m, BS.BlockStateQuery m) =>
        ( BlockState m ->
          m [(TransactionTime, PendingUpdateEffect)]
        )
    query bs = do
        updates <- BS.getUpdates bs
        fuQueue <- foundationAccQueue (UQ._pFoundationAccountQueue . UQ._pendingUpdates $ updates)
        let remainingQueues = flattenUpdateQueues $ UQ._pendingUpdates updates
        return (merge fuQueue remainingQueues)
      where
        -- Flatten all of the pending update queues into one queue ordered by
        -- effective time. This is not the most efficient implementation and scales
        -- linearly with the number of queues, where it could scale logarithmically. But
        -- in practice this will not be an issue since update queues are very small.
        --
        -- The return list is ordered by the transaction time, ascending.
        --
        -- This flattens all queues except the foundation account updates.
        -- That one needs access to account lookup so it is handled by a
        -- separate helper below.
        flattenUpdateQueues ::
            forall cpv.
            (IsChainParametersVersion cpv) =>
            UQ.PendingUpdates cpv ->
            [(TransactionTime, PendingUpdateEffect)]
        flattenUpdateQueues UQ.PendingUpdates{..} =
            queueMapper PUERootKeys _pRootKeysUpdateQueue
                `merge` queueMapper PUELevel1Keys _pLevel1KeysUpdateQueue
                `merge` ( case sAuthorizationsVersionFor cpv of
                            SAuthorizationsVersion0 -> queueMapper PUELevel2KeysV0 _pLevel2KeysUpdateQueue
                            SAuthorizationsVersion1 -> queueMapper PUELevel2KeysV1 _pLevel2KeysUpdateQueue
                        )
                `merge` queueMapper PUEProtocol _pProtocolQueue
                `merge` queueMapperOptional PUEElectionDifficulty _pElectionDifficultyQueue
                `merge` queueMapper PUEEuroPerEnergy _pEuroPerEnergyQueue
                `merge` queueMapper PUEMicroCCDPerEuro _pMicroGTUPerEuroQueue
                `merge` ( case sMintDistributionVersionFor cpv of
                            SMintDistributionVersion0 -> queueMapper PUEMintDistributionV0 _pMintDistributionQueue
                            SMintDistributionVersion1 -> queueMapper PUEMintDistributionV1 _pMintDistributionQueue
                        )
                `merge` queueMapper PUETransactionFeeDistribution _pTransactionFeeDistributionQueue
                `merge` ( case sGasRewardsVersionFor cpv of
                            SGASRewardsVersion0 -> queueMapper PUEGASRewardsV0 _pGASRewardsQueue
                            SGASRewardsVersion1 -> queueMapper PUEGASRewardsV1 _pGASRewardsQueue
                        )
                `merge` ( case sPoolParametersVersionFor cpv of
                            SPoolParametersVersion0 -> queueMapper PUEPoolParametersV0 _pPoolParametersQueue
                            SPoolParametersVersion1 -> queueMapper PUEPoolParametersV1 _pPoolParametersQueue
                        )
                `merge` queueMapper PUEAddAnonymityRevoker _pAddAnonymityRevokerQueue
                `merge` queueMapper PUEAddIdentityProvider _pAddIdentityProviderQueue
                `merge` ( case sCooldownParametersVersionFor cpv of
                            SCooldownParametersVersion0 -> []
                            SCooldownParametersVersion1 -> case _pCooldownParametersQueue of
                                SomeParam queue -> queueMapper PUECooldownParameters queue
                                NoParam -> case cpv of {}
                        )
                `merge` queueMapperOptional PUETimeParameters _pTimeParametersQueue
                `merge` queueMapperOptional PUETimeoutParameters _pTimeoutParametersQueue
                `merge` queueMapperOptional PUEMinBlockTime _pMinBlockTimeQueue
                `merge` queueMapperOptional PUEBlockEnergyLimit _pBlockEnergyLimitQueue
                `merge` queueMapperOptional PUEFinalizationCommitteeParameters _pFinalizationCommitteeParametersQueue
          where
            cpv :: SChainParametersVersion cpv
            cpv = chainParametersVersion
            queueMapper :: (a -> PendingUpdateEffect) -> UQ.UpdateQueue a -> [(TransactionTime, PendingUpdateEffect)]
            queueMapper constructor UQ.UpdateQueue{..} = second constructor <$> _uqQueue

            queueMapperOptional :: (a -> PendingUpdateEffect) -> UQ.OUpdateQueue pt cpv a -> [(TransactionTime, PendingUpdateEffect)]
            queueMapperOptional _ NoParam = []
            queueMapperOptional constructor (SomeParam queue) = queueMapper constructor queue

        -- Merge two ascending lists into an ascending list.
        merge ::
            [(TransactionTime, PendingUpdateEffect)] ->
            [(TransactionTime, PendingUpdateEffect)] ->
            [(TransactionTime, PendingUpdateEffect)]
        merge [] y = y
        merge x [] = x
        merge (x : xs) (y : ys) | fst y < fst x = y : merge (x : xs) ys
        merge (x : xs) (y : ys) = x : merge xs (y : ys)

        foundationAccQueue :: UQ.UpdateQueue AccountIndex -> m [(TransactionTime, PendingUpdateEffect)]
        foundationAccQueue UQ.UpdateQueue{..} = do
            forM _uqQueue $ \(t, ai) -> do
                BS.getAccountByIndex bs ai >>= \case
                    Nothing -> error "Invariant violation. Foundation account index does not exist in the account table."
                    Just (_, acc) -> do
                        (t,) . PUEFoundationAccount <$> BS.getAccountCanonicalAddress acc

-- | Get the chain parameters valid at the end of a given block, as well as the address of the foundation account.
--  The chain parameters contain only the account index of the foundation account.
getBlockChainParameters :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (AccountAddress, EChainParametersAndKeys))
getBlockChainParameters = liftSkovQueryStateBHI query
  where
    query bs = do
        updates <- BS.getUpdates bs
        let params = UQ._currentParameters updates
        BS.getAccountByIndex bs (_cpFoundationAccount params) >>= \case
            Nothing -> error "Invariant violation. Foundation account index does not exist in the account table."
            Just (_, acc) -> do
                foundationAddr <- BS.getAccountCanonicalAddress acc
                return (foundationAddr, EChainParametersAndKeys params (_unhashed (UQ._currentKeyCollection updates)))

-- | Get the finalization record contained in the given block, if any.
getBlockFinalizationSummary :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse BlockFinalizationSummary)
getBlockFinalizationSummary = liftSkovQueryBHI getFinSummarySkovM (\_ -> return NoSummary)
  where
    getFinSummarySkovM ::
        forall pv.
        (SkovMonad (VersionedSkovV0M finconf pv)) =>
        BlockPointerType (VersionedSkovV0M finconf pv) ->
        VersionedSkovV0M finconf pv BlockFinalizationSummary
    getFinSummarySkovM bp = do
        case blockFinalizationData <$> blockFields bp of
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
                return . Summary $! FinalizationSummary{..}
            _ -> return NoSummary

-- | Get next update sequences numbers at the end of a given block.
getNextUpdateSequenceNumbers :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse NextUpdateSequenceNumbers)
getNextUpdateSequenceNumbers = liftSkovQueryStateBHI query
  where
    query bs = do
        updates <- BS.getUpdates bs
        return $ updateQueuesNextSequenceNumbers $ UQ._pendingUpdates updates

-- | Get the total amount of CCD in existence and status of the reward accounts.
getRewardStatus :: BlockHashInput -> MVR finconf (BHIQueryResponse RewardStatus)
getRewardStatus =
    liftSkovQueryBHI
        ( \bp -> do
            reward <- BS.getRewardStatus =<< blockState bp
            gd <- getGenesisData
            let epochToUTC e =
                    timestampToUTCTime $
                        addDuration (gdGenesisTime gd) (fromIntegral e * fromIntegral (gdEpochLength gd) * gdSlotDuration gd)
            return $ epochToUTC <$> reward
        )
        ( \bp -> do
            bState <- blockState bp
            reward <- BS.getRewardStatus bState
            -- The reward status includes the next payday epoch. To convert this to a UTCTime,
            -- we get the current epoch and the trigger block time (which we treat as the time of
            -- the start of the next epoch) from the seed state. For each epoch after the next
            -- epoch, we add the epoch duration to the trigger block time to get the time of the
            -- payday.
            ss <- BS.getSeedState bState
            let nextEpoch = ss ^. epoch + 1
            let deltaEpochs e = if e > nextEpoch then fromIntegral (e - nextEpoch) else 0
            BaseV1.CoreGenesisParametersV1{..} <- SkovV1.gmParameters <$> use SkovV1.genesisMetadata
            let epochToUTC e =
                    timestampToUTCTime $
                        addDuration (ss ^. triggerBlockTime) (deltaEpochs e * genesisEpochDuration)
            return $ epochToUTC <$> reward
        )

-- | Get the birk parameters that applied when a given block was baked.
getBlockBirkParameters :: BlockHashInput -> MVR finconf (BHIQueryResponse BlockBirkParameters)
getBlockBirkParameters =
    liftSkovQueryStateBHI
        ( \bs -> do
            bbpElectionDifficulty <- getED bs
            bbpElectionNonce <- view currentLeadershipElectionNonce <$> BS.getSeedState bs
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
        )
  where
    getED :: forall m. (BS.BlockStateQuery m, MonadProtocolVersion m) => BlockState m -> m (Maybe ElectionDifficulty)
    getED bs = case sConsensusParametersVersionFor (sChainParametersVersionFor (protocolVersion @(MPV m))) of
        SConsensusParametersVersion0 -> Just <$> BS.getCurrentElectionDifficulty bs
        SConsensusParametersVersion1 -> return Nothing -- There is no election difficulty in consensus version 1

-- | Get the cryptographic parameters of the chain at a given block.
getCryptographicParameters :: BlockHashInput -> MVR finconf (BHIQueryResponse CryptographicParameters)
getCryptographicParameters = liftSkovQueryStateBHI BS.getCryptographicParameters

-- | Get all of the identity providers registered in the system as of a given block.
getAllIdentityProviders :: BlockHashInput -> MVR finconf (BHIQueryResponse [IpInfo])
getAllIdentityProviders = liftSkovQueryStateBHI BS.getAllIdentityProviders

-- | Get all of the anonymity revokers registered in the system as of a given block.
getAllAnonymityRevokers :: BlockHashInput -> MVR finconf (BHIQueryResponse [ArInfo])
getAllAnonymityRevokers = liftSkovQueryStateBHI BS.getAllAnonymityRevokers

-- | Get the ancestors of a block (including itself) up to a maximum
--  length.
getAncestors :: BlockHashInput -> BlockHeight -> MVR finconf (BHIQueryResponse [BlockHash])
getAncestors bhi count =
    liftSkovQueryBHI
        ( \bp -> do
            map bpHash <$> iterateForM bpParent (fromIntegral $ min count (1 + bpHeight bp)) bp
        )
        ( \bp -> do
            map getHash <$> iterateForM bpParent (fromIntegral $ min count (1 + SkovV1.blockHeight bp)) bp
        )
        bhi
  where
    iterateForM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
    iterateForM f steps initial = reverse <$> go [] steps initial
      where
        go acc n a
            | n <= 0 = return acc
            | otherwise = do
                a' <- f a
                go (a : acc) (n - 1) a'

-- | Get a list of all accounts in the block state.
getAccountList :: BlockHashInput -> MVR finconf (BHIQueryResponse [AccountAddress])
getAccountList = liftSkovQueryStateBHI BS.getAccountList

-- | Get a list of all smart contract instances in the block state.
getInstanceList :: BlockHashInput -> MVR finconf (BHIQueryResponse [ContractAddress])
getInstanceList = liftSkovQueryStateBHI BS.getContractInstanceList

-- | Get the list of modules present as of a given block.
getModuleList :: BlockHashInput -> MVR finconf (BHIQueryResponse [ModuleRef])
getModuleList = liftSkovQueryStateBHI BS.getModuleList

-- | Get the details of an account in the block state.
--  The account can be given via an address, an account index or a credential registration id.
--  In the latter case we lookup the account the credential is associated with, even if it was
--  removed from the account.
getAccountInfo ::
    BlockHashInput ->
    AccountIdentifier ->
    MVR finconf (BHIQueryResponse (Maybe AccountInfo))
getAccountInfo blockHashInput acct = do
    liftSkovQueryBHI
        (getAccountInfoV0 acct <=< blockState)
        (getAccountInfoV1 acct <=< blockState)
        blockHashInput

-- | Get the details of an account, for the V0 consensus.
getAccountInfoV0 :: (SkovQueryMonad m) => AccountIdentifier -> BlockState m -> m (Maybe AccountInfo)
getAccountInfoV0 = getAccountInfoHelper getASIv0 getCooldownsV0
  where
    getASIv0 acc = do
        gd <- getGenesisData
        let convEpoch e =
                timestampToUTCTime $
                    addDuration
                        (gdGenesisTime gd)
                        (fromIntegral e * fromIntegral (gdEpochLength gd) * gdSlotDuration gd)
        toAccountStakingInfo convEpoch <$> BS.getAccountStake acc
    -- Flexible cooldown is not supported in consensus version 0.
    getCooldownsV0 _ = return []

-- | Get the details of an account, for the V1 consensus.
getAccountInfoV1 ::
    forall m.
    ( BS.BlockStateQuery m,
      MonadProtocolVersion m,
      MonadState (SkovV1.SkovData (MPV m)) m,
      IsConsensusV1 (MPV m)
    ) =>
    AccountIdentifier ->
    BlockState m ->
    m (Maybe AccountInfo)
getAccountInfoV1 ai bs = getAccountInfoHelper getASIv1 getCooldownsV1 ai bs
  where
    getASIv1 acc = toAccountStakingInfoP4 <$> BS.getAccountStake acc
    getCooldownsV1 acc = case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor (MPV m))) of
        STrue ->
            BS.getAccountCooldowns acc >>= \case
                Nothing -> return []
                Just cooldowns -> do
                    ccpEpochDuration <-
                        BaseV1.genesisEpochDuration . SkovV1.gmParameters
                            <$> use SkovV1.genesisMetadata
                    seedState <- BS.getSeedState bs
                    let SeedStateV1
                            { ss1TriggerBlockTime = ccpTriggerTime,
                              ss1Epoch = ccpCurrentEpoch
                            } =
                                seedState
                    ccpNextPayday <- BS.getPaydayEpoch bs
                    chainParams <- BS.getChainParameters bs
                    let ccpRewardPeriodLength =
                            chainParams ^. cpTimeParameters . supportedOParam . tpRewardPeriodLength
                    let ccpCooldownDuration =
                            chainParams ^. cpCooldownParameters . cpUnifiedCooldown
                    return $! toCooldownList CooldownCalculationParameters{..} cooldowns
        SFalse -> return []

-- | Helper for getting the details of an account, given a function for getting the staking
--  information.
getAccountInfoHelper ::
    (BS.BlockStateQuery m) =>
    (Account m -> m AccountStakingInfo) ->
    (Account m -> m [Cooldown]) ->
    AccountIdentifier ->
    BlockState m ->
    m (Maybe AccountInfo)
getAccountInfoHelper getASI getCooldowns acct bs = do
    macc <- case acct of
        AccAddress addr -> BS.getAccount bs addr
        AccIndex idx -> BS.getAccountByIndex bs idx
        CredRegID crid -> BS.getAccountByCredId bs crid
    forM macc $ \(aiAccountIndex, acc) -> do
        aiAccountNonce <- BS.getAccountNonce acc
        aiAccountAmount <- BS.getAccountAmount acc
        aiAccountReleaseSchedule <- BS.getAccountReleaseSummary acc
        aiAccountCredentials <- fmap (Versioned 0) <$> BS.getAccountCredentials acc
        aiAccountThreshold <- aiThreshold <$> BS.getAccountVerificationKeys acc
        aiAccountEncryptedAmount <- BS.getAccountEncryptedAmount acc
        aiAccountEncryptionKey <- BS.getAccountEncryptionKey acc
        aiStakingInfo <- getASI acc
        aiAccountAddress <- BS.getAccountCanonicalAddress acc
        aiAccountCooldowns <- getCooldowns acc
        aiAccountAvailableAmount <- BS.getAccountAvailableAmount acc
        aiAccountIsSuspended <- do
            mAccBaker <- BS.getAccountBaker acc
            case mAccBaker of
                -- If the account doesn't have an associated validator, then it can't be suspended.
                Nothing -> return False
                Just accBaker ->
                    return $
                        fromCondDef (_bieAccountIsSuspended $ _accountBakerInfo accBaker) False
        return AccountInfo{..}

-- | Get the details of a smart contract instance in the block state.
getInstanceInfo :: BlockHashInput -> ContractAddress -> MVR finconf (BHIQueryResponse (Maybe Wasm.InstanceInfo))
getInstanceInfo bhi caddr = do
    liftSkovQueryStateBHI
        (getInstanceInfoHelper caddr)
        bhi

-- | Helper function for getting the 'Wasm.InstanceInfo' for a contract instance.
getInstanceInfoHelper ::
    (BS.BlockStateQuery m) =>
    ContractAddress ->
    BlockState m ->
    m (Maybe Wasm.InstanceInfo)
getInstanceInfoHelper caddr bs = do
    mInstance <- BS.getContractInstance bs caddr
    forM mInstance $ \case
        BS.InstanceInfoV0 BS.InstanceInfoV{..} -> do
            iiModel <- BS.externalContractState iiState
            return $
                Wasm.InstanceInfoV0
                    { Wasm.iiOwner = instanceOwner iiParameters,
                      Wasm.iiAmount = iiBalance,
                      Wasm.iiMethods = instanceReceiveFuns iiParameters,
                      Wasm.iiName = instanceInitName iiParameters,
                      Wasm.iiSourceModule = GSWasm.miModuleRef (instanceModuleInterface iiParameters),
                      ..
                    }
        BS.InstanceInfoV1 BS.InstanceInfoV{..} -> do
            return $
                Wasm.InstanceInfoV1
                    { Wasm.iiOwner = instanceOwner iiParameters,
                      Wasm.iiAmount = iiBalance,
                      Wasm.iiMethods = instanceReceiveFuns iiParameters,
                      Wasm.iiName = instanceInitName iiParameters,
                      Wasm.iiSourceModule = GSWasm.miModuleRef (instanceModuleInterface iiParameters)
                    }

-- | Get the exact state of a smart contract instance in the block state. The
--  return value is 'Nothing' if the instance cannot be found (either the
--  requested block does not exist, or the instance does not exist in that
--  block), @Just . Left@ if the instance is a V0 instance, and @Just . Right@ if
--  the instance is a V1 instance.
getInstanceState :: BlockHashInput -> ContractAddress -> MVR finconf (BHIQueryResponse (Maybe (Either Wasm.ContractState (StateV1.PersistentState, StateV1.LoadCallback))))
getInstanceState bhi caddr = do
    liftSkovQueryStateBHI
        (\bs -> mkII =<< BS.getContractInstance bs caddr)
        bhi
  where
    mkII Nothing = return Nothing
    mkII (Just (BS.InstanceInfoV0 BS.InstanceInfoV{..})) =
        Just . Left <$> BS.externalContractState iiState
    mkII (Just (BS.InstanceInfoV1 BS.InstanceInfoV{..})) = do
        cstate <- BS.externalContractState iiState
        callback <- BS.getV1StateContext
        return . Just . Right $ (cstate, callback)

-- | Get the source of a module as it was deployed to the chain.
getModuleSource :: BlockHashInput -> ModuleRef -> MVR finconf (BHIQueryResponse (Maybe Wasm.WasmModule))
getModuleSource bhi modRef = do
    liftSkovQueryStateBHI
        (\bs -> BS.getModule bs modRef)
        bhi

-- | Get the status of a particular delegation pool.
getPoolStatus :: forall finconf. BlockHashInput -> BakerId -> MVR finconf (BHIQueryResponse (Maybe BakerPoolStatus))
getPoolStatus blockHashInput mbid = do
    liftSkovQueryStateBHI poolStatus blockHashInput
  where
    poolStatus ::
        forall m.
        (BS.BlockStateQuery m, MonadProtocolVersion m) =>
        BlockState m ->
        m (Maybe BakerPoolStatus)
    poolStatus bs = case delegationSupport @(AccountVersionFor (MPV m)) of
        SAVDelegationNotSupported -> return Nothing
        SAVDelegationSupported -> BS.getPoolStatus bs mbid

-- | Get the status of passive delegation.
getPassiveDelegationStatus :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (Maybe PassiveDelegationStatus))
getPassiveDelegationStatus blockHashInput = do
    liftSkovQueryStateBHI poolStatus blockHashInput
  where
    poolStatus ::
        forall m.
        (BS.BlockStateQuery m, MonadProtocolVersion m) =>
        BlockState m ->
        m (Maybe PassiveDelegationStatus)
    poolStatus bs = case delegationSupport @(AccountVersionFor (MPV m)) of
        SAVDelegationNotSupported -> return Nothing
        SAVDelegationSupported -> Just <$> BS.getPassiveDelegationStatus bs

-- | Get a list of all registered baker IDs in the specified block.
getRegisteredBakers :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse [BakerId])
getRegisteredBakers = liftSkovQueryStateBHI BS.getActiveBakers

-- | Error type for querying delegators for some block.
data GetDelegatorsError
    = -- | The block is from a protocol version without delegators.
      GDEUnsupportedProtocolVersion
    | -- | No pool found for the provided baker ID.
      GDEPoolNotFound

-- | Get the list of registered delegators for a given block.
--  Changes to delegation is reflected immediately in this list.
--  If a BakerId is provided it will return the delegators for the corresponding pool otherwise it returns the passive delegators.
getDelegators ::
    forall finconf.
    BlockHashInput ->
    Maybe BakerId ->
    MVR finconf (BHIQueryResponse (Either GetDelegatorsError [DelegatorInfo]))
getDelegators bhi maybeBakerId = do
    liftSkovQueryStateBHI getter bhi
  where
    getter ::
        forall m.
        (BS.BlockStateQuery m, MonadProtocolVersion m) =>
        BlockState m ->
        m (Either GetDelegatorsError [DelegatorInfo])
    getter bs = case delegationSupport @(AccountVersionFor (MPV m)) of
        SAVDelegationNotSupported ->
            return $ Left GDEUnsupportedProtocolVersion
        SAVDelegationSupported -> do
            maybeDelegators <- BS.getActiveDelegators bs maybeBakerId
            return $ maybe (Left GDEPoolNotFound) (Right . fmap toDelegatorInfo) maybeDelegators
    toDelegatorInfo (accountAddress, BS.ActiveDelegatorInfo{..}) =
        DelegatorInfo
            { pdiAccount = accountAddress,
              pdiStake = activeDelegatorStake,
              pdiPendingChanges = activeDelegatorPendingChange
            }

-- | Get the fixed list of delegators contributing stake in the reward period for a given block.
--  If a BakerId is provided it will return the delegators for the corresponding pool otherwise it returns the passive delegators.
getDelegatorsRewardPeriod :: forall finconf. BlockHashInput -> Maybe BakerId -> MVR finconf (BHIQueryResponse (Either GetDelegatorsError [DelegatorRewardPeriodInfo]))
getDelegatorsRewardPeriod bhi maybeBakerId = do
    liftSkovQueryStateBHI getter bhi
  where
    getter ::
        forall m.
        (BS.BlockStateQuery m, MonadProtocolVersion m) =>
        BlockState m ->
        m (Either GetDelegatorsError [DelegatorRewardPeriodInfo])
    getter bs = case delegationSupport @(AccountVersionFor (MPV m)) of
        SAVDelegationNotSupported ->
            return $ Left GDEUnsupportedProtocolVersion
        SAVDelegationSupported -> do
            maybeDelegators <- BS.getCurrentDelegators bs maybeBakerId
            return $ maybe (Left GDEPoolNotFound) (Right . fmap toDelegatorInfo) maybeDelegators
    toDelegatorInfo (accountAddress, DelegatorCapital{..}) =
        DelegatorRewardPeriodInfo
            { pdrpiAccount = accountAddress,
              pdrpiStake = dcDelegatorCapital
            }

-- ** Epoch indexed

data EpochQueryError
    = -- | The specified block was not found.
      EQEBlockNotFound
    | -- | The specified epoch or genesis index is in the future.
      EQEFutureEpoch
    | -- | The epoch is not valid for the specified genesis index.
      EQEInvalidEpoch
    | -- | The genesis index does not support the query.
      EQEInvalidGenesisIndex

-- | Get the first finalized block in a specified epoch.
--  The epoch can be specified directly by the genesis index and epoch number, or indirectly by a
--  'BlockHashInput' that references a block.
--
--  The following failure cases apply:
--    * If the input is a 'BlockHashInput' and the specified block cannot be resolved, this returns
--      'EQEBlockNotFound'.
--    * If the specified genesis index is greater than the current one, returns 'EQEFutureEpoch'.
--    * If the specified epoch is after the epoch of the last finalized block
--       - returns 'EQEFutureEpoch' if the genesis index is the current one
--       - returns 'EQEInvalidEpoch' otherwise.
--    * If the specified epoch contains no finalized blocks then returns 'EQEBlockNotFound'.
--      This only applies to consensus version 0, as epochs cannot be empty in consensus version 1.
getFirstBlockEpoch :: forall finconf. EpochRequest -> MVR finconf (Either EpochQueryError BlockHash)
getFirstBlockEpoch SpecifiedEpoch{..} = MVR $ \mvr -> do
    versions <- readIORef $ mvVersions mvr
    case versions Vec.!? fromIntegral erGenesisIndex of
        Nothing -> return $ Left EQEFutureEpoch
        Just evc -> do
            let isCurrentVersion = fromIntegral erGenesisIndex == Vec.length versions
            liftSkovQuery
                mvr
                evc
                ( do
                    -- Consensus version 0
                    getFirstFinalizedOfEpoch (Left erEpoch) <&> \case
                        Left FutureEpoch
                            | isCurrentVersion -> Left EQEFutureEpoch
                            | otherwise -> Left EQEInvalidEpoch
                        Left EmptyEpoch -> Left EQEBlockNotFound
                        Right block -> Right $! getHash @BlockHash block
                )
                ( do
                    -- Consensus version 1
                    (SkovV1.getFirstFinalizedBlockOfEpoch (Left erEpoch) =<< get) <&> \case
                        Nothing
                            | isCurrentVersion -> Left EQEFutureEpoch
                            | otherwise -> Left EQEInvalidEpoch
                        Just block -> Right $! getHash @BlockHash block
                )
getFirstBlockEpoch (EpochOfBlock blockInput) = do
    versions <- MVR $ readIORef . mvVersions
    let curVI = fromIntegral (Vec.length versions - 1)
    unBHIResponse <$> liftSkovQueryBHIAndVersion (epochOfBlockV0 curVI) (epochOfBlockV1 curVI) blockInput
  where
    unBHIResponse BQRNoBlock = Left EQEBlockNotFound
    unBHIResponse (BQRBlock _ res) = res
    epochOfBlockV0 curVersionIndex vc b =
        getFirstFinalizedOfEpoch (Right b) <&> \case
            Left FutureEpoch
                | vc0Index vc == curVersionIndex -> Left EQEFutureEpoch
                | otherwise -> Left EQEInvalidEpoch
            Left EmptyEpoch -> Left EQEBlockNotFound
            Right epochBlock -> Right (getHash epochBlock)
    epochOfBlockV1 curVersionIndex vc b _ =
        (SkovV1.getFirstFinalizedBlockOfEpoch (Right b) =<< get) <&> \case
            Nothing
                | vc1Index vc == curVersionIndex -> Left EQEFutureEpoch
                | otherwise -> Left EQEInvalidEpoch
            Just epochBlock -> Right (getHash epochBlock)

-- | Get the list of bakers that won the lottery in a particular historical epoch (i.e. the last
--  finalized block is in a later epoch). This lists the winners for each round in the epoch,
--  starting from the round after the last block in the previous epoch, running to the round before
--  the first block in the next epoch. It also indicates if a block in each round was included in
--  the finalized chain.
--  The epoch can be specified directly by the genesis index and epoch number, or indirectly by a
--  'BlockHashInput' that references a block.
--
--  The following failure cases apply:
--    * If the input is a 'BlockHashInput' and the specified block cannot be resolved, this returns
--      'EQEBlockNotFound'.
--    * If the specified genesis index is greater than the current one, returns 'EQEFutureEpoch'.
--    * If the specified genesis index is on consensus version 0, returns 'EQEInvalidGenesisIndex'.
--    * If the specified epoch not less than the epoch of the last finalized block,
--       - returns 'EQEFutureEpoch' if the genesis index is the current one
--       - returns 'EQEInvalidEpoch' otherwise.
getWinningBakersEpoch ::
    forall finconf.
    EpochRequest ->
    MVR finconf (Either EpochQueryError [WinningBaker])
getWinningBakersEpoch SpecifiedEpoch{..} = MVR $ \mvr -> do
    versions <- readIORef $ mvVersions mvr
    case versions Vec.!? fromIntegral erGenesisIndex of
        Nothing -> return $ Left EQEFutureEpoch
        Just evc -> do
            liftSkovQuery
                mvr
                evc
                (return (Left EQEInvalidGenesisIndex))
                ( do
                    let isCurrentVersion = fromIntegral erGenesisIndex == Vec.length versions - 1
                    mwbs <- ConsensusV1.getWinningBakersForEpoch erEpoch =<< get
                    return $! case mwbs of
                        Nothing
                            | isCurrentVersion -> Left EQEFutureEpoch
                            | otherwise -> Left EQEInvalidEpoch
                        Just wbs -> Right wbs
                )
getWinningBakersEpoch (EpochOfBlock blockInput) = do
    versions <- MVR $ readIORef . mvVersions
    let curVersionIndex = fromIntegral (Vec.length versions - 1)
    res <-
        liftSkovQueryBHIAndVersion
            (\_ _ -> return (Left EQEInvalidGenesisIndex))
            ( \vc b _ -> do
                mwbs <- ConsensusV1.getWinningBakersForEpoch (SkovV1.blockEpoch b) =<< get
                return $! case mwbs of
                    Nothing
                        | vc1Index vc == curVersionIndex -> Left EQEFutureEpoch
                        | otherwise -> Left EQEInvalidEpoch
                    Just wbs -> Right wbs
            )
            blockInput
    return $! case res of
        BQRNoBlock -> Left EQEBlockNotFound
        BQRBlock _ r -> r

-- ** Transaction indexed

-- | Get the status of a transaction specified by its hash.
getTransactionStatus :: forall finconf. TransactionHash -> MVR finconf (Maybe TransactionStatus)
getTransactionStatus trHash =
    liftSkovQueryLatestResult
        ( queryTransactionStatus trHash >>= \case
            Nothing -> return Nothing
            Just (TS.Live TT.Received{}) -> return $ Just Received
            Just (TS.Live TT.Committed{..}) -> do
                outcomes <- forM (HM.toList tsResults) $ \(bh, idx) ->
                    resolveBlock bh >>= \case
                        Nothing -> return (bh, Nothing) -- should not happen
                        Just bp -> do
                            bs <- blockState bp
                            outcome <- BS.getTransactionOutcome bs idx
                            return (bh, outcome)
                return $ Just $ Committed (Map.fromList outcomes)
            Just TS.Finalized{..} ->
                resolveBlock ftsBlockHash >>= \case
                    Nothing -> return Nothing -- should not happen
                    Just bp -> do
                        bs <- blockState bp
                        outcome <- BS.getTransactionOutcome bs ftsFinResult
                        return $ Just $ Finalized ftsBlockHash outcome
        )
        ( do
            sd <- get
            SkovV1.lookupTransaction trHash sd >>= \case
                Nothing -> return Nothing
                Just (SkovV1.Live TT.Received{}) -> return $ Just Received
                Just (SkovV1.Live TT.Committed{..}) -> do
                    outcomes <- forM (HM.toList tsResults) $ \(bh, idx) ->
                        case SkovV1.getLiveBlock bh sd of
                            Nothing -> return (bh, Nothing) -- should not happen
                            Just bp -> do
                                bs <- blockState bp
                                outcome <- BS.getTransactionOutcome bs idx
                                return (bh, outcome)
                    return $ Just $ Committed (Map.fromList outcomes)
                Just (SkovV1.Finalized SkovV1.FinalizedTransactionStatus{..}) ->
                    SkovV1.getFinalizedBlockAtHeight ftsBlockHeight >>= \case
                        Nothing -> return Nothing -- should not happen
                        Just bp -> do
                            bs <- blockState bp
                            outcome <- BS.getTransactionOutcome bs ftsIndex
                            return $ Just $ Finalized (getHash bp) outcome
        )

-- | Get the status of a transaction within a particular block.
--
--  Note that, since this does not acquire the write lock, it is possible that
--  'queryTransactionStatus' reports that the transaction is finalized even if it does not occur in
--  any block currently visible in the state.
getTransactionStatusInBlock :: TransactionHash -> BlockHash -> MVR finconf BlockTransactionStatus
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
                Just (TS.Live TT.Received{}) -> return $ Just BTSReceived
                Just (TS.Live TT.Committed{..}) -> case HM.lookup blockHash tsResults of
                    Nothing -> return $ Just BTSNotInBlock
                    Just idx ->
                        resolveBlock blockHash >>= \case
                            Nothing -> return $ Just BTSNotInBlock -- should not happen
                            Just bp -> do
                                bs <- blockState bp
                                outcome <- BS.getTransactionOutcome bs idx
                                return $ Just $ BTSCommitted outcome
                Just TS.Finalized{..} ->
                    if ftsBlockHash == blockHash
                        then
                            resolveBlock blockHash >>= \case
                                Nothing -> return $ Just BTSNotInBlock -- unlikely but possible
                                Just bp -> do
                                    bs <- blockState bp
                                    outcome <- BS.getTransactionOutcome bs ftsFinResult
                                    return $ Just $ BTSFinalized outcome
                        else return $ Just BTSNotInBlock
            )
            ( do
                sd <- get
                SkovV1.lookupTransaction trHash sd >>= \case
                    Nothing ->
                        SkovV1.getRecentBlockStatus blockHash sd >>= \case
                            -- If the block is unknown in this skov version, then try earlier versions.
                            SkovV1.RecentBlock SkovV1.BlockUnknown -> return Nothing
                            -- If the block is known, then we can return BTSNotInBlock already.
                            _ -> return $ Just BTSNotInBlock
                    Just (SkovV1.Live TT.Received{}) -> return $ Just BTSReceived
                    Just (SkovV1.Live TT.Committed{..}) -> case HM.lookup blockHash tsResults of
                        Nothing -> return $ Just BTSNotInBlock
                        Just idx ->
                            case SkovV1.getLiveBlock blockHash sd of
                                Nothing -> return $ Just BTSNotInBlock -- should not happen
                                Just bp -> do
                                    bs <- blockState bp
                                    outcome <- BS.getTransactionOutcome bs idx
                                    return $ Just $ BTSCommitted outcome
                    Just (SkovV1.Finalized SkovV1.FinalizedTransactionStatus{..}) ->
                        SkovV1.getFinalizedBlockAtHeight ftsBlockHeight >>= \case
                            Just bp
                                | getHash bp == blockHash -> do
                                    bs <- blockState bp
                                    outcome <- BS.getTransactionOutcome bs ftsIndex
                                    return $ Just $ BTSFinalized outcome
                            _ -> return $ Just BTSNotInBlock
            )

-- * Smart contract invocations
invokeContract :: BlockHashInput -> InvokeContract.ContractContext -> MVR finconf (BHIQueryResponse InvokeContract.InvokeContractResult)
invokeContract bhi cctx =
    liftSkovQueryBHI
        ( \bp -> do
            bs <- blockState bp
            cm <- ChainMetadata <$> getSlotTimestamp (blockSlot bp)
            InvokeContract.invokeContract cctx cm bs
        )
        ( \bp -> do
            bs <- blockState bp
            let cm = ChainMetadata (SkovV1.blockTimestamp bp)
            InvokeContract.invokeContract cctx cm bs
        )
        bhi

-- * Miscellaneous

-- | Check whether the node is currently a member of the finalization committee.
checkIsCurrentFinalizer :: MVR finconf Bool
checkIsCurrentFinalizer =
    liftSkovQueryLatest isFinalizationCommitteeMember ConsensusV1.isCurrentFinalizer

-- | Check whether consensus has been shut down
checkIsShutDown :: MVR finconf Bool
checkIsShutDown = liftSkovQueryLatest isShutDown ConsensusV1.isShutDown

-- | Result of a baker status query.
data BakerStatus
    = -- | The baker is a member of the current committee
      ActiveInComittee
    | -- | The account has a baker, but it is not yet in the committee
      AddedButNotActiveInCommittee
    | -- | The baker id does not correspond with a current baker
      NotInCommittee
    | -- | The baker may exist, but the keys do not match
      AddedButWrongKeys
    deriving (Eq, Ord, Show)

-- | Determine the status and lottery power of the baker with respect to the current best block.
getBakerStatusBestBlock :: MVR finconf (BakerStatus, Maybe BakerId, Maybe Double)
getBakerStatusBestBlock =
    asks mvBaker >>= \case
        Nothing -> return (NotInCommittee, Nothing, Nothing)
        Just Baker{bakerIdentity = bakerIdent} ->
            liftSkovQueryLatest
                ( do
                    bb <- bestBlock
                    bs <- queryBlockState bb
                    bakers <- BS.getCurrentEpochBakers bs
                    (bakerStatus, bakerLotteryPower) <- case fullBaker bakers (bakerId bakerIdent) of
                        Just fbinfo -> return $! currentBakerStatus bakerIdent bakers fbinfo
                        Nothing -> do
                            -- Not a current baker
                            status <- bakerAccountStatus bakerIdent bs
                            return (status, Nothing)
                    return (bakerStatus, Just $ bakerId bakerIdent, bakerLotteryPower)
                )
                ( do
                    bakers <- gets (SkovV1._bfBakers . SkovV1.bakersForCurrentEpoch)
                    (bakerStatus, bakerLotteryPower) <- case fullBaker bakers (bakerId bakerIdent) of
                        Just fbinfo -> return $! currentBakerStatus bakerIdent bakers fbinfo
                        Nothing -> do
                            -- Not a current baker.
                            lfb <- bestBlockConsensusV1
                            bs <- blockState lfb
                            status <- bakerAccountStatus bakerIdent bs
                            return (status, Nothing)
                    return (bakerStatus, Just $ bakerId bakerIdent, bakerLotteryPower)
                )
  where
    currentBakerStatus bakerIdent bakers fbinfo = (status, Just bakerLotteryPower)
      where
        status =
            if validateBakerKeys (fbinfo ^. bakerInfo) bakerIdent
                then -- Current baker with valid keys
                    ActiveInComittee
                else -- Current baker, but invalid keys
                    AddedButWrongKeys
        bakerLotteryPower = fromIntegral (fbinfo ^. bakerStake) / fromIntegral (bakerTotalStake bakers)
    bakerAccountStatus bakerIdent bs =
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

-- | Get the total number of non-finalized transactions across all accounts.
getNumberOfNonFinalizedTransactions :: MVR finconf Int
getNumberOfNonFinalizedTransactions =
    liftSkovQueryLatest
        queryNumberOfNonFinalizedTransactions
        (use (SkovV1.transactionTable . to TT.getNumberOfNonFinalizedTransactions))

-- | Errors that can occur when querying for block certificates.
data BlockCertificatesError
    = -- | This error indicates that the query was run against a protocol version that
      --  does not support 'ConsensusV1'.
      BlockCertificatesInvalidProtocolVersion

-- | Get the certificates for the block requested.
--  For 'ConsensusV0' this returns @BlockCertificatesInvalidProtocolVersion@ and for genesis blocks in 'ConsensusV1'
--  the function returns a 'QueriesKonsensusV1.BlockCertificates' with empty values.
getBlockCertificates :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (Either BlockCertificatesError QueriesKonsensusV1.BlockCertificates))
getBlockCertificates = liftSkovQueryBHI (\_ -> return $ Left BlockCertificatesInvalidProtocolVersion) (fmap Right . getCertificates)
  where
    getCertificates ::
        forall m.
        ( BS.BlockStateQuery m,
          BlockPointerMonad m,
          BlockPointerType m ~ SkovV1.BlockPointer (MPV m),
          IsConsensusV1 (MPV m)
        ) =>
        SkovV1.BlockPointer (MPV m) ->
        m QueriesKonsensusV1.BlockCertificates
    getCertificates bp =
        case SkovV1.bpBlock bp of
            SkovV1.GenesisBlock{} -> return emptyBlockCertificates
            SkovV1.NormalBlock b -> do
                bs <- blockState bp
                finCommitteeParams <- BS.getCurrentEpochFinalizationCommitteeParameters bs
                bakers <- BS.getCurrentEpochBakers bs
                let finalizationCommittee = ConsensusV1.computeFinalizationCommittee bakers finCommitteeParams
                let SkovV1.BakedBlock{..} = SkovV1.sbBlock b
                return
                    QueriesKonsensusV1.BlockCertificates
                        { bcQuorumCertificate = Just . mkQuorumCertificateOut finalizationCommittee $ bbQuorumCertificate,
                          bcTimeoutCertificate = mkTimeoutCertificateOut finalizationCommittee bbTimeoutCertificate,
                          bcEpochFinalizationEntry = mkEpochFinalizationEntryOut finalizationCommittee bbEpochFinalizationEntry
                        }
    emptyBlockCertificates = QueriesKonsensusV1.BlockCertificates Nothing Nothing Nothing
    -- Get the baker ids (in ascending order) of the finalizers present
    -- in the provided finalizer set.
    finalizerSetToBakerIds :: SkovV1.FinalizationCommittee -> SkovV1.FinalizerSet -> [BakerId]
    finalizerSetToBakerIds committee signatories =
        [ finalizerBakerId
          | SkovV1.FinalizerInfo{..} <- Vec.toList $ SkovV1.committeeFinalizers committee,
            SkovV1.memberFinalizerSet finalizerIndex signatories
        ]
    finalizerRound :: SkovV1.FinalizationCommittee -> SkovV1.FinalizerRounds -> [QueriesKonsensusV1.FinalizerRound]
    finalizerRound committee rounds =
        map
            ( \(r, finSet) ->
                QueriesKonsensusV1.FinalizerRound
                    { frRound = r,
                      frFinalizers = finalizerSetToBakerIds committee finSet
                    }
            )
            (SkovV1.finalizerRoundsList rounds)
    mkQuorumCertificateOut :: SkovV1.FinalizationCommittee -> SkovV1.QuorumCertificate -> QueriesKonsensusV1.QuorumCertificate
    mkQuorumCertificateOut committee qc =
        QueriesKonsensusV1.QuorumCertificate
            { qcBlock = SkovV1.qcBlock qc,
              qcRound = SkovV1.qcRound qc,
              qcEpoch = SkovV1.qcEpoch qc,
              qcAggregateSignature = QueriesKonsensusV1.QuorumCertificateSignature . (SkovV1.theQuorumSignature . SkovV1.qcAggregateSignature) $ qc,
              qcSignatories = finalizerSetToBakerIds committee (SkovV1.qcSignatories qc)
            }
    mkTimeoutCertificateOut :: SkovV1.FinalizationCommittee -> Option SkovV1.TimeoutCertificate -> Maybe QueriesKonsensusV1.TimeoutCertificate
    mkTimeoutCertificateOut _ Absent = Nothing
    mkTimeoutCertificateOut committee (Present tc) =
        Just $
            QueriesKonsensusV1.TimeoutCertificate
                { tcRound = SkovV1.tcRound tc,
                  tcMinEpoch = SkovV1.tcMinEpoch tc,
                  tcFinalizerQCRoundsFirstEpoch = finalizerRound committee $ SkovV1.tcFinalizerQCRoundsFirstEpoch tc,
                  tcFinalizerQCRoundsSecondEpoch = finalizerRound committee $ SkovV1.tcFinalizerQCRoundsSecondEpoch tc,
                  tcAggregateSignature = QueriesKonsensusV1.TimeoutCertificateSignature . (SkovV1.theTimeoutSignature . SkovV1.tcAggregateSignature) $ tc
                }
    mkEpochFinalizationEntryOut :: SkovV1.FinalizationCommittee -> Option (SkovV1.FinalizationEntry pv) -> Maybe QueriesKonsensusV1.EpochFinalizationEntry
    mkEpochFinalizationEntryOut _ Absent = Nothing
    mkEpochFinalizationEntryOut committee (Present SkovV1.FinalizationEntry{..}) =
        Just $
            QueriesKonsensusV1.EpochFinalizationEntry
                { efeFinalizedQC = mkQuorumCertificateOut committee feFinalizedQuorumCertificate,
                  efeSuccessorQC = mkQuorumCertificateOut committee feSuccessorQuorumCertificate,
                  efeSuccessorProof = QueriesKonsensusV1.SuccessorProof $ SkovV1.theSuccessorProof feSuccessorProof
                }

-- | Error type for querying 'BakerRewardPeriodInfo' for some block.
data GetBakersRewardPeriodError
    = -- | The block is from a protocol version without delegators.
      GBRPUnsupportedProtocolVersion

-- | Get a list of 'BakerRewardPeriodInfo' associated with the reward period
--  for a particular block.
getBakersRewardPeriod :: forall finconf. BlockHashInput -> MVR finconf (BHIQueryResponse (Either GetBakersRewardPeriodError [BakerRewardPeriodInfo]))
getBakersRewardPeriod = liftSkovQueryBHI bakerRewardPeriodInfosV0 bakerRewardPeriodInfosV1
  where
    bakerRewardPeriodInfosV0 ::
        forall m.
        ( SkovQueryMonad m,
          BlockPointerType m ~ PersistentBlockPointer (MPV m) (HashedPersistentBlockState (MPV m))
        ) =>
        BlockPointerType (VersionedSkovV0M finconf (MPV m)) ->
        m (Either GetBakersRewardPeriodError [BakerRewardPeriodInfo])
    bakerRewardPeriodInfosV0 bp = case delegationSupport @(AccountVersionFor (MPV m)) of
        -- The protocol version does not support the delegation feature.
        SAVDelegationNotSupported -> return $ Left GBRPUnsupportedProtocolVersion
        SAVDelegationSupported -> do
            result <- getBakersConsensusV0 =<< blockState bp
            return $ Right result
    bakerRewardPeriodInfosV1 ::
        forall m.
        (BS.BlockStateQuery m, IsConsensusV1 (MPV m), BlockPointerMonad m, BlockPointerType m ~ SkovV1.BlockPointer (MPV m)) =>
        SkovV1.BlockPointer (MPV m) ->
        m (Either GetBakersRewardPeriodError [BakerRewardPeriodInfo])
    bakerRewardPeriodInfosV1 bp = do
        result <- getBakersConsensusV1 =<< blockState bp
        return $ Right result
    -- Get the bakers and calculate the finalization committee for protocols using consensus v0.
    getBakersConsensusV0 :: (SkovQueryMonad m, PVSupportsDelegation (MPV m)) => BlockState m -> m [BakerRewardPeriodInfo]
    getBakersConsensusV0 bs = do
        bakers <- BS.getCurrentEpochBakers bs
        finalizationParameters <- genesisFinalizationParameters . _gcCore <$> getGenesisData
        totalCCD <- rsTotalAmount <$> BS.getRewardStatus bs
        let finalizationCommittee = makeFinalizationCommittee finalizationParameters totalCCD bakers
        mapBakersToInfos bs (Vec.toList $ fullBakerInfos bakers) (partyBakerId <$> Vec.toList (parties finalizationCommittee))
    -- Get the bakers and calculate the finalization committee for protocols using consensus v1.
    getBakersConsensusV1 :: (BS.BlockStateQuery m, IsConsensusV1 (MPV m)) => BlockState m -> m [BakerRewardPeriodInfo]
    getBakersConsensusV1 bs = do
        bakers <- BS.getCurrentEpochBakers bs
        finCommitteeParams <- BS.getCurrentEpochFinalizationCommitteeParameters bs
        let finalizationCommittee = ConsensusV1.computeFinalizationCommittee bakers finCommitteeParams
        mapBakersToInfos bs (Vec.toList $ fullBakerInfos bakers) (SkovV1.finalizerBakerId <$> Vec.toList (SkovV1.committeeFinalizers finalizationCommittee))
    -- Map bakers to their associated 'BakerRewardPeriodInfo'.
    -- The supplied bakers and list of baker ids (of the finalization committee) MUST
    -- be sorted in ascending order of their baker id.
    -- Returns a list of BakerRewardPeriodInfo's in ascending order of the baker id.
    mapBakersToInfos ::
        ( BS.BlockStateQuery m,
          PVSupportsDelegation (MPV m)
        ) =>
        -- The block state to request the pool status from.
        BlockState m ->
        -- All bakers for the reward period.
        [FullBakerInfo] ->
        -- The baker ids of the finalizers for the reward period.
        [BakerId] ->
        m [BakerRewardPeriodInfo]
    mapBakersToInfos bs fullBakerInfos finalizersByBakerId = reverse . fst <$> foldM mapBaker ([], finalizersByBakerId) fullBakerInfos
      where
        -- No finalizers left to pick off, so this baker must only be
        -- member of the baking committee.
        mapBaker (acc, []) baker = do
            info <- toBakerRewardPeriodInfo False bs baker
            return (info : acc, [])
        -- Check whether the baker id of the finalizer candidate matches the
        -- bakers id. If this is the case then the baker is member of the finalization committee and
        -- otherwise not.
        mapBaker (acc, finalizers@(candidate : remaining)) baker = do
            let isFinalizer = (baker ^. theBakerInfo . to _bakerIdentity) == candidate
            info <- toBakerRewardPeriodInfo isFinalizer bs baker
            return (info : acc, if isFinalizer then remaining else finalizers)
    -- Map the baker to a 'BakerRewardPeriodInfo'.
    toBakerRewardPeriodInfo ::
        (PVSupportsDelegation (MPV m), BS.BlockStateQuery m) =>
        -- \|Whether the baker is a finalizer.
        Bool ->
        -- \|The block state
        BlockState m ->
        -- \|Baker information.
        FullBakerInfo ->
        m BakerRewardPeriodInfo
    toBakerRewardPeriodInfo isFinalizer bs FullBakerInfo{..} = do
        let bakerId = _bakerIdentity _theBakerInfo
        BS.getPoolStatus bs bakerId >>= \case
            Nothing -> error "A pool for a known baker could not be looked up."
            Just bps
                | Just CurrentPaydayBakerPoolStatus{..} <- psCurrentPaydayStatus bps -> do
                    return
                        BakerRewardPeriodInfo
                            { brpiBaker = _theBakerInfo,
                              brpiEffectiveStake = _bakerStake,
                              brpiCommissionRates = bpsCommissionRates,
                              brpiEquityCapital = bpsBakerEquityCapital,
                              brpiDelegatedCapital = bpsDelegatedCapital,
                              brpiIsFinalizer = isFinalizer
                            }
                | otherwise ->
                    error "The current payday status for a known baker could not be looked up."

-- | Get the earliest time at which a baker is projected to win the lottery.
--  Returns 'Nothing' for consensus version 0.
getBakerEarliestWinTime :: BakerId -> MVR finconf (Maybe Timestamp)
getBakerEarliestWinTime bid =
    liftSkovQueryLatest
        (return Nothing)
        (fmap Just . ConsensusV1.bakerEarliestWinTimestamp bid =<< get)
