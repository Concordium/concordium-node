{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader.Class
import Control.Monad.State
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import qualified Concordium.Types.Accounts as Accounts
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.TreeState as GSTypes
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.Types.SeedState (currentLeadershipElectionNonce, epochTransitionTriggered, triggerBlockTime)
import Concordium.Types.UpdateQueues
import Concordium.Types.Updates

-- |A Monad for broadcasting either a 'TimeoutMessage',
-- 'QuorumMessage' or a 'SignedBlock'.
class MonadBroadcast m where
    -- |Broadcast a 'TimeoutMessage'.
    sendTimeoutMessage :: TimeoutMessage -> m ()

    -- |Broadcast a 'QuorumMessage'.
    sendQuorumMessage :: QuorumMessage -> m ()

    -- |Broadcast a 'SignedBlock'.
    sendBlock :: SignedBlock -> m ()

-- |This class provides event handlers for consensus events. A runner should implement this to
-- handle these events.
class MonadConsensusEvent m where
    -- |Called when a block becomes live.
    onBlock :: BlockPointer (MPV m) -> m ()

    -- |Called when a block becomes finalized. This is called once per finalization with a list
    -- of all the blocks that are newly finalized.
    onFinalize ::
        -- |Finalization entry that establishes finalization.
        FinalizationEntry ->
        -- |List of the newly-finalized blocks by increasing height.
        [BlockPointer (MPV m)] ->
        m ()

    -- |Called when a previously pending block becomes live. This should be used to trigger sending
    -- a catch-up status message to all (non-pending) peers, since they may not be aware of the
    -- block as it was not relayed when first received.
    onPendingLive :: m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: Maybe BakerIdentity
    }

makeClassy ''BakerContext

-- |A Monad for timer related actions.
class MonadTimeout m where
    -- |Reset the timeout from the supplied 'Duration'.
    resetTimer :: Duration -> m ()

-- |Call 'resetTimer' with the current timeout.
resetTimerWithCurrentTimeout :: (MonadTimeout m, MonadState (SkovData (MPV m)) m) => m ()
resetTimerWithCurrentTimeout = resetTimer =<< use (roundStatus . rsCurrentTimeout)

-- |Reset the timeout timer, and clear the collected quorum and timeout messages for the current
-- round. This should not be called directly, except by 'advanceRoundWithTimeout' and
-- 'advanceRoundWithQuorum'.
onNewRound ::
    ( MonadTimeout m,
      MonadState (SkovData (MPV m)) m
    ) =>
    m ()
onNewRound = do
    -- We always reset the timer.
    -- This ensures that the timer is correct for consensus runners which have been
    -- leaving or joining the finalization committee (if we're advancing to the first round
    -- of that new epoch)
    -- Hence it is crucial when throwing the timeout then it must be checked that
    -- the consensus runner is either part of the current epoch (i.e. the new one) OR
    -- the prior epoch, as it could be the case that the consensus runner left the finalization committee
    -- coming into this new (current) epoch - but we still want to ensure that a timeout is thrown either way.
    resetTimerWithCurrentTimeout
    -- Clear the quorum messages collected.
    currentQuorumMessages .= emptyQuorumMessages
    -- Clear the timeout messages collected.
    currentTimeoutMessages .= Absent

-- |Advance the round as the result of a timeout. This will also set the highest certified block
-- if the round timeout contains a higher one.
--
-- PRECONDITION:
--
-- * The round timeout MUST be for a round that is at least current round.
advanceRoundWithTimeout ::
    ( MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m
    ) =>
    RoundTimeout (MPV m) ->
    m ()
advanceRoundWithTimeout roundTimeout@RoundTimeout{..} = do
    onNewRound
    roundStatus %=! updateQC . updateTC . (rsRoundEligibleToBake .~ True)
    updatePersistentRoundStatus (prsLatestTimeout .~ Present rtTimeoutCertificate)
  where
    updateQC rs
        | cbRound (rs ^. rsHighestCertifiedBlock) < cbRound rtCertifiedBlock =
            rs & rsHighestCertifiedBlock .~ rtCertifiedBlock
        | otherwise = rs
    updateTC =
        (rsCurrentRound .~ 1 + tcRound rtTimeoutCertificate)
            . (rsPreviousRoundTimeout .~ Present roundTimeout)

-- |Advance the round as the result of a quorum certificate.
--
-- PRECONDITION:
--
-- * The certified block MUST be for a round that is at least the current round.
advanceRoundWithQuorum ::
    ( MonadTimeout m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |Certified block
    CertifiedBlock (MPV m) ->
    m ()
advanceRoundWithQuorum certBlock = do
    onNewRound
    roundStatus
        %=! (rsCurrentRound .~ 1 + qcRound (cbQuorumCertificate certBlock))
        . (rsPreviousRoundTimeout .~ Absent)
        . (rsHighestCertifiedBlock .~ certBlock)
        . (rsRoundEligibleToBake .~ True)
    -- We clear the latest timeout, but only in memory to avoid a database write.
    -- Having the old value present is not harmful, and this will remove it next time the
    -- persistent round status gets written.
    persistentRoundStatus . prsLatestTimeout .= Absent

-- |Update the highest certified block if the supplied block is for a later round than the previous
-- highest certified block.
checkedUpdateHighestCertifiedBlock ::
    (MonadState (SkovData (MPV m)) m) =>
    -- |Certified block
    CertifiedBlock (MPV m) ->
    m ()
checkedUpdateHighestCertifiedBlock newCB = do
    rs <- use roundStatus
    let isBetterQC = cbRound (rs ^. rsHighestCertifiedBlock) < cbRound newCB
    when isBetterQC $ roundStatus . rsHighestCertifiedBlock .= newCB

-- |Compute the finalization committee given the bakers and the finalization committee parameters.
computeFinalizationCommittee :: FullBakers -> FinalizationCommitteeParameters -> FinalizationCommittee
computeFinalizationCommittee FullBakers{..} FinalizationCommitteeParameters{..} =
    FinalizationCommittee{..}
  where
    -- We construct a map with the top bakers ordered by descending stake and ascending baker ID.
    -- The size of the map is limited to '_fcpMaxFinalizers'.
    insert ::
        Map.Map (Down Amount, BakerId) FullBakerInfo ->
        FullBakerInfo ->
        Map.Map (Down Amount, BakerId) FullBakerInfo
    insert m fbi
        | Map.size m == fromIntegral _fcpMaxFinalizers = case Map.maxViewWithKey m of
            Nothing -> error "computeFinalizationCommittee: _fcpMaxFinalizers must not be 0"
            Just ((k, _), m')
                | insKey < k -> Map.insert insKey fbi m'
                | otherwise -> m
        | otherwise = Map.insert insKey fbi m
      where
        insKey = (Down (fbi ^. bakerStake), fbi ^. Accounts.bakerIdentity)
    amountSortedBakers = Map.elems $ foldl' insert Map.empty fullBakerInfos
    -- Threshold stake required to be a finalizer
    finalizerAmountThreshold :: Amount
    finalizerAmountThreshold =
        ceiling $
            partsPerHundredThousandsToRational _fcpFinalizerRelativeStakeThreshold
                * toRational bakerTotalStake
    -- Given the bakers sorted by their stakes, takes the first 'n' and then those that are
    -- at least at the threshold.
    takeFinalizers 0 fs = takeWhile ((>= finalizerAmountThreshold) . view bakerStake) fs
    takeFinalizers n (f : fs) = f : takeFinalizers (n - 1) fs
    takeFinalizers _ [] = []
    -- Compute the set of finalizers by applying the caps.
    cappedFinalizers = takeFinalizers _fcpMinFinalizers amountSortedBakers
    -- Sort the finalizers by baker ID.
    sortedFinalizers = sortOn (view Accounts.bakerIdentity) cappedFinalizers
    -- Construct finalizer info given the index and baker info.
    mkFinalizer finalizerIndex bi =
        FinalizerInfo
            { finalizerWeight = fromIntegral (bi ^. bakerStake),
              finalizerSignKey = bi ^. Accounts.bakerSignatureVerifyKey,
              finalizerVRFKey = bi ^. Accounts.bakerElectionVerifyKey,
              finalizerBlsKey = bi ^. Accounts.bakerAggregationVerifyKey,
              finalizerBakerId = bi ^. Accounts.bakerIdentity,
              ..
            }
    committeeFinalizers = Vec.fromList $ zipWith mkFinalizer [FinalizerIndex 0 ..] sortedFinalizers
    committeeTotalWeight = sum $ finalizerWeight <$> committeeFinalizers

-- |Compute the finalization committee given the bakers and the finalization committee parameters,
-- returning a 'BakersAndFinalizers'.
computeBakersAndFinalizers :: FullBakers -> FinalizationCommitteeParameters -> BakersAndFinalizers
computeBakersAndFinalizers bakers fcp =
    BakersAndFinalizers
        { _bfBakers = bakers,
          _bfFinalizers = computeFinalizationCommittee bakers fcp
        }

-- |Get the baker identity and finalizer info if we are a finalizer in the specified epoch.
-- This checks that the signing key and aggregate signing key match those for the finalizer,
-- and will log a warning if they do not (instead of invoking the continuation).
withFinalizerForEpoch ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m,
      MonadLogger m
    ) =>
    Epoch ->
    (BakerIdentity -> FinalizerInfo -> m ()) ->
    m ()
withFinalizerForEpoch epoch cont = do
    mBakerIdentity <- view bakerIdentity
    forM_ mBakerIdentity $ \bakerIdent@BakerIdentity{..} -> do
        mBakers <- gets $ getBakersForEpoch epoch
        forM_ mBakers $ \BakersAndFinalizers{..} -> do
            forM_ (finalizerByBakerId _bfFinalizers bakerId) $ \ !finInfo ->
                if finalizerSignKey finInfo /= Sig.verifyKey bakerSignKey
                    || finalizerBlsKey finInfo /= bakerAggregationPublicKey
                    then do
                        logEvent
                            Konsensus
                            LLWarning
                            "Finalizer keys do not match the keys in the current committee."
                    else cont bakerIdent finInfo

-- |Determine if we are a finalizer in the current epoch.
isCurrentFinalizer ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m
    ) =>
    m Bool
isCurrentFinalizer =
    view bakerIdentity >>= \case
        Nothing -> return False
        Just BakerIdentity{..} -> do
            BakersAndFinalizers{..} <- gets bakersForCurrentEpoch
            return $ isJust $ finalizerByBakerId _bfFinalizers bakerId

-- |Determine if consensus is shut down.
isShutDown :: MonadState (SkovData (MPV m)) m => m Bool
isShutDown = use isConsensusShutdown

-- |The current state of the consensus with respect to the next protocol update.
data ProtocolUpdateState pv
    = -- |No protocol update is currently anticipated.
      ProtocolUpdateStateNone
    | -- |A protocol update is currently scheduled.
      ProtocolUpdateStateQueued
        { puQueuedTime :: !TransactionTime,
          puProtocolUpdate :: !ProtocolUpdate
        }
    | -- |A protocol update is effective at the end of the current epoch.
      ProtocolUpdateStatePendingEpoch
        { puTriggerTime :: !Timestamp,
          puProtocolUpdate :: !ProtocolUpdate
        }
    | -- |A protocol update has taken place and the consensus is shut down.
      ProtocolUpdateStateDone
        { puProtocolUpdate :: !ProtocolUpdate,
          puTerminalBlock :: !(BlockPointer pv)
        }

-- |Get the current protocol update state.
getProtocolUpdateState ::
    ( GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BS.BlockStateQuery m,
      MonadState (SkovData (MPV m)) m,
      IsConsensusV1 (MPV m)
    ) =>
    m (ProtocolUpdateState (MPV m))
getProtocolUpdateState = do
    st <- bpState <$> use lastFinalized
    BS.getProtocolUpdateStatus st >>= \case
        ProtocolUpdated pu ->
            use terminalBlock >>= \case
                -- The protocol update is now in effect.
                Present terminal ->
                    return $
                        ProtocolUpdateStateDone
                            { puProtocolUpdate = pu,
                              puTerminalBlock = terminal
                            }
                -- The protocol update is awaiting the terminal block of the epoch to be finalized.
                Absent -> do
                    ss <- BS.getSeedState st
                    return $
                        ProtocolUpdateStatePendingEpoch
                            { puTriggerTime = ss ^. triggerBlockTime,
                              puProtocolUpdate = pu
                            }
        PendingProtocolUpdates [] -> return ProtocolUpdateStateNone
        PendingProtocolUpdates ((ts, pu) : _) ->
            return $
                ProtocolUpdateStateQueued
                    { puQueuedTime = ts,
                      puProtocolUpdate = pu
                    }

-- |Project the earliest timestamp at which the given baker might be required to bake.
-- If the baker is not a baker in the current reward period, this will give a time at the start
-- of the next reward period.
bakerEarliestWinTimestamp ::
    ( GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BS.BlockStateQuery m,
      IsConsensusV1 (MPV m)
    ) =>
    BakerId ->
    SkovData (MPV m) ->
    m Timestamp
bakerEarliestWinTimestamp baker sd = do
    let lfBlock = sd ^. lastFinalized
    -- The bakers with respect to the epoch of the last finalized block.
    let fullBakers = sd ^. currentEpochBakers . bfBakers
    let epochDuration = genesisEpochDuration . gmParameters $ sd ^. genesisMetadata
    lfSeedState <- BS.getSeedState (bpState lfBlock)
    let lfTriggerTime = lfSeedState ^. triggerBlockTime
    chainParams <- BS.getChainParameters (bpState lfBlock)
    let minBlockTime = chainParams ^. cpConsensusParameters . cpMinBlockTime
    if null (fullBaker fullBakers baker)
        then do
            -- The baker is not a baker in the current payday, so take the start of the next payday
            -- as the soonest that the account could be a baker (and thus bake).
            let epochsTillPayday = (sd ^. nextPayday) - blockEpoch lfBlock - 1
            return $!
                addDuration
                    lfTriggerTime
                    (minBlockTime + fromIntegral epochsTillPayday * epochDuration)
        else do
            let findLeader nxtRound nxtTimestamp
                    | baker
                        == getLeaderFullBakers
                            fullBakers
                            (lfSeedState ^. currentLeadershipElectionNonce)
                            nxtRound
                            ^. Accounts.bakerIdentity =
                        nxtTimestamp
                    | nxtTimestamp >= lfTriggerTime = addDuration lfTriggerTime minBlockTime
                    | otherwise = findLeader (nxtRound + 1) (addDuration nxtTimestamp minBlockTime)
            let curRound = sd ^. roundStatus . rsCurrentRound
            return $!
                findLeader
                    curRound
                    ( addDuration
                        (blockTimestamp lfBlock)
                        (fromIntegral (curRound - blockRound lfBlock) * minBlockTime)
                    )
