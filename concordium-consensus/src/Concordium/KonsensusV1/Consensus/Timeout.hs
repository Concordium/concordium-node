{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Timeout where

import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro.Platform

import Concordium.Genesis.Data.BaseV1
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Grow the current timeout duration in response to an elapsed timeout.
-- This updates the timeout to @timeoutIncrease * oldTimeout@.
growTimeout ::
    ( BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      BlockStateQuery m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |Block to take the timeout parameters from
    BlockPointer (MPV m) ->
    m ()
growTimeout blockPtr = do
    chainParams <- getChainParameters $ bpState blockPtr
    let timeoutIncrease =
            chainParams
                ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
    currentTimeout %=! \oldCurrentTimeout ->
        let timeoutIncreaseRational = toRational timeoutIncrease
            currentTimeOutRational = toRational oldCurrentTimeout
            newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational
            newCurrentTimeout = floor newCurrentTimeoutRational
        in  Duration newCurrentTimeout

-- |This is 'uponTimeoutEvent' from the bluepaper. If a timeout occurs, a finalizers should call this function to
-- generate, send out a timeout message and process it.
-- NB: If the caller is not a finalizer, this function does nothing.
uponTimeoutEvent ::
    ( MonadTimeout m,
      MonadMulticast m,
      MonadReader r m,
      HasBakerContext r,
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    m ()
uponTimeoutEvent = do
    maybeBakerIdentity <- view bakerIdentity
    forM_ maybeBakerIdentity $ \BakerIdentity{..} -> do
        eBakers <- use skovEpochBakers

        let finComm = eBakers ^. currentEpochBakers . bfFinalizers
        let maybeFinalizer = finalizerByBakerId finComm bakerId

        forM_ maybeFinalizer $ \finInfo -> do
            lastFinBlockPtr <- use lastFinalized
            growTimeout lastFinBlockPtr

            genesisHash <- use currentGenesisHash
            currentRoundStatus <- use roundStatus
            let curRound = _rsCurrentRound currentRoundStatus
            let highestQC = _rsHighestQC currentRoundStatus

            let timeoutSigMessage =
                    TimeoutSignatureMessage
                        { tsmGenesis = genesisHash,
                          tsmRound = curRound,
                          tsmQCRound = qcRound highestQC,
                          tsmQCEpoch = qcEpoch highestQC
                        }
            let timeoutSig = signTimeoutSignatureMessage timeoutSigMessage bakerAggregationKey

            let timeoutMessageBody =
                    TimeoutMessageBody
                        { tmFinalizerIndex = finalizerIndex finInfo,
                          tmRound = curRound,
                          tmEpoch = _currentEpoch eBakers,
                          tmQuorumCertificate = highestQC,
                          tmAggregateSignature = timeoutSig
                        }
            let timeoutMessage = signTimeoutMessage timeoutMessageBody genesisHash bakerSignKey
            setRoundStatus $!
                currentRoundStatus
                    & rsLastSignedTimeoutMessage .~ Present timeoutMessage
            sendTimeoutMessage timeoutMessage
            processTimeout timeoutMessage

-- |Add a 'TimeoutMessage' to an existing set of timeout messages. Returns 'Nothing' if there is
-- no change (i.e. the new message was from an epoch that is too early).
-- The supplied timeout messages can be 'Absent' (i.e. there are no messages), but the return value
-- is only 'Nothing' when there is no change: if the timeout messages are updated, they will
-- always be non-empty (in particular, including the new message).
updateTimeoutMessages ::
    Option TimeoutMessages ->
    TimeoutMessage ->
    Maybe TimeoutMessages
updateTimeoutMessages tms tm =
    case tms of
        Absent ->
            Just $
                TimeoutMessages
                    { tmFirstEpoch = epoch,
                      tmFirstEpochTimeouts = singletonTimeout,
                      tmSecondEpochTimeouts = Map.empty
                    }
        Present TimeoutMessages{..}
            | epoch == tmFirstEpoch ->
                Just $
                    TimeoutMessages
                        { tmFirstEpochTimeouts = insertTimeout tmFirstEpochTimeouts,
                          ..
                        }
            | epoch == tmFirstEpoch + 1 ->
                Just $
                    TimeoutMessages
                        { tmSecondEpochTimeouts = insertTimeout tmSecondEpochTimeouts,
                          ..
                        }
            | epoch + 1 == tmFirstEpoch && null tmSecondEpochTimeouts ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = epoch,
                          tmFirstEpochTimeouts = singletonTimeout,
                          tmSecondEpochTimeouts = tmFirstEpochTimeouts
                        }
            | epoch == tmFirstEpoch + 2 ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = tmFirstEpoch + 1,
                          tmFirstEpochTimeouts = tmSecondEpochTimeouts,
                          tmSecondEpochTimeouts = singletonTimeout
                        }
            | epoch > tmFirstEpoch + 2 ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = epoch,
                          tmFirstEpochTimeouts = singletonTimeout,
                          tmSecondEpochTimeouts = Map.empty
                        }
            | otherwise -> Nothing
  where
    epoch = tmEpoch $ tmBody tm
    finIndex = tmFinalizerIndex $ tmBody tm
    singletonTimeout = Map.singleton finIndex tm
    insertTimeout = Map.insert finIndex tm

-- |Process a timeout message. This stores the timeout, and makes sure the stored timeout messages
-- do not span more than 2 epochs. If enough timeout messages are stored, we form a timeout certificate and
-- advance round.
--
-- Precondition:
-- * The given 'TimeoutMessage' is valid and has already been checked.
processTimeout ::
    ( MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m
    ) =>
    TimeoutMessage ->
    m ()
processTimeout tm = do
    currentTimeoutMessages <- use receivedTimeoutMessages
    currentRoundStatus <- use roundStatus
    let highestQC = _rsHighestQC currentRoundStatus
    -- Add the new timeout message to the current messages.
    -- If the result is 'Nothing', then there was no change as a result, so nothing left to do.
    let maybeNewTimeoutMessages = updateTimeoutMessages currentTimeoutMessages tm
    forM_ maybeNewTimeoutMessages $ \newTimeoutMessages@TimeoutMessages{..} -> do
        receivedTimeoutMessages .=! Present newTimeoutMessages
        eBakers <- use skovEpochBakers
        let getFinalizersForEpoch epoch = (^. bfFinalizers) <$> getBakersForLiveEpoch epoch eBakers
        -- We should not fail to get the finalizers for the epoch of the highest QC, because it
        -- should either be the current epoch or the previous one.
        let maybeFinComm = getFinalizersForEpoch (qcEpoch highestQC)
        forM_ maybeFinComm $ \finCommQC -> do
            -- The baker IDs of the finalizers who have signed in the first epoch.
            let firstBakerIds
                    | Just firstFinComm <- getFinalizersForEpoch tmFirstEpoch =
                        bakerIdsFor firstFinComm tmFirstEpochTimeouts
                    | otherwise = Set.empty
            -- The baker IDs of the finalizers who have signed in the second epoch.
            let secondBakerIds
                    | not (null tmSecondEpochTimeouts),
                      Just secondFinComm <- getFinalizersForEpoch (tmFirstEpoch + 1) =
                        bakerIdsFor secondFinComm tmSecondEpochTimeouts
                    | otherwise = Set.empty
            -- Compute the voter power in the epoch of the highest QC for a baker by the baker ID.
            let getBakerVoterPower = fmap finalizerWeight . finalizerByBakerId finCommQC
            let voterPowerSum =
                    sum . mapMaybe getBakerVoterPower $
                        Set.toList (firstBakerIds `Set.union` secondBakerIds)
            -- TODO: Factor out finalizer weight check.
            let totalWeightRational = toRational $ committeeTotalWeight finCommQC
            genesisSigThreshold <- toRational . genesisSignatureThreshold . gmParameters <$> use genesisMetadata
            let voterPowerSumRational = toRational voterPowerSum
            when (voterPowerSumRational / totalWeightRational >= genesisSigThreshold) $ do
                let currentRound = _rsCurrentRound currentRoundStatus
                let tc = makeTimeoutCertificate currentRound newTimeoutMessages
                advanceRound (currentRound + 1) (Left (tc, highestQC))
  where
    bakerIdsFor finComm timeouts =
        Set.fromList $
            mapMaybe
                (fmap finalizerBakerId . finalizerByIndex finComm)
                (Map.keys timeouts)

-- |Make a 'TimeoutCertificate' from a 'TimeoutMessages'.
--
-- NB: It is not checked whether enough timeout messages are present.
-- This should be checked before calling 'makeTimeoutCertificate'.
makeTimeoutCertificate :: Round -> TimeoutMessages -> TimeoutCertificate
makeTimeoutCertificate currentRound TimeoutMessages{..} =
    TimeoutCertificate
        { tcRound = currentRound,
          tcMinEpoch = tmFirstEpoch,
          tcFinalizerQCRoundsFirstEpoch = toFinalizerRounds tmFirstEpochTimeouts,
          tcFinalizerQCRoundsSecondEpoch = toFinalizerRounds tmSecondEpochTimeouts,
          tcAggregateSignature =
            fold $ toAggSigs tmFirstEpochTimeouts ++ toAggSigs tmSecondEpochTimeouts
        }
  where
    -- Add an entry to a 'FinalizerRounds' map for a timeout message.
    foldHelper :: Map.Map Round FinalizerSet -> FinalizerIndex -> TimeoutMessage -> Map.Map Round FinalizerSet
    foldHelper finRounds finIndex tm =
        finRounds
            & at' roundOfQC . non emptyFinalizerSet %~ flip addFinalizer finIndex
      where
        roundOfQC = qcRound $ tmQuorumCertificate $ tmBody tm
    -- Convert timeout messages to 'FinalizerRounds'.
    toFinalizerRounds :: Map.Map FinalizerIndex TimeoutMessage -> FinalizerRounds
    toFinalizerRounds = FinalizerRounds . Map.foldlWithKey' foldHelper Map.empty
    -- Convert timeout messages to lists of signatures.
    toAggSigs :: Map.Map FinalizerIndex TimeoutMessage -> [TimeoutSignature]
    toAggSigs = fmap (tmAggregateSignature . tmBody) . Map.elems
