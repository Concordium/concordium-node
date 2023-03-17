{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State

import Data.Bits
import Data.Ratio
import qualified Data.Set as Set
import Data.Word

import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import qualified Concordium.Types.Accounts as Accounts

import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils


-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message over the network
    sendTimeoutMessage :: TimeoutMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }
makeClassy ''BakerContext

-- |Get the private baker aggregation key.
getBakerAggSecretKey :: (MonadReader r m, HasBakerContext r) => m BakerAggregationPrivateKey
getBakerAggSecretKey = do
    bi <- view bakerIdentity
    return $ bakerAggregationKey bi

-- |Get the private baker sign key.
getBakerSignPrivateKey :: (MonadReader r m, HasBakerContext r) => m BakerSignPrivateKey
getBakerSignPrivateKey = do
    bi <- view bakerIdentity
    return $ bakerSignKey bi

-- |Get the baker id.
getBakerId :: (MonadReader r m, HasBakerContext r) => m BakerId
getBakerId = do
    bi <- view bakerIdentity
    return $ bakerId bi

-- |A Monad for timer related actions.
class MonadTimeout m where
    -- |Reset the timeout from the supplied 'Duration'.
    resetTimer :: Duration -> m ()

-- |Make a block if the consensus runner is leader for the
-- current round.
-- TODO: call 'makeBlock' if we're leader for the current round.
makeBlockIfLeader :: MonadState (SkovData (MPV m)) m => m ()
makeBlockIfLeader = return ()

-- |Advance the provided 'RoundStatus' to the provided 'Round'.
--
-- The consensus protocol can advance round in two ways.
-- 1. Via a QC i.e. @Right QuorumCertificate@
-- 2. Via a TC i.e. @Left (TimeoutCertificate, QuorumCertificate)@
--
-- All properties from the old 'RoundStatus' are being carried over to new 'RoundStatus'
-- except for the following.
-- * 'rsCurrentRound' will become the provided 'Round'.
-- * 'rsCurrentQuorumSignatureMessages' will be 'emptySignatureMessages'.
-- * 'rsCurrentTimeoutSignatureMessages' will be 'emptySignatureMessages'.
-- * 'rsPreviousRoundTC' will become 'Absent' if we're progressing via a 'QuorumCertificate' otherwise
--   it will become the values of the supplied @Left (TimeoutCertificate, QuorumCertificate)@.
-- * 'rsHighestQC' will become the supplied @Right QuorumCertificate@ otherwise it is carried over.
advanceRoundStatus ::
    -- |The round to advance to.
    Round ->
    -- |@Left (tc, qc)@ if consensus is advancing from a TC.
    -- @Right qc@ if consensus is advancing from a QC.
    Either (TimeoutCertificate, QuorumCertificate) QuorumCertificate ->
    -- |The 'RoundStatus' we are advancing from.
    RoundStatus ->
    -- |The advanced 'RoundStatus'.
    RoundStatus
advanceRoundStatus toRound (Left (tc, qc)) currentRoundStatus =
    currentRoundStatus
        { rsCurrentRound = toRound,
          rsCurrentQuorumSignatureMessages = emptySignatureMessages,
          rsCurrentTimeoutSignatureMessages = emptySignatureMessages,
          rsPreviousRoundTC = Present (tc, qc)
        }
advanceRoundStatus toRound (Right qc) currentRoundStatus =
    currentRoundStatus
        { rsCurrentRound = toRound,
          rsCurrentQuorumSignatureMessages = emptySignatureMessages,
          rsCurrentTimeoutSignatureMessages = emptySignatureMessages,
          rsHighestQC = qc,
          rsPreviousRoundTC = Absent
        }

-- |Advance to the provided 'Round'.
--
-- This function does the following:
-- * Update the current 'RoundStatus'.
-- * Persist the new 'RoundStatus'.
-- * If the consensus runner is leader in the new
--   round then make the new block.
advanceRound ::
    ( MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |The 'Round' to progress to.
    Round ->
    -- |If we are advancing from a round that timed out
    -- then this will be @Left 'TimeoutCertificate, 'QuorumCertificate')@
    -- The 'TimeoutCertificate' is from the round we're
    -- advancing from and the associated 'QuorumCertificate' verifies it.
    --
    -- Otherwise if we're progressing via a 'QuorumCertificate' then @Right QuorumCertificate@
    -- should be the QC we're advancing round via.
    Either (TimeoutCertificate, QuorumCertificate) QuorumCertificate ->
    m ()
advanceRound newRound newCertificate = do
    currentRoundStatus <- use roundStatus
    -- We always reset the timer.
    -- This ensures that the timer is correct for consensus runners which have been
    -- leaving or joining the finalization committee (if we're advancing to the first round
    -- of that new epoch)
    -- Hence it is crucial when throwing the timeout then it must be checked that
    -- the consensus runner is either part of the current epoch (i.e. the new one) OR
    -- the prior epoch, as it could be the case that the consensus runner left the finalization committee
    -- coming into this new (current) epoch - but we still want to ensure that a timeout is thrown either way.
    resetTimer =<< use currentTimeout
    -- Advance and save the round.
    setRoundStatus $! advanceRoundStatus newRound newCertificate currentRoundStatus
    -- Make a new block if the consensus runner is leader of
    -- the 'Round' progressed to.
    makeBlockIfLeader

-- |Advance the provided 'RoundStatus' to the provided 'Epoch'.
-- In particular this does the following to the provided 'RoundStatus'
--
-- * Set the 'rsCurrentEpoch' to the provided 'Epoch'
advanceRoundStatusEpoch ::
    -- |The 'Epoch' we advance to.
    Epoch ->
    -- |The 'RoundStatus' we're progressing from.
    RoundStatus ->
    -- |The new 'RoundStatus'.
    RoundStatus
advanceRoundStatusEpoch toEpoch currentRoundStatus =
    currentRoundStatus
        { rsCurrentEpoch = toEpoch
        }

-- |Advance the 'Epoch' of the current 'RoundStatus'.
--
-- Advancing epochs in particular carries out the following:
-- * Updates the 'rsCurrentEpoch' to the provided 'Epoch' for the current 'RoundStatus'.
-- * Persist the new 'RoundStatus' to disk.
advanceEpoch ::
    ( MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    Epoch ->
    m ()
advanceEpoch newEpoch = do
    currentRoundStatus <- use roundStatus
    let newRoundStatus = advanceRoundStatusEpoch newEpoch currentRoundStatus
    setRoundStatus newRoundStatus

-- |Compute the finalization committee given the bakers and the finalization committee parameters.
computeFinalizationCommittee :: FullBakers -> FinalizationCommitteeParameters -> FinalizationCommittee
computeFinalizationCommittee FullBakers{..} FinalizationCommitteeParameters{..} =
    FinalizationCommittee{..}
  where
    -- We use an insertion sort to construct the '_fcpMaxFinalizers' top bakers.
    -- Order them by descending stake and ascending baker ID.
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


updateRoundStatus :: Ratio Word64 -> RoundStatus -> RoundStatus
updateRoundStatus timeoutIncrease currentRoundStatus =
    let newNextSignableRound = 1 + rsCurrentRound currentRoundStatus
        timeoutIncreaseRational = toRational timeoutIncrease :: Rational
        currentTimeOutRational = toRational $ rsCurrentTimeout currentRoundStatus :: Rational
        newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational :: Rational
        newCurrentTimeoutInteger = floor newCurrentTimeoutRational :: Integer
        newCurrentTimeout = Duration $ fromIntegral newCurrentTimeoutInteger
    in currentRoundStatus{
        rsNextSignableRound = newNextSignableRound,
        rsCurrentTimeout = newCurrentTimeout}

-- |This is 'uponTimeoutEvent' from the bluepaper. If a timeout occurs, a finalizers should call this function to
-- generate and send out a timeout message.
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
    bakerId <- getBakerId
    currentRoundStatus <- doGetRoundStatus
    -- finComm1 <- (^. currentEpochBakers . bfFinalizers) <$> use skovEpochBakers -- FIXME: Check if this is correct
    eBakers <- use skovEpochBakers
    let maybeFinComm = (^. bfFinalizers) <$> getBakersForLiveEpoch (rsCurrentEpoch currentRoundStatus) eBakers
    let maybeFinalizer = case maybeFinComm of
            Nothing -> Nothing
            Just finComm -> finalizerByBakerId finComm bakerId

    case maybeFinalizer of
        Nothing -> return ()
        Just finInfo -> do
            lastFinBlockPtr <- use lastFinalized
            gc <- use genesisMetadata
            let genesisHash = gmFirstGenesisHash gc
            cp <- getChainParameters $ bpState lastFinBlockPtr
            let timeoutIncrease = cp ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
            doSetRoundStatus $ updateRoundStatus timeoutIncrease currentRoundStatus

            let highestQC = rsHighestQC currentRoundStatus

            let timeoutSigMessage =
                    TimeoutSignatureMessage -- FIXME: Figure out if this should be set in the roundstatus
                        { tsmGenesis = genesisHash,
                        tsmRound = rsCurrentRound currentRoundStatus,
                        tsmQCRound = qcRound highestQC,
                        tsmQCEpoch = qcEpoch highestQC
                        }
            bakerAggSk <- getBakerAggSecretKey
            let timeoutSig = signTimeoutSignatureMessage timeoutSigMessage bakerAggSk


            let timeoutMessageBody =
                    TimeoutMessageBody
                        {
                        tmFinalizerIndex = finalizerIndex finInfo,
                        tmRound = rsCurrentRound currentRoundStatus,
                        tmEpoch = rsCurrentEpoch currentRoundStatus,
                        tmQuorumCertificate = highestQC,
                        tmAggregateSignature = timeoutSig
                        }
            bakerSignSk <- getBakerSignPrivateKey
            let timeoutMessage = signTimeoutMessage timeoutMessageBody genesisHash bakerSignSk
            sendTimeoutMessage timeoutMessage
            processTimeout timeoutMessage

-- |This is 'processTimeout' from the bluepaper. FIXME: add more documentation when spefication is ready.
processTimeout ::
    (MonadTimeout m,
     LowLevel.MonadTreeStateStore m,
     MonadState (SkovData (MPV m)) m) =>
     TimeoutMessage -> m ()
processTimeout tm = do
    -- 1: store timeout locally
    -- 2: if the epochs of all stored timeouts for round currentRound and highestQC span more
    -- than 2 epochs then
    -- 3:     delete all the timeouts with smallest epoch, they are from dishonest people.
    -- [DONE (minus the fixme)]
    -- doStoreTimeoutMessage tm
    currentTimeoutMessages <- use receivedTimeoutMessages
    currentRoundStatus <- doGetRoundStatus
    let epoch = tmEpoch $ tmBody tm
    let highestQC = rsHighestQC currentRoundStatus

    let maybeMinKey = Map.lookupMin currentTimeoutMessages
    let finIndex = tmFinalizerIndex (tmBody tm)
    let maybeNewTimeoutMessages = case maybeMinKey of
                Nothing -> Just (Map.singleton epoch $ Map.singleton finIndex tm)  -- No timeout messages are currently stored. Insert the new timeoutmessage.
                Just (minKey, _)
                    |  epoch == minKey || epoch == minKey + 1 ->
                        Just $ currentTimeoutMessages &
                             at' epoch . non Map.empty . at' finIndex ?~ tm -- Insert.
                    | epoch + 1 == minKey && Map.size currentTimeoutMessages == 1 ->
                        Just $ currentTimeoutMessages &
                                at' epoch . non Map.empty . at' finIndex ?~ tm -- Insert.
                    | epoch > minKey + 1 ->
                        let afterDeletion = Map.filterWithKey (\k _ -> k >= epoch - 1) currentTimeoutMessages -- Delete all epochs < epoch - 1. Then insert epoch.
                        in Just $ afterDeletion &
                                at' epoch . non Map.empty . at' finIndex ?~ tm -- Insert.
                    | otherwise -> Nothing -- Don't insert anything. FIXME: fix this case

    forM_ maybeNewTimeoutMessages $ \newTimeoutMessages -> do
        receivedTimeoutMessages .=! newTimeoutMessages

        -- 4: if the weight of timeouts for round currentRound is > sigThreshold with weights
        -- from the payday of highestQC.epoch then
        eBakers <- use skovEpochBakers
        let maybeFinComm = (^. bfFinalizers) <$> getBakersForLiveEpoch (qcEpoch highestQC) eBakers
        case maybeFinComm of
            Nothing -> return () -- don't do more
            Just finCommQC -> do
                let maybeFirstMessages = Map.lookupMin newTimeoutMessages

                let voterPowerSum = case maybeFirstMessages of
                        Nothing -> 0
                        Just (firstEpoch, firstMessages) ->
                            let maybeFirstFinComm = (^. bfFinalizers) <$> getBakersForLiveEpoch firstEpoch eBakers
                            in case maybeFirstFinComm of
                                Nothing -> 0
                                Just firstFinComm ->
                                    let firstFinIndices = Map.keysSet firstMessages -- fin indices of TMs from first epoch
                                        firstCommitteeFinalizers = committeeFinalizers firstFinComm
                                        firstBakerIds = Set.map (finalizerBakerId -- FIXME: use finalizer by index from common branch
                                                        . (firstCommitteeFinalizers Vec.!)
                                                        . fromIntegral
                                                        . theFinalizerIndex) firstFinIndices
                                        maybeSecondMessages = Map.lookup (firstEpoch + 1) newTimeoutMessages
                                        maybeSecondFinComm = (^. bfFinalizers) <$> getBakersForLiveEpoch (firstEpoch + 1) eBakers

                                        secondBakerIds = case (maybeSecondMessages, maybeSecondFinComm) of
                                            (Just secondMessages, Just secondFinComm) ->
                                                let secondFinIndices = Map.keysSet secondMessages -- fin indices of TMs from second epoch
                                                    secondCommitteeFinalizers = committeeFinalizers secondFinComm
                                                in Set.map (finalizerBakerId
                                                                    . (secondCommitteeFinalizers Vec.!)
                                                                    . fromIntegral
                                                                    . theFinalizerIndex) secondFinIndices
                                            _ -> Set.empty
                                        finsInFinCommQC = finalizerByBakerId finCommQC <$> Set.toList (firstBakerIds `Set.union` secondBakerIds)
                                        getFinalizerWeight = maybe 0 finalizerWeight
                                    in foldl' (+) 0 $ getFinalizerWeight <$> finsInFinCommQC
                let totalWeightRational = toRational $ committeeTotalWeight finCommQC :: Rational
                let threshold = totalWeightRational * (2 % 3) :: Rational
                let thresholdVoterPower = VoterPower $ floor threshold
                when (voterPowerSum > thresholdVoterPower) $ do
                    let currentRound = rsCurrentRound currentRoundStatus
                    let tc = makeTC currentRound newTimeoutMessages
                    advanceRound (currentRound + 1) (Left (tc, highestQC))



-- Helper function for folding over a `Map.Map FinalizerIndex TimeoutMessage` to produce a `Map.Map Round FinalizerSet`
foldHelper :: Map.Map Round FinalizerSet -> FinalizerIndex -> TimeoutMessage -> Map.Map Round FinalizerSet
foldHelper finRounds finIndex tm = Map.insert roundOfQC newFinSet finRounds
    where
        roundOfQC = qcRound $ tmQuorumCertificate $ tmBody tm
        currentFinSetNatural = case Map.lookup roundOfQC finRounds of
            Nothing -> 0 -- The empty set of finalizers.
            Just (FinalizerSet finSet) -> finSet
        newFinSet = FinalizerSet $ currentFinSetNatural `setBit` fromIntegral (theFinalizerIndex finIndex) -- FIXME: improve this

-- TODO: don't take skovdata but just the things needed
-- Precondition: receivedTimeoutMessages MUST be non-empty FIXME: improve docs
makeTC :: Round -> Map.Map Epoch (Map.Map FinalizerIndex TimeoutMessage) -> TimeoutCertificate
makeTC currentRound messagesMap = do
    let maybeKey = Map.lookupMin messagesMap
    case maybeKey of
        Nothing -> error "No timeout messages present" -- should never happen. FIXME: Find out what to do here
        Just minKey ->
            let firstEpoch = snd minKey :: Map.Map FinalizerIndex TimeoutMessage
                firstAggSigs = tmAggregateSignature . tmBody <$> Map.elems firstEpoch
                newMapFirstEpoch = FinalizerRounds $ Map.foldlWithKey' foldHelper Map.empty firstEpoch
                (newMapSecondEpoch, secondAggSigs) = case Map.lookup (fst minKey + 1) $ messagesMap of
                        Nothing -> (FinalizerRounds Map.empty, [])
                        Just secondEpoch -> (FinalizerRounds $ Map.foldlWithKey' foldHelper Map.empty secondEpoch,
                                             tmAggregateSignature . tmBody <$> Map.elems secondEpoch)
            in TimeoutCertificate
                { -- |The round that has timed-out.
                tcRound = currentRound, -- :: !Round,
                -- |The minimum epoch for which we include signatures.
                tcMinEpoch = fst minKey, -- :: !Epoch,
                -- |The rounds for which finalizers have their best QCs in the epoch 'tcMinEpoch'.
                tcFinalizerQCRoundsFirstEpoch = newMapFirstEpoch, -- :: !FinalizerRounds,
                -- |The rounds for which finalizers have their best QCs in the epoch @tcMinEpoch + 1@.
                tcFinalizerQCRoundsSecondEpoch = newMapSecondEpoch, -- :: !FinalizerRounds,
                -- |Aggregate of the finalizers' 'TimeoutSignature's on the round and QC round.
                tcAggregateSignature = fold $ firstAggSigs ++ secondAggSigs -- :: !TimeoutSignature
                }
