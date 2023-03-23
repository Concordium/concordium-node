{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Timeout where

import Control.Monad.Reader
import Control.Monad.State

import Data.Ratio
import Data.Maybe
import qualified Data.Set as Set
import Data.Word

import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import qualified Concordium.Types.Accounts as Accounts

import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.Consensus
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

-- |Helper function for calcuculating a new @currentTimeout@ given the old @currentTimeout@
-- and the @timeoutIncrease@ chain parameter.
updateCurrentTimeout :: Ratio Word64 -> Duration -> Duration
updateCurrentTimeout timeoutIncrease oldCurrentTimeout =
    let timeoutIncreaseRational = toRational timeoutIncrease :: Rational
        currentTimeOutRational = toRational oldCurrentTimeout :: Rational
        newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational :: Rational
        newCurrentTimeoutInteger = floor newCurrentTimeoutRational :: Integer
    in  Duration $ fromIntegral newCurrentTimeoutInteger

-- |Helper function for increasing the '_rsNextSignableRound' of a 'RoundStatus'.
increaseNextSignableRound :: RoundStatus -> RoundStatus
increaseNextSignableRound currentRoundStatus =
    let newNextSignableRound = 1 + _rsCurrentRound currentRoundStatus
    in currentRoundStatus{_rsNextSignableRound = newNextSignableRound}

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
        currentRoundStatus <- doGetRoundStatus
        eBakers <- use skovEpochBakers

        finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
        let maybeFinalizer = finalizerByBakerId finComm bakerId

        case maybeFinalizer of
            Nothing -> return ()
            Just finInfo -> do
                lastFinBlockPtr <- use lastFinalized
                gc <- use genesisMetadata
                let genesisHash = gmFirstGenesisHash gc
                cp <- getChainParameters $ bpState lastFinBlockPtr
                let timeoutIncrease = cp ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
                doSetRoundStatus $ increaseNextSignableRound currentRoundStatus
                oldCurrentTimeout <- use currentTimeout
                currentTimeout .=! updateCurrentTimeout timeoutIncrease oldCurrentTimeout

                let highestQC = _rsHighestQC currentRoundStatus

                let timeoutSigMessage =
                        TimeoutSignatureMessage
                            { tsmGenesis = genesisHash,
                            tsmRound = _rsCurrentRound currentRoundStatus,
                            tsmQCRound = qcRound highestQC,
                            tsmQCEpoch = qcEpoch highestQC
                            }
                let timeoutSig = signTimeoutSignatureMessage timeoutSigMessage bakerAggregationKey


                let timeoutMessageBody =
                        TimeoutMessageBody
                            {
                            tmFinalizerIndex = finalizerIndex finInfo,
                            tmRound = _rsCurrentRound currentRoundStatus,
                            tmEpoch = _currentEpoch eBakers,
                            tmQuorumCertificate = highestQC,
                            tmAggregateSignature = timeoutSig
                            }
                let timeoutMessage = signTimeoutMessage timeoutMessageBody genesisHash bakerSignKey
                sendMessage timeoutMessage
                processTimeout timeoutMessage

-- |Process a timeout message. This stores the timeout, and makes sure the stored timeout messages
-- do not span more than 2 epochs. If enough timeout messages are stored, we form a timeout certificate and
-- advance round. 
--
-- Precondition:
-- * The given 'TimeoutMessage' is valid and has already been checked.
processTimeout ::
    (MonadTimeout m,
     LowLevel.MonadTreeStateStore m,
     MonadState (SkovData (MPV m)) m) =>
     TimeoutMessage -> m ()
processTimeout tm = do
    currentTimeoutMessages <- use receivedTimeoutMessages
    currentRoundStatus <- doGetRoundStatus
    let epoch = tmEpoch $ tmBody tm
    let highestQC = _rsHighestQC currentRoundStatus

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
                    | otherwise -> Nothing

    forM_ maybeNewTimeoutMessages $ \newTimeoutMessages -> do
        receivedTimeoutMessages .=! newTimeoutMessages
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
                                        firstBakerIds2 = Set.map (fmap finalizerBakerId . finalizerByIndex firstFinComm) firstFinIndices
                                        maybeSecondMessages = Map.lookup (firstEpoch + 1) newTimeoutMessages
                                        maybeSecondFinComm = (^. bfFinalizers) <$> getBakersForLiveEpoch (firstEpoch + 1) eBakers

                                        secondBakerIds2 = case (maybeSecondMessages, maybeSecondFinComm) of
                                            (Just secondMessages, Just secondFinComm) ->
                                                let secondFinIndices = Map.keysSet secondMessages -- fin indices of TMs from second epoch
                                                in Set.map (fmap finalizerBakerId . finalizerByIndex secondFinComm) secondFinIndices
                                            _ -> Set.empty
                                        getFin = \case
                                            Nothing -> Nothing
                                            Just bakerId -> finalizerByBakerId finCommQC bakerId
                                        finsInFinCommQC2 = getFin <$> Set.toList (firstBakerIds2 `Set.union` secondBakerIds2)
                                        getFinalizerWeight = maybe 0 finalizerWeight
                                    in foldl' (+) 0 $ getFinalizerWeight <$> finsInFinCommQC2
                let totalWeightRational = toRational $ committeeTotalWeight finCommQC :: Rational
                genesisSigThreshold <- toRational . genesisSignatureThreshold . gmParameters <$> use genesisMetadata
                let voterPowerSumRational = toRational voterPowerSum
                when (voterPowerSumRational / totalWeightRational >= genesisSigThreshold) $ do
                    let currentRound = _rsCurrentRound currentRoundStatus
                    let tc = makeTC currentRound newTimeoutMessages
                    advanceRound (currentRound + 1) (Left (tc, highestQC))



-- Helper function for folding over a 'Map.Map FinalizerIndex TimeoutMessage' to produce a 'Map.Map Round FinalizerSet'
foldHelper :: Map.Map Round FinalizerSet -> FinalizerIndex -> TimeoutMessage -> Map.Map Round FinalizerSet
foldHelper finRounds finIndex tm = Map.insert roundOfQC newFinSet finRounds
    where
        roundOfQC = qcRound $ tmQuorumCertificate $ tmBody tm
        currentFinSetNatural = fromMaybe emptyFinalizerSet (Map.lookup roundOfQC finRounds)
        newFinSet = addFinalizer currentFinSetNatural finIndex

-- |Make a 'TimeoutCertificate' from a 'Map.Map Epoch (Map.Map FinalizerIndex TimeoutMessage)'.
--
-- Precondition:
-- * The given map of timeout messages MUST be non-empty.
-- 
-- NB: It is not checked whether enough timeoutmessages are present in the map. 
-- This should be checked before calling 'makeTC'.
makeTC :: Round -> Map.Map Epoch (Map.Map FinalizerIndex TimeoutMessage) -> TimeoutCertificate
makeTC currentRound messagesMap = do
    let maybeKey = Map.lookupMin messagesMap
    case maybeKey of
        Nothing -> error "No timeout messages present" -- should never happen, since a timeout message is stored just before calling this function.
        Just minKey ->
            let firstEpoch = snd minKey :: Map.Map FinalizerIndex TimeoutMessage
                firstAggSigs = tmAggregateSignature . tmBody <$> Map.elems firstEpoch
                newMapFirstEpoch = FinalizerRounds $ Map.foldlWithKey' foldHelper Map.empty firstEpoch
                (newMapSecondEpoch, secondAggSigs) = case Map.lookup (fst minKey + 1) $ messagesMap of
                        Nothing -> (FinalizerRounds Map.empty, [])
                        Just secondEpoch -> (FinalizerRounds $ Map.foldlWithKey' foldHelper Map.empty secondEpoch,
                                             tmAggregateSignature . tmBody <$> Map.elems secondEpoch)
            in TimeoutCertificate
                { tcRound = currentRound,
                  tcMinEpoch = fst minKey,
                  tcFinalizerQCRoundsFirstEpoch = newMapFirstEpoch,
                  tcFinalizerQCRoundsSecondEpoch = newMapSecondEpoch,
                  tcAggregateSignature = fold $ firstAggSigs ++ secondAggSigs
                }
