{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import qualified Concordium.Types.Accounts as Accounts
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Control.Monad.Reader.Class

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

    -- |Called when a block becomes finalized. This is only called with explicitly finalized blocks.
    onFinalize ::
        -- |The 'FinalizationEntry' that witnesses that the 'BlockPointer pv' is finalized.
        FinalizationEntry ->
        -- |The implicitly finalized blocks, that is ancestors that was live before
        -- a block was finalized.
        [BlockPointer (MPV m)] ->
        -- |The explicityly finalized block.
        BlockPointer (MPV m) ->
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
    resetTimer =<< use (roundStatus . rsCurrentTimeout)
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
      MonadState (SkovData (MPV m)) m
    ) =>
    RoundTimeout (MPV m) ->
    m ()
advanceRoundWithTimeout roundTimeout@RoundTimeout{..} = do
    onNewRound
    roundStatus %=! updateQC . updateTC . (rsRoundEligibleToBake .~ True)
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
