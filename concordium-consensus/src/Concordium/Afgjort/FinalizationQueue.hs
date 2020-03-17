{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Concordium.Afgjort.FinalizationQueue where

import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Foldable
import Control.Monad.State.Class
import Control.Monad
import qualified Data.Vector as Vector
import Control.Exception(assert)

import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types
import Concordium.GlobalState.Finalization

import Concordium.Skov.Monad (UpdateResult(..))
import Concordium.Afgjort.Types
import Concordium.Afgjort.Finalize.Types
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.WMVBA

-- |A 'FinalizationProven' comprises a finalization proof together with
-- witness signatures that could be part of a proof.
--
-- The set of checked and unchecked signatures must always be disjoint.
data FinalizationProven = FinalizationProven {
    fpSessionId :: !FinalizationSessionId,
    fpIndex :: !FinalizationIndex,
    fpBlock :: !BlockHash,
    fpDelay :: !BlockHeight,
    fpCommittee :: !FinalizationCommittee,
    fpCheckedSignatures :: !(Map.Map Party Bls.Signature),
    fpCheckedSignatureSet :: !BitSet.BitSet,
    fpUncheckedSignatures :: !(Map.Map Party Bls.Signature),
    fpUncheckedSignatureSet :: !BitSet.BitSet,
    fpBadSignatureSet :: !BitSet.BitSet,
    fpCheckedAggregateSet :: !BitSet.BitSet,
    fpCheckedAggregateSignature :: !Bls.Signature
}

instance Show FinalizationProven where
    show FinalizationProven{..} = "FinalizationProven{fpIndex=" ++ show fpIndex ++ ",fpBlock=" ++ show fpBlock ++ "}"

newFinalizationProven :: FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> FinalizationProven
newFinalizationProven sessId fc FinalizationRecord{..} = FinalizationProven {
        fpSessionId = sessId,
        fpIndex = finalizationIndex,
        fpBlock = finalizationBlockPointer,
        fpDelay = finalizationDelay,
        fpCommittee = fc,
        fpCheckedSignatures = Map.empty,
        fpCheckedSignatureSet = BitSet.empty,
        fpUncheckedSignatures = Map.empty,
        fpUncheckedSignatureSet = BitSet.empty,
        fpBadSignatureSet = BitSet.empty,
        fpCheckedAggregateSet = BitSet.fromList (finalizationProofParties finalizationProof),
        fpCheckedAggregateSignature = finalizationProofSignature finalizationProof
    }

newFinalizationProvenWithWitnesses :: FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> OutputWitnesses -> FinalizationProven
newFinalizationProvenWithWitnesses sessId fc FinalizationRecord{..} OutputWitnesses{..} = FinalizationProven {
        fpSessionId = sessId,
        fpIndex = finalizationIndex,
        fpBlock = finalizationBlockPointer,
        fpDelay = finalizationDelay,
        fpCommittee = fc,
        fpCheckedSignatures = knownGoodSigs,
        fpCheckedSignatureSet = BitSet.fromList (Map.keys knownGoodSigs),
        fpUncheckedSignatures = unknownSigs,
        fpUncheckedSignatureSet = BitSet.fromList (Map.keys unknownSigs),
        fpBadSignatureSet = knownBadSigs,
        fpCheckedAggregateSet = BitSet.fromList (finalizationProofParties finalizationProof),
        fpCheckedAggregateSignature = finalizationProofSignature finalizationProof
    }

-- |Add a BLS signature that has been checked to be valid.
fpAddCheckedSignature :: Party -> Bls.Signature -> FinalizationProven -> FinalizationProven
fpAddCheckedSignature p sig fp@FinalizationProven{..}
        | p `BitSet.member` fpUncheckedSignatureSet = fp' {
                fpUncheckedSignatures = Map.delete p fpUncheckedSignatures,
                fpUncheckedSignatureSet = BitSet.delete p fpUncheckedSignatureSet
            }
        | otherwise = fp    
    where
        fp' = fp {
                fpCheckedSignatures = Map.insert p sig fpCheckedSignatures,
                fpCheckedSignatureSet = BitSet.insert p fpCheckedSignatureSet
            }

-- |Add a BLS signature that has not been checked to be valid.
-- If we already have 
fpAddUncheckedSignature :: Party -> Bls.Signature -> FinalizationProven -> FinalizationProven
fpAddUncheckedSignature p sig fp@FinalizationProven{..}
    | p > committeeMaxParty fpCommittee
        || p `BitSet.member` fpCheckedSignatureSet
        || p `BitSet.member` fpBadSignatureSet
            = fp
    | otherwise = fp {
            fpUncheckedSignatures = Map.insert p sig fpUncheckedSignatures,
            fpUncheckedSignatureSet = BitSet.insert p fpUncheckedSignatureSet
        }

fpAddCheckedAggregate :: BitSet.BitSet -> Bls.Signature -> FinalizationProven -> FinalizationProven
fpAddCheckedAggregate ps sig fp@FinalizationProven{..}
    | ps `BitSet.isSubsetOf` fpCheckedAggregateSet = fp
    | fpCheckedAggregateSet `BitSet.isSubsetOf` ps = fp'
    | (BitSet.isSubsetOf `on` withChecked) ps fpCheckedAggregateSet = fp
    | (BitSet.isSubsetOf `on` withChecked) fpCheckedAggregateSet ps = fp'
    | (BitSet.isSubsetOf `on` withAll) ps fpCheckedAggregateSet = fp
    | (BitSet.isSubsetOf `on` withAll) fpCheckedAggregateSet ps = fp'
    | otherwise = fp
    where
        fp' = fp{fpCheckedAggregateSet = ps, fpCheckedAggregateSignature = sig}
        withChecked = BitSet.union fpCheckedSignatureSet
        withAll = BitSet.union (fpCheckedSignatureSet `BitSet.union` fpUncheckedSignatureSet)

fpAddFinalizationRecord :: FinalizationRecord -> FinalizationProven -> FinalizationProven
fpAddFinalizationRecord FinalizationRecord{..} fp@FinalizationProven{..}
    | finalizationBlockPointer == fpBlock && finalizationIndex == fpIndex
        = fpAddCheckedAggregate (BitSet.fromList (finalizationProofParties finalizationProof)) (finalizationProofSignature finalizationProof) fp
    | otherwise = fp

-- FIXME: Be sure to check that we are preserving invariants about disjointness of the sets!
fpAddOutputWitnesses :: OutputWitnesses -> FinalizationProven -> FinalizationProven
fpAddOutputWitnesses OutputWitnesses{..} fp@FinalizationProven{..} = fp {
        fpCheckedSignatures = fpCheckedSignatures `Map.union` knownGoodSigs,
        fpCheckedSignatureSet = foldl' (flip BitSet.insert) fpCheckedSignatureSet (Map.keys knownGoodSigs),
        fpUncheckedSignatures = unchecked,
        fpUncheckedSignatureSet = BitSet.fromList (Map.keys unchecked),
        fpBadSignatureSet = fpBadSignatureSet `BitSet.union` knownBadSigs
    }
    where
        unchecked = (fpUncheckedSignatures `Map.difference` knownGoodSigs) `Map.union` (unknownSigs `Map.difference` fpCheckedSignatures)


fpGetProof :: FinalizationProven -> (FinalizationRecord, FinalizationProven)
fpGetProof fp@FinalizationProven{..}
    | done = (makeFR fpCheckedAggregateSet fpCheckedAggregateSignature, fp)
    | otherwise = (makeFR newAggregateSet newAggregate, fp')
    where
        done = bit (fromIntegral (committeeMaxParty fpCommittee) + 1) == (fpCheckedAggregateSet .|. fpBadSignatureSet) + 1
        makeFR parties sig = FinalizationRecord {
                finalizationIndex = fpIndex,
                finalizationBlockPointer = fpBlock,
                finalizationProof = FinalizationProof (BitSet.toList parties, sig),
                finalizationDelay = fpDelay
            }
        key party = partyBlsKey (parties fpCommittee Vector.! fromIntegral party)
        -- The checked aggregate signature, plus any checked signatures that are not already included.
        aggPlusChecked
            | BitSet.null (BitSet.difference fpCheckedSignatureSet fpCheckedAggregateSet) = fpCheckedAggregateSignature
            | otherwise = Bls.aggregateMany (fpCheckedAggregateSignature :
                            [sig | (party, sig) <- Map.toList fpCheckedSignatures, not (party `BitSet.member` fpCheckedAggregateSet)])
        aggPlusCheckedSet = BitSet.union fpCheckedAggregateSet fpCheckedSignatureSet
        -- Extract the unchecked signatures that could usefully be added to the current aggregate
        (candidateUncheckedSigs, stillUncheckedSigs, stillUncheckedSet)
            | BitSet.null (BitSet.difference fpUncheckedSignatureSet fpCheckedAggregateSet) = (Map.empty, fpUncheckedSignatures, fpUncheckedSignatureSet)
            | otherwise = let (ignore, check) = Map.partitionWithKey (\party _ -> party `BitSet.member` aggPlusCheckedSet) fpUncheckedSignatures
                            in (check, ignore, fpUncheckedSignatureSet `BitSet.intersection` aggPlusCheckedSet)
        candidateUncheckedSigsList = Map.toList candidateUncheckedSigs
        -- The message to sign, for validating the unchecked signatures.
        toSign = witnessMessage (roundBaid fpSessionId fpIndex fpDelay) fpBlock
        -- Aggregate the potentially useful unchecked signatures
        aggregateCandidateUnchecked = (mconcat $ snd <$> candidateUncheckedSigsList)
        -- Check these signatures, producing:
        -- * a valid aggregate of them
        -- * a map containing the newly checked signatures
        -- * a set of the newly checked signatures
        -- * a set of the bad signatures
        (additionalCheckedSigs, acsMap, acsSet, badSet)
            | Map.null candidateUncheckedSigs = (mempty, Map.empty, BitSet.empty, fpBadSignatureSet)
            | Bls.verifyAggregate toSign (key . fst <$> candidateUncheckedSigsList) aggregateCandidateUnchecked =
                (aggregateCandidateUnchecked, candidateUncheckedSigs, fpUncheckedSignatureSet `BitSet.difference` stillUncheckedSet, fpBadSignatureSet)
            | otherwise =
                let culprits = findCulprits candidateUncheckedSigsList toSign key
                    culpritSet = BitSet.fromList culprits
                    newBadSet = fpBadSignatureSet `BitSet.union` culpritSet
                    newACSSet = fpUncheckedSignatureSet `BitSet.difference` culpritSet
                    newACSMap = foldl' (flip Map.delete) candidateUncheckedSigs culprits
                    newACS = fold newACSMap
                in (newACS, newACSMap, newACSSet, newBadSet)
        -- To get the new aggregate signature, we add any newly checked signatures
        newAggregate = if BitSet.null acsSet then aggPlusChecked else (aggPlusChecked <> additionalCheckedSigs)
        newAggregateSet = aggPlusCheckedSet `BitSet.union` acsSet
        fp' = fp {
            fpCheckedSignatures = fpCheckedSignatures `Map.union` acsMap,
            fpCheckedSignatureSet = fpCheckedSignatureSet `BitSet.union` acsSet,
            fpUncheckedSignatures = stillUncheckedSigs,
            fpUncheckedSignatureSet = stillUncheckedSet,
            fpBadSignatureSet = badSet,
            fpCheckedAggregateSet = newAggregateSet,
            fpCheckedAggregateSignature = newAggregate
        }

fpGetProofSimple :: FinalizationProven -> (FinalizationRecord, FinalizationProven)
fpGetProofSimple fp@FinalizationProven{..}
    | noExtra = (makeFR fpCheckedAggregateSet fpCheckedAggregateSignature, fp)
    | otherwise = (makeFR newAggSet newAggSig, fp')
    where
        noExtra = BitSet.null (BitSet.difference fpCheckedSignatureSet fpCheckedAggregateSet)
        newAggSig = Bls.aggregateMany (fpCheckedAggregateSignature : 
                        [sig | (party, sig) <- Map.toList fpCheckedSignatures, not (party `BitSet.member` fpCheckedAggregateSet)])
        newAggSet = fpCheckedAggregateSet `BitSet.union` fpCheckedSignatureSet
        fp' = fp {fpCheckedAggregateSet = newAggSet, fpCheckedAggregateSignature = newAggSig}
        makeFR parties sig = FinalizationRecord {
                finalizationIndex = fpIndex,
                finalizationBlockPointer = fpBlock,
                finalizationProof = FinalizationProof (BitSet.toList parties, sig),
                finalizationDelay = fpDelay
            }

fpGetProofTrivial :: FinalizationProven -> FinalizationRecord
fpGetProofTrivial FinalizationProven{..} = FinalizationRecord {
        finalizationIndex = fpIndex,
        finalizationBlockPointer = fpBlock,
        finalizationProof = FinalizationProof (BitSet.toList fpCheckedAggregateSet, fpCheckedAggregateSignature),
        finalizationDelay = fpDelay
    }

-- |The finalization queue stores finalization records that are not yet
-- included in blocks that are themselves finalized.
data FinalizationQueue = FinalizationQueue {
    _fqFirstIndex :: !FinalizationIndex,
    _fqProofs :: !(Seq.Seq FinalizationProven)
} deriving (Show)
makeLenses ''FinalizationQueue

initialFinalizationQueue :: FinalizationQueue
initialFinalizationQueue = FinalizationQueue 1 Seq.empty

class FinalizationQueueLenses s where
    finQueue :: Lens' s FinalizationQueue

instance FinalizationQueueLenses FinalizationQueue where
    finQueue = id

-- |Add a finalization record to the finalization queue. The proof is
-- trusted (i.e. its validity is not checked).
addQueuedFinalization :: (MonadState s m, FinalizationQueueLenses s) => FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> m ()
addQueuedFinalization sessId fc fr@FinalizationRecord{..} = do
        FinalizationQueue{..} <- use finQueue
        case finalizationIndex `compare` (_fqFirstIndex + fromIntegral (Seq.length _fqProofs)) of
            LT -> when (finalizationIndex >= _fqFirstIndex) $
                finQueue . fqProofs . ix (fromIntegral $ finalizationIndex - _fqFirstIndex)
                    %= fpAddFinalizationRecord fr
            EQ -> finQueue . fqProofs %= (Seq.|> newFinalizationProven sessId fc fr)
            GT -> return ()

-- |Add a finalization record to the end of the finalization queue, together
-- with the output witnesses collected by the finalization state.  This
-- must only be called with a finalization record for the next finalization
-- index.
addNewQueuedFinalization :: (MonadState s m, FinalizationQueueLenses s)
    => FinalizationSessionId
    -> FinalizationCommittee
    -> FinalizationRecord
    -> OutputWitnesses
    -> m ()
addNewQueuedFinalization sessId fc fr@FinalizationRecord{..} ow = do
        FinalizationQueue{..} <- use finQueue
        assert (finalizationIndex == _fqFirstIndex + fromIntegral (Seq.length _fqProofs)) $ do
            finQueue . fqProofs %= (Seq.|> newFinalizationProvenWithWitnesses sessId fc fr ow)

-- |Get a finalization record for a given index, if it is available.
getQueuedFinalization :: (MonadState s m, FinalizationQueueLenses s)
    => FinalizationIndex
    -- ^Finalization index to get for
    -> m (Maybe FinalizationRecord)
getQueuedFinalization fi = do
    FinalizationQueue{..} <- use finQueue
    if fi >= _fqFirstIndex then do
        let index = fromIntegral (fi - _fqFirstIndex)
        forM (_fqProofs Seq.!? index) $ \fp -> do
            let (fr, fp') = fpGetProof fp
            finQueue . fqProofs . ix index .= fp'
            return fr
    else
        return Nothing

-- |Update the finalization queue by removing finalization information below the
-- given index. This should be used so that the queue doesn't hold records where
-- a finalized block already finalizes at that index.
updateQueuedFinalizationIndex :: (MonadState s m, FinalizationQueueLenses s) => FinalizationIndex -> m ()
updateQueuedFinalizationIndex fi = do
    oldFi <- use (finQueue . fqFirstIndex)
    when (fi > oldFi) $ do
        finQueue . fqFirstIndex .= fi
        finQueue . fqProofs %= Seq.drop (fromIntegral (fi - oldFi))

-- |Get all finalization records in the queue with finalization index greater than
-- the specified value. The records are returned in ascending order of finalization
-- index.
getQueuedFinalizationsBeyond :: (MonadState s m, FinalizationQueueLenses s) => FinalizationIndex -> m (Seq.Seq FinalizationRecord)
getQueuedFinalizationsBeyond fi = do
        firsti <- use (finQueue . fqFirstIndex)
        let
            offset = fromIntegral (1 + fi - firsti)
            trav i fp = do
                let (fr, fp') = fpGetProof fp
                finQueue . fqProofs . ix (i + offset) .= fp'
                return fr
        use (finQueue . fqProofs) >>= (Seq.traverseWithIndex trav . Seq.drop offset)

-- |This function determines if a witness signature for a party could usefully be added
-- to a queued finalization proof.
tryAddQueuedWitness :: (MonadState s m, FinalizationQueueLenses s) => FinalizationMessage -> m UpdateResult
tryAddQueuedWitness msg@FinalizationMessage{msgHeader=FinalizationMessageHeader{..}, msgBody=WMVBAWitnessCreatorMessage (val, blssig)} =
        use finQueue >>= \case
            FinalizationQueue{..}
                | msgFinalizationIndex >= _fqFirstIndex
                , let fqIndex = fromIntegral (msgFinalizationIndex - _fqFirstIndex)
                , Just FinalizationProven{..} <- _fqProofs Seq.!? fqIndex
                , msgDelta == fpDelay
                , val == fpBlock ->
                    if msgSenderIndex `BitSet.member` (fpCheckedSignatureSet `BitSet.union` fpUncheckedSignatureSet `BitSet.union` fpBadSignatureSet) then
                        return ResultDuplicate
                    else if checkMessage fpCommittee msg then do
                        finQueue . fqProofs . ix fqIndex %= fpAddUncheckedSignature msgSenderIndex blssig
                        return ResultSuccess
                    else
                        return ResultStale
                | otherwise -> return ResultStale
tryAddQueuedWitness _ = return ResultStale

-- |If there is a queued finalization for the given index, return the
-- finalization proof without attempting to add any further signatures.
-- This function is used for determining when we have a finalization proof but
-- do not have the associated block, and subsequently triggering finalization
-- in that case.
getQueuedFinalizationTrivial :: (MonadState s m, FinalizationQueueLenses s)
    => FinalizationIndex
    -- ^Finalization index to get for
    -> m (Maybe FinalizationRecord)
getQueuedFinalizationTrivial fi = do
    FinalizationQueue{..} <- use finQueue
    if fi >= _fqFirstIndex then
        let index = fromIntegral (fi - _fqFirstIndex)
        in return $ fpGetProofTrivial <$> (_fqProofs Seq.!? index)
    else
        return Nothing
