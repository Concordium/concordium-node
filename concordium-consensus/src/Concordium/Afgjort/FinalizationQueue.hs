{-# LANGUAGE ViewPatterns, TemplateHaskell, BangPatterns #-}
-- |This module implements 'FinalizationQueue', a datastructure which stores
-- finalization proofs that are not settled: that is, included in a block
-- that is itself finalized.  The primary purpose of the queue is to allow
-- a baker to obtain the best available finalization proof (i.e. with the most
-- signatures) when it needs to include one in a block.  The queue therefore
-- allows additional witnesses to be added to the accumulated signatures
-- ('tryAddQueuedWitness').
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

import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Utils
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
-- The aggregate signature must always be valid, consist of the signatures
-- from the indicated parties, and have sufficient weight to form a valid
-- finalization proof.
--
-- Working with 'FinalizationProven' records directly should be avoided
-- outside of this module. Higher-level functions are provided to operate
-- on the finalization queue.
data FinalizationProven = FinalizationProven {
    -- |The finalization session
    fpSessionId :: !FinalizationSessionId,
    -- |The finalization index
    fpIndex :: !FinalizationIndex,
    -- |The finalized block hash
    fpBlock :: !BlockHash,
    -- |The delay used in finalization
    fpDelay :: !BlockHeight,
    -- |The finalization committee
    fpCommittee :: !FinalizationCommittee,
    -- |The BLS signatures that are known to be valid
    fpCheckedSignatures :: !(Map.Map Party Bls.Signature),
    -- |The set of parties that have known valid signatures
    fpCheckedSignatureSet :: !BitSet.BitSet,
    -- |The BLS signatures that have not been verified
    fpUncheckedSignatures :: !(Map.Map Party Bls.Signature),
    -- |The set of parties with unverified signatures
    fpUncheckedSignatureSet :: !BitSet.BitSet,
    -- |The set of parties that have had bad signatures
    fpBadSignatureSet :: !BitSet.BitSet,
    -- |The set of parties constributing to the aggregate signature
    fpCheckedAggregateSet :: !BitSet.BitSet,
    -- |A valid aggregate signature
    fpCheckedAggregateSignature :: !Bls.Signature
} deriving(Eq)

instance Show FinalizationProven where
    show FinalizationProven{..} = "FinalizationProven{fpIndex=" ++ show fpIndex ++ ",fpBlock=" ++ show fpBlock ++ "}"

-- |Create a 'FinalizationProven' from a 'FinalizationRecord'.  This is used if
-- we obtain a 'FinalizationRecord' but have not obtained individual witnesses.
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

-- |Create a 'FinalizationProven' from a 'FinalizationRecord' and witnesses.
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
-- The signature is not added if:
-- * the party member is not valid
-- * a checked signature is already available for this party member
-- * a bad signature has already been received for this party member.
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

-- |Add a known valid aggregate signature.  The signature is assumed
-- to be sufficient for a valid finalization proof.  The signature
-- may replace the existing signature if it would lead to more
-- signatures being included in a generated finalization proof.
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

-- |Add a finalization record to a 'FinalizationProven'.  The record is assumed
-- to be valid.
fpAddFinalizationRecord :: FinalizationRecord -> FinalizationProven -> FinalizationProven
fpAddFinalizationRecord FinalizationRecord{..} fp@FinalizationProven{..}
    | finalizationBlockPointer == fpBlock && finalizationIndex == fpIndex
        = fpAddCheckedAggregate (BitSet.fromList (finalizationProofParties finalizationProof)) (finalizationProofSignature finalizationProof) fp
    | otherwise = fp

-- NOTE: currently unused
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

-- |Extract the best available proof from a 'FinalizationProven'.
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

-- |Extract a proof from a 'FinalizationProven' without doing additional BLS
-- checks.  This only uses known valid signatures to generate the proof.
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

-- |Extract a proof from a 'FinalizationProven' without attempting to aggregate
-- any additional signatures.
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
    -- |Index of the first finalization record that is not yet included in any
    -- finalized block.
    _fqFirstIndex :: !FinalizationIndex,
    _fqProofs :: !(Seq.Seq FinalizationProven)
} deriving (Eq, Show)
makeLenses ''FinalizationQueue

-- |An empty finalization queue, where only the genesis block is considered
-- finalized (and settled).
initialFinalizationQueue :: FinalizationQueue
initialFinalizationQueue = FinalizationQueue 1 Seq.empty

-- |A class for a state that includes a finalization queue.
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
            EQ -> finQueue . fqProofs %=! (|>! newFinalizationProven sessId fc fr)
            GT -> return ()

-- |Add a finalization record to the end of the finalization queue, together
-- with the output witnesses collected by the finalization state.  This
-- must only be called with a finalization record for the next finalization
-- index.  The finalization record is assumed to be valid.
addNewQueuedFinalization :: (MonadState s m, FinalizationQueueLenses s)
    => FinalizationSessionId
    -> FinalizationCommittee
    -> FinalizationRecord
    -> OutputWitnesses
    -> m ()
addNewQueuedFinalization sessId fc fr@FinalizationRecord{..} ow = do
        FinalizationQueue{..} <- use finQueue
        when (finalizationIndex == _fqFirstIndex + fromIntegral (Seq.length _fqProofs)) $ do
            finQueue . fqProofs %=! (|>! newFinalizationProvenWithWitnesses sessId fc fr ow)

-- |Get a finalization record for a given index, if it is available.
getQueuedFinalization :: (MonadState s m, FinalizationQueueLenses s)
    => FinalizationIndex
    -- ^Finalization index to get for
    -> m (Maybe (FinalizationSessionId, FinalizationCommittee, FinalizationRecord))
getQueuedFinalization fi = do
    FinalizationQueue{..} <- use finQueue
    if fi >= _fqFirstIndex then do
        let index = fromIntegral (fi - _fqFirstIndex)
        forM (_fqProofs Seq.!? index) $ \fp -> do
            let (fr, fp') = fpGetProof fp
            finQueue . fqProofs . ix index .= fp'
            return (fpSessionId fp', fpCommittee fp', fr)
    else return Nothing

-- |Update the finalization queue by removing finalization information below the
-- given index. This should be used so that the queue doesn't hold records where
-- a finalized block already finalizes at that index.
-- Note: this is superceded in use by 'settledQueuedFinalizationByHash' since it
-- is simpler to resolve the hash than the finalization index.
settleQueuedFinalizationBeforeIndex :: (MonadState s m, FinalizationQueueLenses s) => FinalizationIndex -> m ()
settleQueuedFinalizationBeforeIndex fi = do
    oldFi <- use (finQueue . fqFirstIndex)
    when (fi > oldFi) $ do
        finQueue . fqFirstIndex .= fi
        finQueue . fqProofs %= Seq.drop (fromIntegral (fi - oldFi))

-- |Update the finalization queue by considering the finalization for the given
-- block hash settled. Implicitly, any previous finalizations are also considered
-- settled.
settleQueuedFinalizationByHash :: (MonadState s m, FinalizationQueueLenses s) => BlockHash -> m ()
settleQueuedFinalizationByHash bh = do
    oldProofs <- use (finQueue . fqProofs)
    let
        filt !n (fp Seq.:<| fps)
            | bh == fpBlock fp = (n+1, fps)
            | otherwise = filt (n+1) fps
        filt _ Seq.Empty = (0, oldProofs)
        (offset, newProofs) = filt 0 oldProofs
    finQueue . fqFirstIndex += offset
    finQueue . fqProofs .= newProofs

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
    -> m (Maybe (FinalizationSessionId, FinalizationCommittee, FinalizationRecord))
getQueuedFinalizationTrivial fi = do
    FinalizationQueue{..} <- use finQueue
    if fi >= _fqFirstIndex then
        let index = fromIntegral (fi - _fqFirstIndex)
        in return $ (\fp -> (fpSessionId fp, fpCommittee fp, fpGetProofTrivial fp)) <$> (_fqProofs Seq.!? index)
    else
        return Nothing
