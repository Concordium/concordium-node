module Concordium.Afgjort.FinalizationQueue where

import qualified Data.Sequence as Seq
import Lens.Micro.Platform

import Concordium.GlobalState.Finalization

-- |The finalization queue stores finalization records that are not yet
-- included in blocks that are themselves finalized.
data FinalizationQueue = FinalizationQueue {
    fqFirstIndex :: !FinalizationIndex,
    fqProofs :: !(Seq.Seq FinalizationRecord)
}

initialFinalizationQueue :: FinalizationQueue
initialFinalizationQueue = FinalizationQueue 1 Seq.empty

class FinalizationQueueLenses s where
    finQueue :: Lens' s FinalizationQueue

instance FinalizationQueueLenses FinalizationQueue where
    finQueue = id

-- |Add a finalization record to the finalization queue. The proof is
-- trusted (i.e. its validity is not checked).
addFinalization :: FinalizationRecord -> FinalizationQueue -> FinalizationQueue
addFinalization fr@FinalizationRecord{..} fq@FinalizationQueue{..}
    | finalizationIndex == fqFirstIndex + fromIntegral (Seq.length fqProofs) =
            fq{fqProofs = fqProofs Seq.|> fr}
    | otherwise = fq

-- |Get a finalization record for a given index, if it is available.
getFinalization :: FinalizationIndex -> FinalizationQueue -> Maybe FinalizationRecord
getFinalization fi FinalizationQueue{..}
    | fi >= fqFirstIndex = fqProofs Seq.!? fromIntegral (fi - fqFirstIndex)
    | otherwise = Nothing

-- |Get all finalization records in the queue with finalization index greater than
-- the specified value. The records are returned in ascending order of finalization
-- index.
getFinalizationsBeyond :: FinalizationIndex -> FinalizationQueue -> Seq.Seq FinalizationRecord
getFinalizationsBeyond fi FinalizationQueue{..} = Seq.drop (fromIntegral fi - fromIntegral fqFirstIndex + 1) fqProofs

-- |Update the finalization queue by removing finalization information below the
-- given index. This should be used so that the queue doesn't hold records where
-- a finalized block already finalizes at that index.
updateFinalizationIndex :: FinalizationIndex -> FinalizationQueue -> FinalizationQueue
updateFinalizationIndex fi fq@FinalizationQueue{..}
    | fi <= fqFirstIndex = fq
    | otherwise = FinalizationQueue{fqFirstIndex = fi, fqProofs = Seq.drop (fromIntegral (fi - fqFirstIndex)) fqProofs}