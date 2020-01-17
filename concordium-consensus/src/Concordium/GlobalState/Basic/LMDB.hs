{-# LANGUAGE FlexibleContexts #-}
-- |

module Concordium.GlobalState.Basic.LMDB where

import Concordium.Types
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.Types.HashableTo
import Concordium.GlobalState.Finalization

class (Eq (BlockPointer m),
       Ord (BlockPointer m),
       HashableTo BlockHash (BlockPointer m),
       BlockData (BlockPointer m),
       BlockPointerData (BlockPointer m),
       BlockPendingData (PendingBlock m),
       BlockStateStorage m,
       Monad m) => LMDBStoreMonad m where
   writeBlock :: (BlockPointer m) -> m ()
   readBlock :: BlockHash -> m (Maybe (BlockPointer m))
   writeFinalizationRecord :: FinalizationRecord -> m ()
   readFinalizationRecord :: FinalizationIndex -> m (Maybe FinalizationRecord)


makeBlockPointerFromBlock = undefined
