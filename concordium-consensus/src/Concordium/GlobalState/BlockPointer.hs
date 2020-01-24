{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
-- |

module Concordium.GlobalState.BlockPointer where

import Concordium.GlobalState.Classes
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

class (Monad m, GlobalStateTypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointer m -> m (BlockState m)

    -- |Get the parent of a 'BlockPointer'
    bpParent :: BlockPointer m -> m (BlockPointer m)

    -- |Get the last finalized of a 'BlockPointer'
    bpLastFinalized :: BlockPointer m -> m (BlockPointer m)

instance (Monad (t m), MonadTrans t, BlockPointerMonad m) => BlockPointerMonad (MGSTrans t m) where
  blockState = lift . blockState
  bpParent = lift . bpParent
  bpLastFinalized = lift . bpLastFinalized

deriving via (MGSTrans MaybeT m) instance BlockPointerMonad m => BlockPointerMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance BlockPointerMonad m => BlockPointerMonad (ExceptT e m)
