{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.BlockMonads where

import Concordium.GlobalState.Classes as C
import Concordium.GlobalState.Types
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

-- | This typeclass abstracts the access to the parent and last finalized blocks
-- using the pointers inside the `BlockPointer t p s`.
class (Monad m, GlobalStateTypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointerType m -> m (BlockState m)

    -- |Get the parent block of a 'BlockPointer'
    bpParent :: BlockPointerType m -> m (BlockPointerType m)

    -- |Get the last finalized block of a 'BlockPointer'
    bpLastFinalized :: BlockPointerType m -> m (BlockPointerType m)

instance (Monad (t m), MonadTrans t, BlockPointerMonad m) => BlockPointerMonad (MGSTrans t m) where
    {-# INLINE blockState #-}
    blockState = lift . blockState
    {-# INLINE bpParent #-}
    bpParent = lift . bpParent
    {-# INLINE bpLastFinalized #-}
    bpLastFinalized = lift . bpLastFinalized

deriving via MGSTrans MaybeT m instance BlockPointerMonad m => BlockPointerMonad (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance BlockPointerMonad m => BlockPointerMonad (ExceptT e m)
