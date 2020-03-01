{-# LANGUAGE StandaloneDeriving, DerivingVia, DefaultSignatures, TypeFamilies #-}
-- |This module defines a type class `BlockPointerMonad` that abstracts the access to the parent
-- and last finalized block from a given block pointer.
-- These values might require to read the disk if the Tree State uses the persistent version so
-- the implementation would need to make use of the underlying database.
module Concordium.GlobalState.BlockPointer where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Concordium.GlobalState.Classes
import Concordium.GlobalState.AccountTransactionIndex

class (Monad m, GlobalStateTypes m, ATITypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointer m -> m (BlockState m)

    -- |Get the parent block of a 'BlockPointer'
    bpParent :: BlockPointer m -> m (BlockPointer m)

    -- |Get the last finalized block of a 'BlockPointer'
    bpLastFinalized :: BlockPointer m -> m (BlockPointer m)

    -- |Get the block transaction affect.
    bpTransactionAffectSummaries :: BlockPointer m -> m (ATIStorage m)
    default bpTransactionAffectSummaries :: ATIStorage m ~ () => BlockPointer m -> m (ATIStorage m)
    bpTransactionAffectSummaries = \_ -> return ()
    {-# INLINE bpTransactionAffectSummaries #-}

instance (Monad (t m), MonadTrans t, BlockPointerMonad m) => BlockPointerMonad (MGSTrans t m) where
  {-# INLINE blockState #-}
  blockState = lift . blockState
  {-# INLINE bpParent #-}
  bpParent = lift . bpParent
  {-# INLINE bpLastFinalized #-}
  bpLastFinalized = lift . bpLastFinalized
  {-# INLINE bpTransactionAffectSummaries #-}
  bpTransactionAffectSummaries = lift . bpTransactionAffectSummaries

deriving via (MGSTrans MaybeT m) instance BlockPointerMonad m => BlockPointerMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance BlockPointerMonad m => BlockPointerMonad (ExceptT e m)
