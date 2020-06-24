{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Concordium.GlobalState.BlockMonads where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Concordium.GlobalState.Types
import Concordium.GlobalState.Classes as C
import Concordium.GlobalState.AccountTransactionIndex

-- | This typeclass abstracts the access to the parent and last finalized blocks
-- using the pointers inside the `BlockPointer t p s`.
class (Monad m, GlobalStateTypes m, ATITypes m) => BlockPointerMonad m where
    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointerType m -> m (BlockState m)

    -- |Get the parent block of a 'BlockPointer'
    bpParent :: BlockPointerType m -> m (BlockPointerType m)

    -- |Get the last finalized block of a 'BlockPointer'
    bpLastFinalized :: BlockPointerType m -> m (BlockPointerType m)

    -- |Get the block transaction affect.
    bpTransactionAffectSummaries :: BlockPointerType m -> m (ATIStorage m)
    default bpTransactionAffectSummaries :: ATIStorage m ~ () => BlockPointerType m -> m (ATIStorage m)
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

deriving via MGSTrans MaybeT m instance BlockPointerMonad m => BlockPointerMonad (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance BlockPointerMonad m => BlockPointerMonad (ExceptT e m)
