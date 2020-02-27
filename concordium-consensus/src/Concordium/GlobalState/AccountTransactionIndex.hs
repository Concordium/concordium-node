{-# LANGUAGE DerivingVia, StandaloneDeriving, DefaultSignatures, TypeFamilies, UndecidableInstances #-}
module Concordium.GlobalState.AccountTransactionIndex where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.RWS.Strict

import Concordium.Types
import Concordium.Types.Execution
import Concordium.GlobalState.Classes

class Monad m => ATIMonad m where
  type ATIStorage m
  recordTransactionAffects :: AccountAddress -> TransactionSummary -> ATIStorage m -> m (ATIStorage m)

  emptyATI :: m (ATIStorage m)

  default recordTransactionAffects
      :: ATIStorage m ~ ()
      => AccountAddress
      -> TransactionSummary
      -> ATIStorage m
      -> m (ATIStorage m)
  recordTransactionAffects = \_ _ _ -> return ()
  {-# INLINE recordTransactionAffects #-}

  default emptyATI :: ATIStorage m ~ () => m (ATIStorage m)
  emptyATI = return ()
  {-# INLINE emptyATI #-}

newtype NoIndexATIMonad m a = NoIndexATIMonad (m a)
    deriving(Functor, Applicative, Monad)

instance Monad m => ATIMonad (NoIndexATIMonad m) where
  type ATIStorage (NoIndexATIMonad m) = ()

deriving via NoIndexATIMonad IO instance ATIMonad IO

instance (MonadTrans t, Monad (t m), ATIMonad m) => ATIMonad (MGSTrans t m) where
  type ATIStorage (MGSTrans t m) = ATIStorage m
  {-# INLINE recordTransactionAffects #-}
  recordTransactionAffects s addr ts = lift (recordTransactionAffects s addr ts)

  {-# INLINE emptyATI #-}
  emptyATI = lift emptyATI

deriving via (MGSTrans MaybeT m) instance ATIMonad m => ATIMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance ATIMonad m => ATIMonad (ExceptT e m)
deriving via (MGSTrans (RWST r w s) m) instance (Monoid w, ATIMonad m) => ATIMonad (RWST r w s m)
