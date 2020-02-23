{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
module Concordium.GlobalState.TransactionLogs where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.RWS.Strict

import Concordium.Types
import Concordium.GlobalState.Classes

class Monad m => TransactionLogger m where
  tlNotifyAccountEffect :: TransactionHash -> AccountAddress -> m ()

newtype NoTransactionLogger m a = NoTransactionLogger (m a)
    deriving(Functor, Applicative, Monad)

-- FIXME: Dummy instance to get things going.
instance TransactionLogger IO where
  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect = \_ _ -> return ()

-- An instance that does no transaction logging.
instance Monad m => TransactionLogger (NoTransactionLogger m) where
  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect = \_ _ -> return ()

instance (MonadTrans t, Monad (t m), TransactionLogger m) => TransactionLogger (MGSTrans t m) where
  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect txHash addr = lift (tlNotifyAccountEffect txHash addr)

deriving via NoTransactionLogger Identity instance TransactionLogger Identity

deriving via (MGSTrans MaybeT m) instance TransactionLogger m => TransactionLogger (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance TransactionLogger m => TransactionLogger (ExceptT e m)
deriving via (MGSTrans (RWST r w s) m) instance (Monoid w, TransactionLogger m) => TransactionLogger (RWST r w s m)
