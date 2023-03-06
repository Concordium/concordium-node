{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.Monad where

import Concordium.TimeMonad (TimeMonad)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Kind (Type)
import Lens.Micro.Platform

import Concordium.Types

import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation

-- |The 'MonadSkov' defines a monad suitable for running operations on the underlying 'SkovData'.
newtype SkovMonad (s :: Type) (m :: Type -> Type) (a :: Type) = SkovMonad {runSkovMonad :: s -> m (a, s)}
    deriving (Functor, Applicative, Monad, MonadIO, TimeMonad, MonadState s, MonadReader s) via (StateT s m)
    deriving (MonadTrans) via (StateT s)

--deriving via (StateT s) m instance (MonadProtocolVersion m) => MonadProtocolVersion (SkovMonad s m)
--deriving via (StateT s) m instance GSTypes.BlockStateTypes (SkovMonad s m)

instance (Monad m, MonadState s m, s ~ SkovData (MPV m)) => AccountNonceQuery (SkovMonad s m) where
    getNextAccountNonce addr = do
        sd <- get
        return $! TT.nextAccountNonce addr (sd ^. transactionTable)
