{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.Monad where

import Concordium.TimeMonad (TimeMonad)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Data.Kind (Type)
import Lens.Micro.Platform

import Concordium.Types

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.TreeState (MGSTrans (..))
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation

newtype AccountNonceQueryT (m :: Type -> Type) (a :: Type) = AccountNonceQueryMixin {runAccountNonceQueryT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, TimeMonad, MonadState s, MonadReader r)
    deriving (MonadTrans) via IdentityT

deriving via (MGSTrans AccountNonceQueryT m) instance GSTypes.BlockStateTypes (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance (MonadProtocolVersion m) => MonadProtocolVersion (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance BlockStateQuery m => BlockStateQuery (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance ContractStateOperations m => ContractStateOperations (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance AccountOperations m => AccountOperations (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance ModuleQuery m => ModuleQuery (AccountNonceQueryT m)

instance (MonadState (SkovData (MPV m)) m) => AccountNonceQuery (AccountNonceQueryT m) where
    getNextAccountNonce addr = TT.nextAccountNonce addr . view transactionTable <$> get
    {-# INLINE getNextAccountNonce #-}
