{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module consists of monads required for running the consensus v1 protocol.
-- In particular it contains the following:
-- * 'AccountNonceQueryT' is responsible for retrieving the "next available account nonce"
--   from the underlying tree state, in this case the 'SkovData pv'.
module Concordium.KonsensusV1.Monad where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Data.Kind (Type)
import Lens.Micro.Platform

-- Base package dependencies
import Concordium.Types

-- Consensus package dependencies

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.TreeState (MGSTrans (..))
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.TimeMonad (TimeMonad)

-- |Monad transformer for acquiring the next available account nonce from the
-- underlying tree state.
newtype AccountNonceQueryT (m :: Type -> Type) (a :: Type) = AccountNonceQueryT {runAccountNonceQueryT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, TimeMonad, MonadState s, MonadReader r)
    deriving (MonadTrans) via IdentityT

-- Instance for deducing the protocol version from the parameterized @m@ of the 'AccountNonceQueryT'.
deriving via (MGSTrans AccountNonceQueryT m) instance (MonadProtocolVersion m) => MonadProtocolVersion (AccountNonceQueryT m)

-- Instances required in order to use the 'AccountNonceQueryT' monad from within a block state context.
deriving via (MGSTrans AccountNonceQueryT m) instance GSTypes.BlockStateTypes (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance BlockStateQuery m => BlockStateQuery (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance ContractStateOperations m => ContractStateOperations (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance AccountOperations m => AccountOperations (AccountNonceQueryT m)
deriving via (MGSTrans AccountNonceQueryT m) instance ModuleQuery m => ModuleQuery (AccountNonceQueryT m)

-- |The instance used for acquiring the next available account nonce with respect to  consensus protocol v1.
instance (MonadState (SkovData (MPV m)) m) => AccountNonceQuery (AccountNonceQueryT m) where
    getNextAccountNonce addr = TT.nextAccountNonce addr . view transactionTable <$> get
    {-# INLINE getNextAccountNonce #-}
