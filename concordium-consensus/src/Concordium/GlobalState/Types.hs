{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Concordium.GlobalState.Types where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Kind

import Concordium.GlobalState.BlockPointer (BlockPointerData)
import Concordium.Wasm (WasmVersion)
import Concordium.GlobalState.Classes
import Concordium.Types
import Concordium.Genesis.Data

class (IsProtocolVersion (MPV m)) => MonadProtocolVersion (m :: Type -> Type) where
    type MPV m :: ProtocolVersion

-- |The basic types associated with a monad providing an
-- implementation of block state.
class BlockStateTypes (m :: Type -> Type) where
    type BlockState m :: Type
    type UpdatableBlockState m :: Type
    -- |The type of accounts supported by the block state.
    type Account m :: Type
    -- |The type of contract state, parametrized both by the monad m, as well as
    -- the contract version. The reason that the kind of @ContractState m@ is
    -- @WasmVersion -> Type@ as opposed to parametrizing the type by both @m@
    -- and @WasmVersion@ is that this way we can "partially apply"
    -- @ContractState@, something that would otherwise not be possible.
    type ContractState m :: WasmVersion -> Type
    -- |A reference type for 'BakerInfo'. This is used to avoid duplicating 'BakerInfo' in the
    -- state where possible.
    type BakerInfoRef m :: Type

    type MigrationContext m :: ProtocolVersion -> Type
    type NextBlockState m :: ProtocolVersion -> Type

-- |Data retrieved from the existing skov instance that is needed to construct
-- the new instance. This is an existential type that closes over the **new**
-- protocol version. The existing protocol version is determined from the monad
-- @m@, which will have to be an instance of 'MonadProtocolVersion'.
data PVInit m = forall pv . (IsProtocolVersion pv) => PVInit {
    -- |Genesis data for the new chain.
    pvInitGenesis :: Regenesis pv,
    -- |Instructions on how to migrate from the existing instance to the new one.
    pvInitMigration :: StateMigrationParameters (MPV m) pv,
    -- |(Relative) height of the last finalized block.
    pvInitFinalHeight :: BlockHeight
  }

-- |Account together with its index in the account map.
type IndexedAccount m = (AccountIndex, Account m)

type family BlockStatePointer (bs :: Type) :: Type

type BlockStateRef m = BlockStatePointer (BlockState m)

instance MonadProtocolVersion m => MonadProtocolVersion (MGSTrans t m) where
    type MPV (MGSTrans t m) = MPV m

deriving via MGSTrans MaybeT m instance (MonadProtocolVersion m) => MonadProtocolVersion (MaybeT m)
deriving via
    MGSTrans (ExceptT e) m
    instance
        (MonadProtocolVersion m) => MonadProtocolVersion (ExceptT e m)
deriving via
    MGSTrans (ReaderT r) m
    instance
        (MonadProtocolVersion m) => MonadProtocolVersion (ReaderT r m)

instance BlockStateTypes (MGSTrans t m) where
    type BlockState (MGSTrans t m) = BlockState m
    type UpdatableBlockState (MGSTrans t m) = UpdatableBlockState m
    type Account (MGSTrans t m) = Account m
    type ContractState (MGSTrans t m) = ContractState m
    type BakerInfoRef (MGSTrans t m) = BakerInfoRef m
    type NextBlockState (MGSTrans t m) = NextBlockState m
    type MigrationContext (MGSTrans t m) = MigrationContext m

deriving via MGSTrans MaybeT m instance BlockStateTypes (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance BlockStateTypes (ExceptT e m)
deriving via MGSTrans (ReaderT r) m instance BlockStateTypes (ReaderT r m)

-- |The basic types associated with a monad providing an
-- implementation of the global state.
class (BlockStateTypes m, BlockPointerData (BlockPointerType m)) => GlobalStateTypes m where
    type BlockPointerType m :: Type

instance BlockPointerData (BlockPointerType m) => GlobalStateTypes (MGSTrans t m) where
    type BlockPointerType (MGSTrans t m) = BlockPointerType m

deriving via MGSTrans MaybeT m instance GlobalStateTypes m => GlobalStateTypes (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance GlobalStateTypes m => GlobalStateTypes (ExceptT e m)
