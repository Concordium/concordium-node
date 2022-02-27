{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module Concordium.GlobalState.Types where

import Concordium.GlobalState.Classes
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Kind

import Concordium.Types
import Concordium.GlobalState.BlockPointer (BlockPointerData)
import Concordium.Wasm (WasmVersion)

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

-- |Account together with its index in the account map.
type IndexedAccount m = (AccountIndex, Account m)

type family BlockStatePointer (bs :: Type) :: Type

type BlockStateRef m = BlockStatePointer (BlockState m)

instance BlockStateTypes (MGSTrans t m) where
    type BlockState (MGSTrans t m) = BlockState m
    type UpdatableBlockState (MGSTrans t m) = UpdatableBlockState m
    type Account (MGSTrans t m) = Account m
    type ContractState (MGSTrans t m) = ContractState m

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
