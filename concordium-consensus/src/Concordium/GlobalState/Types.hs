{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Concordium.GlobalState.Types where

import Concordium.GlobalState.Classes
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Kind

import Concordium.GlobalState.BlockPointer (BlockPointerData)
import Concordium.Logger (LoggerT)

-- |The basic types associated with a monad providing an
-- implementation of block state.
class BlockStateTypes (m :: Type -> Type) where
    type BlockState m :: Type
    type UpdatableBlockState m :: Type
    type BirkParameters m :: Type

instance BlockStateTypes (MGSTrans t m) where
    type BlockState (MGSTrans t m) = BlockState m
    type UpdatableBlockState (MGSTrans t m) = UpdatableBlockState m
    type BirkParameters (MGSTrans t m) = BirkParameters m

deriving via MGSTrans MaybeT m instance BlockStateTypes (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance BlockStateTypes (ExceptT e m)
deriving via MGSTrans LoggerT m instance BlockStateTypes (LoggerT m)

-- |The basic types associated with a monad providing an
-- implementation of the global state.
class (BlockStateTypes m, BlockPointerData (BlockPointerType m)) => GlobalStateTypes m where
    type BlockPointerType m :: Type

instance BlockPointerData (BlockPointerType m) => GlobalStateTypes (MGSTrans t m) where
    type BlockPointerType (MGSTrans t m) = BlockPointerType m

deriving via MGSTrans MaybeT m instance GlobalStateTypes m => GlobalStateTypes (MaybeT m)
deriving via MGSTrans (ExceptT e) m instance GlobalStateTypes m => GlobalStateTypes (ExceptT e m)
deriving via MGSTrans LoggerT m instance GlobalStateTypes m => GlobalStateTypes (LoggerT m)
