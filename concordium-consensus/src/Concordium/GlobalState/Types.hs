{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Types where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Kind

import Concordium.GlobalState.BlockPointer (BlockPointerData)
import Concordium.GlobalState.Classes
import Concordium.Types

class (IsProtocolVersion (MPV m)) => MonadProtocolVersion (m :: Type -> Type) where
    type MPV m :: ProtocolVersion

-- |The basic types associated with a monad providing an
-- implementation of block state.
class BlockStateTypes (m :: Type -> Type) where
    type BlockState m :: Type
    type UpdatableBlockState m :: Type
    type Account m :: Type
    -- |A reference type for 'BakerInfo'. This is used to avoid duplicating 'BakerInfo' in the
    -- state where possible.
    type BakerInfoRef m :: Type

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
    type BakerInfoRef (MGSTrans t m) = BakerInfoRef m

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
