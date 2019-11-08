{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies,
        FlexibleInstances,
        UndecidableInstances,
        TypeFamilies,
        GeneralizedNewtypeDeriving,
        StandaloneDeriving,
        DerivingVia,
        QuantifiedConstraints,
        FlexibleContexts
        #-}
module Concordium.GlobalState.Classes where

import Lens.Micro.Platform
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Functor.Identity

import Concordium.GlobalState.Block

-- |Defines a lens for accessing the global state component of a type.
class HasGlobalState g s | s -> g where
    -- |Global state lens.
    globalState :: Lens' s g

instance HasGlobalState g (Identity g) where
    globalState = lens runIdentity (const Identity)

-- |The basic types associated with a monad providing an
-- implementation of block state.
class BlockStateTypes (m :: * -> *) where
    type BlockState m :: *
    type UpdatableBlockState m :: *

-- |The basic types associated with a monad providing an
-- implementation of the global state. 
class (BlockStateTypes m, BlockPointerData (BlockState m) (BlockPointer m)) => GlobalStateTypes m where
    type PendingBlock m :: *
    type BlockPointer m :: *

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, BlockStateTypes)

-- |@MGSTrans t m@ is a newtype wrapper for a monad transformer @t@ applied
-- to a monad @m@.  This wrapper exists to support lifting various monad
-- type classes over monad transfers.  (That is, instances of various typeclasses
-- are defined where @t@ is a monad transformer and @m@ implements the typeclass.)
-- The primary use for this is to provide instances for other types using the
-- deriving via mechanism.
newtype MGSTrans t (m :: * -> *) a = MGSTrans (t m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving instance (MonadReader r (t m)) => MonadReader r (MGSTrans t m)
deriving instance (MonadState s (t m)) => MonadState s (MGSTrans t m)

instance BlockStateTypes (MGSTrans t m) where
    type BlockState (MGSTrans t m) = BlockState m
    type UpdatableBlockState (MGSTrans t m) = UpdatableBlockState m

instance (GlobalStateTypes m) => GlobalStateTypes (MGSTrans t m) where
    type PendingBlock (MGSTrans t m) = PendingBlock m
    type BlockPointer (MGSTrans t m) = BlockPointer m

deriving via (MGSTrans MaybeT m) instance BlockStateTypes (MaybeT m)
deriving via (MGSTrans MaybeT m) instance (GlobalStateTypes m) => GlobalStateTypes (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateTypes (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance (GlobalStateTypes m) => GlobalStateTypes (ExceptT e m)

-- |Defines a lens for accessing the global state context.
class HasGlobalStateContext c r | r -> c where
    -- |The global state context.
    globalStateContext :: Lens' r c

instance HasGlobalStateContext g (Identity g) where
    globalStateContext = lens runIdentity (const Identity)

-- |A newtype wrapper that provides instances of @MonadReader c@ and
-- @MonadState g@ when the underlying monad @m@ satisfies @MonadReader r@
-- and @MonadState s@, with @HasGlobalStateContext c r@ and @HasGlobalState g s@.
newtype FocusGlobalStateM c g m a = FocusGlobalStateM {runFocusGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadState s m, HasGlobalState g s) => MonadState g (FocusGlobalStateM c g m) where
    get = FocusGlobalStateM $ use globalState
    put = FocusGlobalStateM . (globalState .=)
    state upd = FocusGlobalStateM $ state (globalState upd)
    {-# INLINE get #-}
    {-# INLINE put #-}
    {-# INLINE state #-}

instance (MonadReader r m, HasGlobalStateContext c r) => MonadReader c (FocusGlobalStateM c g m) where
    ask = FocusGlobalStateM $ view globalStateContext
    local f (FocusGlobalStateM a) = FocusGlobalStateM $ local (globalStateContext %~ f) a
    {-# INLINE ask #-}
    {-# INLINE local #-}
