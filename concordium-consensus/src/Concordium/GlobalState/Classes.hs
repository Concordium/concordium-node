{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Definition of some basic typeclasses that give access to the basic types
-- used in the implementation and some lenses to access specific components
module Concordium.GlobalState.Classes where

import Data.Kind
import Lens.Micro.Platform
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Data.Functor.Identity
import Concordium.Logger
import Concordium.Cache

-- |Defines a lens for accessing the global state component of a type.
class HasGlobalState g s | s -> g where
    globalState :: Lens' s g

instance HasGlobalState g (Identity g) where
    globalState = lens runIdentity (const Identity)

-- |Defines a lens for accessing the global state context.
class HasGlobalStateContext c r | r -> c where
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

-- |@MGSTrans t m@ is a newtype wrapper for a monad transformer @t@ applied
-- to a monad @m@.  This wrapper exists to support lifting various monad
-- type classes over monad transfers. (That is, instances of various typeclasses
-- are defined where @t@ is a monad transformer and @m@ implements the typeclass.)
-- The primary use for this is to provide instances for other types using the
-- deriving via mechanism.
newtype MGSTrans t (m :: Type -> Type) a = MGSTrans (t m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving instance MonadReader r (t m) => MonadReader r (MGSTrans t m)
deriving instance MonadState s (t m) => MonadState s (MGSTrans t m)
deriving instance MonadWriter w (t m) => MonadWriter w (MGSTrans t m)
deriving instance MonadLogger (t m) => MonadLogger (MGSTrans t m)
deriving instance CacheMonad k v (t m) => CacheMonad k v (MGSTrans t m)
