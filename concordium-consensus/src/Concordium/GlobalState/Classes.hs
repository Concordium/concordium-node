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

class HasGlobalState g s | s -> g where
    globalState :: Lens' s g

instance HasGlobalState g (Identity g) where
    globalState = lens runIdentity (const Identity)

class BlockStateTypes (m :: * -> *) where
    type BlockState m :: *
    type UpdatableBlockState m :: *

class (BlockStateTypes m, BlockPointerData (BlockState m) (BlockPointer m)) => GlobalStateTypes m where
    type PendingBlock m :: *
    type BlockPointer m :: *

newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, BlockStateTypes)

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

class HasGlobalStateContext c r | r -> c where
    globalStateContext :: Lens' r c

instance HasGlobalStateContext g (Identity g) where
    globalStateContext = lens runIdentity (const Identity)


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

{-
class ((forall m r' s'. (HasGlobalState s s', HasGlobalStateContext r r', MonadIO m) )) => GlobaStateConfig s r c | c -> s, r where
    initGlobalState :: c -> IO (r, s)
    -}