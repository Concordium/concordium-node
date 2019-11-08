{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    DerivingVia,
    FlexibleContexts,
    FlexibleInstances,
    DerivingStrategies,
    TypeFamilies,
    RecordWildCards,
    MultiParamTypeClasses,
    QuantifiedConstraints,
    UndecidableInstances,
    CPP,
    RankNTypes,
    ScopedTypeVariables
    #-}
module Concordium.GlobalState where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Proxy
import Data.Functor.Identity
import Data.IORef (newIORef,writeIORef)
import Control.Monad.Trans.RWS.Strict

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Classes
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Basic.TreeState as Basic
import qualified Concordium.GlobalState.Persistent.BlockState as Persistent
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore,destroyTempBlobStore)
import Concordium.GlobalState.Persistent.BlockState (PersistentBlockStateContext(..), PersistentBlockStateMonad, PersistentBlockState)
#ifdef RUST
import qualified Concordium.GlobalState.Implementation.FFI as Rust
import qualified Concordium.GlobalState.Implementation.TreeState as Rust
#endif

-- |A newtype wrapper for providing instances of the block state related monads:
-- 'BlockStateTypes', 'BlockStateQuery', 'BlockStateOperations' and 'BlockStateStorage'.
--
-- For the monad @BlockStateM c r g s m@, the underlying monad @m@ should satisfy
-- @MonadReader r m@ and @MonadState s m@.  The types @c@ and @s@ should be components
-- of the context @r@ and state @s@, satisfying @HasGlobalStateContext c r@ and
-- @HasGlobalState g s@ respectively.
--
-- The particular instances for the block state monads are determined by the @c@ and @g@
-- parameters (although currently @g@ is not used).
--
-- * If @c@ is @()@, the block state is a pure, in-memory, Haskell implementation using
--   'Basic.PureBlockStateMonad'.
--
-- * If @c@ is 'PersistentBlockStateContext', the block state is a persistent, Haskell
--   implementation using 'PersistentBlockStateMonad'.
newtype BlockStateM c r g s m a = BlockStateM (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving via (FocusGlobalStateM c g m)
    instance (HasGlobalStateContext c r, MonadReader r m)
        => MonadReader c (BlockStateM c r g s m)
deriving via (FocusGlobalStateM c g m)
    instance (HasGlobalState g s, MonadState s m)
        => MonadState g (BlockStateM c r g s m)

deriving via (Basic.PureBlockStateMonad m)
    instance (HasGlobalStateContext () r)
        => BlockStateTypes (BlockStateM () r g s m)
deriving via (Basic.PureBlockStateMonad m)
    instance (HasGlobalStateContext () r, Monad m)
        => BlockStateQuery (BlockStateM () r g s m)
deriving via (Basic.PureBlockStateMonad m)
    instance (HasGlobalStateContext () r, Monad m)
        => BlockStateOperations (BlockStateM () r g s m)
deriving via (Basic.PureBlockStateMonad m)
    instance (HasGlobalStateContext () r, Monad m)
        => BlockStateStorage (BlockStateM () r g s m)

deriving via (PersistentBlockStateMonad PersistentBlockStateContext m)
    instance (HasGlobalStateContext PersistentBlockStateContext r)
        => BlockStateTypes (BlockStateM PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m)
        => BlockStateQuery (BlockStateM PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m)
        => BlockStateOperations (BlockStateM PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m)
        => BlockStateStorage (BlockStateM PersistentBlockStateContext r g s m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state is determined by the @g@ parameter (the global state type).
--
-- * If @g@ is 'Basic.SkovData', then the in-memory, Haskell tree state is used.
-- * If @g@ is 'Rust.SkovData', then the Rust tree state is used.
newtype GlobalStateM c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO)
    deriving (BlockStateTypes) via (BlockStateM c r g s m) 

deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateQuery (BlockStateM c r g s m))
        => BlockStateQuery (GlobalStateM c r g s m)
deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateOperations (BlockStateM c r g s m))
        => BlockStateOperations (GlobalStateM c r g s m)
deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateStorage (BlockStateM c r g s m))
        => BlockStateStorage (GlobalStateM c r g s m)

deriving via (TreeStateM (Basic.SkovData bs) (BlockStateM c r (Basic.SkovData bs) s m))
    instance (bs ~ BlockState (BlockStateM c r (Basic.SkovData bs) s m), BlockStateTypes (GlobalStateM c r (Basic.SkovData bs) s m))
        => GlobalStateTypes (GlobalStateM c r (Basic.SkovData bs) s m)
deriving via (TreeStateM (Basic.SkovData bs) (BlockStateM c r (Basic.SkovData bs) s m))
    instance (bs ~ BlockState (BlockStateM c r (Basic.SkovData bs) s m), BlockStateStorage (BlockStateM c r (Basic.SkovData bs) s m), MonadState s m, HasGlobalState (Basic.SkovData bs) s)
        => TreeStateMonad (GlobalStateM c r (Basic.SkovData bs) s m)

#ifdef RUST

deriving via (TreeStateM (Rust.SkovData bs) (BlockStateM c r (Rust.SkovData bs) s m))
    instance (bs ~ BlockState (BlockStateM c r (Rust.SkovData bs) s m), BlockStateTypes (GlobalStateM c r (Rust.SkovData bs) s m))
        => GlobalStateTypes (GlobalStateM c r (Rust.SkovData bs) s m)
deriving via (TreeStateM (Rust.SkovData bs) (BlockStateM c r (Rust.SkovData bs) s m))
    instance (bs ~ BlockState (BlockStateM c r (Rust.SkovData bs) s m), BlockStateStorage (BlockStateM c r (Rust.SkovData bs) s m), MonadState s m, HasGlobalState (Rust.SkovData bs) s, MonadIO m)
        => TreeStateMonad (GlobalStateM c r (Rust.SkovData bs) s m)

#endif

-- |This class is implemented by types that determine configurations for the global state.
class (
        forall m r s. (HasGlobalStateContext (GSContext c) r, MonadReader r m, HasGlobalState (GSState c) s, MonadState s m, MonadIO m) => TreeStateMonad (GlobalStateM (GSContext c) r (GSState c) s m)
    ) => GlobalStateConfig c where
    -- TODO: making these data families could give better error messages
    type GSContext c :: *
    type GSState c :: *
    -- |Generate context and state from the initial configuration. This may
    -- have 'IO' side effects to set up any necessary storage.
    initialiseGlobalState :: c -> IO (GSContext c, GSState c)
    -- |Shutdown the global state.
    shutdownGlobalState :: Proxy c -> GSContext c -> GSState c -> IO ()

-- |Run a (read-only) query against the global state.
queryGlobalState ::
    forall c a. (TreeStateMonad (GlobalStateM (GSContext c) (Identity (GSContext c)) (GSState c) (Identity (GSState c)) (RWST (Identity (GSContext c)) () (Identity (GSState c)) IO)))
    => Proxy c -> GSContext c -> GSState c -> (forall m. (TreeStateMonad m) => m a) -> IO a
queryGlobalState _ = qgs
    where
        qgs :: GSContext c -> GSState c -> GlobalStateM (GSContext c) (Identity (GSContext c)) (GSState c) (Identity (GSState c)) (RWST (Identity (GSContext c)) () (Identity (GSState c)) IO) a -> IO a
        qgs c s a = fst <$> (evalRWST (runGlobalStateM a) (Identity c) (Identity s))

-- |Configuration that uses in-memory, Haskell implementations for both tree state and block state.
data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData Basic.BlockState

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    type GSContext MemoryTreeMemoryBlockConfig = ()
    type GSState MemoryTreeMemoryBlockConfig = Basic.SkovData Basic.BlockState
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = return ((), Basic.initialSkovData rtparams gendata bs)
    shutdownGlobalState _ _ _ = return ()

-- |Configuration that uses the in-memory, Haskell implementation of tree state and the
-- persistent Haskell implementation of block state.
data MemoryTreeDiskBlockConfig = MTDBConfig RuntimeParameters GenesisData Basic.BlockState

-- |Configuration that uses the Rust implementation of tree state and the
-- in-memory, Haskell implmentation of the block state.
instance GlobalStateConfig MemoryTreeDiskBlockConfig where
    type GSContext MemoryTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState MemoryTreeDiskBlockConfig = Basic.SkovData PersistentBlockState
    initialiseGlobalState (MTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore
        pbscModuleCache <- newIORef Persistent.emptyModuleCache
        pbs <- Persistent.makePersistent bs
        return ((PersistentBlockStateContext{..}), Basic.initialSkovData rtparams gendata pbs)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

#ifdef RUST

data DiskTreeMemoryBlockConfig = DTMBConfig RuntimeParameters GenesisData Basic.BlockState Rust.GlobalStatePtr

instance GlobalStateConfig DiskTreeMemoryBlockConfig where
    type GSContext DiskTreeMemoryBlockConfig = ()
    type GSState DiskTreeMemoryBlockConfig = Rust.SkovData Basic.BlockState
    initialiseGlobalState (DTMBConfig rtparams gendata bs gsptr) = do
        isd <- Rust.initialSkovData rtparams gendata bs gsptr
        return ((), isd)
    shutdownGlobalState _ _ _ = return ()

-- |Configuration that uses the Rust implementation of tree state and the
-- persistent Haskell implementation of block state.
data DiskTreeDiskBlockConfig = DTDBConfig RuntimeParameters GenesisData Basic.BlockState Rust.GlobalStatePtr

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    type GSContext DiskTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState DiskTreeDiskBlockConfig = Rust.SkovData PersistentBlockState
    initialiseGlobalState (DTDBConfig rtparams gendata bs gsptr) = do
        pbscBlobStore <- createTempBlobStore
        pbscModuleCache <- newIORef Persistent.emptyModuleCache
        pbs <- Persistent.makePersistent bs
        isd <- Rust.initialSkovData rtparams gendata pbs gsptr
        return ((PersistentBlockStateContext{..}), isd)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

#endif