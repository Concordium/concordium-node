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

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Classes
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.Basic.Block as Basic
import qualified Concordium.GlobalState.Basic.BlockPointer as Basic
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Basic.TreeState as Basic
import qualified Concordium.GlobalState.Persistent.BlockState as Persistent
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore,destroyTempBlobStore)
import Concordium.GlobalState.Persistent.BlockState (PersistentBlockStateContext(..), PersistentBlockStateMonad, PersistentBlockState)
#ifdef RUST
import qualified Concordium.GlobalState.Implementation.FFI as Rust
import qualified Concordium.GlobalState.Implementation.TreeState as Rust
#endif

newtype GlobalStateM0 c r g s m a = GlobalStateM0 (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving via (FocusGlobalStateM c g m) instance (HasGlobalStateContext c r, MonadReader r m) => MonadReader c (GlobalStateM0 c r g s m)
deriving via (FocusGlobalStateM c g m) instance (HasGlobalState g s, MonadState s m) => MonadState g (GlobalStateM0 c r g s m)

deriving via (Basic.PureBlockStateMonad m) instance (HasGlobalStateContext () r) => BlockStateTypes (GlobalStateM0 () r g s m)
deriving via (Basic.PureBlockStateMonad m) instance (HasGlobalStateContext () r, Monad m) => BlockStateQuery (GlobalStateM0 () r g s m)
deriving via (Basic.PureBlockStateMonad m) instance (HasGlobalStateContext () r, Monad m) => BlockStateOperations (GlobalStateM0 () r g s m)
deriving via (Basic.PureBlockStateMonad m) instance (HasGlobalStateContext () r, Monad m) => BlockStateStorage (GlobalStateM0 () r g s m)

deriving via (PersistentBlockStateMonad PersistentBlockStateContext m) instance (HasGlobalStateContext PersistentBlockStateContext r) => BlockStateTypes (GlobalStateM0 PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m)) instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m) => BlockStateQuery (GlobalStateM0 PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m)) instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m) => BlockStateOperations (GlobalStateM0 PersistentBlockStateContext r g s m)
deriving via (PersistentBlockStateMonad PersistentBlockStateContext (FocusGlobalStateM PersistentBlockStateContext g m)) instance (HasGlobalStateContext PersistentBlockStateContext r, MonadReader r m, MonadIO m) => BlockStateStorage (GlobalStateM0 PersistentBlockStateContext r g s m)

newtype GlobalStateM c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO)
    deriving (BlockStateTypes) via (GlobalStateM0 c r g s m) 

deriving via (GlobalStateM0 c r g s m) instance (Monad m, BlockStateQuery (GlobalStateM0 c r g s m)) => BlockStateQuery (GlobalStateM c r g s m)
deriving via (GlobalStateM0 c r g s m) instance (Monad m, BlockStateOperations (GlobalStateM0 c r g s m)) => BlockStateOperations (GlobalStateM c r g s m)
deriving via (GlobalStateM0 c r g s m) instance (Monad m, BlockStateStorage (GlobalStateM0 c r g s m)) => BlockStateStorage (GlobalStateM c r g s m)

deriving via (TreeStateM (Basic.SkovData bs) (GlobalStateM0 c r (Basic.SkovData bs) s m))
    instance (bs ~ BlockState (GlobalStateM0 c r (Basic.SkovData bs) s m), BlockStateTypes (GlobalStateM c r (Basic.SkovData bs) s m))
        => GlobalStateTypes (GlobalStateM c r (Basic.SkovData bs) s m)
deriving via (TreeStateM (Basic.SkovData bs) (GlobalStateM0 c r (Basic.SkovData bs) s m))
    instance (bs ~ BlockState (GlobalStateM0 c r (Basic.SkovData bs) s m), BlockStateStorage (GlobalStateM0 c r (Basic.SkovData bs) s m), MonadState s m, HasGlobalState (Basic.SkovData bs) s)
        => TreeStateMonad (GlobalStateM c r (Basic.SkovData bs) s m)

#ifdef RUST

deriving via (TreeStateM (Rust.SkovData bs) (GlobalStateM0 c r (Rust.SkovData bs) s m))
    instance (bs ~ BlockState (GlobalStateM0 c r (Rust.SkovData bs) s m), BlockStateTypes (GlobalStateM c r (Rust.SkovData bs) s m))
        => GlobalStateTypes (GlobalStateM c r (Rust.SkovData bs) s m)
deriving via (TreeStateM (Rust.SkovData bs) (GlobalStateM0 c r (Rust.SkovData bs) s m))
    instance (bs ~ BlockState (GlobalStateM0 c r (Rust.SkovData bs) s m), BlockStateStorage (GlobalStateM0 c r (Rust.SkovData bs) s m), MonadState s m, HasGlobalState (Rust.SkovData bs) s, MonadIO m)
        => TreeStateMonad (GlobalStateM c r (Rust.SkovData bs) s m)

#endif


{-
bpid :: Basic.BasicBlockPointer bs -> BlockPointer (GlobalStateM c r (Basic.SkovData bs) s m)
bpid = id

md :: (HasGlobalStateContext PersistentBlockStateContext r, HasGlobalState (Basic.SkovData Persistent.PersistentBlockState) s, MonadState s m, MonadReader r m, MonadIO m) => BlockHash -> GlobalStateM PersistentBlockStateContext r (Basic.SkovData Persistent.PersistentBlockState) s m ()
md bh = markDead bh
-}

class (forall m r s. (HasGlobalStateContext (GSContext c) r, MonadReader r m, HasGlobalState (GSState c) s, MonadState s m, MonadIO m) => TreeStateMonad (GlobalStateM (GSContext c) r (GSState c) s m)) => GlobalStateConfig c where
    -- TODO: making these data families could give better error messages
    type GSContext c :: *
    type GSState c :: *
    initialiseGlobalState :: c -> IO (GSContext c, GSState c)
    shutdownGlobalState :: Proxy c -> GSContext c -> GSState c -> IO ()

queryGlobalState :: forall c a. (TreeStateMonad (GlobalStateM (GSContext c) (Identity (GSContext c)) (GSState c) (Identity (GSState c)) (RWST (Identity (GSContext c)) () (Identity (GSState c)) IO))) => Proxy c -> GSContext c -> GSState c -> (forall m. (TreeStateMonad m) => m a) -> IO a
queryGlobalState _ = qgs
    where
        qgs :: GSContext c -> GSState c -> GlobalStateM (GSContext c) (Identity (GSContext c)) (GSState c) (Identity (GSState c)) (RWST (Identity (GSContext c)) () (Identity (GSState c)) IO) a -> IO a
        qgs c s a = fst <$> (evalRWST (runGlobalStateM a) (Identity c) (Identity s))


data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData Basic.BlockState

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    type GSContext MemoryTreeMemoryBlockConfig = ()
    type GSState MemoryTreeMemoryBlockConfig = Basic.SkovData Basic.BlockState
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = return ((), Basic.initialSkovData rtparams gendata bs)
    shutdownGlobalState _ _ _ = return ()

data MemoryTreeDiskBlockConfig = MTDBConfig RuntimeParameters GenesisData Basic.BlockState

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