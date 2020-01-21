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

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Reader
import Data.ByteString (empty)
import Data.Functor.Identity
import Data.IORef (newIORef,writeIORef)
import Data.Proxy
import Data.Serialize.Put (runPut)

import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore, destroyTempBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState

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

deriving via (PureBlockStateMonad m)
    instance (HasGlobalStateContext () r)
        => BlockStateTypes (BlockStateM () r g s m)
deriving via (PureBlockStateMonad m)
    instance (HasGlobalStateContext () r, Monad m)
        => BlockStateQuery (BlockStateM () r g s m)
deriving via (PureBlockStateMonad m)
    instance (HasGlobalStateContext () r, Monad m)
        => BlockStateOperations (BlockStateM () r g s m)
deriving via (PureBlockStateMonad m)
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

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO,
              BlockStateTypes, BlockStateQuery, BlockStateOperations, BlockStateStorage)

deriving via (PureTreeStateMonad bs m)
    instance GlobalStateTypes (TreeStateM (SkovData bs) m)
deriving via (PureTreeStateMonad bs m)
    instance (bs ~ GS.BlockState m,
              BlockStateStorage m,
              MonadIO m,
              MonadState (SkovData bs) m,
              Monad m)
              => TreeStateMonad (TreeStateM (SkovData bs) m)
deriving via (PersistentTreeStateMonad bs m)
    instance GlobalStateTypes (TreeStateM (SkovPersistentData bs) m)
deriving via (PersistentTreeStateMonad bs m)
    instance (bs ~ GS.BlockState m,
              BlockStateStorage m,
              MonadIO m,
              MonadState (SkovPersistentData bs) m,
              Monad m)
              => TreeStateMonad (TreeStateM (SkovPersistentData bs) m)

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

deriving via (TreeStateM (SkovData bs) m)
     instance (bs ~ GS.BlockState m)
        => GlobalStateTypes (GlobalStateM c r (SkovData bs) s m)
deriving via (TreeStateM (SkovData bs) (BlockStateM () r (SkovData bs) s m))
    instance (Monad m,
              bs ~ GS.BlockState (BlockStateM () r (SkovData bs) s m),
              BlockStateStorage (BlockStateM () r (SkovData bs) s m),
              MonadState (SkovData bs) (BlockStateM () r (SkovData bs) s m),
              MonadIO (BlockStateM () r (SkovData bs) s m),
              HasGlobalStateContext () r,
              MonadReader r (BlockStateM () r (SkovData bs) s m))
        => TreeStateMonad (GlobalStateM () r (SkovData bs) s m)

deriving via (TreeStateM (SkovPersistentData bs) m)
    instance (bs ~ GS.BlockState m)
        => GlobalStateTypes (GlobalStateM c r (SkovPersistentData bs) s m)
deriving via (TreeStateM (SkovPersistentData bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m))
    instance (Monad m,
              bs ~ GS.BlockState (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m),
              BlockStateStorage (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m),
              MonadState (SkovPersistentData bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m),
              MonadIO (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m),
              HasGlobalStateContext PersistentBlockStateContext r,
              MonadReader r (BlockStateM PersistentBlockStateContext r (SkovPersistentData bs) s m))
        => TreeStateMonad (GlobalStateM PersistentBlockStateContext r (SkovPersistentData bs) s m)

-- |This class is implemented by types that determine configurations for the global state.
class GlobalStateConfig c where
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
data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    type GSContext MemoryTreeMemoryBlockConfig = ()
    type GSState MemoryTreeMemoryBlockConfig = SkovData BS.BlockState
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = do
      return ((), initialSkovData rtparams gendata bs)
    shutdownGlobalState _ _ _ = return ()

-- |Configuration that uses the in-memory, Haskell implementation of tree state and the
-- persistent Haskell implementation of block state.
data MemoryTreeDiskBlockConfig = MTDBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig MemoryTreeDiskBlockConfig where
    type GSContext MemoryTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState MemoryTreeDiskBlockConfig = SkovData PersistentBlockState
    initialiseGlobalState (MTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore
        pbscModuleCache <- newIORef emptyModuleCache
        let pbsc = PersistentBlockStateContext{..}
        pbs <- makePersistent bs
        _ <- runPut <$> runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        return (pbsc, initialSkovData rtparams gendata pbs)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache emptyModuleCache

-- |Configuration that uses the disk tree state and the memory block state
data DiskTreeMemoryBlockConfig = DTMBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig DiskTreeMemoryBlockConfig where
    type GSContext DiskTreeMemoryBlockConfig = ()
    type GSState DiskTreeMemoryBlockConfig = SkovPersistentData BS.BlockState
    initialiseGlobalState (DTMBConfig rtparams gendata bs) = do
        isd <- initialSkovPersistentData rtparams gendata bs empty
        return ((), isd)
    shutdownGlobalState _ _ _ = return ()

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig = DTDBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    type GSContext DiskTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState DiskTreeDiskBlockConfig = SkovPersistentData PersistentBlockState
    initialiseGlobalState (DTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runPut <$> runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        isd <- initialSkovPersistentData rtparams gendata pbs serBS
        return (pbsc, isd)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache emptyModuleCache
