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
    ScopedTypeVariables,
    ConstraintKinds,
    PartialTypeSignatures
    #-}
module Concordium.GlobalState where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader
import Data.IORef (newIORef,writeIORef)
import Data.Proxy
import Data.Serialize.Put (runPut)
import System.FilePath
import Lens.Micro.Platform
import qualified Data.Text as Text

import Concordium.Types.Transactions

import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.AccountTransactionIndex
import qualified Concordium.GlobalState.Persistent.BlockState as Persistent
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore,destroyTempBlobStore)
import Concordium.GlobalState.Persistent.BlockState (PersistentBlockStateContext(..), PersistentBlockStateMonad, PersistentBlockState)

import Concordium.GlobalState.SQLiteATI

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
--
-- For the monad @TreeStateM s m@, the underlying monad @m@ should satisfy
-- @MonadState s m@.
--
-- * If @s@ is 'SkovData bs', then the in-memory, Haskell tree state is used.
-- * If @s@ is 'SkovPersistentData ati bs', then the persistent Haskell tree state is used.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO,
              BlockStateTypes, BlockStateQuery, BlockStateOperations, BlockStateStorage)

-- For in-memory skov data we do not do any transaction logging for now.
instance ATITypes (TreeStateM (SkovData bs) m) where
  type ATIStorage (TreeStateM (SkovData bs) m) = ()
instance (Monad m) => PerAccountDBOperations (TreeStateM (SkovData bs) m) where
  -- default implementation

-- No log instance
deriving via PersistentTreeStateMonad () bs m
   instance (PerAccountDBOperations m,
             ATIStorage m ~ ()) => PerAccountDBOperations (TreeStateM (SkovPersistentData () bs) m)

-- |Global State types for the memory Tree State implementation
deriving via (PureTreeStateMonad bs m)
    instance GlobalStateTypes (TreeStateM (SkovData bs) m)
-- |TreeStateMonad instance for the memory Tree State implementation
deriving via (PureTreeStateMonad bs m)
    instance (bs ~ GS.BlockState m,
              BlockStateStorage m,
              MonadIO m,
              MonadState (SkovData bs) m,
              ATIStorage m ~ (),
              Monad m)
              => TreeStateMonad (TreeStateM (SkovData bs) m)

-- |Global State types for the disk Tree State implementation
deriving via (PersistentTreeStateMonad ati bs m)
    instance GlobalStateTypes (TreeStateM (SkovPersistentData ati bs) m)

instance ATITypes (TreeStateM (SkovPersistentData () bs) m) where
  type ATIStorage (TreeStateM (SkovPersistentData () bs) m) = ()

-- |TreeStateMonad instance for the disk Tree State implementation
deriving via (PersistentTreeStateMonad () bs m)
    instance (bs ~ GS.BlockState m,
              BlockStateStorage m,
              MonadIO m,
              MonadState (SkovPersistentData () bs) m,
              ATIStorage m ~ (),
              PerAccountDBOperations m,
              Monad m)
              => TreeStateMonad (TreeStateM (SkovPersistentData () bs) m)

-- |Common constraints needed on the monad m and Skov state with block state 'bs'
-- to implement the block pointer monad.
type BPMStateConstraints m (f :: * -> *) (bs :: *) = (bs ~ GS.BlockState m, Monad m, MonadState (f bs) m)

deriving via (PureTreeStateMonad bs m)
    instance (BPMStateConstraints m SkovData bs)
              => BlockPointerMonad (TreeStateM (SkovData bs) m)

deriving via (PersistentTreeStateMonad () bs m)
    instance (BPMStateConstraints m (SkovPersistentData ()) bs,
              MonadIO m,
              ATIStorage m ~ (),
              ATITypes m,
              BlockStateStorage m)
              => BlockPointerMonad (TreeStateM (SkovPersistentData () bs) m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state monad instances are derived directly from 'TreeStateM'.
-- The arguments c, r, g, s, m, a are as in BlockStateM, whereas the argument @db@
-- is an additional context that manages auxiliary databases not needed by consensus.
-- In particular this means the index of transactions that affect a given account.
newtype GlobalStateM db c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO)
    deriving (BlockStateTypes) via (BlockStateM c r g s m)

deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateQuery (BlockStateM c r g s m))
        => BlockStateQuery (GlobalStateM db c r g s m)
deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateOperations (BlockStateM c r g s m))
        => BlockStateOperations (GlobalStateM db c r g s m)
deriving via (BlockStateM c r g s m)
    instance (Monad m, BlockStateStorage (BlockStateM c r g s m))
        => BlockStateStorage (GlobalStateM db c r g s m)

-- Deriving the global state types depending on the tree state implementation
deriving via (TreeStateM (SkovData bs) m)
     instance (bs ~ GS.BlockState (BlockStateM c r (SkovData bs) s m))
        => GlobalStateTypes (GlobalStateM NoLogContext c r (SkovData bs) s m)
-- Deriving the persistent implementation of the tree state monad
deriving via (TreeStateM (SkovPersistentData ati bs) m)
    instance (bs ~ GS.BlockState (BlockStateM c r (SkovPersistentData ati bs) s m))
        => GlobalStateTypes (GlobalStateM db c r (SkovPersistentData ati bs) s m)

-- Generic instances which do no logging of transactions.
instance ATITypes (GlobalStateM NoLogContext c r bs s m) where
  type ATIStorage (GlobalStateM NoLogContext c r bs s m) = ()
instance (Monad m) => PerAccountDBOperations (GlobalStateM NoLogContext c r bs s m) where
  -- default implementation

-- Instances which do no transaction logging.
instance ATITypes (BlockStateM c r (SkovData bs) s m) where
  type ATIStorage (BlockStateM c r (SkovData bs) s m) = ()

instance ATITypes (BlockStateM c r (SkovPersistentData () bs) s m) where
  type ATIStorage (BlockStateM c r (SkovPersistentData () bs) s m) = ()

instance (Monad m) => PerAccountDBOperations (BlockStateM c r (SkovPersistentData () bs) s m) where
  -- default implementation

instance (Monad m) => PerAccountDBOperations (BlockStateM c r (SkovData bs) s m) where
  -- default implementation

-- |Common requirements to implement the tree state monad.
-- The meaning of the arguments is
--
-- * ctx - Context for the block state
-- * r   - The context type of the monad, has to support retrieving the 'ctx'.
-- * skovData - The type of skov data (persistent or in-memory), parametrized by the block state type 'bs.
-- * bs - The type of block state.
-- * s - The state type which can support the block state type.
-- * m The underlying monad.
type TSMStateConstraints ctx r (skovData :: * -> *) bs s m =
  (Monad m,
   MonadState s m,
   HasGlobalState (skovData bs) s,
   HasGlobalStateContext ctx r,
   MonadReader r m,
   bs ~ GS.BlockState (BlockStateM ctx r (skovData bs) s m)
  )

-- |Memory Tree & Memory Block instance
deriving via (TreeStateM (SkovData bs) (BlockStateM () r (SkovData bs) s m))
    instance (TSMStateConstraints () r SkovData bs s m,
              BlockStateStorage (BlockStateM () r (SkovData bs) s m),
              MonadIO m)
        => TreeStateMonad (GlobalStateM NoLogContext () r (SkovData bs) s m)

-- |Memory Tree & Disk Block instance
deriving via (TreeStateM (SkovData bs) (BlockStateM PersistentBlockStateContext r (SkovData bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r SkovData bs s m,
              BlockStateStorage (BlockStateM PersistentBlockStateContext r (SkovData bs) s m),
              MonadIO m)
        => TreeStateMonad (GlobalStateM NoLogContext PersistentBlockStateContext r (SkovData bs) s m)

-- |Disk Tree & Disk Block instance without any transaction logging.
deriving via (TreeStateM (SkovPersistentData () bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData () bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r (SkovPersistentData ()) bs s m,
              BlockStateStorage (BlockStateM PersistentBlockStateContext r (SkovPersistentData () bs) s m),
              ATIStorage m ~ (),
              MonadIO m)
        => TreeStateMonad (GlobalStateM NoLogContext PersistentBlockStateContext r (SkovPersistentData () bs) s m)

-- |Memory Tree & Memory Block instance
deriving via (TreeStateM (SkovData bs) (BlockStateM () r (SkovData bs) s m))
    instance (TSMStateConstraints () r SkovData bs s m)
        => BlockPointerMonad (GlobalStateM NoLogContext () r (SkovData bs) s m)

-- |Memory Tree & Disk Block instance
deriving via (TreeStateM (SkovData bs) (BlockStateM PersistentBlockStateContext r (SkovData bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r SkovData bs s m)
        => BlockPointerMonad (GlobalStateM NoLogContext PersistentBlockStateContext r (SkovData bs) s m)

-- |Disk Tree & Disk Block instance without any transaction logging.
deriving via (TreeStateM (SkovPersistentData () bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData () bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r (SkovPersistentData ()) bs s m,
              ATIStorage m ~ (),
              MonadIO m)
        => BlockPointerMonad (GlobalStateM NoLogContext PersistentBlockStateContext r (SkovPersistentData () bs) s m)


-- |This class is implemented by types that determine configurations for the global state.
class GlobalStateConfig c where
    -- TODO: making these data families could give better error messages
    type GSContext c :: *
    type GSState c :: *
    type GSLogContext c :: *
    -- |Generate context and state from the initial configuration. This may
    -- have 'IO' side effects to set up any necessary storage.
    initialiseGlobalState :: c -> IO (GSContext c, GSState c, GSLogContext c)
    -- |Shutdown the global state.
    shutdownGlobalState :: Proxy c -> GSContext c -> GSState c -> GSLogContext c -> IO ()

-- |Configuration that uses in-memory, Haskell implementations for both tree state and block state.
data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    type GSContext MemoryTreeMemoryBlockConfig = ()
    type GSState MemoryTreeMemoryBlockConfig = SkovData BS.BlockState
    type GSLogContext MemoryTreeMemoryBlockConfig = NoLogContext
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = do
      return ((), initialSkovData rtparams gendata bs, NoLogContext)
    shutdownGlobalState _ _ _ _ = return ()

-- |Configuration that uses the in-memory, Haskell implementation of tree state and the
-- persistent Haskell implementation of block state.
data MemoryTreeDiskBlockConfig = MTDBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the Haskell implementation of tree state and the
-- in-memory, Haskell implmentation of the block state.
instance GlobalStateConfig MemoryTreeDiskBlockConfig where
    type GSContext MemoryTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState MemoryTreeDiskBlockConfig = SkovData PersistentBlockState
    type GSLogContext MemoryTreeDiskBlockConfig = NoLogContext
    initialiseGlobalState (MTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        let pbsc = PersistentBlockStateContext{..}
        pbs <- makePersistent bs
        _ <- runPut <$> runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        return (pbsc, initialSkovData rtparams gendata pbs, NoLogContext)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig = DTDBConfig RuntimeParameters GenesisData BS.BlockState

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    type GSContext DiskTreeDiskBlockConfig = PersistentBlockStateContext
    type GSState DiskTreeDiskBlockConfig = SkovPersistentData () PersistentBlockState
    type GSLogContext DiskTreeDiskBlockConfig = NoLogContext
    initialiseGlobalState (DTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        isd <- initialSkovPersistentData rtparams gendata pbs ((), NoLogContext) serBS
        return (pbsc, isd, NoLogContext)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

-- * Instances with logging.

-- |Disk Tree & Disk Block instance with transaction logging.

data PerAccountAffectIndex = PAAIConfig Text.Text
-- When we want to dump data to disk.
data DiskDump
type instance ATIValues DiskDump = AccountTransactionIndex
type instance ATIContext DiskDump = PerAccountAffectIndex

instance ATITypes (BlockStateM c r (SkovPersistentData DiskDump bs) s m) where
  type ATIStorage (BlockStateM c r (SkovPersistentData DiskDump bs) s m) = ATIValues DiskDump

instance ATITypes (GlobalStateM PerAccountAffectIndex c r (SkovPersistentData DiskDump bs) s m) where
  type ATIStorage (GlobalStateM PerAccountAffectIndex c r (SkovPersistentData DiskDump bs) s m) = ATIValues DiskDump

instance ATITypes (TreeStateM (SkovPersistentData DiskDump bs) m) where
  type ATIStorage (TreeStateM (SkovPersistentData DiskDump bs) m) = ATIValues DiskDump

-- This really should be derived

-- FIXME: We should derive the two instances which use this instead of using this hack.
doFlushBlockSummaries :: (MonadState s m,
                          HasLogContext (ATIContext DiskDump) s,
                          MonadIO m) =>
                          BlockContext -> ATIValues DiskDump -> [SpecialTransactionOutcome] -> m ()
doFlushBlockSummaries bh ati sos = do
    PAAIConfig handle <- use logContext
    liftIO $ writeEntries handle bh ati sos

instance (MonadIO m, s ~ (SkovPersistentData DiskDump bs), MonadState s m, HasLogContext (ATIContext DiskDump) s)
         => PerAccountDBOperations (TreeStateM (SkovPersistentData DiskDump bs) m) where
  flushBlockSummaries = doFlushBlockSummaries

instance HasLogContext PerAccountAffectIndex (SkovPersistentData DiskDump bs) where
  logContext = atiCtx

deriving via (TreeStateM (SkovPersistentData DiskDump bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData DiskDump bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r (SkovPersistentData DiskDump) bs s m,
              MonadIO m)
        => BlockPointerMonad (GlobalStateM PerAccountAffectIndex PersistentBlockStateContext r (SkovPersistentData DiskDump bs) s m)

-- Horrible misuse of undecidable instances
deriving via (PersistentTreeStateMonad DiskDump bs (TreeStateM (SkovPersistentData DiskDump bs) m))
    instance (BPMStateConstraints m (SkovPersistentData DiskDump) bs,
              MonadIO m,
              BlockStateStorage m)
              => BlockPointerMonad (TreeStateM (SkovPersistentData DiskDump bs) m)

deriving via (PersistentTreeStateMonad DiskDump bs (TreeStateM (SkovPersistentData DiskDump bs) m))
    instance (bs ~ GS.BlockState m,
              BlockStateStorage m,
              MonadIO m,
              MonadState (SkovPersistentData DiskDump bs) m,
              Monad m)
             => TreeStateMonad (TreeStateM (SkovPersistentData DiskDump bs) m)

deriving via (TreeStateM (SkovPersistentData DiskDump bs) m) instance
  (MonadIO m, MonadState (SkovPersistentData DiskDump bs) m) => PerAccountDBOperations (GlobalStateM
                                       PerAccountAffectIndex
                                       PersistentBlockStateContext
                                       r
                                       (SkovPersistentData DiskDump bs)
                                       s
                                       (TreeStateM (SkovPersistentData DiskDump bs) m))

instance (MonadIO m, MonadState s m, HasLogContext (ATIContext DiskDump) s)
         => PerAccountDBOperations
            (GlobalStateM
             PerAccountAffectIndex
             PersistentBlockStateContext
             r
             (SkovPersistentData DiskDump bs)
             s
             m) where
  flushBlockSummaries = doFlushBlockSummaries

deriving via (TreeStateM (SkovPersistentData DiskDump bs) (BlockStateM PersistentBlockStateContext r (SkovPersistentData DiskDump bs) s m))
    instance (TSMStateConstraints PersistentBlockStateContext r (SkovPersistentData DiskDump) bs s m,
              BlockStateStorage (BlockStateM PersistentBlockStateContext r (SkovPersistentData DiskDump bs) s m),
              HasLogContext (ATIContext DiskDump) s,
              MonadIO m)
        => TreeStateMonad (GlobalStateM PerAccountAffectIndex PersistentBlockStateContext r (SkovPersistentData DiskDump bs) s m)

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state, as well as a 
data DiskTreeDiskBlockWithLogConfig = DTDBWLConfig {
  configRP :: RuntimeParameters,
  configGD :: GenesisData,
  configBS :: BS.BlockState,
  configTxLog :: !FilePath
  }

instance GlobalStateConfig DiskTreeDiskBlockWithLogConfig where
    type GSContext DiskTreeDiskBlockWithLogConfig = PersistentBlockStateContext
    type GSState DiskTreeDiskBlockWithLogConfig = SkovPersistentData DiskDump PersistentBlockState
    type GSLogContext DiskTreeDiskBlockWithLogConfig = PerAccountAffectIndex

    initialiseGlobalState (DTDBWLConfig rtparams gendata bs txLog) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        let handle = Text.pack txLog
        createTable handle
        let ati = defaultValue
        isd <- initialSkovPersistentData rtparams gendata pbs (ati, PAAIConfig handle) serBS
        return (pbsc, isd, PAAIConfig handle)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ (PAAIConfig _) = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache
