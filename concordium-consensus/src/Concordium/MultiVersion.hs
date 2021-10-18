{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module defines a multi-version consensus runner. In particular, this supports protocol
-- updates by restarting the consensus with a new genesis block derived from the state of the last
-- finalized block of the previous chain. This is embodied in the 'MultiVersionRunner' type, which
-- encapsulates the state and context required to run multiple consensus instances. The 'MVR' monad
-- is used to run operations within the multi-version runner.
module Concordium.MultiVersion where

import Control.Concurrent
import Control.Exception
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Kind
import Data.Serialize
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec
import System.FilePath

import Concordium.Logger hiding (Baker)
import Concordium.Types
import Concordium.Types.Block
import Concordium.Types.Transactions
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus (..))
import Concordium.Types.Updates

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Birk.Bake
import Concordium.GlobalState
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Paired
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState (TreeStateMonad)
import Concordium.ImportExport
import Concordium.ProtocolUpdate
import Concordium.Skov as Skov
import Concordium.TimeMonad
import Concordium.TimerMonad

-- |Handler configuration for supporting protocol updates.
-- This handler defines an instance of 'HandlerConfigHandlers' that responds to finalization events
-- by checking for protocol updates (see 'checkForProtocolUpdate').
data UpdateHandler = UpdateHandler

-- |State for the 'UpdateHandler' handler configuration.
-- This records whether the handler was notified about a (pending) protocol update.
-- Since a pending protocol update may be replaced, the last seen pending protocol update is
-- recorded to detect any such replacement.
data UpdateHandlerState
    = -- |Indicates that no pending update has been notified.
      NeverNotified
    | -- |Indicates that the specified protocol update has already been notified.
      AlreadyNotified
        { -- |The effective time of the update.
          uhNotifiedEffectiveTimestamp :: !TransactionTime,
          -- |The contents of the update itself.
          uhNotifiedProtocolUpdate :: !ProtocolUpdate
        }
    deriving (Eq)

instance HandlerConfig UpdateHandler where
    type HCContext UpdateHandler = ()
    type HCState UpdateHandler = UpdateHandlerState
    initialiseHandler UpdateHandler = ((), NeverNotified)

instance
    ( MultiVersionStateConfig gc,
      MultiVersion gc fc,
      SkovConfiguration gc fc UpdateHandler,
      SkovMonad pv (VersionedSkovM gc fc pv),
      IsProtocolVersion pv
    ) =>
    HandlerConfigHandlers UpdateHandler (VersionedSkovM gc fc pv)
    where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ -> checkForProtocolUpdate

-- |Configuration for the global state that uses disk storage
-- for both tree state and block state.
newtype DiskStateConfig = DiskStateConfig
    { -- |Root directory for the global state.
      stateBasePath :: FilePath
    }

-- |Configuration for the global state that logs finalized
-- transactions in an external database.
newtype TransactionDBConfig = TransactionDBConfig
    { -- |Database connection string.
      dbConnString :: ByteString
    }

-- |Configuration information for a multi-version runner.
-- The first type parameter defines the global state configuration, and should be an instance of
-- 'MultiVersionStateConfig' (and, as a superclass, 'GlobalStateConfig').
-- The second type parameter defines the finalization configuration, and should be an instance of
-- 'FinalizationConfig'.
data MultiVersionConfiguration gsconf finconf = MultiVersionConfiguration
    { -- |Configuration for the global state.
      mvcStateConfig :: !(StateConfig gsconf),
      -- |Configuration for transaction logging.
      mvcTXLogConfig :: !(TXLogConfig gsconf),
      -- |Configuration for finalization.
      mvcFinalizationConfig :: !finconf,
      -- |Runtime parameters.
      mvcRuntimeParameters :: !RuntimeParameters
    }

-- |This class provides a mechanism for instantiating a global state configuration for a new
-- genesis.
class
    (GlobalStateConfig gsconf) =>
    MultiVersionStateConfig (gsconf :: ProtocolVersion -> Type)
    where
    -- |Type of state configuration data.
    type StateConfig gsconf

    -- |Type of transaction logging data.
    type TXLogConfig gsconf

    -- |Create a global state configuration for a specific genesis.
    globalStateConfig ::
        IsProtocolVersion pv =>
        StateConfig gsconf ->
        TXLogConfig gsconf ->
        RuntimeParameters ->
        GenesisIndex ->
        -- |Absolute height of the genesis block.
        AbsoluteBlockHeight ->
        GenesisData pv ->
        gsconf pv

instance MultiVersionStateConfig MemoryTreeMemoryBlockConfig where
    type StateConfig MemoryTreeMemoryBlockConfig = ()
    type TXLogConfig MemoryTreeMemoryBlockConfig = ()
    globalStateConfig _ _ rtp _ _ gd = MTMBConfig rtp gd

instance MultiVersionStateConfig DiskTreeDiskBlockConfig where
    type StateConfig DiskTreeDiskBlockConfig = DiskStateConfig
    type TXLogConfig DiskTreeDiskBlockConfig = ()
    globalStateConfig DiskStateConfig{..} _ rtp gi _ gd =
        ( DTDBConfig
            { dtdbRuntimeParameters = rtp,
              dtdbTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
              dtdbBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat",
              dtdbGenesisData = gd
            }
        )

instance MultiVersionStateConfig DiskTreeDiskBlockWithLogConfig where
    type StateConfig DiskTreeDiskBlockWithLogConfig = DiskStateConfig
    type TXLogConfig DiskTreeDiskBlockWithLogConfig = TransactionDBConfig
    globalStateConfig DiskStateConfig{..} TransactionDBConfig{..} rtp gi genHeight gd =
        ( DTDBWLConfig
            { dtdbwlRuntimeParameters = rtp,
              dtdbwlTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
              dtdbwlBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat",
              dtdbwlGenesisData = gd,
              dtdbwlTxDBConnectionString = dbConnString,
              dtdbwlGenesisHeight = genHeight
            }
        )

instance
    (MultiVersionStateConfig c1, MultiVersionStateConfig c2) =>
    MultiVersionStateConfig (PairGSConfig c1 c2)
    where
    type StateConfig (PairGSConfig c1 c2) = (StateConfig c1, StateConfig c2)
    type TXLogConfig (PairGSConfig c1 c2) = (TXLogConfig c1, TXLogConfig c2)
    globalStateConfig (sc1, sc2) (txc1, txc2) rtp gi gh gd =
        PairGSConfig
            ( globalStateConfig sc1 txc1 rtp gi gh gd,
              globalStateConfig sc2 txc2 rtp gi gh gd
            )

-- |Callback functions for communicating with the network layer.
data Callbacks = Callbacks
    { -- | Broadcast a (versioned) block on the network.
      broadcastBlock :: GenesisIndex -> ByteString -> IO (),
      -- | Broadcast a (versioned) finalization message on the network.
      broadcastFinalizationMessage :: GenesisIndex -> ByteString -> IO (),
      -- | Broadcast a (versioned) finalization record on the network.
      -- TODO: Possibly deprecate this.
      broadcastFinalizationRecord :: GenesisIndex -> ByteString -> IO (),
      -- |Send a catch-up status message to all (non-pending) peers.
      -- This should be used when a pending block becomes live.
      -- The status message should be neither a request nor a response.
      notifyCatchUpStatus :: GenesisIndex -> ByteString -> IO (),
      -- |Notify the P2P layer that we have a new genesis block.
      notifyRegenesis :: BlockHash -> IO ()
    }

-- |Baker identity and baking thread 'MVar'.
data Baker = Baker
    { -- |The baker ID and keys to use for baking.
      bakerIdentity :: !BakerIdentity,
      -- |An 'MVar' holding the 'ThreadId' of the baker thread.
      -- If present, the 'ThreadId' should be that of the singular baker thread.
      -- Stopping the baker thread is accomplished by taking the 'MVar' and
      -- calling 'killThread'.
      bakerThread :: !(MVar ThreadId)
    }

-- |Configuration for the consensus at a particular genesis index.
data VersionedConfiguration gsconf finconf (pv :: ProtocolVersion) = VersionedConfiguration
    { -- |The 'SkovContext' (immutable)
      vcContext :: !(SkovContext (SkovConfig pv gsconf finconf UpdateHandler)),
      -- |The 'SkovState' (mutable) via an 'IORef'. This should only be updated
      -- by a thread that holds the global lock.
      vcState :: !(IORef (SkovState (SkovConfig pv gsconf finconf UpdateHandler))),
      -- |The genesis index
      vcIndex :: GenesisIndex,
      -- |The absolute block height of the genesis block
      vcGenesisHeight :: AbsoluteBlockHeight,
      -- |Shutdown the skov. This should only be called by a thread that holds the global lock
      -- and the configuration should not be used subsequently.
      vcShutdown :: LogIO ()
    }

-- |'SkovConfig' instantiated for the multi-version runner.
type VersionedConfig gsconf finconf pv = SkovConfig pv gsconf finconf UpdateHandler

-- |'SkovHandlers' instantiated for the multi-version runner.
type VersionedHandlers gsconf finconf (pv :: ProtocolVersion) =
    SkovHandlers pv ThreadTimer (VersionedConfig gsconf finconf pv) (MVR gsconf finconf)

-- |The 'SkovT' monad instantiated for the multi-version runner.
type VersionedSkovM gsconf finconf pv =
    SkovT
        pv
        (VersionedHandlers gsconf finconf pv)
        (VersionedConfig gsconf finconf pv)
        (MVR gsconf finconf)

-- |An existential wrapper around a 'VersionedConfiguration' that abstracts the protocol version.
-- We require 'SkovMonad' and 'FinalizationMonad' instances for 'VersionedSkovM' instantiated at
-- the abstracted protocol version.
data EVersionedConfiguration gsconf finconf
    = forall (pv :: ProtocolVersion).
        ( SkovMonad pv (VersionedSkovM gsconf finconf pv),
          FinalizationMonad (VersionedSkovM gsconf finconf pv),
          BakerMonad pv (VersionedSkovM gsconf finconf pv)
        ) =>
      EVersionedConfiguration (VersionedConfiguration gsconf finconf pv)

-- |This class makes it possible to use a multi-version configuration at a specific version.
-- Essentially, this class provides instances of 'SkovMonad', 'FinalizationMonad' and
-- 'TreeStateMonad' for 'VersionedSkovM' instantiated with the configuration parameters and at any
-- 'ProtocolVersion'.
--
-- There is only one instance for this class, but it is fully general.  The reason that this needs
-- to be a class (rather than a constraint alias) is that the constraints involved are quantified.
-- This makes it problematic for the type-checker to correctly simplify them.
class MultiVersion gsconf finconf where
    -- |Convert a 'VersionedConfiguration' to an 'EVersionedConfiguration'.
    newVersion ::
        IsProtocolVersion pv =>
        VersionedConfiguration gsconf finconf pv ->
        EVersionedConfiguration gsconf finconf

    -- |Supply a 'VersionedSkovM' action with instances of 'SkovMonad', 'FinalizationMonad' and
    -- 'TreeStateMonad'.
    liftSkov ::
        IsProtocolVersion pv =>
        ( ( SkovMonad pv (VersionedSkovM gsconf finconf pv),
            FinalizationMonad (VersionedSkovM gsconf finconf pv),
            TreeStateMonad pv (VersionedSkovM gsconf finconf pv)
          ) =>
          VersionedSkovM gsconf finconf pv a
        ) ->
        VersionedSkovM gsconf finconf pv a

instance
    ( forall pv. IsProtocolVersion pv => SkovMonad pv (VersionedSkovM gsconf finconf pv),
      forall pv. IsProtocolVersion pv => FinalizationMonad (VersionedSkovM gsconf finconf pv),
      forall pv. IsProtocolVersion pv => BakerMonad pv (VersionedSkovM gsconf finconf pv),
      forall pv. IsProtocolVersion pv => TreeStateMonad pv (VersionedSkovM gsconf finconf pv)
    ) =>
    MultiVersion gsconf finconf
    where
    newVersion = EVersionedConfiguration
    liftSkov a = a

-- |State of catch-up buffering.  This is used for buffering the sending of catch-up status messages
-- that need to be sent as a result of pending blocks becoming live.  See
-- 'bufferedHandlePendingLive' for details.
data CatchUpStatusBufferState
    = -- |We are not currently waiting to send a catch-up status message.
      BufferEmpty
    | -- |We are currently waiting to send a catch-up status message.
      BufferPending
        { -- |The genesis index for which the status message is buffered.
          cusbsGenesisIndex :: !GenesisIndex,
          -- |The soonest the status message should be sent.
          cusbsSoonest :: !UTCTime,
          -- |The latest the status message should be sent.
          cusbsLatest :: !UTCTime
        }
    | -- |We should not send any more catch-up status messages.
      BufferShutdown

-- |The context for managing multi-version consensus.
data MultiVersionRunner gsconf finconf = MultiVersionRunner
    { -- |Base configuration.
      mvConfiguration :: !(MultiVersionConfiguration gsconf finconf),
      -- |Callback functions for sending messsages to the network.
      mvCallbacks :: !Callbacks,
      -- |Baker identity.
      mvBaker :: !(Maybe Baker),
      -- |Thread that periodically purges uncommitted transactions.
      mvTransactionPurgingThread :: !(MVar ThreadId),
      -- |Vector of states, indexed by the genesis index.
      -- This is only ever updated by extending the vector, and only while
      -- the write lock is held.
      mvVersions :: !(IORef (Vec.Vector (EVersionedConfiguration gsconf finconf))),
      -- |Global write lock.
      mvWriteLock :: !(MVar ()),
      -- |State for buffering catch-up status message events.
      mvCatchUpStatusBuffer :: !(MVar CatchUpStatusBufferState),
      -- |Log method.
      mvLog :: LogMethod IO
    }

-- |Multi-version runner monad. Ultimately, @MVR gsconf finconf@ is a newtype wrapper around
-- @ReaderT (MultiVersionRunner gsconf finconf) IO@. That is, it is an @IO@ monad with a
-- @MultiVersionRunner@ context.
newtype MVR gsconf finconf a = MVR {runMVR :: MultiVersionRunner gsconf finconf -> IO a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadIO,
          MonadReader (MultiVersionRunner gsconf finconf),
          TimeMonad,
          MonadThrow,
          MonadCatch,
          MonadMask
        )
        via (ReaderT (MultiVersionRunner gsconf finconf) IO)

instance MonadLogger (MVR gsconf finconf) where
    logEvent src lvl msg = MVR $ \mvr -> mvLog mvr src lvl msg
    {-# INLINE logEvent #-}

-- |Perform an action while holding the global state write lock.
-- If the action throws an exception, this ensures that the lock is
-- released.
withWriteLock :: MVR gsconf finconf a -> MVR gsconf finconf a
{-# INLINE withWriteLock #-}
withWriteLock a = MVR $ \mvr -> withWriteLockIO mvr (runMVR a mvr)

-- |Perform an action while holding the global state write lock.
-- If the action throws an exception, this ensures that the lock is
-- released.
withWriteLockIO :: MultiVersionRunner gsconf finconf -> IO a -> IO a
{-# INLINE withWriteLockIO #-}
withWriteLockIO MultiVersionRunner{..} a =
    bracketOnError (takeMVar mvWriteLock) (tryPutMVar mvWriteLock) $ \_ -> do
        tid <- myThreadId
        mvLog Runner LLTrace $ "Acquired global state lock on thread " ++ show tid
        res <- a
        putMVar mvWriteLock ()
        mvLog Runner LLTrace $ "Released global state lock on thread " ++ show tid
        return res

-- |Lift a 'LogIO' action into the 'MVR' monad.
mvrLogIO :: LogIO a -> MVR gsconf finconf a
{-# INLINE mvrLogIO #-}
mvrLogIO a = MVR $ \mvr -> runLoggerT a (mvLog mvr)

-- |Start a consensus with a new genesis.
-- It is assumed that the thread holds the write lock.
-- This calls 'notifyRegenesis' to alert the P2P layer of the new genesis block and that catch-up
-- should be invoked.
newGenesis ::
    forall gsconf finconf.
    ( MultiVersionStateConfig gsconf,
      MultiVersion gsconf finconf,
      SkovConfiguration gsconf finconf UpdateHandler
    ) =>
    -- |Genesis data
    PVGenesisData ->
    -- |Absolute height of the new genesis block
    AbsoluteBlockHeight ->
    MVR gsconf finconf ()
newGenesis (PVGenesisData (gd :: GenesisData pv)) vcGenesisHeight =
    MVR $
        \mvr@MultiVersionRunner
            { mvCallbacks = Callbacks{..},
              mvConfiguration = MultiVersionConfiguration{..},
              ..
            } -> do
                mvLog Runner LLInfo $
                    "Starting new chain with genesis block "
                        ++ show (genesisBlockHash gd)
                        ++ " at absolute height "
                        ++ show vcGenesisHeight
                oldVersions <- readIORef mvVersions
                let vcIndex = fromIntegral (length oldVersions)
                (vcContext, st) <-
                    runLoggerT
                        ( initialiseSkov
                            ( SkovConfig @pv @gsconf @finconf
                                ( globalStateConfig
                                    mvcStateConfig
                                    mvcTXLogConfig
                                    mvcRuntimeParameters
                                    vcIndex
                                    vcGenesisHeight
                                    gd
                                )
                                mvcFinalizationConfig
                                UpdateHandler
                            )
                        )
                        mvLog
                vcState <- newIORef st
                let vcShutdown = shutdownSkov vcContext =<< liftIO (readIORef vcState)
                let newEConfig :: VersionedConfiguration gsconf finconf pv
                    newEConfig = VersionedConfiguration{..}
                writeIORef mvVersions (oldVersions `Vec.snoc` newVersion newEConfig)
                notifyRegenesis (genesisBlockHash gd)
                -- Because this may be restoring an existing state, it is possible that a protocol
                -- update has already happened on this chain.  Therefore, we must handle this
                -- contingency.
                runMVR (liftSkovUpdate newEConfig checkForProtocolUpdate) mvr

-- |Determine if a protocol update has occurred, and handle it.
-- When a protocol update first becomes pending, this logs the update that will occur (if it is
-- of a known type) or logs an error message (if it is unknown).
-- When the protocol update takes effect, this will create the new genesis block, starting up a
-- new instances of consensus and shutting down the old one (which is still available for queries).
-- However, if the new protocol is unknown, no update will take place, but the old consensus will
-- effectively stop accepting blocks.
-- It is assumed that the thread holds the write lock.
checkForProtocolUpdate ::
    forall gc fc pv.
    ( MultiVersionStateConfig gc,
      MultiVersion gc fc,
      SkovConfiguration gc fc UpdateHandler,
      IsProtocolVersion pv
    ) =>
    VersionedSkovM gc fc pv ()
checkForProtocolUpdate = liftSkov body
  where
    body ::
        ( SkovMonad pv (VersionedSkovM gc fc pv),
          TreeStateMonad pv (VersionedSkovM gc fc pv)
        ) =>
        VersionedSkovM gc fc pv ()
    body =
        Skov.getProtocolUpdateStatus >>= \case
            ProtocolUpdated pu -> case checkUpdate @pv pu of
                Left err ->
                    logEvent Kontrol LLError $
                        "An unsupported protocol update (" ++ err ++ ") has taken effect:"
                            ++ showPU pu
                Right upd -> do
                    regenesis <- updateRegenesis upd
                    lfbHeight <- bpHeight <$> lastFinalizedBlock
                    latestEraGenesisHeight <- lift $
                        MVR $ \mvr -> do
                            versions <- readIORef $ mvVersions mvr
                            return $ case Vec.last versions of
                                EVersionedConfiguration vc -> vcGenesisHeight vc
                    let newGenesisHeight = 1 + localToAbsoluteBlockHeight latestEraGenesisHeight lfbHeight
                    lift $ newGenesis regenesis $! newGenesisHeight
                    -- Close down the state and get the non-finalized transactions.
                    oldTransactions <- terminateSkov
                    -- Transfer the non-finalized transactions to the new version.
                    lift $ do
                        vvec <- liftIO . readIORef =<< asks mvVersions
                        case Vec.last vvec of
                            (EVersionedConfiguration vc) ->
                                liftSkovUpdate vc $ mapM_ Skov.receiveTransaction oldTransactions
                    return ()
            PendingProtocolUpdates [] -> return ()
            PendingProtocolUpdates ((ts, pu) : _) -> do
                -- There is a queued protocol update, but only log about it
                -- if we have not done so already.
                alreadyNotified <-
                    state
                        ( \s ->
                            if ssHandlerState s == AlreadyNotified ts pu
                                then (True, s)
                                else (False, s{ssHandlerState = AlreadyNotified ts pu})
                        )
                unless alreadyNotified $ case checkUpdate @pv pu of
                    Left err ->
                        logEvent Kontrol LLError $
                            "An unsupported protocol update (" ++ err ++ ") will take effect at "
                                ++ show (timestampToUTCTime $ transactionTimeToTimestamp ts)
                                ++ ": "
                                ++ showPU pu
                    Right upd ->
                        logEvent Kontrol LLInfo $
                            "A protocol update will take effect at "
                                ++ show (timestampToUTCTime $ transactionTimeToTimestamp ts)
                                ++ ": "
                                ++ showPU pu
                                ++ "\n"
                                ++ show upd
    showPU ProtocolUpdate{..} =
        Text.unpack puMessage ++ "\n["
            ++ Text.unpack puSpecificationURL
            ++ " (hash "
            ++ show puSpecificationHash
            ++ ")]"

-- |Make a 'MultiVersionRunner' for a given configuration.
makeMultiVersionRunner ::
    ( MultiVersionStateConfig gsconf,
      MultiVersion gsconf finconf,
      SkovConfiguration gsconf finconf UpdateHandler
    ) =>
    MultiVersionConfiguration gsconf finconf ->
    Callbacks ->
    Maybe BakerIdentity ->
    LogMethod IO ->
    PVGenesisData ->
    IO (MultiVersionRunner gsconf finconf)
makeMultiVersionRunner
    mvConfiguration
    mvCallbacks
    mbakerIdentity
    mvLog
    genesis = do
        mvBaker <- forM mbakerIdentity $ \bakerIdentity -> do
            bakerThread <- newEmptyMVar
            return Baker{..}
        mvVersions <- newIORef Vec.empty
        mvWriteLock <- newEmptyMVar
        mvCatchUpStatusBuffer <- newMVar BufferEmpty
        mvTransactionPurgingThread <- newEmptyMVar
        let mvr = MultiVersionRunner{..}
        runMVR (newGenesis genesis 0) mvr
        putMVar mvWriteLock ()
        startTransactionPurgingThread mvr
        return mvr

-- |Start a thread to periodically purge uncommitted transactions.
-- This is only intended to be called once, during 'makeMultiVersionRunner'.
-- Calling it a second time is expected to deadlock.
--
-- If the specified delay is less than or equal to zero, no purging thread
-- will be started.
startTransactionPurgingThread :: MultiVersionRunner gsconf finconf -> IO ()
startTransactionPurgingThread mvr@MultiVersionRunner{..} =
    when (delay > 0) $
        putMVar mvTransactionPurgingThread <=< forkIO $
            ( do
                mvLog Runner LLInfo "Transaction purging thread started."
                forever $ do
                    threadDelay delay
                    mvLog Runner LLTrace "Purging transactions."
                    withWriteLockIO mvr $ do
                        EVersionedConfiguration vc <- Vec.last <$> readIORef mvVersions
                        runMVR (liftSkovUpdate vc purgeTransactions) mvr
            )
                `finally` mvLog Runner LLInfo "Transaction purging thread stopped."
  where
    delay = rpTransactionsPurgingDelay (mvcRuntimeParameters mvConfiguration) * 1_000_000

-- |Start a baker thread associated with a 'MultiVersionRunner'.
-- This will only succeed if the runner was initialised with baker credentials.
startBaker :: MultiVersionRunner gsconf finconf -> IO ()
startBaker MultiVersionRunner{mvBaker = Nothing, ..} =
    mvLog
        Runner
        LLError
        "Attempted to start baker thread, but consensus was started without baker credentials."
startBaker mvr@MultiVersionRunner{mvBaker = Just Baker{..}, ..} = do
    _ <- forkOS $ do
        tid <- myThreadId
        started <- tryPutMVar bakerThread tid
        if started
            then do
                mvLog Runner LLInfo "Starting baker thread"
                bakerLoop 0 0 `finally` mvLog Runner LLInfo "Exiting baker thread"
            else mvLog Runner LLInfo "Starting baker thread aborted: baker is already running"
    -- This synchronises on the baker MVar to ensure that a baker should definitely be
    -- running before startBaker returns.  This is to ensure that if stopBaker is subsequently
    -- called then the baker will be stopped.
    modifyMVarMasked_ bakerThread return
  where
    -- The baker loop takes the current genesis index and last known slot that we baked for, and
    -- will continually attempt to bake until the consensus is shut down.
    bakerLoop :: GenesisIndex -> Slot -> IO ()
    bakerLoop lastGenIndex slot = do
        (genIndex, res) <-
            withWriteLockIO mvr $ do
                EVersionedConfiguration vc <- Vec.last <$> readIORef mvVersions
                -- If the genesis index has changed, we reset the slot counter to 0, since this
                -- is a different chain.
                let nextSlot = if vcIndex vc == lastGenIndex then slot else 0
                (vcIndex vc,) <$> runMVR (liftSkovUpdate vc (tryBake bakerIdentity nextSlot)) mvr
        case res of
            BakeSuccess slot' block -> do
                broadcastBlock mvCallbacks genIndex block
                bakerLoop genIndex slot'
            BakeWaitUntil slot' ts -> do
                now <- utcTimeToTimestamp <$> currentTime
                when (now < ts) $ threadDelay $ fromIntegral (tsMillis (ts - now)) * 1_000
                bakerLoop genIndex slot'
            BakeShutdown -> do
                -- Note that on a successful protocol update this should not occur because a new
                -- genesis should be started up when the old one is shut down within the same
                -- critical region. i.e. while the write lock is held.
                -- If the protocol update was unsuccessful (i.e. we do not know how to continue)
                -- then exiting the baker thread is the appropriate behaviour
                mvLog Runner LLInfo "Consensus is shut down; baking will terminate."
                -- Since we are exiting the baker thread without being killed, we drain the MVar.
                -- This may not be necessary, but should ensure that the thread can be garbage
                -- collected.
                void $ takeMVar bakerThread

-- |Stop the baker thread associated with a 'MultiVersionRunner'.
stopBaker :: MultiVersionRunner gsconf finconf -> IO ()
stopBaker MultiVersionRunner{mvBaker = Nothing, ..} =
    mvLog
        Runner
        LLError
        "Attempted to stop baker thread, but consensus was started without baker credentials."
stopBaker MultiVersionRunner{mvBaker = Just Baker{..}, ..} = do
    mask_ $
        tryTakeMVar bakerThread >>= \case
            Nothing -> mvLog Runner LLWarning "Attempted to stop baker thread, but it was not running."
            Just thrd -> killThread thrd

shutdownMultiVersionRunner :: MultiVersionRunner gsconf finconf -> IO ()
shutdownMultiVersionRunner MultiVersionRunner{..} = mask_ $ do
    -- Kill the baker thread, if any.
    forM_ mvBaker $ \Baker{..} -> tryTakeMVar bakerThread >>= mapM_ killThread
    -- Kill the transaction purging thread, if any.
    tryTakeMVar mvTransactionPurgingThread >>= mapM_ killThread
    -- Acquire the write lock. This prevents further updates, as they will block.
    takeMVar mvWriteLock
    versions <- readIORef mvVersions
    runLoggerT (forM_ versions $ \(EVersionedConfiguration vc) -> vcShutdown vc) mvLog

-- |Lift a skov action to the 'MVR' monad, running it on a
-- particular 'VersionedConfiguration'. Note that this does not
-- acquire the write lock: the caller must ensure that the lock
-- is held.
liftSkovUpdate ::
    VersionedConfiguration gsconf finconf pv ->
    VersionedSkovM gsconf finconf pv a ->
    MVR gsconf finconf a
liftSkovUpdate vc a = MVR $ \mvr -> do
    oldState <- readIORef (vcState vc)
    (res, newState) <- runMVR (runSkovT a (mvrSkovHandlers vc mvr) (vcContext vc) oldState) mvr
    writeIORef (vcState vc) newState
    return res

-- |Run a transaction that may affect the state.
-- This acquires the write lock for the duration of the operation.
-- If the action throws an exception, the state will not be updated,
-- but the lock is guaranteed to be released.
runSkovTransaction ::
    VersionedConfiguration gsconf finconf pv ->
    VersionedSkovM gsconf finconf pv a ->
    MVR gsconf finconf a
runSkovTransaction vc a = withWriteLock $ liftSkovUpdate vc a

-- |An instance of 'SkovHandlers' for running operations on a
-- 'VersionedConfiguration' within a 'MultiVersionRunner'.
mvrSkovHandlers ::
    VersionedConfiguration gsconf finconf pv ->
    MultiVersionRunner gsconf finconf ->
    SkovHandlers pv ThreadTimer (SkovConfig pv gsconf finconf UpdateHandler) (MVR gsconf finconf)
mvrSkovHandlers vc mvr@MultiVersionRunner{mvCallbacks = Callbacks{..}} =
    SkovHandlers
        { shBroadcastFinalizationMessage =
            liftIO . broadcastFinalizationMessage (vcIndex vc) . runPut . putVersionedFPMV0,
          shOnTimeout =
            \timeout a ->
                liftIO $
                    makeThreadTimer timeout $
                        void $ runMVR (runSkovTransaction vc a) mvr,
          shCancelTimer = liftIO . cancelThreadTimer,
          shPendingLive = bufferedHandlePendingLive vc
        }

-- |Handle a pending block becoming live by sending out a catch-up status message
-- within at most 30 seconds (subject to scheduling). This uses the following principles:
--
--   * If we are not already waiting to send a catch-up status message, start
--     waiting for at least 5 seconds and at most 30 seconds.
--   * If we are already waiting to send a catch-up status message for the same
--     genesis index, update the soonest time to send that to be 5 seconds from
--     now, or the latest possible time, whichever is sooner.
--   * If we are waiting to send a catch-up for an earlier genesis index, send
--     the catch up status for that index immediately and update the wait to be
--     for at least 5 seconds and at most 30 seconds (but now for the new index).
--   * If the buffer has been shut down (i.e. the entire consensus is being torn
--     down) then do nothing.
bufferedHandlePendingLive ::
    VersionedConfiguration gsconf finconf pv ->
    MVR gsconf finconf ()
bufferedHandlePendingLive vc = MVR $ \mvr@MultiVersionRunner{..} -> do
    now <- currentTime
    let soonest = addUTCTime 5 now
    mask $ \restore ->
        takeMVar mvCatchUpStatusBuffer >>= \case
            BufferEmpty -> do
                putMVar mvCatchUpStatusBuffer $
                    BufferPending
                        { cusbsGenesisIndex = vcIndex vc,
                          cusbsSoonest = soonest,
                          cusbsLatest = addUTCTime 30 now
                        }
                void $ forkIO $ waitLoop mvr soonest
            BufferPending{..}
                | cusbsGenesisIndex == vcIndex vc ->
                    putMVar mvCatchUpStatusBuffer $
                        BufferPending
                            { cusbsSoonest = min soonest cusbsLatest,
                              ..
                            }
                | cusbsGenesisIndex < vcIndex vc -> do
                    putMVar mvCatchUpStatusBuffer $
                        BufferPending
                            { cusbsGenesisIndex = vcIndex vc,
                              cusbsSoonest = soonest,
                              cusbsLatest = addUTCTime 30 now
                            }
                    restore $ runMVR (sendCatchUpStatus cusbsGenesisIndex) mvr
                | otherwise -> restore $ runMVR (sendCatchUpStatus (vcIndex vc)) mvr
            BufferShutdown -> putMVar mvCatchUpStatusBuffer BufferShutdown
  where
    waitLoop mvr@MultiVersionRunner{..} till = do
        now <- currentTime
        let waitDurationMicros = truncate (diffUTCTime till now * 1e6)
        when (waitDurationMicros > 0) $ threadDelay waitDurationMicros
        mask $ \restore ->
            takeMVar mvCatchUpStatusBuffer >>= \case
                BufferEmpty -> putMVar mvCatchUpStatusBuffer BufferEmpty
                v@BufferPending{..} -> do
                    now' <- currentTime
                    if now' >= cusbsSoonest
                        then do
                            putMVar mvCatchUpStatusBuffer BufferEmpty
                            restore $ runMVR (sendCatchUpStatus cusbsGenesisIndex) mvr
                        else do
                            putMVar mvCatchUpStatusBuffer v
                            restore $ waitLoop mvr cusbsSoonest
                BufferShutdown -> return ()

-- |Send a catch-up status message for a particular genesis index.
sendCatchUpStatus :: GenesisIndex -> MVR gsconf finconf ()
sendCatchUpStatus genIndex = MVR $ \mvr@MultiVersionRunner{..} -> do
    vvec <- readIORef mvVersions
    case vvec Vec.! fromIntegral genIndex of
        EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv) -> do
            st <- readIORef (vcState vc)
            cus <-
                runMVR
                    ( evalSkovT @_ @pv
                        (getCatchUpStatus False)
                        (mvrSkovHandlers vc mvr)
                        (vcContext vc)
                        st
                    )
                    mvr
            notifyCatchUpStatus mvCallbacks (vcIndex vc) $ runPut $ putVersionedCatchUpStatus cus

-- |Perform an operation with the latest chain version, as long as
-- it is at the expected genesis index.  If the genesis index is
-- for an older version, this returns 'ResultConsensusShutDown'
-- instead.  If the genesis index is for an (as yet) unknown version,
-- this returns 'ResultInvalidGenesisIndex'.
--
-- Note that if it is absolutely necessary that the genesis index
-- should be for the latest version, then the write lock needs to
-- be acquired when the check is made.  However, typically the
-- operation should respond with 'ResultConsensusShutDown' in any
-- case if the genesis index is not the latest version.
--
-- TODO: Currently, when the genesis index is in the future (and
-- thus unknown) we simply respond with 'ResultInvalidGenesisIndex'.
-- During a protocol update, it might be wise to cache messages for
-- the forthcoming protocol version. For the time being, we will
-- simply rely on catch-up.
withLatestExpectedVersion ::
    GenesisIndex ->
    (EVersionedConfiguration gsconf finconf -> MVR gsconf finconf UpdateResult) ->
    MVR gsconf finconf UpdateResult
withLatestExpectedVersion gi a = do
    vvec <- liftIO . readIORef =<< asks mvVersions
    -- Length is an Int and GenesisIndex is a Word32.
    -- Assuming a 64-bit system, there is no risk of over/underflow.
    case compare (Vec.length vvec - 1) (fromIntegral gi) of
        EQ -> a (Vec.last vvec)
        LT -> return ResultInvalidGenesisIndex
        GT -> return ResultConsensusShutDown

-- |Deserialize and receive a block at a given genesis index.
receiveBlock :: GenesisIndex -> ByteString -> MVR gsconf finconf UpdateResult
receiveBlock gi blockBS = withLatestExpectedVersion gi $
    \(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) -> do
        now <- currentTime
        case deserializeExactVersionedPendingBlock (protocolVersion @pv) blockBS now of
            Left err -> do
                logEvent Runner LLDebug err
                return ResultSerializationFail
            Right block -> runSkovTransaction vc (storeBlock block)

-- |Deserialize and receive a finalization message at a given genesis index.
receiveFinalizationMessage :: GenesisIndex -> ByteString -> MVR gsconf finconf UpdateResult
receiveFinalizationMessage gi finMsgBS = withLatestExpectedVersion gi $
    \(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) ->
        case runGet getExactVersionedFPM finMsgBS of
            Left err -> do
                logEvent Runner LLDebug $ "Could not deserialize finalization message: " ++ err
                return ResultSerializationFail
            Right finMsg -> runSkovTransaction vc (finalizationReceiveMessage finMsg)

-- |Deserialize and receive a finalization record at a given genesis index.
receiveFinalizationRecord :: GenesisIndex -> ByteString -> MVR gsconf finconf UpdateResult
receiveFinalizationRecord gi finRecBS = withLatestExpectedVersion gi $
    \(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) ->
        case runGet getExactVersionedFinalizationRecord finRecBS of
            Left err -> do
                logEvent Runner LLDebug $ "Could not deserialized finalization record: " ++ err
                return ResultSerializationFail
            Right finRec -> runSkovTransaction vc (finalizationReceiveRecord False finRec)

-- |Configuration parameters for handling receipt of a catch-up status message.
data CatchUpConfiguration = CatchUpConfiguration
    { -- |Maximum number of block and finalization record messages to send in response.
      catchUpMessageLimit :: Int,
      -- |Callback for sending blocks, finalization records and the response catch up status
      -- message.
      catchUpCallback :: MessageType -> ByteString -> IO ()
    }

-- |Handle receipt of a catch-up message.
receiveCatchUpStatus ::
    forall gsconf finconf.
    GenesisIndex ->
    ByteString ->
    CatchUpConfiguration ->
    MVR gsconf finconf UpdateResult
receiveCatchUpStatus gi catchUpBS CatchUpConfiguration{..} =
    case runGet getExactVersionedCatchUpStatus catchUpBS of
        Left err -> do
            logEvent Runner LLDebug $ "Could not deserialize catch-up status message: " ++ err
            return ResultSerializationFail
        Right catchUp -> do
            logEvent Runner LLDebug $ "Catch-up status message deserialized: " ++ show catchUp
            vvec <- liftIO . readIORef =<< asks mvVersions
            case vvec Vec.!? fromIntegral gi of
                -- If we have a (re)genesis as the given index then...
                Just (EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) ->
                    MVR $ \mvr -> do
                        st <- readIORef (vcState vc)
                        -- Evaluate handleCatchUpStatus to determine the response.
                        -- Note that this should not perform a state update, so there is no need to
                        -- acquire the write lock, or to store the resulting state.
                        (mmsgs, res) <-
                            runMVR
                                ( evalSkovT @_ @pv
                                    ( handleCatchUpStatus @pv @(VersionedSkovM gsconf finconf pv)
                                        catchUp
                                        catchUpMessageLimit
                                    )
                                    (mvrSkovHandlers vc mvr)
                                    (vcContext vc)
                                    st
                                )
                                mvr
                        -- Send out the messages, where necessary.
                        forM_ mmsgs $ \(blocksFins, cusResp) -> do
                            mvLog mvr Runner LLTrace $
                                "Sending " ++ show (length blocksFins) ++ " blocks/finalization records"
                            forM_ blocksFins $ uncurry catchUpCallback
                            mvLog mvr Runner LLDebug $
                                "Catch-up response status message: " ++ show cusResp
                            catchUpCallback MessageCatchUpStatus $
                                runPut $ putVersionedCatchUpStatus cusResp
                        return res
                -- If we have no regenesis at the given index then...
                Nothing -> case catchUp of
                    -- if it is a request, inform the peer we have no genesis and queue to catch up
                    CatchUpStatus{cusIsRequest = True} -> do
                        liftIO $
                            catchUpCallback MessageCatchUpStatus $
                                runPut $ putVersionedCatchUpStatus NoGenesisCatchUpStatus
                        return ResultPendingBlock
                    -- if it not a request, no response is necessary, but we should mark the
                    -- peer as pending
                    CatchUpStatus{} -> return ResultPendingBlock
                    -- if the peer (also!) has no genesis at this index, we do not reply
                    -- or initiate catch-up
                    NoGenesisCatchUpStatus -> return ResultSuccess

-- |Get the catch-up status for the current version of the chain.  This returns the current
-- genesis index, as well as the catch-up request message serialized with its version.
getCatchUpRequest :: forall gsconf finconf. MVR gsconf finconf (GenesisIndex, LBS.ByteString)
getCatchUpRequest = do
    mvr <- ask
    vvec <- liftIO $ readIORef $ mvVersions mvr
    case Vec.last vvec of
        (EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) -> do
            st <- liftIO $ readIORef $ vcState vc
            cus <- evalSkovT (getCatchUpStatus @pv True) (mvrSkovHandlers vc mvr) (vcContext vc) st
            return (vcIndex vc, runPutLazy $ putVersionedCatchUpStatus cus)

-- |Deserialize and receive a transaction.  The transaction is passed to
-- the current version of the chain.
--
-- In future, multiple versions may use/require different deserializations
-- of transactions.  This may be handled in one of two ways:
--
-- 1. Deserialize to a common format, which is then converted to the format
--    of the current genesis index.
-- 2. Determine the current genesis index before deserializing to ensure
--    that the correct format is used.
receiveTransaction :: ByteString -> MVR gsconf finconf UpdateResult
receiveTransaction transactionBS = do
    now <- utcTimeToTransactionTime <$> currentTime
    case runGet (getExactVersionedBlockItem now) transactionBS of
        Left err -> do
            logEvent Runner LLDebug err
            return ResultSerializationFail
        Right transaction -> withWriteLock $ do
            vvec <- liftIO . readIORef =<< asks mvVersions
            case Vec.last vvec of
                (EVersionedConfiguration vc) ->
                    liftSkovUpdate vc $ Skov.receiveTransaction transaction

-- |Import a block file for out-of-band catch-up.
importBlocks :: FilePath -> MVR gsconf finconf UpdateResult
importBlocks importFile = do
    vvec <- liftIO . readIORef =<< asks mvVersions
    case Vec.last vvec of
        EVersionedConfiguration vc -> do
            -- Import starting from the genesis index of the latest consensus
            res <- importBlocksV3 importFile (vcIndex vc) doImport
            case res of
                Left ImportSerializationFail -> return ResultSerializationFail
                Left (ImportOtherError a) -> return a
                Right _ -> return ResultSuccess
  where
    doImport (ImportBlock _ gi bs) = fixResult <$> receiveBlock gi bs
    doImport (ImportFinalizationRecord _ gi bs) = fixResult <$> receiveFinalizationRecord gi bs
    fixResult ResultSuccess = Right ()
    fixResult ResultDuplicate = Right ()
    fixResult ResultConsensusShutDown = Right ()
    fixResult e = Left (ImportOtherError e)
