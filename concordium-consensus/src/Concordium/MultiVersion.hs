{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.MultiVersion where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.IORef
import Data.Kind
import Data.Serialize
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec
import System.FilePath

import Concordium.Logger
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.Updates

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Birk.Bake
import Concordium.GlobalState
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads (BlockPointerMonad)
import Concordium.GlobalState.BlockState (BlockStateStorage)
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState (GlobalStateTypes, TreeStateMonad)
import Concordium.ProtocolUpdate
import Concordium.Skov as Skov
import Concordium.TimeMonad
import Concordium.TimerMonad

-- |Handler configuration for supporting protocol updates.
data UpdateHandler = UpdateHandler

-- |State for the 'UpdateHandler' handler configuration.
data UpdateHandlerState
    = -- |Indicates that no pending update has been logged.
      NeverNotified
    | -- |Indicates that the specified protocol update has already been logged.
      AlreadyNotified
        { uhNotifiedEffectiveTimestamp :: !TransactionTime,
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
-- transactions in an external database
newtype TransactionDBConfig = TransactionDBConfig
    { -- |Database connection string.
      dbConnString :: ByteString
    }

-- |Configuration information for a multi-version runner.
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

class (GlobalStateConfig gsconf) => MultiVersionStateConfig (gsconf :: ProtocolVersion -> Type) where
    type StateConfig gsconf
    type TXLogConfig gsconf
    globalStateConfig :: IsProtocolVersion pv => StateConfig gsconf -> TXLogConfig gsconf -> RuntimeParameters -> GenesisIndex -> GenesisData pv -> gsconf pv

instance MultiVersionStateConfig DiskTreeDiskBlockConfig where
    type StateConfig DiskTreeDiskBlockConfig = DiskStateConfig
    type TXLogConfig DiskTreeDiskBlockConfig = ()
    globalStateConfig DiskStateConfig{..} _ rtp gi gd =
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
    globalStateConfig DiskStateConfig{..} TransactionDBConfig{..} rtp gi gd =
        ( DTDBWLConfig
            { dtdbwlRuntimeParameters = rtp,
              dtdbwlTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
              dtdbwlBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat",
              dtdbwlGenesisData = gd,
              dtdbwlTxDBConnectionString = dbConnString
            }
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
      -- |Send a catch-up status message to app (non-pending) peers.
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
      vcIndex :: GenesisIndex
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
          FinalizationMonad (VersionedSkovM gsconf finconf pv)
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
      forall pv. IsProtocolVersion pv => TreeStateMonad pv (VersionedSkovM gsconf finconf pv)
    ) =>
    MultiVersion gsconf finconf
    where
    newVersion = EVersionedConfiguration
    liftSkov a = a

-- |State of catch-up buffering.
data CatchUpStatusBufferState
    = -- |We are not currently waiting to send a catch-up status message.
      BufferEmpty
    | -- |We are currently waiting to send a catch-up status message.
      BufferPending
        { cusbsGenesisIndex :: !GenesisIndex,
          cusbsSoonest :: !UTCTime,
          cusbsLatest :: !UTCTime
        }
    | -- |We should not send any more catch-up status messages.
      BufferShutdown

data MultiVersionRunner gsconf finconf = MultiVersionRunner
    { mvConfiguration :: !(MultiVersionConfiguration gsconf finconf),
      mvCallbacks :: !Callbacks,
      mvBaker :: !(Maybe Baker),
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

newtype MVR gsconf finconf a = MVR {runMVR :: MultiVersionRunner gsconf finconf -> IO a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadIO,
          MonadReader (MultiVersionRunner gsconf finconf),
          TimeMonad
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
withWriteLock a = MVR $ \mvr@MultiVersionRunner{..} ->
    bracketOnError (takeMVar mvWriteLock) (tryPutMVar mvWriteLock) $ \_ -> do
        tid <- myThreadId
        mvLog Runner LLTrace $ "Acquired global state lock on thread " ++ show tid
        res <- runMVR a mvr
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
    PVGenesisData ->
    MVR gsconf finconf ()
newGenesis (PVGenesisData (gd :: GenesisData pv)) =
    MVR $
        \mvr@MultiVersionRunner
            { mvCallbacks = Callbacks{..},
              mvConfiguration = MultiVersionConfiguration{..},
              ..
            } -> do
                oldVersions <- readIORef mvVersions
                let vcIndex = fromIntegral (length oldVersions)
                (vcContext, st) <-
                    runLoggerT
                        ( initialiseSkov
                            ( SkovConfig @pv @gsconf @finconf
                                (globalStateConfig mvcStateConfig mvcTXLogConfig mvcRuntimeParameters vcIndex gd)
                                mvcFinalizationConfig
                                UpdateHandler
                            )
                        )
                        mvLog
                vcState <- newIORef st
                let newEConfig :: VersionedConfiguration gsconf finconf pv
                    newEConfig = VersionedConfiguration{..}
                writeIORef mvVersions (oldVersions `Vec.snoc` newVersion newEConfig)
                notifyRegenesis (genesisBlockHash gd)
                -- Detect if the consensus is already shut down.
                -- (This can happen when restoring a state.)
                runMVR (liftSkovUpdate newEConfig checkForProtocolUpdate) mvr
                return ()

-- |Determine if a protocol update has occurred, and handle it.
-- When a protocol update first becomes pending, this logs the update that will occur (if it is
-- of a known type) or logs an error message (if it is unknown).
-- When the protocol update takes effect, this will create the new genesis block, starting up a
-- new instances of consensus and shutting down the old one (which is still available for queries).
-- However, if the new protocol is unknown, no update will take place, but the old consensus will
-- effectively stop accepting blocks.
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
            Left pu -> case checkUpdate @pv pu of
                Left err ->
                    logEvent Kontrol LLError $
                        "An unsupported protocol update (" ++ err ++ ") has taken effect:"
                            ++ showPU pu
                Right upd -> do
                    regenesis <- updateRegenesis upd
                    lift $ newGenesis regenesis
                    -- Close down the state and get the non-finalized transactions.
                    oldTransactions <- terminateSkov
                    -- Transfer the non-finalized transactions to the new version.
                    lift $ do
                        vvec <- liftIO . readIORef =<< asks mvVersions
                        case Vec.last vvec of
                            (EVersionedConfiguration vc) ->
                                liftSkovUpdate vc $ mapM_ Skov.receiveTransaction oldTransactions
                    return ()
            Right [] -> return ()
            Right ((ts, pu) : _) -> do
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
        let mvr = MultiVersionRunner{..}
        runMVR (newGenesis genesis) mvr
        putMVar mvWriteLock ()
        return mvr

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
            cus <- runMVR (evalSkovT @_ @pv (getCatchUpStatus False) (mvrSkovHandlers vc mvr) (vcContext vc) st) mvr
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
                logEvent External LLDebug err
                return ResultSerializationFail
            Right block -> runSkovTransaction vc (storeBlock block)

-- |Deserialize and receive a finalization message at a given genesis index.
receiveFinalizationMessage :: GenesisIndex -> ByteString -> MVR gsconf finconf UpdateResult
receiveFinalizationMessage gi finMsgBS = withLatestExpectedVersion gi $
    \(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) ->
        case runGet getExactVersionedFPM finMsgBS of
            Left err -> do
                logEvent External LLDebug err
                return ResultSerializationFail
            Right finMsg -> runSkovTransaction vc (finalizationReceiveMessage finMsg)

-- |Deserialize and receive a finalization record at a given genesis index.
receiveFinalizationRecord :: GenesisIndex -> ByteString -> MVR gsconf finconf UpdateResult
receiveFinalizationRecord gi finRecBS = withLatestExpectedVersion gi $
    \(EVersionedConfiguration (vc :: VersionedConfiguration gsconf finconf pv)) ->
        case runGet getExactVersionedFinalizationRecord finRecBS of
            Left err -> do
                logEvent External LLDebug err
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
            logEvent External LLDebug err
            return ResultSerializationFail
        Right catchUp -> do
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
                                    (handleCatchUpStatus @pv @(VersionedSkovM gsconf finconf pv) catchUp catchUpMessageLimit)
                                    (mvrSkovHandlers vc mvr)
                                    (vcContext vc)
                                    st
                                )
                                mvr
                        -- Send out the messages, where necessary.
                        forM_ mmsgs $ \(blocksFins, cusResp) -> do
                            forM_ blocksFins $ uncurry catchUpCallback
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
            logEvent External LLDebug err
            return ResultSerializationFail
        Right transaction -> withWriteLock $ do
            vvec <- liftIO . readIORef =<< asks mvVersions
            case Vec.last vvec of
                (EVersionedConfiguration vc) ->
                    liftSkovUpdate vc $ Skov.receiveTransaction transaction
