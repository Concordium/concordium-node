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
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow (throwM))
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
import Concordium.GlobalState.TreeState (TreeStateMonad (getLastFinalizedHeight))
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

-- |Configuration information for a multi-version runner.
-- The first type parameter defines the global state configuration, and should be an instance of
-- 'MultiVersionStateConfig' (and, as a superclass, 'GlobalStateConfig').
-- The second type parameter defines the finalization configuration, and should be an instance of
-- 'FinalizationConfig'.
data MultiVersionConfiguration gsconf finconf = MultiVersionConfiguration
    { -- |Configuration for the global state.
      mvcStateConfig :: !(StateConfig gsconf),
      -- |Configuration for finalization.
      mvcFinalizationConfig :: !finconf,
      -- |Runtime parameters.
      mvcRuntimeParameters :: !RuntimeParameters
    }

-- |This class provides a mechanism for instantiating a global state configuration for a new
-- genesis.
class
    (GlobalStateConfig gsconf) =>
    MultiVersionStateConfig (gsconf :: Type)
    where
    -- |Type of state configuration data.
    type StateConfig gsconf

    -- |Create a global state configuration for a specific genesis.
    globalStateConfig ::
        StateConfig gsconf ->
        RuntimeParameters ->
        GenesisIndex ->
        -- |Absolute height of the genesis block.
        AbsoluteBlockHeight ->
        gsconf

instance MultiVersionStateConfig MemoryTreeMemoryBlockConfig where
    type StateConfig MemoryTreeMemoryBlockConfig = ()
    globalStateConfig _ rtp _ _ = MTMBConfig rtp

instance MultiVersionStateConfig DiskTreeDiskBlockConfig where
    type StateConfig DiskTreeDiskBlockConfig = DiskStateConfig
    globalStateConfig DiskStateConfig{..} rtp gi _ =
        ( DTDBConfig
            { dtdbRuntimeParameters = rtp,
              dtdbTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
              dtdbBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat"
            }
        )

instance
    (MultiVersionStateConfig c1, MultiVersionStateConfig c2) =>
    MultiVersionStateConfig (PairGSConfig c1 c2)
    where
    type StateConfig (PairGSConfig c1 c2) = (StateConfig c1, StateConfig c2)
    globalStateConfig (sc1, sc2) rtp gi gh =
        PairGSConfig
            ( globalStateConfig sc1 rtp gi gh,
              globalStateConfig sc2 rtp gi gh
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
      -- |Notify the P2P layer that we have a new genesis block, or Nothing
      -- if an unrecognized update took effect.
      notifyRegenesis :: Maybe BlockHash -> IO ()
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
        ( SkovMonad (VersionedSkovM gsconf finconf pv),
          FinalizationMonad (VersionedSkovM gsconf finconf pv),
          BakerMonad (VersionedSkovM gsconf finconf pv)
        ) =>
      EVersionedConfiguration (VersionedConfiguration gsconf finconf pv)

-- |Activate an 'EVersionedConfiguration'. This means caching the state and
-- establishing state invariants so that the configuration can be used as the
-- currently active one for processing blocks, transactions, etc.
activateConfiguration :: SkovConfiguration gsconf finconf UpdateHandler => EVersionedConfiguration gsconf finconf -> LogIO ()
activateConfiguration (EVersionedConfiguration vc) = do
  activeState <- activateSkovState (vcContext vc) =<< liftIO (readIORef (vcState vc))
  liftIO (writeIORef (vcState vc) activeState)

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
    liftSkov :: IsProtocolVersion pv =>
        ( ( SkovMonad (VersionedSkovM gsconf finconf pv),
            FinalizationMonad (VersionedSkovM gsconf finconf pv),
            TreeStateMonad (VersionedSkovM gsconf finconf pv)
          ) =>
          VersionedSkovM gsconf finconf pv a
        ) ->
        VersionedSkovM gsconf finconf pv a

instance
    ( forall pv. IsProtocolVersion pv => BakerMonad (VersionedSkovM gsconf finconf pv),
      forall pv. IsProtocolVersion pv => TreeStateMonad (VersionedSkovM gsconf finconf pv)
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
      -- |Flag to stop importing blocks. When importing blocks from a file is in progress,
      -- setting this flag to True will cause the import to stop.
      mvShouldStopImportingBlocks :: !(IORef Bool),
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
-- It is assumed that the thread holds the write lock. This calls
-- 'notifyRegenesis' to alert the P2P layer of the new genesis block and that
-- catch-up should be invoked.
--
-- This should only be used to process a live protocol update, i.e., a protocol
-- update that will start an additional genesis that the node does not yet know
-- about.
--
-- 'startupSkov' should be used for starting a node up until the last genesis we
-- know about.
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
                    "Starting new chain"
                        ++ " at absolute height "
                        ++ show vcGenesisHeight
                oldVersions <- readIORef mvVersions
                let vcIndex = fromIntegral (length oldVersions)
                (vcContext, st) <-
                    runLoggerT
                        ( initialiseNewSkov
                            gd
                            ( SkovConfig @pv @gsconf @finconf
                                ( globalStateConfig
                                    mvcStateConfig
                                    mvcRuntimeParameters
                                    vcIndex
                                    vcGenesisHeight
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
                (genConf, _) <- runMVR (runSkovT (liftSkov getGenesisData) (mvrSkovHandlers newEConfig mvr) vcContext st) mvr
                -- Notify the network layer we have a new genesis.
                notifyRegenesis (Just (_gcCurrentHash genConf))

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
        ( SkovMonad (VersionedSkovM gc fc pv),
          TreeStateMonad (VersionedSkovM gc fc pv)
        ) =>
        VersionedSkovM gc fc pv ()
    body =
        Skov.getProtocolUpdateStatus >>= \case
            ProtocolUpdated pu -> case checkUpdate @pv pu of
                Left err -> do
                    logEvent Kontrol LLError $
                        "An unsupported protocol update (" ++ err ++ ") has taken effect:"
                            ++ showPU pu
                    lift $ do
                      callbacks <- asks mvCallbacks
                      liftIO (notifyRegenesis callbacks Nothing)
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
    -- |Either encoded or already parsed genesis data. The former is useful
    -- when genesis data is large and expensive to decode.
    Either ByteString PVGenesisData ->
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
        mvShouldStopImportingBlocks <- newIORef False
        mvCatchUpStatusBuffer <- newMVar BufferEmpty
        mvTransactionPurgingThread <- newEmptyMVar
        let mvr = MultiVersionRunner{..}
        runMVR (startupSkov genesis) mvr
        -- Cache accounts, contracts, etc., and establish all invariants for an active consensus.
        runLoggerT (activateConfiguration . Vec.last =<< liftIO (readIORef mvVersions)) mvLog
        putMVar mvWriteLock ()
        startTransactionPurgingThread mvr
        return mvr


-- |Start a consensus with a new genesis.
-- It is assumed that the thread holds the write lock.
-- This calls 'notifyRegenesis' to alert the P2P layer of the new genesis block so that p2p layer
-- starts up with a correct list of genesis blocks.
--
-- Startup works as follows.
--
-- 1. Genesis data is partially decoded to determine the protocol version where
-- the chain is supposed to start.
-- 2. For each protocol version starting at the initial one we attempt to load
-- the existing state for the protocol version and genesis index. This loading
-- is minimal and no state caching is done. Only enough state is loaded so that
-- queries for old blocks, data in blocks, are supported.
-- 3. After this process we end up in one of two slightly different options.
--    - Either there is no state at all. In that case we need to decode the
--    supplied genesis data fully and start a chain with it. We start a new
--    chain with 'newGenesis'.
--    - Or we have loaded at least one state. In this case we must check whether
--      the state already contains an effective protocol update or not.
--      'checkForProtocolUpdate' is used for this and it might start a new chain.
startupSkov ::
    forall gsconf finconf.
    ( MultiVersionStateConfig gsconf,
      MultiVersion gsconf finconf,
      SkovConfiguration gsconf finconf UpdateHandler
    ) =>
    -- |Genesis data, either an unparsed byte array or already deserialized. The
    -- former is useful when genesis is expensive to deserialize, and its
    -- parsing is not needed if the node already has an existing state. The
    -- latter is useful for testing and test runners.
    Either ByteString PVGenesisData ->
    MVR gsconf finconf ()
startupSkov genesis = do
    initProtocolVersion <- case genesis of
        Left genBS -> case runGet getPVGenesisDataPV genBS of
            Left err -> throwM (InvalidGenesisData err)
            Right spv -> return spv
        Right (PVGenesisData (_ :: GenesisData pvOrig)) -> return (SomeProtocolVersion (protocolVersion @pvOrig))
    let loop :: SomeProtocolVersion
             -- ^Protocol version at which to attempt to load the state.
             -> Maybe (EVersionedConfiguration gsconf finconf)
             -- ^If this is the first iteration of the loop then this will be 'Nothing'. Otherwise it is the
             -- versioned configuration produced in the previous iteration of the loop.
             -> GenesisIndex
             -- ^Genesis index at which to attempt to load the state.
             -> AbsoluteBlockHeight
             -- ^Absolute block height of the genesis block of the new chain.
             -> MVR gsconf finconf ()
        loop (SomeProtocolVersion (_ :: SProtocolVersion pv)) first vcIndex vcGenesisHeight = do
            let comp = MVR $
                    \mvr@MultiVersionRunner
                        { mvCallbacks = Callbacks{..},
                          mvConfiguration = MultiVersionConfiguration{..},
                          ..
                        } -> do
                              r <- runLoggerT
                                      ( initialiseExistingSkov
                                          ( SkovConfig @pv @gsconf @finconf
                                              ( globalStateConfig
                                                  mvcStateConfig
                                                  mvcRuntimeParameters
                                                  vcIndex
                                                  vcGenesisHeight
                                              )
                                              mvcFinalizationConfig
                                              UpdateHandler
                                          )
                                      )
                                      mvLog
                              case r of
                                Just (vcContext, st) -> do
                                  vcState <- newIORef st
                                  let vcShutdown = shutdownSkov vcContext =<< liftIO (readIORef vcState)
                                  let newEConfig :: VersionedConfiguration gsconf finconf pv
                                      newEConfig = VersionedConfiguration{..}
                                  oldVersions <- readIORef mvVersions
                                  writeIORef mvVersions (oldVersions `Vec.snoc` newVersion newEConfig)
                                  let getCurrentGenesisAndHeight :: VersionedSkovM gsconf finconf pv (BlockHash, AbsoluteBlockHeight, Maybe SomeProtocolVersion)
                                      getCurrentGenesisAndHeight = liftSkov $ do
                                        currentGenesis <- getGenesisData
                                        lfHeight <- getLastFinalizedHeight
                                        nextPV <- getNextProtocolVersion
                                        return (_gcCurrentHash currentGenesis, localToAbsoluteBlockHeight vcGenesisHeight lfHeight, nextPV)
                                  ((genesisHash, lastFinalizedHeight, nextPV), _) <- runMVR (runSkovT getCurrentGenesisAndHeight (mvrSkovHandlers newEConfig mvr) vcContext st) mvr
                                  notifyRegenesis (Just genesisHash)
                                  return (Left (newVersion newEConfig, lastFinalizedHeight, nextPV))
                                Nothing ->
                                  case first of
                                    Nothing -> return (Right Nothing)
                                    Just newEConfig -> return (Right (Just newEConfig))
            comp >>= \case
                -- We successfully loaded a configuration.
                Left (newEConfig@(EVersionedConfiguration newEConfig'), lastFinalizedHeight, nextPV) ->
                    -- If there is a next protocol then we attempt another loop.
                    -- If there isn't we attempt to start with the last loaded
                    -- state as the active state.
                    case nextPV of
                        Nothing -> liftSkovUpdate newEConfig' checkForProtocolUpdate
                        Just nextSPV -> loop nextSPV (Just newEConfig) (vcIndex + 1) (fromIntegral lastFinalizedHeight + 1)
                -- We failed to load anything in the first iteration of the
                -- loop. Decode the provided genesis and attempt to start the
                -- chain.
                Right Nothing ->
                    case genesis of
                        Left genBS -> case runGet getPVGenesisData genBS of
                            Left err -> do
                                logEvent Runner LLError $ "Failed to decode genesis data: " ++ err
                                throwM (InvalidGenesisData err)
                            Right gd -> newGenesis gd 0
                        Right gd -> newGenesis gd 0
                    -- We loaded some protocol versions. Attempt to start in the
                    -- last one we loaded.
                Right (Just (EVersionedConfiguration newEConfig')) -> liftSkovUpdate newEConfig' checkForProtocolUpdate
    loop initProtocolVersion Nothing 0 0

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

-- |Set the flag to stop importing the blocks to `True`.
stopImportingBlocks :: MultiVersionRunner gsconf finconf -> IO ()
stopImportingBlocks MultiVersionRunner {..} = mask_ $ do
    writeIORef mvShouldStopImportingBlocks True

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
                                    ( handleCatchUpStatus @(VersionedSkovM gsconf finconf pv)
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
            cus <- evalSkovT (getCatchUpStatus @(VersionedSkovM _ _ pv) True) (mvrSkovHandlers vc mvr) (vcContext vc) st
            return (vcIndex vc, runPutLazy $ putVersionedCatchUpStatus cus)

currentProtocolVersion :: MVR gsconf finconf SomeProtocolVersion
currentProtocolVersion = do
    vvec <- liftIO . readIORef =<< asks mvVersions
    case Vec.last vvec of
        EVersionedConfiguration (_ :: VersionedConfiguration gsconf finconf pv) ->
            return $ SomeProtocolVersion $ protocolVersion @pv

-- |Deserialize and receive a transaction.  The transaction is passed to
-- the current version of the chain.
--
-- Currently, the 'BlockItem' type is common among all protocol versions.
-- We deserialize it using the current protocol version.
-- In principle, the protocol version could be different when we call 'Skov.receiveTransaction'.
-- We rely on two principles to ensure this is OK:
--
-- 1. 'Skov.receiveTransaction' must gracefully handle block items from legacy protocol versions.
--
-- 2. A transaction cannot be deserialized to two different block items in different protocol
--    versions.
receiveTransaction :: forall gsconf finconf. ByteString -> MVR gsconf finconf UpdateResult
receiveTransaction transactionBS = do
    now <- utcTimeToTransactionTime <$> currentTime
    SomeProtocolVersion spv <- currentProtocolVersion
    case runGet (getExactVersionedBlockItem spv now) transactionBS of
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
    -- reset the stop flag to False, in case it was set to True
    shouldStop <- asks mvShouldStopImportingBlocks
    liftIO $ writeIORef shouldStop False
    case Vec.last vvec of
        EVersionedConfiguration vc -> do
            -- Import starting from the genesis index of the latest consensus
            res <- importBlocksV3 importFile (vcIndex vc) doImport
            case res of
                Left ImportSerializationFail -> return ResultSerializationFail
                Left (ImportOtherError a) -> return a
                Right _ -> return ResultSuccess
  where
    doImport (ImportBlock _ gi bs) = do
        -- Check if the import should be stopped.
        shouldStop <- liftIO . readIORef =<< asks mvShouldStopImportingBlocks
        if shouldStop
            then return $ fixResult ResultConsensusShutDown
            else fixResult <$> receiveBlock gi bs

    doImport (ImportFinalizationRecord _ gi bs) = fixResult <$> receiveFinalizationRecord gi bs
    fixResult ResultSuccess = Right ()
    fixResult ResultDuplicate = Right ()
    fixResult ResultConsensusShutDown = Right ()
    fixResult e = Left (ImportOtherError e)
