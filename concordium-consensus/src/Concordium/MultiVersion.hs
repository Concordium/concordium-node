{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Control.Monad.State.Strict as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Serialize
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec
import System.FilePath

import Concordium.Logger hiding (Baker)
import Concordium.Types
import Concordium.Types.Block
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Types.Transactions
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus (..))
import Concordium.Types.Updates

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Birk.Bake
import Concordium.GlobalState
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer (BlockPointer (..), BlockPointerData (..))
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.TreeState as SkovV0
import Concordium.GlobalState.TreeState (PVInit (..), TreeStateMonad (getLastFinalizedHeight))
import Concordium.ImportExport
import qualified Concordium.KonsensusV1 as KonsensusV1
import qualified Concordium.KonsensusV1.Consensus as SkovV1
import qualified Concordium.KonsensusV1.Consensus.Blocks as SkovV1
import qualified Concordium.KonsensusV1.Consensus.CatchUp as KonsensusV1
import qualified Concordium.KonsensusV1.SkovMonad as SkovV1
import qualified Concordium.KonsensusV1.Transactions as SkovV1
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as LowLevelDB
import qualified Concordium.KonsensusV1.TreeState.Types as SkovV1
import Concordium.KonsensusV1.Types (Option (..))
import qualified Concordium.KonsensusV1.Types as KonsensusV1
import Concordium.ProtocolUpdate
import qualified Concordium.Skov as Skov
import Concordium.TimeMonad
import Concordium.TimerMonad
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.CatchUp

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

instance Skov.HandlerConfig UpdateHandler where
    type HCContext UpdateHandler = ()
    type HCState UpdateHandler = UpdateHandlerState
    initialiseHandler UpdateHandler = ((), NeverNotified)

instance
    ( IsProtocolVersion pv,
      IsConsensusV0 pv,
      MultiVersion fc,
      Skov.SkovConfiguration fc UpdateHandler
    ) =>
    Skov.HandlerConfigHandlers UpdateHandler (VersionedSkovV0M fc pv)
    where
    -- Notice that isHomeBaked (in the code below) represents whether this block is baked by the
    -- baker ID of this node and it could be the case that the block was not baked by this node,
    -- if another node using the same baker ID.
    -- The information is used to count the number of baked blocks exposed by a prometheus metric.
    -- An alternative implementation would be to extend the @onBlock@ handler (part of @OnSkov@)
    -- to take an extra argument indicating whether the block was just baked or processed as part of a received
    -- block. This would mean that only blocks baked since start by this node would be counted,
    -- not blocks received as part of catchup. However the same cannot be done for finalized blocks as easily
    -- and so for consistency between these two methods we chose to also count blocks received as part of catchup
    -- in both.
    handleBlock bp = liftSkov $ do
        lift (asks (notifyBlockArrived . mvCallbacks)) >>= \case
            Nothing -> return ()
            Just notifyCallback -> do
                versionsRef <- lift (asks mvVersions)
                versions <- liftIO (readIORef versionsRef)
                let latestEraGenesisHeight =
                        evcGenesisHeight $ Vec.last versions
                let height = localToAbsoluteBlockHeight latestEraGenesisHeight (bpHeight bp)
                nodeBakerIdMaybe <- lift (asks (fmap (bakerId . bakerIdentity) . mvBaker))
                let isHomeBaked = case nodeBakerIdMaybe of
                        Nothing -> False
                        Just nodeBakerId -> Just nodeBakerId == (blockBaker <$> blockFields (_bpBlock bp))
                liftIO (notifyCallback (bpHash bp) height isHomeBaked)

    -- Notice that isHomeBaked (in the code below) represents whether this block is baked by the
    -- baker ID of this node and it could be the case that the block was not baked by this node,
    -- if another node using the same baker ID.
    -- The information is used to count the number of finalized baked blocks exposed by a prometheus
    -- metric.
    handleFinalize _ lfbp bps = liftSkov $ do
        -- Trigger callback notifying Rust of a finalized block.
        lift (asks (notifyBlockFinalized . mvCallbacks)) >>= \case
            Nothing -> return ()
            Just notifyCallback -> do
                -- Notify a new block was finalized first.
                versionsRef <- lift (asks mvVersions)
                versions <- liftIO (readIORef versionsRef)
                let latestEraGenesisHeight =
                        evcGenesisHeight $ Vec.last versions
                nodeBakerIdMaybe <- lift (asks (fmap (bakerId . bakerIdentity) . mvBaker))
                forM_ (reverse bps) $ \bp -> do
                    let height = localToAbsoluteBlockHeight latestEraGenesisHeight (bpHeight bp)
                    let isHomeBaked = case nodeBakerIdMaybe of
                            Nothing -> False
                            Just nodeBakerId -> Just nodeBakerId == (blockBaker <$> blockFields (_bpBlock bp))
                    liftIO (notifyCallback (bpHash bp) height isHomeBaked)
                let height = localToAbsoluteBlockHeight latestEraGenesisHeight (bpHeight lfbp)
                let isHomeBaked = case nodeBakerIdMaybe of
                        Nothing -> False
                        Just myBakerId -> Just myBakerId == (blockBaker <$> blockFields (_bpBlock lfbp))
                liftIO (notifyCallback (bpHash lfbp) height isHomeBaked)
        -- And then check for protocol update.
        checkForProtocolUpdate

-- |Configuration for the global state that uses disk storage
-- for both tree state and block state.
newtype DiskStateConfig = DiskStateConfig
    { -- |Root directory for the global state.
      stateBasePath :: FilePath
    }

-- |Configuration information for a multi-version runner.
-- The type parameter defines the finalization configuration, and should be an instance of
-- 'FinalizationConfig'.
data MultiVersionConfiguration finconf = MultiVersionConfiguration
    { -- |Configuration for the global state.
      mvcStateConfig :: !DiskStateConfig,
      -- |Configuration for finalization.
      mvcFinalizationConfig :: !finconf,
      -- |Runtime parameters.
      mvcRuntimeParameters :: !RuntimeParameters
    }

-- |Create a global state configuration for a specific genesis.
globalStateConfig ::
    DiskStateConfig ->
    RuntimeParameters ->
    GenesisIndex ->
    -- |Absolute height of the genesis block.
    AbsoluteBlockHeight ->
    GlobalStateConfig
globalStateConfig DiskStateConfig{..} rtp gi _ =
    ( GlobalStateConfig
        { dtdbRuntimeParameters = rtp,
          dtdbTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
          dtdbBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat"
        }
    )

-- |Create a global state configuration for a specific genesis.
globalStateConfigV1 ::
    DiskStateConfig ->
    RuntimeParameters ->
    GenesisIndex ->
    SkovV1.GlobalStateConfig
globalStateConfigV1 DiskStateConfig{..} rtp gi =
    ( SkovV1.GlobalStateConfig
        { gscRuntimeParameters = rtp,
          gscTreeStateDirectory = stateBasePath </> ("treestate-" ++ show gi),
          gscBlockStateFile = stateBasePath </> ("blockstate-" ++ show gi) <.> "dat"
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
      -- |Send a catch-up status message to all (non-pending) peers.
      -- This should be used when a pending block becomes live.
      -- The status message should be neither a request nor a response.
      notifyCatchUpStatus :: GenesisIndex -> ByteString -> IO (),
      -- |Notify the P2P layer that we have a new genesis block, or Nothing
      -- if an unrecognized update took effect.
      notifyRegenesis :: Maybe BlockHash -> IO (),
      -- |Notify a block was added to the tree. The arguments are
      -- the hash of the block, its absolute height and whether the block was produced by the baker id configured for this node.
      notifyBlockArrived :: Maybe (BlockHash -> AbsoluteBlockHeight -> Bool -> IO ()),
      -- |Notify a block was finalized. The arguments are the hash of the block,
      -- its absolute height and whether the block was produced by the baker id configured for this node.
      notifyBlockFinalized :: Maybe (BlockHash -> AbsoluteBlockHeight -> Bool -> IO ()),
      -- |Notify unsupported protocol update is pending when called.
      -- Takes the effective time of the update as argument.
      notifyUnsupportedProtocolUpdate :: Maybe (Timestamp -> IO ())
    }

-- |Handler context used by the version-1 consensus. The handlers run on the 'MVR' monad, which
-- they use to resolve the appropriate callbacks.
skovV1Handlers ::
    forall pv finconf.
    GenesisIndex ->
    AbsoluteBlockHeight ->
    SkovV1.HandlerContext pv (MVR finconf)
skovV1Handlers gi genHeight = SkovV1.HandlerContext{..}
  where
    _sendTimeoutHandler timeoutMsg = MVR $ \mvr -> do
        broadcastFinalizationMessage
            (mvCallbacks mvr)
            gi
            (encode (KonsensusV1.FMTimeoutMessage timeoutMsg))
    _sendQuorumHandler quorumMsg = MVR $ \mvr -> do
        broadcastFinalizationMessage
            (mvCallbacks mvr)
            gi
            (encode (KonsensusV1.FMQuorumMessage quorumMsg))
    _sendBlockHandler block = MVR $ \mvr -> do
        broadcastBlock
            (mvCallbacks mvr)
            gi
            (runPut (KonsensusV1.putSignedBlock block))

    _onBlockHandler :: SkovV1.BlockPointer pv -> MVR finconf ()
    _onBlockHandler block = do
        -- Notice that isHomeBaked (in the code below) represents whether this block is baked by the
        -- baker ID of this node and it could be the case that the block was not baked by this node,
        -- if another node using the same baker ID.
        -- The information is used to count the number of baked blocks exposed by a prometheus metric.
        -- An alternative implementation would be to extend the @onBlock@ handler (part of @OnSkov@)
        -- to take an extra argument indicating whether the block was just baked or processed as part of a received
        -- block. This would mean that only blocks baked since start by this node would be counted,
        -- not blocks received as part of catchup. However the same cannot be done for finalized blocks as easily
        -- and so for consistency between these two methods we chose to also count blocks received as part of catchup
        -- in both.
        asks (notifyBlockArrived . mvCallbacks) >>= \case
            Nothing -> return ()
            Just notifyCallback -> do
                let height = localToAbsoluteBlockHeight genHeight (SkovV1.blockHeight block)
                nodeBakerIdMaybe <- asks (fmap (bakerId . bakerIdentity) . mvBaker)
                let isHomeBaked = case nodeBakerIdMaybe of
                        Nothing -> False
                        Just nodeBakerId ->
                            KonsensusV1.Present nodeBakerId
                                == (KonsensusV1.blockBaker <$> KonsensusV1.blockBakedData block)
                liftIO (notifyCallback (getHash block) height isHomeBaked)

    _onFinalizeHandler :: w -> [SkovV1.BlockPointer pv] -> MVR finconf ()
    _onFinalizeHandler _ finalizedBlocks = do
        asks (notifyBlockFinalized . mvCallbacks) >>= \case
            Nothing -> return ()
            Just notifyCallback -> do
                nodeBakerIdMaybe <- asks (fmap (bakerId . bakerIdentity) . mvBaker)
                forM_ finalizedBlocks $ \bp -> do
                    let height = localToAbsoluteBlockHeight genHeight (SkovV1.blockHeight bp)
                    let isHomeBaked = case nodeBakerIdMaybe of
                            Nothing -> False
                            Just nodeBakerId ->
                                KonsensusV1.Present nodeBakerId
                                    == (KonsensusV1.blockBaker <$> KonsensusV1.blockBakedData bp)
                    liftIO (notifyCallback (getHash bp) height isHomeBaked)
        -- FIXME: Run protocol update check. Issue #825
        return ()

    _onPendingLiveHandler = do
        -- Notify peers of our catch-up status, since they may not be aware of the now-live pending
        -- blocks.
        bufferedSendCatchUpStatus

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

-- |Configuration for the version-0 consensus at a particular genesis index.
data VersionedConfigurationV0 finconf (pv :: ProtocolVersion) = VersionedConfigurationV0
    { -- |The 'SkovContext' (immutable)
      vc0Context :: !(Skov.SkovContext (Skov.SkovConfig pv finconf UpdateHandler)),
      -- |The 'SkovState' (mutable) via an 'IORef'. This should only be updated
      -- by a thread that holds the global lock.
      vc0State :: !(IORef (Skov.SkovState (Skov.SkovConfig pv finconf UpdateHandler))),
      -- |The genesis index
      vc0Index :: GenesisIndex,
      -- |The absolute block height of the genesis block
      vc0GenesisHeight :: AbsoluteBlockHeight,
      -- |Shutdown the skov. This should only be called by a thread that holds the global lock
      -- and the configuration should not be used subsequently.
      vc0Shutdown :: LogIO ()
    }

-- |Configuration for the version-1 consensus at a particular genesis index.
data VersionedConfigurationV1 finconf (pv :: ProtocolVersion) = VersionedConfigurationV1
    { -- |The immutable 'SkovV1.SkovV1Context'.
      vc1Context :: !(SkovV1.SkovV1Context pv (MVR finconf)),
      -- |The 'SkovV1.SkovV1State' (mutable), wrapped in an 'IORef'. This should only be updated
      -- by a thread that holds the global lock.
      vc1State :: !(IORef (SkovV1.SkovV1State pv)),
      -- |The genesis index
      vc1Index :: GenesisIndex,
      -- |The absolute block height of the genesis block
      vc1GenesisHeight :: AbsoluteBlockHeight,
      -- |Shutdown the skov. This should only be called by a thread that holds the global lock
      -- and the configuration should not be used subsequently.
      vc1Shutdown :: LogIO ()
    }

-- |'SkovConfig' instantiated for the multi-version runner.
type VersionedConfig finconf pv = Skov.SkovConfig pv finconf UpdateHandler

-- |'SkovHandlers' instantiated for the multi-version runner.
type VersionedHandlers finconf (pv :: ProtocolVersion) =
    Skov.SkovHandlers pv ThreadTimer (VersionedConfig finconf pv) (MVR finconf)

-- |The 'SkovT' monad instantiated for the multi-version runner.
-- This monad is used for running operations on the version-0 consensus.
type VersionedSkovV0M finconf pv =
    Skov.SkovT
        pv
        (VersionedHandlers finconf pv)
        (VersionedConfig finconf pv)
        (MVR finconf)

-- |The monad used for running operations on the version-1 consensus.
type VersionedSkovV1M finconf pv =
    SkovV1.SkovV1T pv (MVR finconf)

-- |An existential wrapper around a 'VersionedConfigurationV0' or 'VersionedConfigurationV1' that
-- abstracts the protocol version. For 'VersionedConfigurationV0', we require 'SkovMonad' and
-- 'FinalizationMonad' instances for 'VersionedSkovV0M' instantiated at the abstracted protocol
-- version. For 'VersionedConfigurationV1', it is sufficient to have 'IsConsensusV1' and
-- 'IsProtocolVersion' for the protocol version.
data EVersionedConfiguration finconf
    = -- |A configuration for consensus version 0.
      forall (pv :: ProtocolVersion).
        ( Skov.SkovMonad (VersionedSkovV0M finconf pv),
          FinalizationMonad (VersionedSkovV0M finconf pv),
          BakerMonad (VersionedSkovV0M finconf pv)
        ) =>
      EVersionedConfigurationV0 (VersionedConfigurationV0 finconf pv)
    | -- |A configuration for consensus version 1.
      forall (pv :: ProtocolVersion).
        ( IsConsensusV1 pv,
          IsProtocolVersion pv
        ) =>
      EVersionedConfigurationV1 (VersionedConfigurationV1 finconf pv)

-- |Get the genesis height of an 'EVersionedConfiguration'.
evcGenesisHeight :: EVersionedConfiguration finconf -> AbsoluteBlockHeight
evcGenesisHeight (EVersionedConfigurationV0 vc) = vc0GenesisHeight vc
evcGenesisHeight (EVersionedConfigurationV1 vc) = vc1GenesisHeight vc

-- |Shutdown the skov of an 'EVersionedConfiguration'. This should only by invoked by a thread
-- that holds the global lock, and the configuration should not be used subsequently.
evcShutdown :: EVersionedConfiguration finconf -> LogIO ()
evcShutdown (EVersionedConfigurationV0 vc) = vc0Shutdown vc
evcShutdown (EVersionedConfigurationV1 vc) = vc1Shutdown vc

-- |Get the genesis index of an 'EVersionedConfiguration'.
evcIndex :: EVersionedConfiguration finconf -> GenesisIndex
evcIndex (EVersionedConfigurationV0 vc) = vc0Index vc
evcIndex (EVersionedConfigurationV1 vc) = vc1Index vc

-- |Get the protocol version associated with an 'EVersionedConfiguration'.
evcProtocolVersion :: EVersionedConfiguration finconf -> ProtocolVersion
evcProtocolVersion (EVersionedConfigurationV0 (_ :: VersionedConfigurationV0 finconf pv)) =
    demoteProtocolVersion (protocolVersion @pv)
evcProtocolVersion (EVersionedConfigurationV1 (_ :: VersionedConfigurationV1 finconf pv)) =
    demoteProtocolVersion (protocolVersion @pv)

-- |Activate an 'EVersionedConfiguration'. This means caching the state and
-- establishing state invariants so that the configuration can be used as the
-- currently active one for processing blocks, transactions, etc.
activateConfiguration :: Skov.SkovConfiguration finconf UpdateHandler => EVersionedConfiguration finconf -> MVR finconf ()
activateConfiguration (EVersionedConfigurationV0 vc) = do
    activeState <- mvrLogIO . Skov.activateSkovState (vc0Context vc) =<< liftIO (readIORef (vc0State vc))
    liftIO (writeIORef (vc0State vc) activeState)
activateConfiguration (EVersionedConfigurationV1 vc) = do
    activeState <- mvrLogIO . SkovV1.activateSkovV1State (vc1Context vc) =<< liftIO (readIORef (vc1State vc))
    liftIO (writeIORef (vc1State vc) activeState)

-- |This class makes it possible to use a multi-version configuration at a specific version.
-- Essentially, this class provides instances of 'SkovMonad', 'FinalizationMonad' and
-- 'TreeStateMonad' for 'VersionedSkovV0M' instantiated with the configuration parameters and at any
-- 'ProtocolVersion'.
--
-- There is only one instance for this class, but it is fully general.  The reason that this needs
-- to be a class (rather than a constraint alias) is that the constraints involved are quantified.
-- This makes it problematic for the type-checker to correctly simplify them.
class MultiVersion finconf where
    -- |Convert a 'VersionedConfigurationV0' to an 'EVersionedConfiguration'.
    newVersionV0 ::
        (IsProtocolVersion pv, IsConsensusV0 pv) =>
        VersionedConfigurationV0 finconf pv ->
        EVersionedConfiguration finconf

    -- |Convert a 'VersionedConfigurationV1' to an 'EVersionedConfiguration'.
    newVersionV1 ::
        (IsProtocolVersion pv, IsConsensusV1 pv) =>
        VersionedConfigurationV1 finconf pv ->
        EVersionedConfiguration finconf

    -- |Supply a 'VersionedSkovV0M' action with instances of 'SkovMonad', 'FinalizationMonad' and
    -- 'TreeStateMonad'.
    liftSkov ::
        (IsProtocolVersion pv, IsConsensusV0 pv) =>
        ( ( Skov.SkovMonad (VersionedSkovV0M finconf pv),
            FinalizationMonad (VersionedSkovV0M finconf pv),
            TreeStateMonad (VersionedSkovV0M finconf pv)
          ) =>
          VersionedSkovV0M finconf pv a
        ) ->
        VersionedSkovV0M finconf pv a

instance
    ( forall pv. (IsProtocolVersion pv, IsConsensusV0 pv) => BakerMonad (VersionedSkovV0M finconf pv),
      forall pv. IsProtocolVersion pv => TreeStateMonad (VersionedSkovV0M finconf pv)
    ) =>
    MultiVersion finconf
    where
    newVersionV0 = EVersionedConfigurationV0
    newVersionV1 = EVersionedConfigurationV1
    liftSkov a = a

-- |State of catch-up buffering.  This is used for buffering the sending of catch-up status messages
-- that need to be sent as a result of pending blocks becoming live, or as a result of making
-- progress due to catch-up.  See
-- 'bufferedSendCatchUpStatus' for details.
data CatchUpStatusBufferState
    = -- |We are not currently waiting to send a catch-up status message.
      BufferEmpty
    | -- |We are currently waiting to send a catch-up status message.
      BufferPending
        { -- |The soonest the status message should be sent.
          cusbsSoonest :: !UTCTime,
          -- |The latest the status message should be sent.
          cusbsLatest :: !UTCTime
        }
    | -- |We should not send any more catch-up status messages.
      BufferShutdown

-- |The context for managing multi-version consensus.
data MultiVersionRunner finconf = MultiVersionRunner
    { -- |Base configuration.
      mvConfiguration :: !(MultiVersionConfiguration finconf),
      -- |Callback functions for sending messsages to the network.
      mvCallbacks :: !Callbacks,
      -- |Baker identity.
      mvBaker :: !(Maybe Baker),
      -- |Thread that periodically purges uncommitted transactions.
      mvTransactionPurgingThread :: !(MVar ThreadId),
      -- |Vector of states, indexed by the genesis index.
      -- This is only ever updated by extending the vector, and only while
      -- the write lock is held.
      mvVersions :: !(IORef (Vec.Vector (EVersionedConfiguration finconf))),
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

-- |Multi-version runner monad. Ultimately, @MVR finconf@ is a newtype wrapper around
-- @ReaderT (MultiVersionRunner finconf) IO@. That is, it is an @IO@ monad with a
-- @MultiVersionRunner@ context.
newtype MVR finconf a = MVR {runMVR :: MultiVersionRunner finconf -> IO a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadIO,
          MonadReader (MultiVersionRunner finconf),
          TimeMonad,
          MonadThrow,
          MonadCatch,
          MonadMask
        )
        via (ReaderT (MultiVersionRunner finconf) IO)

instance MonadLogger (MVR finconf) where
    logEvent src lvl msg = MVR $ \mvr -> mvLog mvr src lvl msg
    {-# INLINE logEvent #-}

-- |Perform an action while holding the global state write lock.
-- If the action throws an exception, this ensures that the lock is
-- released.
withWriteLock :: MVR finconf a -> MVR finconf a
{-# INLINE withWriteLock #-}
withWriteLock a = MVR $ \mvr -> withWriteLockIO mvr (runMVR a mvr)

-- |Perform an action while holding the global state write lock.
-- If the action throws an exception, this ensures that the lock is
-- released.
withWriteLockIO :: MultiVersionRunner finconf -> IO a -> IO a
{-# INLINE withWriteLockIO #-}
withWriteLockIO MultiVersionRunner{..} a =
    bracketOnError
        (takeMVar mvWriteLock)
        ( \() ->
            tryPutMVar mvWriteLock ()
                >> mvLog Runner LLWarning "Released global state lock following error."
        )
        $ \_ -> do
            res <- a
            putMVar mvWriteLock ()
            return res

-- |Perform an action while holding the global state write lock. Optionally, when the action
-- completes, a thread is forked to perform a follow-up action before releasing the lock.
-- If either the action or the follow-up action throws an exception, the write lock will be
-- released.
withWriteLockMaybeFork :: MVR finconf (a, Maybe b) -> (b -> MVR finconf ()) -> MVR finconf a
withWriteLockMaybeFork action followup = MVR $ \mvr ->
    withWriteLockMaybeForkIO mvr (runMVR action mvr) (\b -> runMVR (followup b) mvr)

-- |Perform an action while holding the global state write lock. Optionally, when the action
-- completes, a thread is forked to perform a follow-up action before releasing the lock.
-- If either the action or the follow-up action throws an exception, the write lock will be
-- released.
withWriteLockMaybeForkIO :: MultiVersionRunner finconf -> IO (a, Maybe b) -> (b -> IO ()) -> IO a
{-# INLINE withWriteLockMaybeForkIO #-}
withWriteLockMaybeForkIO MultiVersionRunner{..} action followup = mask $ \unmask -> do
    () <- takeMVar mvWriteLock
    (res, mContinue) <- unmask action `onException` tryPutMVar mvWriteLock ()
    case mContinue of
        Just continueArg -> do
            let release = putMVar mvWriteLock ()
            -- forkIO is guaranteed to be uninterruptible, so we can be sure that an async exception
            -- won't prevent the lock being released. Also note that the masking state of the thread
            -- is inherited, so we unmask when running the follow-up.
            void $ forkIO (unmask (try @SomeException (followup continueArg)) >> release)
        Nothing -> do
            putMVar mvWriteLock ()
    return res

-- |Lift a 'LogIO' action into the 'MVR' monad.
mvrLogIO :: LogIO a -> MVR finconf a
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
    forall finconf.
    ( MultiVersion finconf,
      Skov.SkovConfiguration finconf UpdateHandler
    ) =>
    -- |Genesis data
    PVGenesisData ->
    -- |Absolute height of the new genesis block
    AbsoluteBlockHeight ->
    MVR finconf ()
newGenesis (PVGenesisData (gd :: GenesisData pv)) genesisHeight = case consensusVersionFor (protocolVersion @pv) of
    ConsensusV0 ->
        MVR $
            \MultiVersionRunner
                { mvCallbacks = Callbacks{..},
                  mvConfiguration = MultiVersionConfiguration{..},
                  ..
                } -> do
                    mvLog Runner LLInfo $
                        "Starting new chain"
                            ++ " at absolute height "
                            ++ show genesisHeight
                    oldVersions <- readIORef mvVersions
                    let vc0Index = fromIntegral (length oldVersions)
                    (vc0Context, st) <-
                        runLoggerT
                            ( Skov.initialiseNewSkov
                                gd
                                ( Skov.SkovConfig @pv @finconf
                                    ( globalStateConfig
                                        mvcStateConfig
                                        mvcRuntimeParameters
                                        vc0Index
                                        genesisHeight
                                    )
                                    mvcFinalizationConfig
                                    UpdateHandler
                                )
                            )
                            mvLog
                    vc0State <- newIORef st
                    let vc0Shutdown = Skov.shutdownSkov vc0Context =<< liftIO (readIORef vc0State)
                    let newEConfig :: VersionedConfigurationV0 finconf pv
                        newEConfig = VersionedConfigurationV0{vc0GenesisHeight = genesisHeight, ..}
                    writeIORef mvVersions (oldVersions `Vec.snoc` newVersionV0 newEConfig)
                    -- Notify the network layer we have a new genesis.
                    notifyRegenesis (Just (genesisBlockHash gd))
    ConsensusV1 ->
        MVR $
            \mvr@MultiVersionRunner
                { mvCallbacks = Callbacks{..},
                  mvConfiguration = MultiVersionConfiguration{..},
                  ..
                } -> do
                    mvLog Runner LLInfo $
                        "Starting new chain"
                            ++ " at absolute height "
                            ++ show genesisHeight
                    oldVersions <- readIORef mvVersions
                    let vc1Index = fromIntegral (length oldVersions)
                    -- We need an "unlift" operation to run a SkovV1 transaction in an IO context.
                    -- This is used for implementing timer handlers.
                    -- The "unlift" is implemented by using an 'MVar' to store the configuration in,
                    -- that will be set after initialization.
                    configRef <- newEmptyMVar
                    let unliftSkov :: forall b. VersionedSkovV1M finconf pv b -> IO b
                        unliftSkov a = do
                            config <- readMVar configRef
                            runMVR (runSkovV1Transaction config a) mvr
                    let !handlers = skovV1Handlers vc1Index genesisHeight
                    (vc1Context, st) <-
                        runLoggerT
                            ( SkovV1.initialiseNewSkovV1
                                gd
                                (SkovV1.BakerContext (bakerIdentity <$> mvBaker))
                                -- Handler context
                                handlers
                                -- lift SkovV1T
                                unliftSkov
                                -- Global state config
                                (globalStateConfigV1 mvcStateConfig mvcRuntimeParameters vc1Index)
                            )
                            mvLog
                    vc1State <- newIORef st
                    let vc1Shutdown = SkovV1.shutdownSkovV1 vc1Context
                    let newEConfig :: VersionedConfigurationV1 finconf pv
                        newEConfig = VersionedConfigurationV1{vc1GenesisHeight = genesisHeight, ..}
                    putMVar configRef newEConfig
                    writeIORef mvVersions (oldVersions `Vec.snoc` newVersionV1 newEConfig)
                    -- Notify the network layer we have a new genesis.
                    notifyRegenesis (Just (genesisBlockHash gd))
                    -- Start the consensus
                    runMVR (liftSkovV1Update newEConfig KonsensusV1.startEvents) mvr

-- |Determine if a protocol update has occurred, and handle it.
-- When a protocol update first becomes pending, this logs the update that will occur (if it is
-- of a known type) or logs an error message (if it is unknown).
-- When the protocol update takes effect, this will create the new genesis block, starting up a
-- new instances of consensus and shutting down the old one (which is still available for queries).
-- However, if the new protocol is unknown, no update will take place, but the old consensus will
-- effectively stop accepting blocks.
-- It is assumed that the thread holds the write lock.
checkForProtocolUpdate ::
    forall lastpv fc.
    ( IsProtocolVersion lastpv,
      IsConsensusV0 lastpv,
      MultiVersion fc,
      Skov.SkovConfiguration fc UpdateHandler
    ) =>
    VersionedSkovV0M fc lastpv ()
checkForProtocolUpdate = liftSkov body
  where
    body ::
        ( Skov.SkovMonad (VersionedSkovV0M fc lastpv),
          TreeStateMonad (VersionedSkovV0M fc lastpv)
        ) =>
        VersionedSkovV0M fc lastpv ()
    body =
        check >>= \case
            Nothing -> return ()
            Just (PVInit{pvInitGenesis = nextGenesis :: Regenesis newpv, ..}) ->
                case consensusVersionFor (protocolVersion @newpv) of
                    ConsensusV0 -> do
                        MultiVersionRunner{..} <- lift ask
                        existingVersions <- liftIO (readIORef mvVersions)
                        let latestEraGenesisHeight = evcGenesisHeight $ Vec.last existingVersions
                        let vc0Index = fromIntegral (length existingVersions)
                        let vc0GenesisHeight = 1 + localToAbsoluteBlockHeight latestEraGenesisHeight pvInitFinalHeight
                        -- construct the new skov instance
                        let newGSConfig =
                                Skov.SkovConfig @newpv @fc
                                    ( globalStateConfig
                                        (mvcStateConfig mvConfiguration)
                                        (mvcRuntimeParameters mvConfiguration)
                                        vc0Index
                                        vc0GenesisHeight
                                    )
                                    (mvcFinalizationConfig mvConfiguration)
                                    UpdateHandler
                        -- clear data we no longer need after the protocol update
                        Skov.clearSkovOnProtocolUpdate
                        -- migrate the final block state into the new skov instance, and establish
                        -- all the necessary transaction table, and other, invariants.
                        (vc0Context, st) <- do
                            ctx <- asks Skov.srContext
                            Skov.SkovT $ do
                                currentState <- State.get
                                liftIO $
                                    runLoggerT
                                        (Skov.migrateExistingSkov ctx currentState pvInitMigration nextGenesis newGSConfig)
                                        mvLog
                        -- Close down and resources that the old instance retains. We do this after
                        -- since, e.g., caches and the transaction table are needed during migration.
                        Skov.terminateSkov
                        -- wrap up, notify the network layer, and add the new instance to
                        -- the end of the mvVersions list
                        liftIO $ do
                            vc0State <- liftIO $ newIORef st
                            let vc0Shutdown = Skov.shutdownSkov vc0Context =<< liftIO (readIORef vc0State)
                            let newEConfig :: VersionedConfigurationV0 fc newpv
                                newEConfig = VersionedConfigurationV0{..}
                            writeIORef mvVersions (existingVersions `Vec.snoc` newVersionV0 newEConfig)
                            -- Notify the network layer we have a new genesis.
                            let Callbacks{..} = mvCallbacks
                            liftIO $ notifyRegenesis (Just (regenesisBlockHash nextGenesis))
                    ConsensusV1 -> do
                        -- Clear the old skov instance.
                        Skov.clearSkovOnProtocolUpdate
                        mvr@MultiVersionRunner
                            { mvConfiguration = MultiVersionConfiguration{..},
                              mvCallbacks = Callbacks{..},
                              ..
                            } <-
                            lift ask
                        existingVersions <- liftIO (readIORef mvVersions)
                        let latestEraGenesisHeight = evcGenesisHeight $ Vec.last existingVersions
                        let vc1Index = fromIntegral (length existingVersions)
                            vc1GenesisHeight = 1 + localToAbsoluteBlockHeight latestEraGenesisHeight pvInitFinalHeight
                        configRef <- liftIO newEmptyMVar
                        -- We need an "unlift" operation to run a SkovV1 transaction in an IO context.
                        -- This is used for implementing timer handlers.
                        -- The "unlift" is implemented by using an 'MVar' to store the configuration in,
                        -- that will be set after initialization.
                        let
                            unliftSkov :: forall b. VersionedSkovV1M fc newpv b -> IO b
                            unliftSkov a = do
                                config <- readMVar configRef
                                runMVR (runSkovV1Transaction config a) mvr
                        let !handlers = skovV1Handlers vc1Index vc1GenesisHeight
                        -- get the last finalized block state
                        lastFinBlockState <- Skov.queryBlockState =<< Skov.lastFinalizedBlock
                        -- the existing persistent block state context.
                        existingPbsc <- asks $ Skov.scGSContext . Skov.srContext
                        -- the current transaction table.
                        oldTT <- Skov.SkovT $ SkovV0._transactionTable . Skov.ssGSState <$> State.get
                        -- Migrate the old state to the new protocol and
                        -- get the new skov context and state.
                        (vc1Context, newState) <-
                            liftIO $
                                runLoggerT
                                    ( SkovV1.migrateSkovFromConsensusV0
                                        -- regenesis
                                        nextGenesis
                                        -- migration
                                        pvInitMigration
                                        -- Global state config
                                        (globalStateConfigV1 mvcStateConfig mvcRuntimeParameters vc1Index)
                                        -- The existing persistent block state context
                                        existingPbsc
                                        -- The last finalized state of the chain we're migrating from.
                                        lastFinBlockState
                                        -- The baker context
                                        (SkovV1.BakerContext (bakerIdentity <$> mvBaker))
                                        -- Handler context
                                        handlers
                                        -- lift SkovV1T
                                        unliftSkov
                                        -- the current transaction table
                                        oldTT
                                    )
                                    mvLog
                        -- Shutdown the old skov instance as it is
                        -- no longer required after the migration.
                        Skov.terminateSkov
                        -- Create a reference for the new state.
                        vc1State <- liftIO $ newIORef newState
                        let vc1Shutdown = SkovV1.shutdownSkovV1 vc1Context
                            newECConfig :: VersionedConfigurationV1 fc newpv
                            newECConfig = VersionedConfigurationV1{..}
                        liftIO $ do
                            -- Write the new configuration reference.
                            putMVar configRef newECConfig
                            -- Write out the new versions with the appended new protocol version.
                            writeIORef mvVersions (existingVersions `Vec.snoc` newVersionV1 newECConfig)
                            -- Notify the network layer about the new genesis.
                            notifyRegenesis (Just (regenesisBlockHash nextGenesis))
                            -- startup the new consensus
                            runMVR (liftSkovV1Update newECConfig KonsensusV1.startEvents) mvr
    showPU ProtocolUpdate{..} =
        Text.unpack puMessage
            ++ "\n["
            ++ Text.unpack puSpecificationURL
            ++ " (hash "
            ++ show puSpecificationHash
            ++ ")]"

    -- Check whether a protocol update has taken effect. If it did return
    -- information needed to initialize a new skov instance.
    check ::
        ( Skov.SkovMonad (VersionedSkovV0M fc lastpv),
          TreeStateMonad (VersionedSkovV0M fc lastpv)
        ) =>
        VersionedSkovV0M fc lastpv (Maybe (PVInit (VersionedSkovV0M fc lastpv)))
    check =
        Skov.getProtocolUpdateStatus >>= \case
            ProtocolUpdated pu -> case checkUpdate @lastpv pu of
                Left err -> do
                    logEvent Kontrol LLError $
                        "An unsupported protocol update ("
                            ++ err
                            ++ ") has taken effect:"
                            ++ showPU pu
                    lift $ do
                        callbacks <- asks mvCallbacks
                        liftIO (notifyRegenesis callbacks Nothing)
                        return Nothing
                Right upd -> do
                    logEvent Kontrol LLInfo $ "Starting protocol update."
                    initData <- updateRegenesis upd
                    return (Just initData)
            PendingProtocolUpdates [] -> return Nothing
            PendingProtocolUpdates ((ts, pu) : _) -> do
                -- There is a queued protocol update, but only log about it
                -- if we have not done so already.
                alreadyNotified <-
                    State.state
                        ( \s ->
                            if Skov.ssHandlerState s == AlreadyNotified ts pu
                                then (True, s)
                                else (False, s{Skov.ssHandlerState = AlreadyNotified ts pu})
                        )
                unless alreadyNotified $ case checkUpdate @lastpv pu of
                    Left err -> do
                        logEvent Kontrol LLError $
                            "An unsupported protocol update ("
                                ++ err
                                ++ ") will take effect at "
                                ++ show (timestampToUTCTime $ transactionTimeToTimestamp ts)
                                ++ ": "
                                ++ showPU pu
                        callbacks <- lift $ asks mvCallbacks
                        case notifyUnsupportedProtocolUpdate callbacks of
                            Just notifyCallback -> liftIO $ notifyCallback $ transactionTimeToTimestamp ts
                            Nothing -> return ()
                    Right upd -> do
                        logEvent Kontrol LLInfo $
                            "A protocol update will take effect at "
                                ++ show (timestampToUTCTime $ transactionTimeToTimestamp ts)
                                ++ ": "
                                ++ showPU pu
                                ++ "\n"
                                ++ show upd
                return Nothing

-- |Make a 'MultiVersionRunner' for a given configuration.
makeMultiVersionRunner ::
    ( MultiVersion finconf,
      Skov.SkovConfiguration finconf UpdateHandler
    ) =>
    MultiVersionConfiguration finconf ->
    Callbacks ->
    Maybe BakerIdentity ->
    LogMethod IO ->
    -- |Either encoded or already parsed genesis data. The former is useful
    -- when genesis data is large and expensive to decode.
    Either ByteString PVGenesisData ->
    IO (MultiVersionRunner finconf)
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
    forall finconf.
    ( MultiVersion finconf,
      Skov.SkovConfiguration finconf UpdateHandler
    ) =>
    -- |Genesis data, either an unparsed byte array or already deserialized. The
    -- former is useful when genesis is expensive to deserialize, and its
    -- parsing is not needed if the node already has an existing state. The
    -- latter is useful for testing and test runners.
    Either ByteString PVGenesisData ->
    MVR finconf ()
startupSkov genesis = do
    initProtocolVersion <- case genesis of
        Left genBS -> case runGet getPVGenesisDataPV genBS of
            Left err -> throwM (InvalidGenesisData err)
            Right spv -> return spv
        Right (PVGenesisData (_ :: GenesisData pvOrig)) ->
            return (SomeProtocolVersion (protocolVersion @pvOrig))
    loadLoop initProtocolVersion activateGenesis 0 0
  where
    loadLoop ::
        SomeProtocolVersion ->
        -- Continuation to activate the last configuration.
        -- For the first iteration of the loop, this should initialise the configuration from the
        -- genesis data.
        MVR finconf () ->
        -- Genesis index at which to attempt to load the state.
        GenesisIndex ->
        -- Absolute block height of the genesis block of the new chain.
        AbsoluteBlockHeight ->
        MVR finconf ()
    loadLoop (SomeProtocolVersion (spv :: SProtocolVersion pv)) activateLast genIndex genHeight = do
        mvr <- ask
        let Callbacks{..} = mvCallbacks mvr
        let MultiVersionConfiguration{..} = mvConfiguration mvr
        case consensusVersionFor spv of
            ConsensusV0 -> do
                r <-
                    mvrLogIO $
                        Skov.initialiseExistingSkov
                            ( Skov.SkovConfig @pv @finconf
                                ( globalStateConfig
                                    mvcStateConfig
                                    mvcRuntimeParameters
                                    genIndex
                                    genHeight
                                )
                                mvcFinalizationConfig
                                UpdateHandler
                            )
                case r of
                    Just (vc0Context, st) -> do
                        logEvent Runner LLTrace "Loaded configuration"
                        vc0State <- liftIO $ newIORef st
                        let vc0Shutdown = Skov.shutdownSkov vc0Context =<< liftIO (readIORef vc0State)
                        let newEConfig :: VersionedConfigurationV0 finconf pv
                            newEConfig =
                                VersionedConfigurationV0
                                    { vc0Index = genIndex,
                                      vc0GenesisHeight = genHeight,
                                      ..
                                    }
                        liftIO $ do
                            oldVersions <- readIORef (mvVersions mvr)
                            writeIORef (mvVersions mvr) (oldVersions `Vec.snoc` newVersionV0 newEConfig)
                        let getCurrentGenesisAndHeight ::
                                VersionedSkovV0M
                                    finconf
                                    pv
                                    (BlockHash, AbsoluteBlockHeight, Maybe SomeProtocolVersion)
                            getCurrentGenesisAndHeight = liftSkov $ do
                                currentGenesis <- Skov.getGenesisData
                                lfHeight <- getLastFinalizedHeight
                                nextPV <- getNextProtocolVersion
                                return
                                    ( _gcCurrentHash currentGenesis,
                                      localToAbsoluteBlockHeight genHeight lfHeight,
                                      nextPV
                                    )
                        (genesisHash, lastFinalizedHeight, nextPV) <-
                            Skov.evalSkovT getCurrentGenesisAndHeight (mvrSkovHandlers newEConfig mvr) vc0Context st
                        liftIO $ notifyRegenesis (Just genesisHash)
                        logEvent Runner LLTrace "Load configuration done"
                        let activateThis = do
                                activateConfiguration (newVersionV0 newEConfig)
                                liftSkovV0Update newEConfig checkForProtocolUpdate
                        case nextPV of
                            Nothing -> do
                                -- This is still the current configuration (i.e. no protocol update
                                -- has occurred), so activate it.
                                activateThis
                            Just nextSPV -> do
                                -- A protocol update has occurred for this configuration, so
                                -- continue to the next iteration. If the state for the next
                                -- configuration is missing, 'activateThis' is called which will
                                -- activate the configuration and trigger the protocol update.
                                loadLoop nextSPV activateThis (genIndex + 1) (fromIntegral lastFinalizedHeight + 1)
                    Nothing -> activateLast
            ConsensusV1 -> do
                let !handlers = skovV1Handlers genIndex genHeight
                -- We need an "unlift" operation to run a SkovV1 transaction in an IO context.
                -- This is used for implementing timer handlers.
                -- The "unlift" is implemented by using an 'MVar' to store the configuration in,
                -- that will be set after initialization.
                configRef <- liftIO newEmptyMVar
                let unliftSkov :: forall b. VersionedSkovV1M finconf pv b -> IO b
                    unliftSkov a = do
                        config <- readMVar configRef
                        runMVR (runSkovV1Transaction config a) mvr
                r <-
                    mvrLogIO $
                        SkovV1.initialiseExistingSkovV1
                            (SkovV1.BakerContext (bakerIdentity <$> mvBaker mvr))
                            -- Handler context
                            handlers
                            -- lift SkovV1T
                            unliftSkov
                            -- Global state config
                            (globalStateConfigV1 mvcStateConfig mvcRuntimeParameters genIndex)
                case r of
                    Just SkovV1.ExistingSkov{..} -> do
                        logEvent Runner LLTrace "Loaded configuration"
                        vc1State <- liftIO $ newIORef esState
                        let vc1Shutdown = SkovV1.shutdownSkovV1 esContext
                        let newEConfig :: VersionedConfigurationV1 finconf pv
                            newEConfig =
                                VersionedConfigurationV1
                                    { vc1Index = genIndex,
                                      vc1GenesisHeight = genHeight,
                                      vc1Context = esContext,
                                      ..
                                    }
                        liftIO $ do
                            putMVar configRef newEConfig
                            oldVersions <- readIORef (mvVersions mvr)
                            writeIORef (mvVersions mvr) (oldVersions `Vec.snoc` newVersionV1 newEConfig)
                        liftIO $ notifyRegenesis (Just esGenesisHash)
                        logEvent Runner LLTrace "Load configuration done"
                        let activateThis = do
                                activateConfiguration (newVersionV1 newEConfig)
                                -- FIXME: Support protocol updates. Issue #825
                                return ()
                        case esNextProtocolVersion of
                            Nothing -> do
                                -- This is still the current configuration (i.e. no protocol update
                                -- has occurred), so activate it.
                                activateThis
                                -- Start the consensus
                                liftSkovV1Update newEConfig KonsensusV1.startEvents
                            Just nextSPV -> do
                                -- A protocol update has occurred for this configuration, so
                                -- continue to the next iteration. If the state for the next
                                -- configuration is missing, 'activateThis' is called which will
                                -- activate the configuration and trigger the protocol update.
                                loadLoop
                                    nextSPV
                                    activateThis
                                    (genIndex + 1)
                                    (fromIntegral esLastFinalizedHeight + 1)
                    Nothing -> activateLast

    activateGenesis :: MVR finconf ()
    activateGenesis = do
        logEvent Runner LLTrace "Attempting to decode genesis"
        case genesis of
            Left genBS -> case runGet getPVGenesisData genBS of
                Left err -> do
                    logEvent Runner LLError $ "Failed to decode genesis data: " ++ err
                    throwM (InvalidGenesisData err)
                Right gd -> newGenesis gd 0
            Right gd -> newGenesis gd 0

-- |Start a thread to periodically purge uncommitted transactions.
-- This is only intended to be called once, during 'makeMultiVersionRunner'.
-- Calling it a second time is expected to deadlock.
--
-- If the specified delay is less than or equal to zero, no purging thread
-- will be started.
startTransactionPurgingThread :: MultiVersionRunner finconf -> IO ()
startTransactionPurgingThread mvr@MultiVersionRunner{..} =
    when (delay > 0) $
        putMVar mvTransactionPurgingThread <=< forkIO $
            ( do
                mvLog Runner LLInfo "Transaction purging thread started."
                forever $ do
                    threadDelay delay
                    mvLog Runner LLTrace "Purging transactions."
                    (withWriteLockIO mvr :: IO () -> IO ()) $ do
                        versions <- readIORef mvVersions
                        case Vec.last versions of
                            EVersionedConfigurationV0 vc ->
                                runMVR (liftSkovV0Update vc Skov.purgeTransactions) mvr
                            EVersionedConfigurationV1 vc ->
                                runMVR (liftSkovV1Update vc KonsensusV1.purgeTransactions) mvr
            )
                `finally` mvLog Runner LLInfo "Transaction purging thread stopped."
  where
    delay = rpTransactionsPurgingDelay (mvcRuntimeParameters mvConfiguration) * 1_000_000

-- |Start a baker thread associated with a 'MultiVersionRunner'.
-- This will only succeed if the runner was initialised with baker credentials.
startBaker :: MultiVersionRunner finconf -> IO ()
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
                versions <- readIORef mvVersions
                case Vec.last versions of
                    EVersionedConfigurationV0 vc -> do
                        -- If the genesis index has changed, we reset the slot counter to 0, since this
                        -- is a different chain.
                        let nextSlot = if vc0Index vc == lastGenIndex then slot else 0
                        (vc0Index vc,) . Just
                            <$> runMVR (liftSkovV0Update vc (tryBake bakerIdentity nextSlot)) mvr
                    EVersionedConfigurationV1 vc -> do
                        return (vc1Index vc, Nothing)
        case res of
            Just (BakeSuccess slot' block) -> do
                broadcastBlock mvCallbacks genIndex block
                bakerLoop genIndex slot'
            Just (BakeWaitUntil slot' ts) -> do
                now <- utcTimeToTimestamp <$> currentTime
                when (now < ts) $ threadDelay $ fromIntegral (tsMillis (ts - now)) * 1_000
                bakerLoop genIndex slot'
            Just BakeShutdown -> do
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
            Nothing -> do
                mvLog Runner LLInfo "Baking thread is not required and will shut down."
                void $ takeMVar bakerThread

-- |Stop the baker thread associated with a 'MultiVersionRunner'.
stopBaker :: MultiVersionRunner finconf -> IO ()
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
stopImportingBlocks :: MultiVersionRunner finconf -> IO ()
stopImportingBlocks MultiVersionRunner{..} = mask_ $ do
    writeIORef mvShouldStopImportingBlocks True

shutdownMultiVersionRunner :: MultiVersionRunner finconf -> IO ()
shutdownMultiVersionRunner MultiVersionRunner{..} = mask_ $ do
    -- Kill the baker thread, if any.
    forM_ mvBaker $ \Baker{..} -> tryTakeMVar bakerThread >>= mapM_ killThread
    -- Kill the transaction purging thread, if any.
    tryTakeMVar mvTransactionPurgingThread >>= mapM_ killThread
    -- Acquire the write lock. This prevents further updates, as they will block.
    takeMVar mvWriteLock
    versions <- readIORef mvVersions
    runLoggerT (forM_ versions evcShutdown) mvLog

-- |Lift a version-0 consensus skov action to the 'MVR' monad, running it on a
-- particular 'VersionedConfigurationV0'. Note that this does not
-- acquire the write lock: the caller must ensure that the lock
-- is held.
liftSkovV0Update ::
    VersionedConfigurationV0 finconf pv ->
    VersionedSkovV0M finconf pv a ->
    MVR finconf a
liftSkovV0Update vc a = MVR $ \mvr -> do
    oldState <- readIORef (vc0State vc)
    (res, newState) <- runMVR (Skov.runSkovT a (mvrSkovHandlers vc mvr) (vc0Context vc) oldState) mvr
    writeIORef (vc0State vc) $! newState
    return $! res

-- |Lift a version-1 consensus skov action to the 'MVR' monad, running it on a particular
-- 'VersionedConfigurationV1'. Note that this does not acquire the write lock: the caller must
-- ensure that the lock is held.
liftSkovV1Update ::
    VersionedConfigurationV1 finconf pv ->
    VersionedSkovV1M finconf pv a ->
    MVR finconf a
liftSkovV1Update vc a = MVR $ \mvr -> do
    oldState <- readIORef (vc1State vc)
    (res, newState) <- runMVR (SkovV1.runSkovT a (vc1Context vc) oldState) mvr
    writeIORef (vc1State vc) $! newState
    return $! res

-- |Run a version-0 consensus transaction that may affect the state.
-- This acquires the write lock for the duration of the operation.
-- If the action throws an exception, the state will not be updated,
-- but the lock is guaranteed to be released.
runSkovV0Transaction ::
    VersionedConfigurationV0 finconf pv ->
    VersionedSkovV0M finconf pv a ->
    MVR finconf a
runSkovV0Transaction vc a = withWriteLock $ liftSkovV0Update vc a

-- |Run a version-1 consensus transaction that may affect the state.
-- This acquires the write lock for the duration of the operation.
-- If the action throws an exception, the state will not be updated,
-- but the lock is guaranteed to be released.
runSkovV1Transaction ::
    VersionedConfigurationV1 finconf pv ->
    VersionedSkovV1M finconf pv a ->
    MVR finconf a
runSkovV1Transaction vc a = withWriteLock $ liftSkovV1Update vc a

-- |An instance of 'SkovHandlers' for running operations on a
-- 'VersionedConfigurationV0' within a 'MultiVersionRunner'.
mvrSkovHandlers ::
    VersionedConfigurationV0 finconf pv ->
    MultiVersionRunner finconf ->
    Skov.SkovHandlers pv ThreadTimer (Skov.SkovConfig pv finconf UpdateHandler) (MVR finconf)
mvrSkovHandlers vc mvr@MultiVersionRunner{mvCallbacks = Callbacks{..}} =
    Skov.SkovHandlers
        { shBroadcastFinalizationMessage =
            liftIO . broadcastFinalizationMessage (vc0Index vc) . runPut . putVersionedFPMV0,
          shOnTimeout =
            \timeout a ->
                liftIO $
                    makeThreadTimer timeout $
                        void $
                            runMVR (runSkovV0Transaction vc a) mvr,
          shCancelTimer = liftIO . cancelThreadTimer,
          shPendingLive = bufferedSendCatchUpStatus
        }

-- |Send out a catch-up status message within at most 30 seconds (subject to scheduling).
-- This uses the following principles:
--
--   * If we are not already waiting to send a catch-up status message, start
--     waiting for at least 5 seconds and at most 30 seconds.
--   * If we are already waiting to send a catch-up status message, update the
--     soonest time to send that to be 5 seconds from now, or the latest possible
--     time, whichever is sooner.
--   * If the buffer has been shut down (i.e. the entire consensus is being torn
--     down) then do nothing.
bufferedSendCatchUpStatus ::
    MVR finconf ()
bufferedSendCatchUpStatus = MVR $ \mvr@MultiVersionRunner{..} -> do
    now <- currentTime
    let soonest = addUTCTime 5 now
    mask_ $
        takeMVar mvCatchUpStatusBuffer >>= \case
            BufferEmpty -> do
                putMVar mvCatchUpStatusBuffer $
                    BufferPending
                        { cusbsSoonest = soonest,
                          cusbsLatest = addUTCTime 30 now
                        }
                void $ forkIO $ waitLoop mvr soonest
            BufferPending{..} ->
                putMVar mvCatchUpStatusBuffer $
                    BufferPending
                        { cusbsSoonest = min soonest cusbsLatest,
                          ..
                        }
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
                            restore $ runMVR sendCatchUpStatus mvr
                        else do
                            putMVar mvCatchUpStatusBuffer v
                            restore $ waitLoop mvr cusbsSoonest
                BufferShutdown -> return ()

-- |Send a catch-up status message for the latest genesis index.
sendCatchUpStatus :: MVR finconf ()
sendCatchUpStatus = MVR $ \mvr@MultiVersionRunner{..} -> do
    vvec <- readIORef mvVersions
    case Vec.last vvec of
        EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv) -> do
            st <- readIORef (vc0State vc)
            cus <-
                runMVR
                    ( Skov.evalSkovT @_ @pv
                        (Skov.getCatchUpStatus False)
                        (mvrSkovHandlers vc mvr)
                        (vc0Context vc)
                        st
                    )
                    mvr
            notifyCatchUpStatus mvCallbacks (vc0Index vc) $
                encode $
                    VersionedCatchUpStatusV0 cus
        EVersionedConfigurationV1 (vc :: VersionedConfigurationV1 finconf pv) -> do
            st <- readIORef (vc1State vc)
            let cus = KonsensusV1.makeCatchUpStatusMessage $ SkovV1._v1sSkovData st
            notifyCatchUpStatus mvCallbacks (vc1Index vc) $
                encode $
                    VersionedCatchUpStatusV1 cus

-- |Perform an operation with the latest chain version, as long as
-- it is at the expected genesis index. The function returns a tuple consisting
-- of a 'Skov.UpdateResult and an optional 'a'.
-- This variant is used if the (typically) underlying 'Skov' action also returns some value 'a' in
-- addition to the 'Skov.UpdateResult'.
-- If the genesis index is for an older version, this returns 'ResultConsensusShutDown'
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
    (EVersionedConfiguration finconf -> MVR finconf (Skov.UpdateResult, Maybe a)) ->
    MVR finconf (Skov.UpdateResult, Maybe a)
withLatestExpectedVersion gi a = do
    vvec <- liftIO . readIORef =<< asks mvVersions
    -- Length is an Int and GenesisIndex is a Word32.
    -- Assuming a 64-bit system, there is no risk of over/underflow.
    case compare (Vec.length vvec - 1) (fromIntegral gi) of
        EQ -> a (Vec.last vvec)
        LT -> return (Skov.ResultInvalidGenesisIndex, Nothing)
        GT -> return (Skov.ResultConsensusShutDown, Nothing)

-- |Performs an operation with the latest chain version via 'withLatestExectedVersion'.
-- This variant throws away the second component of the tuple result returned from 'withLatestExectedVersion',
-- i.e. this version of 'withLatestExpectedVersion' only yields the 'Skov.UpdateResult'
withLatestExpectedVersion_ ::
    GenesisIndex ->
    (EVersionedConfiguration finconf -> MVR finconf Skov.UpdateResult) ->
    MVR finconf Skov.UpdateResult
withLatestExpectedVersion_ gi a = fst <$> withLatestExpectedVersion gi (fmap (,Nothing) <$> a)

-- |A continuation for executing a block that has been received
-- and verified.
newtype ExecuteBlock = ExecuteBlock {runBlock :: IO Skov.UpdateResult}

-- |Deserialize and receive a block at a given genesis index.
-- Return a continuation ('Maybe ExecuteBlock') only if the 'Skov.UpdateResult' is 'ResultSuccess'.
--
-- An initial write lock is acquired and released for receiving the block.
-- This is used for marking blocks as pending or even dead
--
-- The continuation for executing the block is running a skov transaction thus
-- the continuation when called is holding the write lock when executing the block
-- and releasing it again when it is finished.
--
-- The continuation is expected to be invoked via 'executeBlock'.
receiveBlock ::
    GenesisIndex ->
    ByteString ->
    MVR finconf (Skov.UpdateResult, Maybe ExecuteBlock)
receiveBlock gi blockBS = withLatestExpectedVersion gi $ \case
    (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) -> do
        MVR $ \mvr -> do
            now <- currentTime
            case deserializeExactVersionedPendingBlock (protocolVersion @pv) blockBS now of
                Left err -> do
                    mvLog mvr Runner LLDebug err
                    return (Skov.ResultSerializationFail, Nothing)
                Right block -> do
                    (updateResult, mVerifiedPendingBlock) <- runMVR (runSkovV0Transaction vc (Skov.receiveBlock block)) mvr
                    case mVerifiedPendingBlock of
                        Nothing -> return (updateResult, Nothing)
                        Just verifiedPendingBlock -> do
                            let exec = do
                                    runSkovV0Transaction vc (Skov.executeBlock verifiedPendingBlock)
                            let cont = ExecuteBlock $ runMVR exec mvr
                            return (updateResult, Just cont)
    (EVersionedConfigurationV1 (vc :: VersionedConfigurationV1 finconf pv)) -> do
        MVR $ \mvr -> do
            now <- currentTime
            case SkovV1.deserializeExactVersionedPendingBlock (protocolVersion @pv) blockBS now of
                Left err -> do
                    mvLog mvr Runner LLDebug err
                    return (Skov.ResultSerializationFail, Nothing)
                Right block -> do
                    blockResult <- runMVR (runSkovV1Transaction vc (SkovV1.uponReceivingBlock block)) mvr
                    case blockResult of
                        SkovV1.BlockResultSuccess vb -> do
                            let exec = do
                                    runSkovV1Transaction vc (SkovV1.executeBlock vb)
                                    return Skov.ResultSuccess
                            let cont = ExecuteBlock $ runMVR exec mvr
                            return (Skov.ResultSuccess, Just cont)
                        SkovV1.BlockResultDoubleSign vb -> do
                            runMVR (runSkovV1Transaction vc (SkovV1.executeBlock vb)) mvr
                            return (Skov.ResultDoubleSign, Nothing)
                        SkovV1.BlockResultInvalid -> return (Skov.ResultInvalid, Nothing)
                        SkovV1.BlockResultStale -> return (Skov.ResultStale, Nothing)
                        SkovV1.BlockResultPending -> return (Skov.ResultPendingBlock, Nothing)
                        SkovV1.BlockResultEarly -> return (Skov.ResultEarlyBlock, Nothing)
                        SkovV1.BlockResultDuplicate -> return (Skov.ResultDuplicate, Nothing)

-- |Invoke the continuation yielded by 'receiveBlock'.
-- The continuation performs a transaction which will acquire the write lock
-- before trying to add the block to the tree and release the lock again afterwards.
executeBlock :: ExecuteBlock -> MVR finconf Skov.UpdateResult
executeBlock = liftIO . runBlock

-- |Deserialize and receive a finalization message at a given genesis index.
receiveFinalizationMessage :: GenesisIndex -> ByteString -> MVR finconf Skov.UpdateResult
receiveFinalizationMessage gi finMsgBS = withLatestExpectedVersion_ gi $ \case
    (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) ->
        case runGet getExactVersionedFPM finMsgBS of
            Left err -> do
                logEvent Runner LLDebug $ "Could not deserialize finalization message: " ++ err
                return Skov.ResultSerializationFail
            Right finMsg -> runSkovV0Transaction vc (finalizationReceiveMessage finMsg)
    (EVersionedConfigurationV1 (vc :: VersionedConfigurationV1 finconf pv)) ->
        case decode finMsgBS of
            Left err -> do
                logEvent Runner LLDebug $ "Could not deserialize finalization message: " ++ err
                return Skov.ResultSerializationFail
            Right finMsg -> do
                let receive = liftSkovV1Update vc $ do
                        res <- KonsensusV1.receiveFinalizationMessage finMsg
                        return $ case res of
                            Left leftRes -> (leftRes, Nothing)
                            Right cont -> (Skov.ResultSuccess, Just cont)
                    followup = liftSkovV1Update vc
                -- We spawn a thread to perform the follow-up so that the P2P layer can immediately
                -- relay the message, since the follow-up action can be time consuming (including
                -- finalizing blocks and baking a new block).
                withWriteLockMaybeFork receive followup

-- |Deserialize and receive a finalization record at a given genesis index.
receiveFinalizationRecord :: GenesisIndex -> ByteString -> MVR finconf Skov.UpdateResult
receiveFinalizationRecord gi finRecBS = withLatestExpectedVersion_ gi $ \case
    (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) ->
        case runGet getExactVersionedFinalizationRecord finRecBS of
            Left err -> do
                logEvent Runner LLDebug $ "Could not deserialized finalization record: " ++ err
                return Skov.ResultSerializationFail
            Right finRec -> runSkovV0Transaction vc (finalizationReceiveRecord False finRec)
    (EVersionedConfigurationV1 _) -> do
        logEvent Runner LLDebug "Unexpected finalization record event in consensus version 1."
        return Skov.ResultInvalid

-- |Configuration parameters for handling receipt of a catch-up status message.
data CatchUpConfiguration = CatchUpConfiguration
    { -- |Maximum number of block and finalization record messages to send in response.
      catchUpMessageLimit :: Int,
      -- |Callback for sending blocks, finalization records and the response catch up status
      -- message.
      catchUpCallback :: Skov.MessageType -> ByteString -> IO ()
    }

-- |Handle receipt of a catch-up message.
receiveCatchUpStatus ::
    forall finconf.
    GenesisIndex ->
    ByteString ->
    CatchUpConfiguration ->
    MVR finconf Skov.UpdateResult
receiveCatchUpStatus gi catchUpBS cuConfig@CatchUpConfiguration{..} =
    case runGet getVersionedCatchUpStatus catchUpBS of
        Left err -> do
            logEvent Runner LLDebug $ "Could not deserialize catch-up status message: " ++ err
            return Skov.ResultSerializationFail
        Right vcatchUp -> do
            logEvent Runner LLDebug $ "Catch-up status message deserialized."
            vvec <- liftIO . readIORef =<< asks mvVersions
            case vvec Vec.!? fromIntegral gi of
                -- If we have a (re)genesis as the given index then...
                Just (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) ->
                    case vcatchUp of
                        VersionedCatchUpStatusNoGenesis -> return Skov.ResultSuccess
                        VersionedCatchUpStatusV0 catchUp -> MVR $ \mvr -> do
                            st <- readIORef (vc0State vc)
                            -- Evaluate handleCatchUpStatus to determine the response.
                            -- Note that this should not perform a state update, so there is no need to
                            -- acquire the write lock, or to store the resulting state.
                            (mmsgs, res) <-
                                runMVR
                                    ( Skov.evalSkovT @_ @pv
                                        ( Skov.handleCatchUpStatus @(VersionedSkovV0M finconf pv)
                                            catchUp
                                            catchUpMessageLimit
                                        )
                                        (mvrSkovHandlers vc mvr)
                                        (vc0Context vc)
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
                                catchUpCallback Skov.MessageCatchUpStatus $
                                    runPut $
                                        putVersionedCatchUpStatus $
                                            VersionedCatchUpStatusV0 cusResp
                            return res
                        _ -> do
                            logEvent Runner LLDebug $
                                "Unsupported catch-up status message at consensus v0:" ++ show vcatchUp
                            return Skov.ResultInvalid
                Just (EVersionedConfigurationV1 vc) -> case vcatchUp of
                    VersionedCatchUpStatusNoGenesis ->
                        return Skov.ResultSuccess
                    VersionedCatchUpStatusV1 catchUpMsg ->
                        handleCatchUpStatusV1 cuConfig vc catchUpMsg
                    _ -> do
                        logEvent Runner LLDebug $
                            "Unsupported catch-up status message at consensus v1:" ++ show vcatchUp
                        return Skov.ResultInvalid
                -- If we have no regenesis at the given index then...
                Nothing
                    | isCatchUpRequest vcatchUp -> do
                        -- if it is a request, inform the peer we have no genesis and queue to catch up
                        liftIO $
                            catchUpCallback Skov.MessageCatchUpStatus $
                                encode VersionedCatchUpStatusNoGenesis
                        return Skov.ResultContinueCatchUp
                    | VersionedCatchUpStatusNoGenesis <- vcatchUp -> do
                        -- if the peer (also!) has no genesis at this index, we do not reply
                        -- or initiate catch-up
                        return Skov.ResultSuccess
                    | otherwise -> do
                        -- otherwise, the peer is ahead so we should initiate catch-up
                        return Skov.ResultPendingBlock

handleCatchUpStatusV1 ::
    forall finconf pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    CatchUpConfiguration ->
    VersionedConfigurationV1 finconf pv ->
    KonsensusV1.CatchUpMessage ->
    MVR finconf Skov.UpdateResult
handleCatchUpStatusV1 CatchUpConfiguration{..} vc = handleMsg
  where
    handleMsg KonsensusV1.CatchUpStatusMessage{..} = do
        st <- liftIO $ readIORef $ vc1State vc
        checkShouldCatchUp cumStatus st False
    handleMsg KonsensusV1.CatchUpRequestMessage{..} = do
        startState <- liftIO $ readIORef $ vc1State vc
        x <- runLowLevel $ KonsensusV1.handleCatchUpRequest cumStatus (SkovV1._v1sSkovData startState)
        let blockLoop !count (KonsensusV1.CatchUpPartialResponseDone terminal) = do
                return (count, Present terminal)
            blockLoop !count KonsensusV1.CatchUpPartialResponseBlock{..} = do
                liftIO $
                    catchUpCallback Skov.MessageBlock $
                        runPut $
                            KonsensusV1.putSignedBlock cuprNextBlock
                if count >= catchUpMessageLimit
                    then do
                        (count,) <$> runLowLevel cuprFinish
                    else do
                        next <- runLowLevel cuprContinue
                        blockLoop (count + 1) next
        (count, terminal) <- blockLoop 0 x
        logEvent Runner LLTrace $
            "Sent " ++ show count ++ " blocks in response to a finalization request."
        -- We compute the catch-up status with respect to the state now (after sending the blocks)
        -- because we could have new information since startState that might have been sent to the
        -- peer while it was receiving our catch-up blocks. In that event, the peer might discard
        -- the information. By using the more recent state, we allow the peer to determine if
        -- any such blocks now need to be caught up.
        endState <- liftIO $ readIORef $ vc1State vc
        let status = KonsensusV1.makeCatchUpStatus (SkovV1._v1sSkovData endState)
        liftIO $
            catchUpCallback Skov.MessageCatchUpStatus $
                encode $
                    VersionedCatchUpStatusV1 $
                        KonsensusV1.CatchUpResponseMessage
                            { cumStatus = status,
                              cumTerminalData = terminal
                            }
        checkShouldCatchUp cumStatus endState False
    handleMsg KonsensusV1.CatchUpResponseMessage{cumTerminalData = Present terminal, ..} = do
        res <- runSkovV1Transaction vc $ do
            KonsensusV1.processCatchUpTerminalData terminal
        -- If we made progress as a result of processing the terminal data, then we should send a
        -- catch-up status to our non-pending peers in case they need to catch up.
        when (KonsensusV1.tdrProgress res) bufferedSendCatchUpStatus
        case res of
            KonsensusV1.TerminalDataResultValid{} -> do
                st <- liftIO $ readIORef $ vc1State vc
                checkShouldCatchUp cumStatus st True
            KonsensusV1.TerminalDataResultInvalid{} -> do
                return Skov.ResultInvalid
    handleMsg KonsensusV1.CatchUpResponseMessage{cumTerminalData = Absent, ..} = do
        st <- liftIO $ readIORef $ vc1State vc
        checkShouldCatchUp cumStatus st True
    runLowLevel ::
        LowLevelDB.DiskLLDBM pv (ReaderT (SkovV1.SkovV1Context pv (MVR finconf)) (MVR finconf)) a ->
        MVR finconf a
    runLowLevel a = runReaderT (LowLevelDB.runDiskLLDBM a) (vc1Context vc)
    checkShouldCatchUp status st isResponse = do
        shouldCatchUp <-
            runLowLevel $
                KonsensusV1.isCatchUpRequired status (SkovV1._v1sSkovData st)
        return $!
            if shouldCatchUp
                then if isResponse then Skov.ResultPendingBlock else Skov.ResultContinueCatchUp
                else Skov.ResultSuccess

-- |Get the catch-up status for the current version of the chain.  This returns the current
-- genesis index, as well as the catch-up request message serialized with its version.
getCatchUpRequest :: forall finconf. MVR finconf (GenesisIndex, LBS.ByteString)
getCatchUpRequest = do
    mvr <- ask
    vvec <- liftIO $ readIORef $ mvVersions mvr
    case Vec.last vvec of
        (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) -> do
            st <- liftIO $ readIORef $ vc0State vc
            cus <- Skov.evalSkovT (Skov.getCatchUpStatus @(VersionedSkovV0M _ pv) True) (mvrSkovHandlers vc mvr) (vc0Context vc) st
            return (vc0Index vc, encodeLazy $ VersionedCatchUpStatusV0 cus)
        (EVersionedConfigurationV1 vc) -> do
            st <- liftIO $ readIORef $ vc1State vc
            let cus = KonsensusV1.makeCatchUpRequestMessage $ SkovV1._v1sSkovData st
            return (vc1Index vc, encodeLazy $ VersionedCatchUpStatusV1 cus)

-- |Deserialize and receive a transaction.  The transaction is passed to
-- the current version of the chain.
--
-- Currently, the 'BlockItem' type is common among all protocol versions.
-- We deserialize it using the current protocol version.
-- We then (pre)verify the transaction in a snapshot of the state.
-- If the verification is successful, we take the write lock.
-- In principle, a protocol update could have occurred since verifying the transaction.
-- If none has occurred, we call 'Skov.addPreverifiedTransaction' to add the transaction.
-- However, if a protocol update has occurred, we instead call 'Skov.receiveTransaction', which
-- re-does the transaction verification. While in practice we could probably still use the known
-- verification result, we opt not to on the basis that in future we may need to perform non-trivial
-- migration on the verification result to do so correctly.
--
-- We rely on two principles to ensure that it is OK to call 'Skov.receiveTransaction' even when
-- the protocol version has changed since the 'BlockItem' was deserialized:
--
-- 1. 'Skov.receiveTransaction' must gracefully handle block items from legacy protocol versions.
--
-- 2. A transaction cannot be deserialized to two different block items in different protocol
--    versions.
--
-- The return value is a pair of potentially the hash of the transaction, and
-- the result of the update. The hash is present unless the transaction could
-- not be deserialized.
receiveTransaction :: forall finconf. ByteString -> MVR finconf (Maybe TransactionHash, Skov.UpdateResult)
receiveTransaction transactionBS = do
    now <- utcTimeToTransactionTime <$> currentTime
    mvr <- ask
    vvec <- liftIO $ readIORef $ mvVersions mvr
    case Vec.last vvec of
        (EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv)) ->
            withDeserializedTransaction (protocolVersion @pv) now $ \transaction -> do
                st <- liftIO $ readIORef $ vc0State vc
                (known, verRes) <-
                    Skov.evalSkovT @_ @pv
                        (Skov.preverifyTransaction transaction)
                        (mvrSkovHandlers vc mvr)
                        (vc0Context vc)
                        st
                if known
                    then return Skov.ResultDuplicate
                    else case verRes of
                        TVer.Ok okRes -> withWriteLock $ do
                            vvec' <- liftIO $ readIORef $ mvVersions mvr
                            if Vec.length vvec == Vec.length vvec'
                                then do
                                    -- There hasn't been a protocol update since we did
                                    -- the preverification, so we add the transaction
                                    -- with the verification result.
                                    liftSkovV0Update vc $
                                        Skov.addPreverifiedTransaction transaction okRes
                                else do
                                    -- There HAS been a protocol update since we did the
                                    -- preverification, so we call 'receiveTransaction',
                                    -- which re-does the verification.
                                    receiveUnverified (Vec.last vvec') transaction
                        _ -> return $! Skov.transactionVerificationResultToUpdateResult verRes
        (EVersionedConfigurationV1 (vc :: VersionedConfigurationV1 finconf pv)) ->
            withDeserializedTransaction (protocolVersion @pv) now $ \transaction -> do
                st <- liftIO $ readIORef $ vc1State vc
                (known, verRes) <-
                    SkovV1.evalSkovT (SkovV1.preverifyTransaction transaction) (vc1Context vc) st
                if known
                    then return Skov.ResultDuplicate
                    else case verRes of
                        TVer.Ok okRes -> withWriteLock $ do
                            vvec' <- liftIO $ readIORef $ mvVersions mvr
                            if Vec.length vvec == Vec.length vvec'
                                then do
                                    -- No protocol update has occurred.
                                    liftSkovV1Update vc $
                                        KonsensusV1.addTransactionResult
                                            <$> SkovV1.addPreverifiedTransaction transaction okRes
                                else do
                                    -- A protocol update has occurred.
                                    receiveUnverified (Vec.last vvec') transaction
                        _ -> return $! Skov.transactionVerificationResultToUpdateResult verRes
  where
    withDeserializedTransaction ::
        SProtocolVersion spv ->
        TransactionTime ->
        (BlockItem -> MVR finconf Skov.UpdateResult) ->
        MVR finconf (Maybe TransactionHash, Skov.UpdateResult)
    withDeserializedTransaction spv now cont =
        case runGet (getExactVersionedBlockItem spv now) transactionBS of
            Left err -> do
                logEvent Runner LLDebug err
                return (Nothing, Skov.ResultSerializationFail)
            Right transaction -> (Just (wmdHash transaction),) <$> cont transaction
    receiveUnverified (EVersionedConfigurationV0 vc') transaction =
        liftSkovV0Update vc' $
            Skov.receiveTransaction transaction
    receiveUnverified (EVersionedConfigurationV1 vc') transaction =
        liftSkovV1Update vc' $
            KonsensusV1.addTransactionResult
                <$> SkovV1.processBlockItem transaction

-- |Receive and execute the block immediately.
-- Used for importing blocks i.e. out of band catchup.
receiveExecuteBlock :: GenesisIndex -> ByteString -> MVR finconf Skov.UpdateResult
receiveExecuteBlock gi blockBS = withLatestExpectedVersion_ gi $ \case
    EVersionedConfigurationV0 (vc :: VersionedConfigurationV0 finconf pv) -> do
        now <- currentTime
        case deserializeExactVersionedPendingBlock (protocolVersion @pv) blockBS now of
            Left err -> do
                logEvent Runner LLDebug err
                return Skov.ResultSerializationFail
            Right block -> runSkovV0Transaction vc (Skov.receiveExecuteBlock block)
    EVersionedConfigurationV1 (vc :: VersionedConfigurationV1 finconf pv) -> do
        now <- currentTime
        case SkovV1.deserializeExactVersionedPendingBlock (protocolVersion @pv) blockBS now of
            Left err -> do
                logEvent Runner LLDebug err
                return Skov.ResultSerializationFail
            Right block ->
                runSkovV1Transaction vc $ do
                    res <- SkovV1.uponReceivingBlock block
                    case res of
                        SkovV1.BlockResultSuccess vb ->
                            Skov.ResultSuccess <$ SkovV1.executeBlock vb
                        SkovV1.BlockResultDoubleSign vb ->
                            Skov.ResultSuccess <$ SkovV1.executeBlock vb
                        SkovV1.BlockResultInvalid -> return Skov.ResultInvalid
                        SkovV1.BlockResultStale -> return Skov.ResultStale
                        SkovV1.BlockResultPending -> return Skov.ResultPendingBlock
                        SkovV1.BlockResultEarly -> return Skov.ResultEarlyBlock
                        SkovV1.BlockResultDuplicate -> return Skov.ResultDuplicate

-- |Import a block file for out-of-band catch-up.
importBlocks :: FilePath -> MVR finconf Skov.UpdateResult
importBlocks importFile = do
    vvec <- liftIO . readIORef =<< asks mvVersions
    -- Import starting from the genesis index of the latest consensus
    let genIndex = evcIndex (Vec.last vvec)
    res <- importBlocksV3 importFile genIndex doImport
    case res of
        Left ImportSerializationFail -> return Skov.ResultSerializationFail
        Left (ImportOtherError a) -> return a
        Right _ -> return Skov.ResultSuccess
  where
    doImport (ImportBlock _ gi bs) = do
        shouldStop <- liftIO . readIORef =<< asks mvShouldStopImportingBlocks
        -- Check if the import should be stopped.
        if shouldStop
            then return $ fixResult Skov.ResultConsensusShutDown
            else local disableBroadcastCallbacks $ fixResult <$> receiveExecuteBlock gi bs
    doImport (ImportFinalizationRecord _ gi bs) = local disableBroadcastCallbacks $ fixResult <$> receiveFinalizationRecord gi bs
    fixResult Skov.ResultSuccess = Right ()
    fixResult Skov.ResultDuplicate = Right ()
    fixResult Skov.ResultConsensusShutDown = Right ()
    fixResult e = Left (ImportOtherError e)
    -- Disable broadcast callbacks as the network layer is not started at the point of the out-of-band-catchup.
    disableBroadcastCallbacks mvr@MultiVersionRunner{..} = do
        mvr
            { mvCallbacks =
                mvCallbacks
                    { broadcastBlock = \_ _ -> return (),
                      broadcastFinalizationMessage = \_ _ -> return (),
                      broadcastFinalizationRecord = \_ _ -> return ()
                    }
            }
