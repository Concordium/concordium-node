{-# LANGUAGE
    ScopedTypeVariables,
    UndecidableInstances,
    CPP #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.ByteString as BS
import Data.Serialize
import Data.IORef
import Control.Monad.IO.Class
import Data.Time.Clock

import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.TreeState as TS

import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
import Concordium.Skov.Hooks
import Concordium.Skov.CatchUp (CatchUpStatus)
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.Getters


type SkovBlockPointer c = BlockPointer (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO)

data SimpleOutMessage c
    = SOMsgNewBlock (SkovBlockPointer c)
    | SOMsgFinalization FinalizationPseudoMessage
    | SOMsgFinalizationRecord FinalizationRecord

data SyncRunner c = SyncRunner {
    syncBakerIdentity :: BakerIdentity,
    syncState :: MVar (SkovState c),
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO,
    syncCallback :: SimpleOutMessage c -> IO (),
    syncFinalizationCatchUpActive :: MVar (Maybe (IORef Bool)),
    syncContext :: !(SkovContext c),
    syncHandlePendingLive :: IO ()
}

instance (SkovQueryConfigMonad c LogIO) => SkovStateQueryable (SyncRunner c) (SkovT () c LogIO) where
    runStateQuery sr a = do
        s <- readMVar (syncState sr)
        runLoggerT (evalSkovT a () (syncContext sr) s) (syncLogMethod sr)

bufferedHandlePendingLive :: IO () -> MVar (Maybe (UTCTime, UTCTime)) -> IO ()
bufferedHandlePendingLive hpl bufferMVar = do
        now <- currentTime
        takeMVar bufferMVar >>= \case
            Nothing -> do
                putMVar bufferMVar $ Just (addUTCTime 5 now, addUTCTime 30 now)
                void $ forkIO $ waitLoop (addUTCTime 5 now)
            Just (_, upper) -> putMVar bufferMVar $ Just (min (addUTCTime 5 now) upper, upper)
    where
        waitLoop till = do
            now <- currentTime
            let waitDurationMicros = truncate (diffUTCTime till now * 1e6)
            when (waitDurationMicros > 0) $ threadDelay waitDurationMicros
            takeMVar bufferMVar >>= \case
                Nothing -> putMVar bufferMVar Nothing
                v@(Just (lower, _)) -> do
                    now' <- currentTime
                    if now' >= lower then do
                        putMVar bufferMVar Nothing
                        hpl
                    else do
                        putMVar bufferMVar v
                        waitLoop lower
-- |Make a 'SyncRunner' without starting a baker thread.
makeSyncRunner :: (SkovConfiguration c, SkovQueryConfigMonad c LogIO) => LogMethod IO ->
                  BakerIdentity ->
                  c ->
                  (SimpleOutMessage c -> IO ()) ->
                  (CatchUpStatus -> IO ()) ->
                  IO (SyncRunner c)
makeSyncRunner syncLogMethod syncBakerIdentity config syncCallback cusCallback = do
        (syncContext, st0) <- initialiseSkov config
        syncState <- newMVar st0
        syncBakerThread <- newEmptyMVar
        syncFinalizationCatchUpActive <- newMVar Nothing
        pendingLiveMVar <- newMVar Nothing
        let
            syncHandlePendingLive = bufferedHandlePendingLive (getCatchUpStatus sr False >>= cusCallback) pendingLiveMVar
            sr = SyncRunner{..}
        return sr

-- |Run a computation, atomically using the state.  If the computation fails with an
-- exception, the state is restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar s -> LogMethod IO -> (s -> LogIO (a, s)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        tid <- myThreadId
        logm Runner LLTrace $ "Acquired consensus lock on thread " ++ show tid
        (ret, state') <- runLoggerT (a state0) logm
        putMVar mvState state'
        logm Runner LLTrace $ "Released consensus lock on thread " ++ show tid
        return ret


runSkovTransaction :: SyncRunner c -> SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO a -> IO a
{-# INLINE runSkovTransaction #-}
runSkovTransaction sr@SyncRunner{..} a = runWithStateLog syncState syncLogMethod (runSkovT a (syncSkovHandlers sr) syncContext)

syncSkovHandlers :: forall c. SyncRunner c -> SkovHandlers ThreadTimer c LogIO
syncSkovHandlers sr@SyncRunner{..} = handlers
    where
        handlers :: SkovHandlers ThreadTimer c LogIO
        handlers = SkovHandlers{..}
        shBroadcastFinalizationMessage = liftIO . syncCallback . SOMsgFinalization
        shBroadcastFinalizationRecord = liftIO . syncCallback . SOMsgFinalizationRecord
        shOnTimeout timeout a = liftIO $ makeThreadTimer timeout $ void $ runSkovTransaction sr a
        shCancelTimer = liftIO . cancelThreadTimer
        shPendingLive = liftIO syncHandlePendingLive

-- |Start the baker thread for a 'SyncRunner'.
startSyncRunner :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO,
    SkovQueryConfigMonad c LogIO
    ) => SyncRunner c -> IO ()
startSyncRunner sr@SyncRunner{..} = do
        let
            runBaker = (bakeLoop 0 `catch` \(e :: SomeException) -> (syncLogMethod Runner LLError ("Message loop exited with exception: " ++ show e)))
                            `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop lastSlot = do
                (mblock, sfs', curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <-
                                        if curSlot > lastSlot then
                                            bakeForSlot syncBakerIdentity curSlot
                                        else
                                            return Nothing
                                return (mblock, curSlot)
                        -- TODO: modify handlers to send out finalization messages AFTER any generated block
                        ((mblock, curSlot), sfs') <-
                          runSkovT bake (syncSkovHandlers sr) syncContext sfs
                        return ((mblock, sfs', curSlot), sfs'))
                forM_ mblock $ syncCallback . SOMsgNewBlock
                delay <- runLoggerT (evalSkovT (do
                    ttns <- timeUntilNextSlot
                    curSlot' <- getCurrentSlot
                    return $! if curSlot == curSlot' then truncate (ttns * 1e6) else 0) () syncContext sfs') syncLogMethod
                when (delay > 0) $ threadDelay delay
                bakeLoop curSlot
        _ <- forkIO $ do
            tid <- myThreadId
            putRes <- tryPutMVar syncBakerThread tid
            if putRes then do
                syncLogMethod Runner LLInfo "Starting baker thread"
                runBaker
            else
                syncLogMethod Runner LLInfo "Starting baker thread aborted: baker is already running"
        return ()

-- |Stop the baker thread for a 'SyncRunner'.
stopSyncRunner :: SyncRunner c -> IO ()
stopSyncRunner SyncRunner{..} = mask_ $ tryTakeMVar syncBakerThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd

-- |Stop any baker thread and dispose resources used by the 'SyncRunner'.
-- This should only be called once. Any subsequent call may diverge or throw an exception.
shutdownSyncRunner :: (SkovConfiguration c) => SyncRunner c -> IO ()
shutdownSyncRunner sr@SyncRunner{..} = do
        stopSyncRunner sr
        takeMVar syncState >>= shutdownSkov syncContext


syncReceiveBlock :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO)
    => SyncRunner c
    -> PendingBlock (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO)
    -> IO UpdateResult
syncReceiveBlock syncRunner block = do
  maxBlockSlot <- runSkovTransaction syncRunner (computeMaxBlockSlot)
  if blockSlot block > maxBlockSlot then return ResultBlockFromFuture
  else runSkovTransaction syncRunner (storeBlock block)
    where
      computeMaxBlockSlot = do
        fbt <- rpFutureBlockThreshold <$> TS.getRuntimeParameters
        currentSlot <- getCurrentSlot
        return ((fromIntegral fbt) + currentSlot)

syncReceiveTransaction :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO)
    => SyncRunner c -> Transaction -> IO UpdateResult
syncReceiveTransaction syncRunner trans = runSkovTransaction syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: (SkovFinalizationConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO)
    => SyncRunner c -> FinalizationPseudoMessage -> IO UpdateResult
syncReceiveFinalizationMessage syncRunner finMsg = runSkovTransaction syncRunner (receiveFinalizationPseudoMessage finMsg)

syncReceiveFinalizationRecord :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO)
    => SyncRunner c -> FinalizationRecord -> IO UpdateResult
syncReceiveFinalizationRecord syncRunner finRec = runSkovTransaction syncRunner (finalizeBlock finRec)

syncHookTransaction :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO, TransactionHookLenses (SkovState c))
    => SyncRunner c -> TransactionHash -> IO HookResult
syncHookTransaction syncRunner th = runSkovTransaction syncRunner (hookQueryTransaction th)


data SyncPassiveRunner c = SyncPassiveRunner {
    syncPState :: MVar (SkovState c),
    syncPLogMethod :: LogMethod IO,
    syncPContext :: !(SkovContext c),
    syncPHandlers :: !(SkovPassiveHandlers LogIO)
}

instance (SkovQueryConfigMonad c LogIO) => SkovStateQueryable (SyncPassiveRunner c) (SkovT () c LogIO) where
    runStateQuery sr a = do
        s <- readMVar (syncPState sr)
        runLoggerT (evalSkovT a () (syncPContext sr) s) (syncPLogMethod sr)


runSkovPassive :: SyncPassiveRunner c -> SkovT (SkovPassiveHandlers LogIO) c LogIO a -> IO a
{-# INLINE runSkovPassive #-}
runSkovPassive SyncPassiveRunner{..} a = runWithStateLog syncPState syncPLogMethod (runSkovT a syncPHandlers syncPContext)


-- |Make a 'SyncPassiveRunner', which does not support a baker thread.
makeSyncPassiveRunner :: (SkovConfiguration c, SkovQueryConfigMonad c LogIO) => LogMethod IO ->
                        c ->
                        (CatchUpStatus -> IO ()) ->
                        IO (SyncPassiveRunner c)
makeSyncPassiveRunner syncPLogMethod config cusCallback = do
        (syncPContext, st0) <- initialiseSkov config
        syncPState <- newMVar st0
        pendingLiveMVar <- newMVar Nothing
        let
            sphPendingLive = liftIO $ bufferedHandlePendingLive (getCatchUpStatus spr False >>= cusCallback) pendingLiveMVar
            syncPHandlers = SkovPassiveHandlers {..}
            spr = SyncPassiveRunner{..}
        return spr

shutdownSyncPassiveRunner :: SkovConfiguration c => SyncPassiveRunner c -> IO ()
shutdownSyncPassiveRunner SyncPassiveRunner{..} = takeMVar syncPState >>= shutdownSkov syncPContext

syncPassiveReceiveBlock :: (SkovConfigMonad (SkovPassiveHandlers LogIO) c LogIO) => SyncPassiveRunner c -> PendingBlock (SkovT (SkovPassiveHandlers LogIO) c LogIO) -> IO UpdateResult
syncPassiveReceiveBlock spr block = do
  maxBlockSlot <- runSkovPassive spr (computeMaxBlockSlot)
  if blockSlot block > maxBlockSlot then return ResultBlockFromFuture
  else runSkovPassive spr (storeBlock block)
    where
      computeMaxBlockSlot = do
        fbt <- rpFutureBlockThreshold <$> TS.getRuntimeParameters
        currentSlot <- getCurrentSlot
        return ((fromIntegral fbt) + currentSlot)

syncPassiveReceiveTransaction :: (SkovConfigMonad (SkovPassiveHandlers LogIO) c LogIO) => SyncPassiveRunner c -> Transaction -> IO UpdateResult
syncPassiveReceiveTransaction spr trans = runSkovPassive spr (receiveTransaction trans)

syncPassiveReceiveFinalizationRecord :: (SkovConfigMonad (SkovPassiveHandlers LogIO) c LogIO) => SyncPassiveRunner c -> FinalizationRecord -> IO UpdateResult
syncPassiveReceiveFinalizationRecord spr finRec = runSkovPassive spr (finalizeBlock finRec)

syncPassiveHookTransaction :: (SkovConfigMonad (SkovPassiveHandlers LogIO) c LogIO, TransactionHookLenses (SkovState c)) => SyncPassiveRunner c -> TransactionHash -> IO HookResult
syncPassiveHookTransaction syncRunner th = runSkovPassive syncRunner (hookQueryTransaction th)



data InMessage src =
    MsgShutdown
    | MsgBlockReceived src !BS.ByteString
    | MsgTransactionReceived !BS.ByteString
    | MsgFinalizationReceived src !BS.ByteString
    | MsgFinalizationRecordReceived src !BS.ByteString
    | MsgCatchUpStatusReceived src !BS.ByteString

data OutMessage peer =
    MsgNewBlock !BS.ByteString
    | MsgFinalization !BS.ByteString
    | MsgFinalizationRecord !BS.ByteString
    | MsgCatchUpRequired peer
    | MsgDirectedBlock peer !BS.ByteString
    | MsgDirectedFinalizationRecord peer !BS.ByteString
    | MsgDirectedCatchUpStatus peer !BS.ByteString

-- |This is provided as a compatibility wrapper for the test runners.
-- FIXME: Currently ignores pending blocks/fin-recs becoming live, which
-- should typically trigger sending a catch-up message to peers.
makeAsyncRunner :: forall c source.
    (SkovFinalizationConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO,
    SkovQueryConfigMonad c LogIO)
    => LogMethod IO
    -> BakerIdentity
    -> c
    -> IO (Chan (InMessage source), Chan (OutMessage source), SyncRunner c)
makeAsyncRunner logm bkr config = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm bkr config somHandler (\_ -> logm Runner LLInfo "*** should send catch-up status to peers ***")
        startSyncRunner sr
        let
            msgLoop = readChan inChan >>= \case
                MsgShutdown -> stopSyncRunner sr
                MsgBlockReceived src blockBS -> do
                    now <- currentTime
                    runSkovTransaction sr (TS.importPendingBlock blockBS now) >>= \case
                        Left _ -> return ()
                        Right pblock -> syncReceiveBlock sr pblock >>= handleResult src
                    msgLoop
                MsgTransactionReceived transBS -> do
                    now <- getTransactionTime
                    case runGet (getUnverifiedTransaction now) transBS of
                        Right trans -> void $ syncReceiveTransaction sr trans
                        _ -> return ()
                    msgLoop
                MsgFinalizationReceived src bs -> do
                    case runGet get bs of
                        Right finMsg -> do
                            res <- syncReceiveFinalizationMessage sr finMsg
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgFinalizationRecordReceived src finRecBS -> do
                    case runGet get finRecBS of
                        Right finRec -> do
                            res <- syncReceiveFinalizationRecord sr finRec
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgCatchUpStatusReceived src cuBS -> do
                    case runGet get cuBS of
                        Right cu -> do
                            res <- handleCatchUpStatus sr cu
                            case res of
                                Right (d, flag) -> do
                                    let
                                        send (Left fr) = writeChan outChan (MsgDirectedFinalizationRecord src (encode fr))
                                        send (Right b) = writeChan outChan (MsgDirectedBlock src (runPut $ putBlock b))
                                    forM_ d $ \(frbs, rcus) -> do
                                        mapM_ send frbs
                                        writeChan outChan (MsgDirectedCatchUpStatus src (encode rcus))
                                    when flag $ writeChan outChan (MsgCatchUpRequired src)
                                _ -> return ()
                        _ -> return ()
                    msgLoop
            handleResult src ResultPendingBlock = writeChan outChan (MsgCatchUpRequired src)
            handleResult src ResultPendingFinalization = writeChan outChan (MsgCatchUpRequired src)
            handleResult _ _ = return ()
        _ <- forkIO (msgLoop `catch` \(e :: SomeException) -> (logm Runner LLError ("Message loop exited with exception: " ++ show e) >> Prelude.putStrLn ("// **** " ++ show e)))
        return (inChan, outChan, sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock $ runPut $ putBlock block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization $ runPut $ put finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord $ runPut $ put finRec
