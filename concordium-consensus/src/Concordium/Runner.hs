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

import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import qualified Concordium.GlobalState.TreeState as TS

import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
import Concordium.Skov.Hooks
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
    syncContext :: !(SkovContext c)
}

instance (SkovQueryConfigMonad c IO) => SkovStateQueryable (SyncRunner c) (SkovT () c IO) where
    runStateQuery sr a = readMVar (syncState sr) >>= evalSkovT a () (syncContext sr)

-- |Make a 'SyncRunner' without starting a baker thread.
makeSyncRunner :: (SkovConfiguration c) => LogMethod IO ->
                  BakerIdentity ->
                  c ->
                  (SimpleOutMessage c -> IO ()) ->
                  IO (SyncRunner c)
makeSyncRunner syncLogMethod syncBakerIdentity config syncCallback = do
        (syncContext, st0) <- initialiseSkov config
        syncState <- newMVar st0
        syncBakerThread <- newEmptyMVar
        syncFinalizationCatchUpActive <- newMVar Nothing
        return $ SyncRunner{..}

-- |Run a computation, atomically using the state.  If the computation fails with an
-- exception, the state is restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar s -> LogMethod IO -> (s -> LogIO (a, s)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        (ret, state') <- runLoggerT (a state0) logm
        putMVar mvState state'
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

-- |Start the baker thread for a 'SyncRunner'.
startSyncRunner :: (SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO,
    SkovQueryConfigMonad c IO
    ) => SyncRunner c -> IO ()
startSyncRunner sr@SyncRunner{..} = do
        let
            runBaker = bakeLoop 0 `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
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
                        ((mblock, curSlot), sfs') <-
                          runSkovT bake (syncSkovHandlers sr) syncContext sfs
                        return ((mblock, sfs', curSlot), sfs'))
                forM_ mblock $ syncCallback . SOMsgNewBlock
                delay <- evalSkovT (do
                    ttns <- timeUntilNextSlot
                    curSlot' <- getCurrentSlot
                    return $! if curSlot == curSlot' then truncate (ttns * 1e6) else 0) () syncContext sfs'
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
syncReceiveBlock syncRunner block = runSkovTransaction syncRunner (storeBlock block)

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
    syncPContext :: !(SkovContext c)
}

instance (SkovQueryConfigMonad c IO) => SkovStateQueryable (SyncPassiveRunner c) (SkovT () c IO) where
    runStateQuery sr a = readMVar (syncPState sr) >>= evalSkovT a () (syncPContext sr)


runSkovPassive :: SyncPassiveRunner c -> SkovT () c LogIO a -> IO a
{-# INLINE runSkovPassive #-}
runSkovPassive SyncPassiveRunner{..} a = runWithStateLog syncPState syncPLogMethod (runSkovT a () syncPContext)


-- |Make a 'SyncPassiveRunner', which does not support a baker thread.
makeSyncPassiveRunner :: (SkovConfiguration c) => LogMethod IO ->
                        c ->
                        IO (SyncPassiveRunner c)
makeSyncPassiveRunner syncPLogMethod config = do
        (syncPContext, st0) <- initialiseSkov config
        syncPState <- newMVar st0
        return $ SyncPassiveRunner{..}

shutdownSyncPassiveRunner :: SkovConfiguration c => SyncPassiveRunner c -> IO ()
shutdownSyncPassiveRunner SyncPassiveRunner{..} = takeMVar syncPState >>= shutdownSkov syncPContext

syncPassiveReceiveBlock :: (SkovConfigMonad () c LogIO) => SyncPassiveRunner c -> PendingBlock (SkovT () c LogIO) -> IO UpdateResult
syncPassiveReceiveBlock spr block = runSkovPassive spr (storeBlock block)

syncPassiveReceiveTransaction :: (SkovConfigMonad () c LogIO) => SyncPassiveRunner c -> Transaction -> IO UpdateResult
syncPassiveReceiveTransaction spr trans = runSkovPassive spr (receiveTransaction trans)

syncPassiveReceiveFinalizationRecord :: (SkovConfigMonad () c LogIO) => SyncPassiveRunner c -> FinalizationRecord -> IO UpdateResult
syncPassiveReceiveFinalizationRecord spr finRec = runSkovPassive spr (finalizeBlock finRec)

syncPassiveHookTransaction :: (SkovConfigMonad () c LogIO, TransactionHookLenses (SkovState c)) => SyncPassiveRunner c -> TransactionHash -> IO HookResult
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
makeAsyncRunner :: forall c source.
    (SkovFinalizationConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO,
    SkovQueryConfigMonad c IO)
    => LogMethod IO
    -> BakerIdentity
    -> c
    -> IO (Chan (InMessage source), Chan (OutMessage source), SyncRunner c)
makeAsyncRunner logm bkr config = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm bkr config somHandler
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
                    case runGet (getVerifiedTransaction now) transBS of
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
        _ <- forkIO msgLoop
        return (inChan, outChan, sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock $ runPut $ putBlock block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization $ runPut $ put finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord $ runPut $ put finRec
