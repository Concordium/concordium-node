{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.State.Class
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Writer.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.IORef
import Data.Monoid
import Data.Time.Clock
import Data.Either

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState(BlockState)
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
import Concordium.Skov.Update
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer
import Concordium.Logger
import Concordium.Getters

data InMessage src =
    MsgShutdown
    | MsgTimer
    | MsgBlockReceived src BakedBlock
    | MsgTransactionReceived Transaction
    | MsgFinalizationReceived src FinalizationMessage
    | MsgFinalizationRecordReceived src FinalizationRecord

data OutMessage src = 
    MsgNewBlock BakedBlock
    | MsgFinalization FinalizationMessage
    | MsgFinalizationRecord FinalizationRecord
    | MsgMissingBlock src BlockHash BlockHeight
    | MsgMissingFinalization src (Either BlockHash FinalizationIndex)

makeRunner :: forall m source. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (FSM m) -> IO (Chan (InMessage source), Chan (OutMessage source), IORef SkovFinalizationState)
makeRunner logm bkr gen initBS = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let
            syncFinalizationInstance = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs = initialSkovFinalizationState syncFinalizationInstance gen initBS
        out <- newIORef sfs
        _ <- forkIO $ runLoggerT (execFSM' (msgLoop inChan outChan out 0 MsgTimer) syncFinalizationInstance gen initBS) logm
        return (inChan, outChan, out)
    where
        updateFinState :: IORef SkovFinalizationState -> FSM' LogIO ()
        updateFinState out = get >>= liftIO . writeIORef out
        msgLoop :: Chan (InMessage source) -> Chan (OutMessage source) -> IORef SkovFinalizationState -> Slot -> (InMessage source) -> FSM' LogIO ()
        msgLoop _ _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan out lastBake MsgTimer = do
            cs <- getCurrentSlot
            handleMessagesNoSource outChan out $ when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> do
                        updateFinState out
                        liftIO $ writeChan outChan (MsgNewBlock block)
            ns <- timeUntilNextSlot
            _ <- liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out cs
        msgLoop inChan outChan out lastBake (MsgBlockReceived src block) = do
            _ <- handleMessages outChan out src $ storeBlock block
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgTransactionReceived trans) = do
            _ <- handleMessagesNoSource outChan out $ receiveTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationReceived src bs) = do
            _ <- handleMessages outChan out src $ receiveFinalizationMessage bs
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationRecordReceived src fr) = do
            _ <- handleMessages outChan out src $ finalizeBlock fr
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake    
        handleMessages :: Chan (OutMessage source) -> IORef SkovFinalizationState -> source -> FSM' LogIO r -> FSM' LogIO r
        handleMessages outChan out src a = censor (const (Endo id)) $ do
            (r, Endo evs) <- listen a
            updateFinState out
            let
                handleMessage (SkovFinalization (BroadcastFinalizationMessage fmsg)) = liftIO $ writeChan outChan (MsgFinalization fmsg)
                handleMessage (SkovFinalization (BroadcastFinalizationRecord frec)) = liftIO $ writeChan outChan (MsgFinalizationRecord frec)
                handleMessage (SkovMissingBlock bh delta) = liftIO $ writeChan outChan (MsgMissingBlock src bh delta)
                handleMessage (SkovMissingFinalization fr) = liftIO $ writeChan outChan (MsgMissingFinalization src fr)
            forM_ (evs []) handleMessage
            return r
        handleMessagesNoSource :: Chan (OutMessage source) -> IORef SkovFinalizationState -> FSM' LogIO r -> FSM' LogIO r
        handleMessagesNoSource outChan out a = censor (const (Endo id)) $ do
            (r, Endo evs) <- listen a
            updateFinState out
            let
                handleMessage (SkovFinalization (BroadcastFinalizationMessage fmsg)) = liftIO $ writeChan outChan (MsgFinalization fmsg)
                handleMessage (SkovFinalization (BroadcastFinalizationRecord frec)) = liftIO $ writeChan outChan (MsgFinalizationRecord frec)
                handleMessage _ = return ()
            forM_ (evs []) handleMessage
            return r


data SyncRunner = SyncRunner {
    syncFinalizationInstance :: FinalizationInstance,
    syncState :: MVar SkovBufferedFinalizationState,
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO,
    syncFinalizationMessageCallback :: FinalizationMessage -> IO ()
}

instance SkovStateQueryable SyncRunner (SimpleSkovMonad SkovBufferedFinalizationState IO) where
    runStateQuery sr a = readMVar (syncState sr) >>= evalSSM a

data SimpleOutMessage
    = SOMsgNewBlock BakedBlock
    | SOMsgFinalization FinalizationMessage
    | SOMsgFinalizationRecord FinalizationRecord

-- |Run a computation, atomically using the state.  If the computation fails with an
-- exception, the state is restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar s -> LogMethod IO -> (s -> LogIO (a, s)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        (ret, state') <- runLoggerT (a state0) logm
        putMVar mvState state'
        return ret

asyncNotify :: MVar SkovBufferedFinalizationState -> LogMethod IO -> (FinalizationMessage -> IO ()) -> NotifyEvent -> IO ()
asyncNotify mvState logm cbk ne@(timeout, _) = void $ forkIO $ do
        now <- getCurrentTime
        let delay = diffUTCTime timeout now
        when (delay > 0) $ threadDelay (truncate $ delay * 1e6)
        mmsg <- runWithStateLog mvState logm (runStateT $ notifyBuffer ne)
        forM_ mmsg cbk


makeSyncRunner :: forall m. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (BFSM m) -> (SimpleOutMessage -> IO ()) -> IO SyncRunner
makeSyncRunner syncLogMethod bkr gen initBS bakerCallback = do
        let
            syncFinalizationInstance = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs0 = initialSkovBufferedFinalizationState syncFinalizationInstance gen initBS
            syncFinalizationMessageCallback = bakerCallback . SOMsgFinalization
        syncState <- newMVar sfs0
        let
            runBaker = bakeLoop 0 `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop lastSlot = do
                (mblock, sfs', evs, curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <- if (curSlot > lastSlot) then bakeForSlot bkr curSlot else return Nothing
                                return (mblock, curSlot)
                        ((mblock, curSlot), sfs', evs) <- runBFSM bake syncFinalizationInstance sfs
                        return ((mblock, sfs', evs, curSlot), sfs'))
                forM_ mblock $ bakerCallback . SOMsgNewBlock
                let
                    handleMessage (BufferedEvent (SkovFinalization (BroadcastFinalizationMessage fmsg))) = bakerCallback (SOMsgFinalization fmsg)
                    handleMessage (BufferedEvent (SkovFinalization (BroadcastFinalizationRecord frec))) = bakerCallback (SOMsgFinalizationRecord frec)
                    handleMessage (BufferNotification ne) = asyncNotify syncState syncLogMethod syncFinalizationMessageCallback ne
                    handleMessage _ = return () -- This should not be possible.
                forM_ (appEndo evs []) handleMessage
                delay <- evalSSM (do
                    ttns <- timeUntilNextSlot
                    curSlot' <- getCurrentSlot
                    return $! if curSlot == curSlot' then truncate (ttns * 1e6) else 0) sfs'
                when (delay > 0) $ threadDelay delay
                bakeLoop curSlot
        bakerThread <- forkIO $ runBaker
        syncBakerThread <- newMVar bakerThread
        return $ SyncRunner{..}

stopSyncRunner :: SyncRunner -> IO ()
stopSyncRunner SyncRunner{..} = mask_ $ tryTakeMVar syncBakerThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd

runBFSMWithStateLog :: SyncRunner -> BFSM LogIO a -> IO (a, [SkovFinalizationEvent])
runBFSMWithStateLog SyncRunner{..} a = do
        (ret, evts) <- runWithStateLog syncState syncLogMethod (\sfs -> (\(ret, sfs', Endo evs) -> ((ret, evs []), sfs')) <$> runBFSM a syncFinalizationInstance sfs)
        let (aevts, bevts) = partitionEithers $ evtToEither <$> evts
        forM_ bevts $ asyncNotify syncState syncLogMethod syncFinalizationMessageCallback
        return (ret, aevts)
    where
        evtToEither (BufferedEvent e) = Left e
        evtToEither (BufferNotification n) = Right n

syncReceiveBlock :: SyncRunner -> BakedBlock -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveBlock syncRunner block = runBFSMWithStateLog syncRunner (storeBlock block)

syncReceiveTransaction :: SyncRunner -> Transaction -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveTransaction syncRunner trans = runBFSMWithStateLog syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: SyncRunner -> FinalizationMessage -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveFinalizationMessage syncRunner finMsg = runBFSMWithStateLog syncRunner (receiveFinalizationMessage finMsg)

syncReceiveFinalizationRecord :: SyncRunner -> FinalizationRecord -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveFinalizationRecord syncRunner finRec = runBFSMWithStateLog syncRunner (finalizeBlock finRec)

-- |This is provided as a compatibility wrapper for the test runners.
makeAsyncRunner :: forall m source. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (BFSM m) -> IO (Chan (InMessage source), Chan (OutMessage source), MVar SkovBufferedFinalizationState)
makeAsyncRunner logm bkr gen initBS = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm bkr gen initBS somHandler
        let
            msgLoop = readChan inChan >>= \case
                MsgShutdown -> stopSyncRunner sr
                MsgTimer -> msgLoop -- TODO: Remove MsgTimer altogether
                MsgBlockReceived src block -> do
                    (_, evts) <- syncReceiveBlock sr block
                    forM_ evts $ handleMessage src
                    msgLoop
                MsgTransactionReceived trans -> do
                    (_, evts) <- syncReceiveTransaction sr trans
                    forM_ evts $ handleMessage undefined
                    msgLoop
                MsgFinalizationReceived src bs -> do
                    (_, evts) <- syncReceiveFinalizationMessage sr bs
                    forM_ evts $ handleMessage src
                    msgLoop
                MsgFinalizationRecordReceived src finRec -> do
                    (_, evts) <- syncReceiveFinalizationRecord sr finRec
                    forM_ evts $ handleMessage src
                    msgLoop
            handleMessage _ (SkovFinalization (BroadcastFinalizationMessage fmsg)) = writeChan outChan (MsgFinalization fmsg)
            handleMessage _ (SkovFinalization (BroadcastFinalizationRecord frec)) = writeChan outChan (MsgFinalizationRecord frec)
            handleMessage src (SkovMissingBlock bh delta) = writeChan outChan (MsgMissingBlock src bh delta)
            handleMessage src (SkovMissingFinalization fr) = writeChan outChan (MsgMissingFinalization src fr)
        _ <- forkIO msgLoop
        return (inChan, outChan, syncState sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord finRec
