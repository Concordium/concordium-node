{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Trans.State hiding (get)
import Control.Monad
import Control.Exception
import Data.Monoid
import Data.Time.Clock
import Data.Either

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState(BlockState)
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


data InMessage src =
    MsgShutdown
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
