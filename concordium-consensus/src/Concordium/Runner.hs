{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad
import Control.Exception
import Data.Monoid
import Data.Time.Clock
import Data.Either
import Data.ByteString as BS
import Data.Serialize

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState(BlockState)
import Concordium.GlobalState.Rust.TreeState
import Concordium.GlobalState.Rust.FFI
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Basic.Block(Block(NormalBlock))
import Concordium.TimeMonad
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
-- import Concordium.Skov.Update
import Concordium.Skov.Hooks
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer
import Concordium.Logger
import Concordium.Getters

data SyncRunner = SyncRunner {
    syncBakerIdentity :: BakerIdentity,
    syncState :: MVar SkovBufferedHookedState,
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO,
    syncCallback :: SimpleOutMessage -> IO ()
}

bakerFinalizationInstance :: BakerIdentity -> FinalizationInstance
bakerFinalizationInstance bkr = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)

instance SkovStateQueryable SyncRunner (SkovQueryM SkovBufferedHookedState IO) where
    runStateQuery sr a = readMVar (syncState sr) >>= evalSkovQueryM a

data SimpleOutMessage
    = SOMsgNewBlock BlockPointer
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

asyncNotify :: MVar SkovBufferedHookedState -> LogMethod IO -> (FinalizationMessage -> IO ()) -> NotifyEvent -> IO ()
asyncNotify mvState logm cbk ne@(timeout, _) = void $ forkIO $ do
        now <- getCurrentTime
        let delay = diffUTCTime timeout now
        when (delay > 0) $ threadDelay (truncate $ delay * 1e6)
        mmsg <- runWithStateLog mvState logm (runStateT $ notifyBuffer ne)
        forM_ mmsg cbk

-- |Make a 'SyncRunner' without starting a baker thread.
makeSyncRunner :: forall m. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (SkovBufferedM m) -> GlobalStatePtr -> (SimpleOutMessage -> IO ()) -> IO SyncRunner
makeSyncRunner syncLogMethod syncBakerIdentity gen initBS gsptr syncCallback = do
        let
            syncFinalizationInstance = bakerFinalizationInstance syncBakerIdentity
        sfs0 <- initialSkovBufferedHookedState syncFinalizationInstance gen initBS gsptr
        syncState <- newMVar sfs0
        syncBakerThread <- newEmptyMVar
        return $ SyncRunner{..}

-- |Start the baker thread for a 'SyncRunner'.
startSyncRunner :: SyncRunner -> IO ()
startSyncRunner SyncRunner{..} = do
        let
            runBaker = bakeLoop 0 `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop lastSlot = do
                (mblock, sfs', evs, curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <- if (curSlot > lastSlot) then bakeForSlot syncBakerIdentity curSlot else return Nothing
                                return (mblock, curSlot)
                        ((mblock, curSlot), sfs', evs) <- runSkovBufferedHookedM bake (bakerFinalizationInstance syncBakerIdentity) sfs
                        return ((mblock, sfs', evs, curSlot), sfs'))
                forM_ mblock $ syncCallback . SOMsgNewBlock
                let
                    handleMessage (BufferedEvent (SkovFinalization (BroadcastFinalizationMessage fmsg))) = syncCallback (SOMsgFinalization fmsg)
                    handleMessage (BufferedEvent (SkovFinalization (BroadcastFinalizationRecord frec))) = syncCallback (SOMsgFinalizationRecord frec)
                    handleMessage (BufferNotification ne) = asyncNotify syncState syncLogMethod (syncCallback . SOMsgFinalization) ne
                    handleMessage _ = return () -- This should not be possible.
                forM_ (appEndo evs []) handleMessage
                delay <- evalSkovQueryM (do
                    ttns <- timeUntilNextSlot
                    curSlot' <- getCurrentSlot
                    return $! if curSlot == curSlot' then truncate (ttns * 1e6) else 0) sfs'
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
stopSyncRunner :: SyncRunner -> IO ()
stopSyncRunner SyncRunner{..} = mask_ $ tryTakeMVar syncBakerThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd

runSkovBufferedMWithStateLog :: SyncRunner -> SkovBufferedHookedM LogIO a -> IO (a, [SkovFinalizationEvent])
runSkovBufferedMWithStateLog SyncRunner{..} a = do
        (ret, evts) <- runWithStateLog syncState syncLogMethod (\sfs ->
            (\(ret, sfs', Endo evs) -> ((ret, evs []), sfs')) <$> runSkovBufferedHookedM a (bakerFinalizationInstance syncBakerIdentity) sfs)
        let (aevts, bevts) = partitionEithers $ evtToEither <$> evts
        forM_ bevts $ asyncNotify syncState syncLogMethod (syncCallback . SOMsgFinalization)
        return (ret, aevts)
    where
        evtToEither (BufferedEvent e) = Left e
        evtToEither (BufferNotification n) = Right n

syncReceiveBlock :: SyncRunner -> PendingBlock -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveBlock syncRunner block = runSkovBufferedMWithStateLog syncRunner (storeBlock block)

syncReceiveTransaction :: SyncRunner -> Transaction -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveTransaction syncRunner trans = runSkovBufferedMWithStateLog syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: SyncRunner -> FinalizationMessage -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveFinalizationMessage syncRunner finMsg = runSkovBufferedMWithStateLog syncRunner (receiveFinalizationMessage finMsg)

syncReceiveFinalizationRecord :: SyncRunner -> FinalizationRecord -> IO (UpdateResult, [SkovFinalizationEvent])
syncReceiveFinalizationRecord syncRunner finRec = runSkovBufferedMWithStateLog syncRunner (finalizeBlock finRec)

syncHookTransaction :: SyncRunner -> TransactionHash -> IO HookResult
-- hookQueryTransaction does not generate any events, so it is safe to drop them.
syncHookTransaction syncRunner th = fst <$> runSkovBufferedMWithStateLog syncRunner (hookQueryTransaction th)

data SyncPassiveRunner = SyncPassiveRunner {
    syncPState :: MVar SkovPassiveHookedState,
    syncPLogMethod :: LogMethod IO
}

-- |Make a 'SyncPassiveRunner', which does not support a baker thread.
makeSyncPassiveRunner :: forall m. LogMethod IO -> GenesisData -> BlockState (SkovPassiveHookedM m) -> GlobalStatePtr -> IO SyncPassiveRunner
makeSyncPassiveRunner syncPLogMethod gen initBS gsptr = do
  initialState <- initialSkovPassiveHookedState gen initBS gsptr
  syncPState <- newMVar $ initialState
  return $ SyncPassiveRunner{..}

runSkovPassiveMWithStateLog :: SyncPassiveRunner -> SkovPassiveHookedM LogIO a -> IO (a, [SkovMissingEvent])
runSkovPassiveMWithStateLog SyncPassiveRunner{..} a =
        runWithStateLog syncPState syncPLogMethod (\sss ->
            (\(ret, sss', Endo evs) -> ((ret, evs []), sss')) <$> runSkovPassiveHookedM a sss)

syncPassiveReceiveBlock :: SyncPassiveRunner -> PendingBlock -> IO (UpdateResult, [SkovMissingEvent])
syncPassiveReceiveBlock spr block = runSkovPassiveMWithStateLog spr (storeBlock block)

syncPassiveReceiveTransaction :: SyncPassiveRunner -> Transaction -> IO (UpdateResult, [SkovMissingEvent])
syncPassiveReceiveTransaction spr trans = runSkovPassiveMWithStateLog spr (receiveTransaction trans)

syncPassiveReceiveFinalizationMessage :: SyncPassiveRunner -> FinalizationMessage -> IO (UpdateResult, [SkovMissingEvent])
syncPassiveReceiveFinalizationMessage spr finMsg = runSkovPassiveMWithStateLog spr (passiveReceiveFinalizationMessage finMsg)

syncPassiveReceiveFinalizationRecord :: SyncPassiveRunner -> FinalizationRecord -> IO (UpdateResult, [SkovMissingEvent])
syncPassiveReceiveFinalizationRecord spr finRec = runSkovPassiveMWithStateLog spr (finalizeBlock finRec)

syncPassiveHookTransaction :: SyncPassiveRunner -> TransactionHash -> IO HookResult
-- hookQueryTransaction does not generate any events, so it is safe to drop them.
syncPassiveHookTransaction syncRunner th = fst <$> runSkovPassiveMWithStateLog syncRunner (hookQueryTransaction th)


data InMessage src =
    MsgShutdown
    | MsgBlockReceived src !BS.ByteString
    | MsgTransactionReceived !BS.ByteString
    | MsgFinalizationReceived src !BS.ByteString
    | MsgFinalizationRecordReceived src !BS.ByteString

data OutMessage src =
    MsgNewBlock !BS.ByteString
    | MsgFinalization !BS.ByteString
    | MsgFinalizationRecord !BS.ByteString
    | MsgMissingBlock src BlockHash BlockHeight
    | MsgMissingFinalization src (Either BlockHash FinalizationIndex)

-- |This is provided as a compatibility wrapper for the test runners.
makeAsyncRunner :: forall m source. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (SkovBufferedM m) -> GlobalStatePtr -> IO (Chan (InMessage source), Chan (OutMessage source), MVar SkovBufferedHookedState)
makeAsyncRunner logm bkr gen initBS gsptr = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm bkr gen initBS gsptr somHandler
        startSyncRunner sr
        let
            msgLoop = readChan inChan >>= \case
                MsgShutdown -> stopSyncRunner sr
                MsgBlockReceived src blockBS -> do
                    case runGet get blockBS of
                        Right (NormalBlock block) -> do
                            now <- currentTime
                            pblock <- makePendingBlock gsptr block now
                            (_, evts) <- syncReceiveBlock sr pblock
                            forM_ evts $ handleMessage src
                        _ -> return ()
                    msgLoop
                MsgTransactionReceived transBS -> do
                    case runGet get transBS of
                        Right trans -> do
                            (_, evts) <- syncReceiveTransaction sr trans
                            forM_ evts $ handleMessage undefined
                        _ -> return ()
                    msgLoop
                MsgFinalizationReceived src bs -> do
                    case runGet get bs of
                        Right finMsg -> do
                            (_, evts) <- syncReceiveFinalizationMessage sr finMsg
                            forM_ evts $ handleMessage src
                        _ -> return ()
                    msgLoop
                MsgFinalizationRecordReceived src finRecBS -> do
                    case runGet get finRecBS of
                        Right finRec -> do
                            (_, evts) <- syncReceiveFinalizationRecord sr finRec
                            forM_ evts $ handleMessage src
                        _ -> return ()
                    msgLoop
            handleMessage _ (SkovFinalization (BroadcastFinalizationMessage fmsg)) = writeChan outChan (MsgFinalization $ runPut $ put fmsg)
            handleMessage _ (SkovFinalization (BroadcastFinalizationRecord frec)) = writeChan outChan (MsgFinalizationRecord $ runPut $ put frec)
            handleMessage src (SkovMissing (SkovMissingBlock bh delta)) = writeChan outChan (MsgMissingBlock src bh delta)
            handleMessage src (SkovMissing (SkovMissingFinalization fr)) = writeChan outChan (MsgMissingFinalization src fr)
        _ <- forkIO msgLoop
        return (inChan, outChan, syncState sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock $ runPut $ putBlock block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization $ runPut $ put finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord $ runPut $ put finRec
