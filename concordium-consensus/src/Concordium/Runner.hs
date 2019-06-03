{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables, RecordWildCards #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.IORef
import Data.Monoid

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState(BlockState)
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
import Concordium.Skov.Update (execFSM', FSM')
import Concordium.Afgjort.Finalize
import Concordium.Logger

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
            handleMessagesNoSource outChan out $ receiveTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationReceived src bs) = do
            handleMessages outChan out src $ receiveFinalizationMessage bs
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationRecordReceived src fr) = do
            handleMessages outChan out src $ finalizeBlock fr
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
    syncState :: MVar SkovFinalizationState,
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO
}

data SimpleOutMessage
    = SOMsgNewBlock BakedBlock
    | SOMsgFinalization FinalizationMessage
    | SOMsgFinalizationRecord FinalizationRecord

-- |Run a computation, atomically using the skov/finalization state.  If the computation fails with an
-- exception, the state may be restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar SkovFinalizationState -> LogMethod IO -> (SkovFinalizationState -> LogIO (a, SkovFinalizationState)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        (ret, state') <- runLoggerT (a state0) logm
        putMVar mvState state'
        return ret

makeSyncRunner :: forall m. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (FSM m) -> (SimpleOutMessage -> IO ()) -> IO SyncRunner
makeSyncRunner syncLogMethod bkr gen initBS bakerCallback = do
        let
            syncFinalizationInstance = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs0 = initialSkovFinalizationState syncFinalizationInstance gen initBS
        syncState <- newMVar sfs0
        let
            runBaker = bakeLoop 0 `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop lastSlot = do
                (mblock, sfs', evs, curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <- if (curSlot > lastSlot) then bakeForSlot bkr curSlot else return Nothing
                                return (mblock, curSlot)
                        ((mblock, curSlot), sfs', evs) <- runFSM bake syncFinalizationInstance sfs
                        return ((mblock, sfs', evs, curSlot), sfs'))
                forM_ mblock $ bakerCallback . SOMsgNewBlock
                let
                    handleMessage (SkovFinalization (BroadcastFinalizationMessage fmsg)) = bakerCallback (SOMsgFinalization fmsg)
                    handleMessage (SkovFinalization (BroadcastFinalizationRecord frec)) = bakerCallback (SOMsgFinalizationRecord frec)
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

runFSMWithStateLog :: SyncRunner -> FSM LogIO a -> IO (a, [SkovFinalizationEvent])
runFSMWithStateLog SyncRunner{..} a = runWithStateLog syncState syncLogMethod (\sfs -> (\(ret, sfs', Endo evs) -> ((ret, evs []), sfs')) <$> runFSM a syncFinalizationInstance sfs)

syncReceiveBlock :: SyncRunner -> BakedBlock -> IO [SkovFinalizationEvent]
syncReceiveBlock syncRunner block = snd <$> runFSMWithStateLog syncRunner (storeBlock block)

syncReceiveTransaction :: SyncRunner -> Transaction -> IO [SkovFinalizationEvent]
syncReceiveTransaction syncRunner trans = snd <$> runFSMWithStateLog syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: SyncRunner -> FinalizationMessage -> IO [SkovFinalizationEvent]
syncReceiveFinalizationMessage syncRunner finMsg = snd <$> runFSMWithStateLog syncRunner (receiveFinalizationMessage finMsg)

syncReceiveFinalizationRecord :: SyncRunner -> FinalizationRecord -> IO [SkovFinalizationEvent]
syncReceiveFinalizationRecord syncRunner finRec = snd <$> runFSMWithStateLog syncRunner (finalizeBlock finRec)
