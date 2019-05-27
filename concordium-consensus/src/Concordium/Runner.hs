{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
-- import Control.Monad.Trans.RWS hiding (get)
import qualified Data.ByteString as BS
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
    | MsgFinalizationReceived src BS.ByteString
    | MsgFinalizationRecordReceived src FinalizationRecord

data OutMessage src = 
    MsgNewBlock BakedBlock
    | MsgFinalization BS.ByteString
    | MsgFinalizationRecord FinalizationRecord
    | MsgMissingBlock src BlockHash BlockHeight
    | MsgMissingFinalization src (Either BlockHash FinalizationIndex)

makeRunner :: forall m source. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (FSM m) -> IO (Chan (InMessage source), Chan (OutMessage source), IORef SkovFinalizationState)
makeRunner logm bkr gen initBS = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let
            finInst = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs = initialSkovFinalizationState finInst gen initBS
        out <- newIORef sfs
        _ <- forkIO $ runLoggerT (execFSM' (msgLoop inChan outChan out 0 MsgTimer) finInst gen initBS) logm
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

{-
data SyncRunner = SyncRunner {
    syncState :: MVar SkovFinalizationState,
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO
}

data SimpleOutMessage
    = SOMsgNewBlock BakedBlock
    | SOMsgFinalization FinalizationMessage
    | SOMsgFinalizationRecord FinalizationRecord

runWithStateLog :: MVar SkovFinalizationState -> LogMethod IO -> FSM LogIO a -> IO a
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state ->
        runLoggerT 

makeSyncRunner :: forall m. LogMethod IO -> BakerIdentity -> GenesisData -> BlockState (FSM m) -> (SimpleOutMessage -> IO ()) -> IO SyncRunner
makeSyncRunner syncLogMethod bkr gen initBS bakerCallback = do
        let
            finInst = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs = initialSkovFinalizationState finInst gen initBS
        syncState <- newMVar sfs
        let
            runBaker = bakeLoop `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop = do
                curSfs <- takeMVar syncState
                runLoggerT syncLogMethod 
        syncBakerThread <- newEmptyMVar
    where
        runBaker mvState = bakeLoop `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            where
                bakeLoop

 -}