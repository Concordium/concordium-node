{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State.Class
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Control.Monad.Trans.RWS hiding (get)
import qualified Data.ByteString as BS
import Data.Monoid

import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Payload.Transaction
import Concordium.Birk.Bake
import Concordium.Kontrol.Monad
import Concordium.Skov.Monad()
import Concordium.Payload.Monad
import Concordium.Afgjort.Finalize
import Concordium.Logger

data InMessage =
    MsgShutdown
    | MsgTimer
    | MsgBlockReceived Block
    | MsgTransactionReceived Transaction
    | MsgFinalizationReceived BS.ByteString
    | MsgFinalizationRecordReceived FinalizationRecord

data OutMessage = 
    MsgNewBlock Block
    | MsgFinalization BS.ByteString
    | MsgFinalizationRecord FinalizationRecord

makeRunner :: LogMethod IO -> BakerIdentity -> GenesisData -> IO (Chan InMessage, Chan OutMessage, IORef SkovFinalizationState)
makeRunner logm bkr gen = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let
            finInst = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs = initialSkovFinalizationState finInst gen
        out <- newIORef sfs
        _ <- forkIO $ fst <$> runLoggerT (evalRWST (msgLoop inChan outChan out 0 MsgTimer) finInst sfs) logm
        return (inChan, outChan, out)
    where
        updateFinState :: IORef SkovFinalizationState -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState LogIO ()
        updateFinState out = get >>= liftIO . writeIORef out
        msgLoop :: Chan InMessage -> Chan OutMessage -> IORef SkovFinalizationState -> Slot -> InMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState LogIO ()
        msgLoop _ _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan out lastBake MsgTimer = do
            cs <- getCurrentSlot
            handleMessages outChan $ when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> 
                        liftIO $ writeChan outChan (MsgNewBlock block)
            updateFinState out
            ns <- timeUntilNextSlot
            _ <- liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out cs
        msgLoop inChan outChan out lastBake (MsgBlockReceived block) = do
            _ <- handleMessages outChan $ storeBlock block
            updateFinState out
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgTransactionReceived trans) = do
            handleMessages outChan $ addPendingTransaction trans
            updateFinState out
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationReceived bs) = do
            handleMessages outChan $ receiveFinalizationMessage bs
            updateFinState out
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationRecordReceived fr) = do
            handleMessages outChan $ finalizeBlock fr
            updateFinState out
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake    
        handleMessages :: Chan OutMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState LogIO r -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState LogIO r
        handleMessages outChan a = censor (const (Endo id)) $ do
            (r, Endo evs) <- listen a
            let
                handleMessage (BroadcastFinalizationMessage fmsg) = liftIO $ writeChan outChan (MsgFinalization fmsg)
                handleMessage (BroadcastFinalizationRecord frec) = liftIO $ writeChan outChan (MsgFinalizationRecord frec)
            forM_ (evs []) handleMessage
            return r
