{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import qualified Data.ByteString as BS

import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Payload.Transaction
import Concordium.Birk.Bake
import Concordium.Kontrol.Monad
import Concordium.Skov.Monad
import Concordium.Payload.Monad
import Concordium.Afgjort.Finalize

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

makeRunner :: BakerIdentity -> GenesisData -> IO (Chan InMessage, Chan OutMessage)
makeRunner bkr gen = do
        inChan <- newChan
        outChan <- newChan
        let
            finInst = FinalizationInstance (bakerSignKey bkr) (bakerSignPublicKey bkr) (bakerElectionKey bkr) (bakerElectionPublicKey bkr)
            sfs = initialSkovFinalizationState finInst gen
        _ <- forkIO $ fst <$> evalRWST (msgLoop inChan outChan 0 MsgTimer) finInst sfs
        return (inChan, outChan)
    where
        msgLoop :: Chan InMessage -> Chan OutMessage -> Slot -> InMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO ()
        msgLoop _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan lastBake MsgTimer = do
            cs <- getCurrentSlot
            handleMessages outChan $ when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> liftIO $ writeChan outChan (MsgNewBlock block)
            ns <- timeUntilNextSlot
            liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan cs
        msgLoop inChan outChan lastBake (MsgBlockReceived block) = do
            handleMessages outChan $ storeBlock block
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake
        msgLoop inChan outChan lastBake (MsgTransactionReceived trans) = do
            handleMessages outChan $ addPendingTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake
        msgLoop inChan outChan lastBake (MsgFinalizationReceived bs) = do
            handleMessages outChan $ receiveFinalizationMessage bs
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake
        msgLoop inChan outChan lastBake (MsgFinalizationRecordReceived fr) = do
            handleMessages outChan $ finalizeBlock fr
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake    
        handleMessages :: Chan OutMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO r -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO r
        handleMessages outChan a = censor (const (Endo id)) $ do
            (r, Endo evs) <- listen a
            let
                handleMessage (BroadcastFinalizationMessage fmsg) = liftIO $ writeChan outChan (MsgFinalization fmsg)
                handleMessage (BroadcastFinalizationRecord frec) = liftIO $ writeChan outChan (MsgFinalizationRecord frec)
            forM_ (evs []) handleMessage
            return r
            


