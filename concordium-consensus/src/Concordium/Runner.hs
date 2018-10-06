{-# LANGUAGE LambdaCase #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class

import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Payload.Transaction
import Concordium.Birk.Bake
import Concordium.Kontrol.Monad
import Concordium.Skov.Monad
import Concordium.Payload.Monad

data InMessage =
    MsgShutdown
    | MsgTimer
    | MsgBlockReceived Block
    | MsgTransactionReceived Transaction

data OutMessage = 
    MsgNewBlock Block

makeRunner :: BakerIdentity -> GenesisData -> IO (Chan InMessage, Chan OutMessage)
makeRunner bkr gen = do
        inChan <- newChan
        outChan <- newChan
        _ <- forkIO $ evalStateT (msgLoop inChan outChan 0 MsgTimer) (initialSkovData gen)
        return (inChan, outChan)
    where
        msgLoop :: Chan InMessage -> Chan OutMessage -> Slot -> InMessage -> StateT SkovData IO ()
        msgLoop _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan lastBake MsgTimer = do
            cs <- getCurrentSlot
            when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> liftIO $ writeChan outChan (MsgNewBlock block)
            ns <- timeUntilNextSlot
            liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan cs
        msgLoop inChan outChan lastBake (MsgBlockReceived block) = do
            storeBlock block
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake
        msgLoop inChan outChan lastBake (MsgTransactionReceived trans) = do
            addPendingTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan lastBake
            
            


