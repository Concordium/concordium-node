{-# LANGUAGE LambdaCase #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Payload.Transaction
import Concordium.Birk.Bake
import Concordium.Kontrol.Monad
import Concordium.Skov.Monad()
import Concordium.Payload.Monad
import Concordium.Kontrol.BestBlock(bestBlockBefore)

import Concordium.Getters

data InMessage =
    MsgShutdown
    | MsgTimer
    | MsgBlockReceived Block
    | MsgTransactionReceived Transaction

data OutMessage = 
    MsgNewBlock Block

makeRunner :: BakerIdentity -> GenesisData -> IO (Chan InMessage, Chan OutMessage, IORef BlockInfo)
makeRunner bkr gen = do
        inChan <- newChan
        outChan <- newChan
        let initSkov = initialSkovData gen
        out <- let gbPtr = _skovGenesisBlockPointer initSkov in newIORef (mkBlockInfo (bpBlock gbPtr) (bpState gbPtr))
        _ <- forkIO $ evalStateT (msgLoop inChan outChan out 0 MsgTimer) initSkov
        return (inChan, outChan, out)
    where
        msgLoop :: Chan InMessage -> Chan OutMessage -> IORef BlockInfo -> Slot -> InMessage -> StateT SkovData IO ()
        msgLoop _ _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan out lastBake MsgTimer = do
            cs <- getCurrentSlot
            when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> do

                      cs' <- getCurrentSlot -- it could have changed by now I suppose
                      bPtr <- bestBlockBefore (cs'+1)
                      liftIO $ writeIORef out (mkBlockInfo (bpBlock bPtr) (bpState bPtr))

                      liftIO $ writeChan outChan (MsgNewBlock block)

            ns <- timeUntilNextSlot
            liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out cs
        msgLoop inChan outChan out lastBake (MsgBlockReceived block) = do
            storeBlock block

            cs <- getCurrentSlot
            bPtr <- bestBlockBefore (cs+1)
            liftIO $ writeIORef out (mkBlockInfo (bpBlock bPtr) (bpState bPtr))

            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgTransactionReceived trans) = do
            addPendingTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
            
            


