{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Control.Monad.RWS
import qualified Data.ByteString as BS
import Lens.Micro.Platform

import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Payload.Transaction
import Concordium.Birk.Bake
import Concordium.Kontrol.Monad
import Concordium.Skov.Monad()
import Concordium.Payload.Monad
import Concordium.Kontrol.BestBlock(bestBlockBefore)
import Concordium.Getters
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

makeRunner :: BakerIdentity -> GenesisData -> IO (Chan InMessage, Chan OutMessage, IORef BlockInfo)
makeRunner bkr gen = do
        inChan <- newChan
        outChan <- newChan
        let
            finInst = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)
            sfs = initialSkovFinalizationState finInst gen
        out <- let gbPtr = sfs ^. genesisBlockPointer in newIORef (mkBlockInfo gbPtr)
        _ <- forkIO $ fst <$> evalRWST (msgLoop inChan outChan out 0 MsgTimer) finInst sfs
        return (inChan, outChan, out)
    where
        msgLoop :: Chan InMessage -> Chan OutMessage -> IORef BlockInfo -> Slot -> InMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO ()
        msgLoop _ _ _ _ MsgShutdown = return ()
        msgLoop inChan outChan out lastBake MsgTimer = do
            cs <- getCurrentSlot
            handleMessages outChan $ when (cs > lastBake) $
                bakeForSlot bkr cs >>= \case
                    Nothing -> return ()
                    Just block -> do

                      cs' <- getCurrentSlot -- it could have changed by now I suppose
                      bPtr <- bestBlockBefore (cs'+1)
                      liftIO $ writeIORef out (mkBlockInfo bPtr)

                      liftIO $ writeChan outChan (MsgNewBlock block)

            ns <- timeUntilNextSlot
            liftIO $ forkIO $ do
                threadDelay $ truncate (ns * 1e6)
                writeChan inChan MsgTimer
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out cs
        msgLoop inChan outChan out lastBake (MsgBlockReceived block) = do
            handleMessages outChan $ storeBlock block

            cs <- getCurrentSlot
            bPtr <- bestBlockBefore (cs+1)
            liftIO $ writeIORef out (mkBlockInfo bPtr)

            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgTransactionReceived trans) = do
            handleMessages outChan $ addPendingTransaction trans
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationReceived bs) = do
            handleMessages outChan $ receiveFinalizationMessage bs
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake
        msgLoop inChan outChan out lastBake (MsgFinalizationRecordReceived fr) = do
            handleMessages outChan $ finalizeBlock fr
            (liftIO $ readChan inChan) >>= msgLoop inChan outChan out lastBake    
        handleMessages :: Chan OutMessage -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO r -> RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState IO r
        handleMessages outChan a = censor (const (Endo id)) $ do
            (r, Endo evs) <- listen a
            let
                handleMessage (BroadcastFinalizationMessage fmsg) = liftIO $ writeChan outChan (MsgFinalization fmsg)
                handleMessage (BroadcastFinalizationRecord frec) = liftIO $ writeChan outChan (MsgFinalizationRecord frec)
            forM_ (evs []) handleMessage
            return r
            


