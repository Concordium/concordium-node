{-# LANGUAGE
    ScopedTypeVariables,
    BangPatterns,
    UndecidableInstances,
    ConstraintKinds,
    TypeFamilies
#-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Serialize
import Data.IORef
import Control.Monad.IO.Class
import Data.Time.Clock
import System.IO.Error

import Concordium.Common.Version
import Concordium.GlobalState.Block
import Concordium.GlobalState.Types
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState (TreeStateMonad, purgeTransactionTable)

import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Logger
import Concordium.Getters

type SkovBlockPointer c = BlockPointerType (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO)

data SimpleOutMessage c
    = SOMsgNewBlock !(SkovBlockPointer c)
    | SOMsgFinalization !FinalizationPseudoMessage
    | SOMsgFinalizationRecord !FinalizationRecord

data SyncRunner c = SyncRunner {
    syncBakerIdentity :: !BakerIdentity,
    syncState :: !(MVar (SkovState c)),
    syncBakerThread :: !(MVar ThreadId),
    syncLogMethod :: LogMethod IO,
    syncCallback :: SimpleOutMessage c -> IO (),
    syncFinalizationCatchUpActive :: MVar (Maybe (IORef Bool)),
    syncContext :: !(SkovContext c),
    syncHandlePendingLive :: !(IO ()),
    syncTransactionPurgingThread :: !(MVar ThreadId)
}

instance (SkovQueryMonad (SkovT () c LogIO)) => SkovStateQueryable (SyncRunner c) (SkovT () c LogIO) where
    runStateQuery sr a = do
        s <- readMVar (syncState sr)
        runLoggerT (evalSkovT a () (syncContext sr) s) (syncLogMethod sr)

bufferedHandlePendingLive :: IO () -> MVar (Maybe (UTCTime, UTCTime)) -> IO ()
bufferedHandlePendingLive hpl bufferMVar = do
        now <- currentTime
        takeMVar bufferMVar >>= \case
            Nothing -> do
                putMVar bufferMVar $ Just (addUTCTime 5 now, addUTCTime 30 now)
                void $ forkIO $ waitLoop (addUTCTime 5 now)
            Just (_, upper) -> do
              let !m = min (addUTCTime 5 now) upper
              putMVar bufferMVar $ Just (m, upper)
    where
        waitLoop till = do
            now <- currentTime
            let waitDurationMicros = truncate (diffUTCTime till now * 1e6)
            when (waitDurationMicros > 0) $ threadDelay waitDurationMicros
            takeMVar bufferMVar >>= \case
                Nothing -> putMVar bufferMVar Nothing
                v@(Just (lower, _)) -> do
                    now' <- currentTime
                    if now' >= lower then do
                        putMVar bufferMVar Nothing
                        hpl
                    else do
                        putMVar bufferMVar v
                        waitLoop lower

-- |Make a 'SyncRunner' without starting a baker thread. This will also create a timer for purging the transaction table periodically.
makeSyncRunner :: (SkovConfiguration c, SkovQueryMonad (SkovT () c LogIO)) => LogMethod IO ->
                  BakerIdentity ->
                  c ->
                  (SimpleOutMessage c -> IO ()) ->
                  (CatchUpStatus -> IO ()) ->
                  IO (SyncRunner c)
makeSyncRunner syncLogMethod syncBakerIdentity config syncCallback cusCallback = do
        (syncContext, st0) <- runLoggerT (initialiseSkov config) syncLogMethod
        syncState <- newMVar st0
        syncTransactionPurgingThread <- newEmptyMVar
        syncBakerThread <- newEmptyMVar
        syncFinalizationCatchUpActive <- newMVar Nothing
        pendingLiveMVar <- newMVar Nothing
        let
            syncHandlePendingLive = bufferedHandlePendingLive (runStateQuery sr (getCatchUpStatus False) >>= cusCallback) pendingLiveMVar
            sr = SyncRunner{..}
        return sr

-- |Run a computation, atomically using the state.  If the computation fails with an
-- exception, the state is restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar s -> LogMethod IO -> (s -> LogIO (a, s)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        tid <- myThreadId
        logm Runner LLTrace $ "Acquired consensus lock on thread " ++ show tid
        (ret, !state') <- runLoggerT (a state0) logm
        putMVar mvState state'
        logm Runner LLTrace $ "Released consensus lock on thread " ++ show tid
        return ret


runSkovTransaction :: SyncRunner c -> SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO a -> IO a
{-# INLINE runSkovTransaction #-}
runSkovTransaction sr@SyncRunner{..} a = runWithStateLog syncState syncLogMethod (runSkovT a (syncSkovHandlers sr) syncContext)

syncSkovHandlers :: forall c. SyncRunner c -> SkovHandlers ThreadTimer c LogIO
syncSkovHandlers sr@SyncRunner{..} = SkovHandlers{
        shBroadcastFinalizationMessage = liftIO . syncCallback . SOMsgFinalization,
        shBroadcastFinalizationRecord = liftIO . syncCallback . SOMsgFinalizationRecord,
        shOnTimeout = \timeout a -> liftIO $ makeThreadTimer timeout $ void $ runSkovTransaction sr a,
        shCancelTimer = liftIO . cancelThreadTimer,
        shPendingLive = liftIO syncHandlePendingLive
    }

-- |Start the baker thread for a 'SyncRunner'. This will also spawn a background thread for purging the transaction table periodically.
startSyncRunner :: forall c. (
    (SkovQueryMonad (SkovT () c LogIO)),
    (BakerMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO)),
    (TreeStateMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    ) => SyncRunner c -> IO ()
startSyncRunner sr@SyncRunner{..} = do
        let
            runBaker :: IO ()
            runBaker = (bakeLoop 0 `catch` \(e :: SomeException) -> (syncLogMethod Runner LLError ("Message loop exited with exception: " ++ show e)))
                            `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop :: Slot -> IO ()
            bakeLoop lastSlot = do
                (mblock, sfs', curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <-
                                        if curSlot > lastSlot then do
                                            bakeForSlot syncBakerIdentity curSlot
                                        else
                                            return Nothing
                                return (mblock, curSlot)
                        -- TODO: modify handlers to send out finalization messages AFTER any generated block
                        ((mblock, curSlot), sfs') <-
                          runSkovT bake (syncSkovHandlers sr) syncContext sfs
                        return ((mblock, sfs', curSlot), sfs'))
                forM_ mblock $ syncCallback . SOMsgNewBlock
                delay <- runLoggerT (evalSkovT (do
                    curSlot' <- getCurrentSlot
                    if curSlot == curSlot' then do
                      ttns <- timeUntilNextSlot
                      return $! truncate (ttns * 1e6)
                    else return 0) () syncContext sfs') syncLogMethod
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
        rp <- runSkovTransaction sr getRuntimeParameters
        let delay = rpTransactionsPurgingDelay rp * 10 ^ (6 :: Int)
            purgingLoop = do
              tm <- currentTime
              runSkovTransaction sr (purgeTransactionTable True tm)
              threadDelay delay
              purgingLoop
        putMVar syncTransactionPurgingThread =<< forkIO purgingLoop
        return ()

-- |Stop the baker thread for a 'SyncRunner'.
stopSyncRunner :: SyncRunner c -> IO ()
stopSyncRunner SyncRunner{..} = do
  mask_ $ tryTakeMVar syncBakerThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd
  mask_ $ tryTakeMVar syncTransactionPurgingThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd


-- |Stop any baker thread and dispose resources used by the 'SyncRunner'.
-- This should only be called once. Any subsequent call may diverge or throw an exception.
shutdownSyncRunner :: (SkovConfiguration c) => SyncRunner c -> IO ()
shutdownSyncRunner sr@SyncRunner{..} = do
        stopSyncRunner sr
        takeMVar syncState >>= flip runLoggerT syncLogMethod . shutdownSkov syncContext

isSlotTooEarly :: (TimeMonad m, SkovQueryMonad m) => Slot -> m Bool
isSlotTooEarly s = do
    threshold <- rpEarlyBlockThreshold <$> getRuntimeParameters
    now <- currentTimestamp
    slotTime <- getSlotTimestamp s
    return $ slotTime > now + threshold

syncReceiveBlock :: (SkovMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    => SyncRunner c
    -> PendingBlock
    -> IO UpdateResult
syncReceiveBlock syncRunner block = do
    blockTooEarly <- runSkovTransaction syncRunner (isSlotTooEarly (blockSlot block))
    if blockTooEarly then
        return ResultEarlyBlock
    else
        runSkovTransaction syncRunner (storeBlock block)

syncReceiveTransaction :: (SkovMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    => SyncRunner c -> BlockItem -> IO UpdateResult
syncReceiveTransaction syncRunner trans = runSkovTransaction syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: (FinalizationMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    => SyncRunner c -> FinalizationPseudoMessage -> IO UpdateResult
syncReceiveFinalizationMessage syncRunner finMsg = runSkovTransaction syncRunner (finalizationReceiveMessage finMsg)

syncReceiveFinalizationRecord :: (FinalizationMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    => SyncRunner c -> FinalizationRecord -> IO UpdateResult
syncReceiveFinalizationRecord syncRunner finRec = runSkovTransaction syncRunner (finalizationReceiveRecord False finRec)

syncReceiveCatchUp :: (SkovMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
    => SyncRunner c
    -> CatchUpStatus
    -> Int
    -> IO (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)
syncReceiveCatchUp syncRunner c limit = runSkovTransaction syncRunner (handleCatchUpStatus c limit)

{-
syncHookTransaction :: (TreeStateMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO), SkovQueryMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO), {-SkovConfigMonad (SkovHandlers ThreadTimer c LogIO) c LogIO,-} TransactionHookLenses (SkovState c))
    => SyncRunner c -> TransactionHash -> IO HookResult
syncHookTransaction syncRunner th = runSkovTransaction syncRunner (hookQueryTransaction th)
-}

data SyncPassiveRunner c = SyncPassiveRunner {
    syncPState :: !(MVar (SkovState c)),
    syncPLogMethod :: LogMethod IO,
    syncPContext :: !(SkovContext c),
    syncPHandlers :: !(SkovPassiveHandlers c LogIO),
    syncPTransactionPurgingThread :: !(MVar ThreadId)
}

instance (SkovQueryMonad (SkovT () c LogIO)) => SkovStateQueryable (SyncPassiveRunner c) (SkovT () c LogIO) where
    runStateQuery sr a = do
        s <- readMVar (syncPState sr)
        runLoggerT (evalSkovT a () (syncPContext sr) s) (syncPLogMethod sr)


runSkovPassive :: SyncPassiveRunner c -> SkovT (SkovPassiveHandlers c LogIO) c LogIO a -> IO a
{-# INLINE runSkovPassive #-}
runSkovPassive SyncPassiveRunner{..} a = runWithStateLog syncPState syncPLogMethod (runSkovT a syncPHandlers syncPContext)


-- |Make a 'SyncPassiveRunner', which does not support a baker thread. This will also spawn a background thread for purging the transaction table periodically.
makeSyncPassiveRunner :: (SkovConfiguration c, SkovQueryMonad (SkovT () c LogIO), TreeStateMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO)) => LogMethod IO ->
                        c ->
                        (CatchUpStatus -> IO ()) ->
                        IO (SyncPassiveRunner c)
makeSyncPassiveRunner syncPLogMethod config cusCallback = do
        (syncPContext, st0) <- runLoggerT (initialiseSkov config) syncPLogMethod
        syncPState <- newMVar st0
        pendingLiveMVar <- newMVar Nothing
        syncPTransactionPurgingThread <- newEmptyMVar
        let
            sphPendingLive = liftIO $ bufferedHandlePendingLive (runStateQuery spr (getCatchUpStatus False) >>= cusCallback) pendingLiveMVar
            syncPHandlers = SkovPassiveHandlers {..}
            spr = SyncPassiveRunner{..}
        rp <- runSkovPassive spr getRuntimeParameters
        let delay = rpTransactionsPurgingDelay rp * 10 ^ (6 :: Int)
        let loop = do
              tm <- currentTime
              runSkovPassive spr (purgeTransactionTable True tm)
              threadDelay delay
              loop
        putMVar syncPTransactionPurgingThread =<< forkIO loop
        return spr

shutdownSyncPassiveRunner :: SkovConfiguration c => SyncPassiveRunner c -> IO ()
shutdownSyncPassiveRunner SyncPassiveRunner{..} = do
  takeMVar syncPState >>= flip runLoggerT syncPLogMethod . shutdownSkov syncPContext
  mask_ $ tryTakeMVar syncPTransactionPurgingThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd

syncPassiveReceiveBlock :: (SkovMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO))
                        => SyncPassiveRunner c -> PendingBlock -> IO UpdateResult
syncPassiveReceiveBlock spr block = do
  blockTooEarly <- runSkovPassive spr (isSlotTooEarly (blockSlot block))
  if blockTooEarly then
      return ResultEarlyBlock
  else
      runSkovPassive spr (storeBlock block)

syncPassiveReceiveTransaction :: (SkovMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO)) => SyncPassiveRunner c -> BlockItem -> IO UpdateResult
syncPassiveReceiveTransaction spr trans = runSkovPassive spr (receiveTransaction trans)

syncPassiveReceiveFinalizationMessage :: (FinalizationMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO))
    => SyncPassiveRunner c -> FinalizationPseudoMessage -> IO UpdateResult
syncPassiveReceiveFinalizationMessage spr finMsg = runSkovPassive spr (finalizationReceiveMessage finMsg)

syncPassiveReceiveFinalizationRecord :: (FinalizationMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO)) => SyncPassiveRunner c -> FinalizationRecord -> IO UpdateResult
syncPassiveReceiveFinalizationRecord spr finRec = runSkovPassive spr (finalizationReceiveRecord False finRec)

syncPassiveReceiveCatchUp :: (SkovMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO))
    => SyncPassiveRunner c
    -> CatchUpStatus
    -> Int
    -> IO (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)
syncPassiveReceiveCatchUp spr c limit = runSkovPassive spr (handleCatchUpStatus c limit)

data InMessage src =
    MsgShutdown
    | MsgBlockReceived !src !BS.ByteString
    | MsgTransactionReceived !BS.ByteString
    | MsgFinalizationReceived !src !BS.ByteString
    | MsgFinalizationRecordReceived !src !BS.ByteString
    | MsgCatchUpStatusReceived !src !BS.ByteString

data OutMessage peer =
    MsgNewBlock !BS.ByteString
    | MsgFinalization !BS.ByteString
    | MsgFinalizationRecord !BS.ByteString
    | MsgCatchUpRequired !peer
    | MsgDirectedBlock !peer !BS.ByteString
    | MsgDirectedFinalizationRecord !peer !BS.ByteString
    | MsgDirectedCatchUpStatus !peer !BS.ByteString

-- |This is provided as a compatibility wrapper for the test runners.
-- FIXME: Currently ignores pending blocks/fin-recs becoming live, which
-- should typically trigger sending a catch-up message to peers.
makeAsyncRunner :: forall c source.
    (SkovConfiguration c,
    (SkovQueryMonad (SkovT () c LogIO)),
    (TreeStateMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO)),
    (BakerMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
        )
    => LogMethod IO
    -> BakerIdentity
    -> c
    -> IO (Chan (InMessage source), Chan (OutMessage source), SyncRunner c)
makeAsyncRunner logm bkr config = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm bkr config somHandler (\_ -> logm Runner LLInfo "*** should send catch-up status to peers ***")
        startSyncRunner sr
        let
            msgLoop :: IO ()
            msgLoop = readChan inChan >>= \case
                MsgShutdown -> stopSyncRunner sr
                MsgBlockReceived src blockBS -> do
                    now <- currentTime
                    case deserializeExactVersionedPendingBlock blockBS now of
                        Left err -> logm Runner LLWarning err
                        Right !block -> syncReceiveBlock sr block >>= handleResult src
                    msgLoop
                MsgTransactionReceived transBS -> do
                    now <- getTransactionTime
                    case runGet (getExactVersionedBlockItem now) transBS of
                        Right !trans -> void $ syncReceiveTransaction sr trans
                        _ -> return ()
                    msgLoop
                MsgFinalizationReceived src bs -> do
                    case runGet getExactVersionedFPM bs of
                        Right !finMsg -> do
                            res <- syncReceiveFinalizationMessage sr finMsg
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgFinalizationRecordReceived src finRecBS -> do
                    case runGet getExactVersionedFinalizationRecord finRecBS of
                        Right !finRec -> do
                            res <- syncReceiveFinalizationRecord sr finRec
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgCatchUpStatusReceived src cuBS -> do
                    case runGet get cuBS of
                        Right !cu -> do
                            (resp, res) <- syncReceiveCatchUp sr cu catchUpLimit
                            forM_ resp $ \(msgs, cus) -> do
                                let
                                    send (MessageBlock, bs) = writeChan outChan (MsgDirectedBlock src bs)
                                    send (MessageFinalizationRecord, bs) = writeChan outChan (MsgDirectedFinalizationRecord src bs)
                                mapM_ send msgs
                                writeChan outChan (MsgDirectedCatchUpStatus src (encode cus))
                            when (res == ResultPendingBlock || res == ResultContinueCatchUp) $
                                writeChan outChan (MsgCatchUpRequired src)
                        _ -> return ()
                    msgLoop
            handleResult src ResultPendingBlock = writeChan outChan (MsgCatchUpRequired src)
            handleResult src ResultPendingFinalization = writeChan outChan (MsgCatchUpRequired src)
            handleResult _ _ = return ()
        _ <- forkIO (msgLoop `catch` \(e :: SomeException) -> (logm Runner LLError ("Message loop exited with exception: " ++ show e) >> Prelude.putStrLn ("// **** " ++ show e)))
        return (inChan, outChan, sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock $ runPut $ putVersionedBlockV1 block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization $ runPut $ putVersionedFPMV0 finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord $ runPut $ putVersionedFinalizationRecordV0 finRec

        catchUpLimit = 100

-- |Handle an exception that happended during block import
handleImportException :: LogMethod IO -> IOException -> IO UpdateResult
handleImportException logm e =
    if isDoesNotExistError e then do
      logm External LLError $ "The provided file for importing blocks doesn't exist."
      return ResultMissingImportFile
    else do
      logm External LLError $ "An IO exception occurred during import phase: " ++ show e
      return ResultInvalid

-- | Given a file path in the third argument, it will deserialize each block in the file
-- and import it into the active global state.
syncImportBlocks :: (SkovMonad (SkovT (SkovHandlers ThreadTimer c LogIO) c LogIO))
                 => SyncRunner c
                 -> FilePath
                 -> IO UpdateResult
syncImportBlocks syncRunner filepath =
  handle (handleImportException logm) $ do
    -- NB: It is very important to use lazy a bytestring here since we are
    -- loading the whole file into it. We need to do that lazily.
    lbs <- LBS.readFile filepath
    now <- getCurrentTime
    readBlocks lbs now logm syncReceiveBlock syncRunner
  where logm = syncLogMethod syncRunner

-- | Given a file path in the third argument, it will deserialize each block in the file
-- and import it into the passive global state.
syncPassiveImportBlocks :: (SkovMonad (SkovT (SkovPassiveHandlers c LogIO) c LogIO))
                        => SyncPassiveRunner c
                        -> FilePath
                        -> IO UpdateResult
syncPassiveImportBlocks syncRunner filepath =
  handle (handleImportException logm) $ do
    -- NB: It is very important to use lazy a bytestring here since we are
    -- loading the whole file into it. We need to do that lazily.
    lbs <- LBS.readFile filepath
    now <- getCurrentTime
    readBlocks lbs now logm syncPassiveReceiveBlock syncRunner
  where
    logm = syncPLogMethod syncRunner

-- |Read the header of the export file.
-- The header consists of
--
-- - version number that determines the format of all the subsequent blocks in the file.
--
-- The function returns, if successfull, the version, and the unconsumed input.
readHeader :: LBS.ByteString -> Either String (Version, LBS.ByteString)
readHeader = runGetLazyState get

readBlocks :: LBS.ByteString
           -> UTCTime
           -> LogMethod IO
           -> (t -> PendingBlock -> IO UpdateResult)
           -> t
           -> IO UpdateResult
readBlocks lbs tm logm f syncRunner = do
  case readHeader lbs of
      Left err -> do
        logm External LLError $ "Error deserializing header: " ++ err
        return ResultSerializationFail
      Right (version, rest)
          | version == 1 -> loopV1 rest
          | otherwise -> do
              logm External LLError $ "Unsupported version: " ++ show version
              return ResultSerializationFail
  where loopV1 rest
            | LBS.null rest = return ResultSuccess -- we've reached the end of the file
            | otherwise =
                case runGetLazyState blockGetter rest of
                  Left err -> do
                    logm External LLError $ "Error reading block bytes: " ++ err
                    return ResultSerializationFail
                  Right (blockBS, remainingBS) -> do
                    result <- importBlockV1 blockBS tm logm f syncRunner
                    case result of
                      ResultSuccess -> loopV1 remainingBS
                      ResultPendingBlock -> do
                        -- this shouldn't happen
                        logm External LLWarning $ "Imported pending block."
                        loopV1 remainingBS
                      ResultDuplicate -> loopV1 remainingBS
                      err -> do -- stop processing at first error that we encounter.
                        logm External LLError $ "Error importing block: " ++ show err
                        return err
        blockGetter = do
          len <- getWord64be
          getByteString (fromIntegral len)

importBlockV1 :: ByteString
              -> UTCTime
              -> LogMethod IO
              -> (t -> PendingBlock -> IO UpdateResult)
              -> t
              -> IO UpdateResult
importBlockV1 blockBS tm logm f syncRunner =
  case deserializePendingBlockV1 blockBS tm of
    Left err -> do
      logm External LLError $ "Can't deserialize block: " ++ show err
      return ResultSerializationFail
    Right block -> f syncRunner block
