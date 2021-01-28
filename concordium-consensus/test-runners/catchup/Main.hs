{-# LANGUAGE
    OverloadedStrings,
    TypeFamilies,
    CPP #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX
import System.IO
import Data.IORef
import Data.Serialize
import Data.Word
import System.Directory

import Concordium.TimerMonad
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instance
import Concordium.Types.AnonymityRevokers

import Concordium.GlobalState.BlockState
import Concordium.GlobalState
import Concordium.GlobalState.DummyData (dummyAuthorizations)

import Concordium.Types
import Concordium.Runner
import Concordium.Logger
import Concordium.Skov
import Concordium.Getters
import Concordium.Afgjort.Finalize (FinalizationPseudoMessage(..),FinalizationInstance(..))
import Concordium.Birk.Bake
import Concordium.Kontrol (currentTimestamp)

import Concordium.Startup
import qualified Concordium.Types.DummyData as Dummy
import qualified Concordium.GlobalState.DummyData as Dummy
import qualified Concordium.Crypto.DummyData as Dummy

type TreeConfig = DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ DTDBConfig rt genData

type ActiveConfig = SkovConfig TreeConfig (BufferedFinalization ThreadTimer) NoHandler

newtype Peer = Peer {
    peerChan :: Chan (InMessage Peer)
}

nContracts :: Int
nContracts = 2

transactions :: StdGen -> [BlockItem]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Word8])
    where
        trs n (amnt:amnts)= Dummy.makeTransferTransaction (Dummy.mateuszKP, Dummy.mateuszAccount) Dummy.mateuszAccount (fromIntegral amnt) n : trs (n+1) amnts
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Chan (InMessage Peer) -> [BlockItem] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived $ runPut (putVersionedBlockItemV0 t))
        -- r <- randomRIO (5000, 15000)
        threadDelay 100000
        sendTransactions chan ts
sendTransactions _ _ = return ()

{-
relayIn :: Chan InEvent -> Chan (InMessage Peer) -> MVar SkovBufferedHookedState -> IORef Bool -> IO ()
relayIn msgChan bakerChan sfsRef connectedRef = loop
    where
        loop = do
            msg <- readChan msgChan
            connected <- readIORef connectedRef
            when connected $ case msg of
                IEMessage imsg -> writeChan bakerChan imsg
                IECatchUpRequired peer -> undefined
{-}                IECatchupFinalization fpBS reciprocate chan -> do
                    case runGet get fpBS of
                        Right fp -> do
                            finMsgs <- Get.getFinalizationMessages sfsRef fp
                            forM_ finMsgs $ writeChan chan . IEMessage . MsgFinalizationReceived sfsRef . runPut . put
                            when reciprocate $ do
                                myFp <- runPut . put <$> Get.getFinalizationPoint sfsRef
                                writeChan chan $ IECatchupFinalization myFp False msgChan
                        Left _ -> return () -}
            loop
-}


relay :: Peer -> Chan (OutMessage Peer) -> SyncRunner ActiveConfig -> IORef Bool -> Chan (Either (BlockHash, BakedBlock, [Instance]) FinalizationRecord) -> Chan (InMessage Peer) -> [Chan (InMessage Peer)] -> IO ()
relay myPeer inp sr connectedRef monitor _loopback outps = loop
    where
        chooseDelay = do
            factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
            truncate . (*(factor :: Double)) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
        delayed a = void $ forkIO $ do
            threadDelay =<< chooseDelay
            a
        usually a = do
            -- Do the action most of the time, but randomly don't do it.
            r <- randomRIO (0,9::Int)
            unless (r == 0) a
        loop = do
            msg <- readChan inp
            connected <- readIORef connectedRef
            now <- getTransactionTime
            -- If we're connected, relay the message as required
            if connected then case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getExactVersionedBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
                        _ -> return ()
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgBlockReceived myPeer blockBS)
                MsgFinalization bs ->
                    -- We drop all finalization messages except catch-ups
                    case decode bs of
                        Right (FPMCatchUp _) ->
                            forM_ outps $ \outp -> delayed $
                                writeChan outp (MsgFinalizationReceived myPeer bs)
                        _ -> return ()
                MsgFinalizationRecord fr -> do
                    case runGet get fr of
                        Right fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgFinalizationRecordReceived myPeer fr)
                MsgCatchUpRequired target -> do
                    cur <- runStateQuery sr (getCatchUpStatus True)
                    delayed $ writeChan (peerChan target) (MsgCatchUpStatusReceived myPeer (encode cur))
                MsgDirectedBlock target b -> usually $ delayed $ writeChan (peerChan target) (MsgBlockReceived myPeer b)
                MsgDirectedFinalizationRecord target fr -> usually $ delayed $ writeChan (peerChan target) (MsgFinalizationReceived myPeer fr)
                MsgDirectedCatchUpStatus target cu -> usually $ delayed $ writeChan (peerChan target) (MsgCatchUpStatusReceived myPeer cu)
            -- If we're not connected, don't relay, but still send to the monitor channel
            else case msg of
                MsgNewBlock blockBS ->
                    case runGet (getExactVersionedBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
                        _ -> return ()
                MsgFinalizationRecord fr -> case runGet get fr of
                        Right fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                _ -> return ()
            loop
        bInsts bh = do
            bst <- resolveBlock bh
            case bst of
                Nothing -> return []
                Just bs -> getContractInstanceList =<< queryBlockState bs

toggleConnection :: LogMethod IO -> SyncRunner ActiveConfig -> IORef Bool -> Chan (InMessage Peer) -> [Chan (InMessage Peer)] -> IO ()
toggleConnection logM _sr connectedRef _loopback _outps = readIORef connectedRef >>= loop
    where
        loop connected = do
            delay <- (^(2::Int)) <$> randomRIO (if connected then (3200,7800) else (0,4500))
            threadDelay delay
            tid <- myThreadId
            if connected then do
                putStrLn $ "// " ++ show tid ++ ": toggle off"
                logM External LLInfo $ "Disconnected"
                writeIORef connectedRef False
                loop False
            else do
                -- Reconnect
                putStrLn $ "// " ++ show tid ++ ": toggle on"
                logM External LLInfo $ "Reconnected"
                writeIORef connectedRef True
                -- TODO: could force catch up with peers here
                loop True


removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

{-
gsToString :: PersistentBlockState -> String
gsToString gs = intercalate "\\l" . map show $ keys
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nContracts-1)
-}

dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders = emptyIdentityProviders

emptyArs :: AnonymityRevokers
emptyArs = emptyAnonymityRevokers

main :: IO ()
main = do
    let n = 3
    now <- currentTimestamp
    let (gen, bis) =
          makeGenesisData now n 100
            defaultFinalizationParameters{finalizationMinimumSkip = 1}
            Dummy.dummyCryptographicParameters
            dummyIdentityProviders
            emptyArs
            [Dummy.createCustomAccount 1000000000000 Dummy.mateuszKP Dummy.mateuszAccount]
            (Energy maxBound)
            dummyAuthorizations
            (makeChainParameters (makeElectionDifficulty 0.5) 1 1 4 10 Dummy.dummyRewardParameters (fromIntegral n))
    trans <- transactions <$> newStdGen
    createDirectoryIfMissing True "data"
    chans <- mapM (\(bakerId, (bid, _)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bakerId ++ ".log"
        logChan <- newChan
        let logLoop = do
                logMsg <- readChan logChan
                appendFile logFile logMsg
                logLoop
        _ <- forkIO logLoop
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    writeChan logChan $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        gsconfig <- makeGlobalStateConfig (defaultRuntimeParameters { rpTreeStateDir = "data/treestate-" ++ show now ++ "-" ++ show bakerId, rpBlockStateFile = "data/blockstate-" ++ show now ++ "-" ++ show bakerId }) gen
        let
            finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid))
            hconfig = NoHandler
            config = SkovConfig gsconfig finconfig hconfig
        (cin, cout, sr) <- makeAsyncRunner logM bid config
        -- cin' <- newChan
        connectedRef <- newIORef True
        -- _ <- forkIO $ relayIn cin' cin stateRef connectedRef
        _ <- forkIO $ sendTransactions cin trans
        return (cin, cout, sr, connectedRef, logM)) (zip [0::Int ..] bis)
    monitorChan <- newChan
    forM_ (removeEach chans) $ \((cin, cout, sr, connectedRef, logM), cs) -> do
        let cs' = (\(c, _, _, _, _) -> c) <$> cs
        when True $ do
            _ <- forkIO $ toggleConnection logM sr connectedRef cin cs'
            return ()
        forkIO $ relay (Peer cin) cout sr connectedRef monitorChan cin cs'
    let loop =
            readChan monitorChan >>= \case
                Left (bh, block, _) -> do
                    let ts = blockTransactions block
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker $ bbFields block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer $ bbFields block) ++ ";"
                    case (blockFinalizationData block) of
                        NoFinalizationData -> return ()
                        BlockFinalizationData fr ->
                            putStrLn $ " n" ++ show bh ++ " -> n" ++ show (finalizationBlockPointer fr) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop
