{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time.Clock.POSIX
import System.IO
import Data.IORef
import Lens.Micro.Platform
import Data.List(intercalate)

import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instances
import Concordium.GlobalState.TreeState(BlockPointerData(..))
import Concordium.GlobalState.TreeState.Basic
import Concordium.Scheduler.Utils.Init.Example as Example

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Birk.Bake
import Concordium.Types
import Concordium.Runner
import Concordium.Logger
import Concordium.Skov
import Concordium.Afgjort.Finalize(FinalizationPoint)
import qualified Concordium.Getters as Get

import Concordium.Startup


type Peer = IORef SkovFinalizationState

data InEvent
    = IEMessage (InMessage Peer)
    | IECatchupFinalization FinalizationPoint Bool (Chan InEvent)

nAccounts :: Int
nAccounts = 2

transactions :: StdGen -> [Transaction]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        contr i = ContractAddress (fromIntegral $ i `mod` nAccounts) 0
        trs n (a : b : rs) = Example.makeTransaction (a `mod` 9 /= 0) (contr b) n : trs (n+1) rs
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Chan (InEvent) -> [Transaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (IEMessage $ MsgTransactionReceived t)
        -- r <- randomRIO (5000, 15000)
        threadDelay 50000
        sendTransactions chan ts
sendTransactions _ _ = return ()

makeBaker :: BakerId -> LotteryPower -> IO (BakerInfo, BakerIdentity, Account)
makeBaker bid lot = do
        ek@(VRF.KeyPair _ epk) <- VRF.newKeyPair
        sk                     <- Sig.newKeyPair
        let spk = Sig.verifyKey sk
        let account = makeBakerAccount bid
        return (BakerInfo epk spk lot (_accountAddress account), BakerIdentity bid sk spk ek epk, account)

relayIn :: Chan InEvent -> Chan (InMessage Peer) -> IORef SkovFinalizationState -> IORef Bool -> IO ()
relayIn msgChan bakerChan sfsRef connectedRef = loop
    where
        loop = do
            msg <- readChan msgChan
            connected <- readIORef connectedRef
            when connected $ case msg of
                IEMessage imsg -> writeChan bakerChan imsg
                IECatchupFinalization fp reciprocate chan -> do
                    finMsgs <- Get.getFinalizationMessages sfsRef fp
                    forM_ finMsgs $ writeChan chan . IEMessage . MsgFinalizationReceived sfsRef
                    when reciprocate $ do
                        myFp <- Get.getFinalizationPoint sfsRef
                        writeChan chan $ IECatchupFinalization myFp False msgChan
            loop


relay :: Chan (OutMessage Peer) -> IORef SkovFinalizationState -> IORef Bool -> Chan (Either (BlockHash, BakedBlock, Maybe BlockState) FinalizationRecord) -> Chan InEvent -> [Chan InEvent] -> IO ()
relay inp sfsRef connectedRef monitor loopback outps = loop
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
            when connected $ case msg of
                MsgNewBlock block -> do
                    let bh = getHash block :: BlockHash
                    sfs <- readIORef sfsRef
                    bp <- runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (resolveBlock bh)
                    -- when (isNothing bp) $ error "Block is missing!"
                    writeChan monitor (Left (bh, block, bpState <$> bp))
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (IEMessage $ MsgBlockReceived sfsRef block)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> delayed $
                        writeChan outp (IEMessage $ MsgFinalizationReceived sfsRef bs)
                MsgFinalizationRecord fr -> do
                    writeChan monitor (Right fr)
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (IEMessage $ MsgFinalizationRecordReceived sfsRef fr)
                MsgMissingBlock src bh 0 -> do
                    mb <- Get.getBlockData src bh
                    case mb of
                        Just (NormalBlock bb) -> writeChan loopback (IEMessage $ MsgBlockReceived src bb)
                        _ -> return ()
                MsgMissingBlock src bh delta -> do
                    mb <- Get.getBlockDescendant src bh delta
                    case mb of
                        Just (NormalBlock bb) -> writeChan loopback (IEMessage $ MsgBlockReceived src bb)
                        _ -> return ()
                MsgMissingFinalization src fin -> do
                    mf <- case fin of
                        Left bh -> Get.getBlockFinalization src bh
                        Right fi -> Get.getIndexedFinalization src fi
                    forM_ mf $ \fr -> writeChan loopback (IEMessage $ MsgFinalizationRecordReceived src fr)
            loop

toggleConnection :: LogMethod IO -> IORef SkovFinalizationState -> IORef Bool -> Chan InEvent -> [Chan InEvent] -> IO ()
toggleConnection logM sfsRef connectedRef loopback outps = readIORef connectedRef >>= loop
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
                fp <- Get.getFinalizationPoint sfsRef
                forM_ outps $ \outp -> writeChan outp (IECatchupFinalization fp True loopback)
                loop True


removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

gsToString :: BlockState -> String
gsToString gs = intercalate "\\l" . map show $ keys
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nAccounts-1)

main :: IO ()
main = do
    let n = 20
    let bns = [1..n]
    let bakeShare = (1.0 / (fromInteger $ toInteger n))
    bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.5
                (Map.fromList [(i, b) | (i, (b, _, _)) <- bis])
    let fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _ _, _, _)) <- bis]
    now <- truncate <$> getPOSIXTime
    let bakerAccounts = map (\(_, (_, _, acc)) -> acc) bis
    let gen = GenesisData now 1 bps bakerAccounts fps
    let iState = Example.initialState bps bakerAccounts nAccounts
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bix, (_, bid, _)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bix ++ ".log"
        logChan <- newChan
        let logLoop = do
                logMsg <- readChan logChan
                appendFile logFile logMsg
                logLoop
        _ <- forkIO logLoop
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    writeChan logChan $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        (cin, cout, out) <- makeRunner logM bid gen iState
        cin' <- newChan
        connectedRef <- newIORef True
        _ <- forkIO $ relayIn cin' cin out connectedRef
        _ <- forkIO $ sendTransactions cin' trans
        return (cin', cout, out, connectedRef, logM)) bis
    monitorChan <- newChan
    forM_ (removeEach chans) $ \((cin, cout, stateRef, connectedRef, logM), cs) -> do
        let cs' = ((\(c, _, _, _, _) -> c) <$> cs)
        _ <- forkIO $ toggleConnection logM stateRef connectedRef cin cs'
        forkIO $ relay cout stateRef connectedRef monitorChan cin cs'
    let loop = do
            readChan monitorChan >>= \case
                Left (bh, block, gs') -> do
                    let ts = blockTransactions block
                    let stateStr = case gs' of
                                    Nothing -> ""
                                    Just gs -> gsToString gs
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker $ bbFields block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ stateStr ++ "\\l\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer $ bbFields block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized $ bbFields block) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop

