{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import GHC.Stack
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
import qualified Concordium.Getters as Get


nAccounts :: Int
nAccounts = 2

transactions :: StdGen -> [Transaction]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        contr i = ContractAddress (fromIntegral $ i `mod` nAccounts) 0
        trs n (a : b : rs) = Example.makeTransaction (a `mod` 9 /= 0) (contr b) n : trs (n+1) rs
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Chan (InMessage a) -> [Transaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived t)
        -- r <- randomRIO (5000, 15000)
        threadDelay 50000
        sendTransactions chan ts
sendTransactions _ _ = return ()

makeBaker :: BakerId -> LotteryPower -> IO (BakerInfo, BakerIdentity)
makeBaker bid lot = do
        ek@(VRF.KeyPair _ epk) <- VRF.newKeyPair
        sk                     <- Sig.newKeyPair
        let spk = Sig.verifyKey sk in 
            return (BakerInfo epk spk lot, BakerIdentity bid sk spk ek epk)

type Peer = IORef SkovFinalizationState

relay :: HasCallStack => Chan (OutMessage Peer) -> IORef SkovFinalizationState -> Chan (Either (BlockHash, BakedBlock, Maybe BlockState) FinalizationRecord) -> Chan (InMessage Peer) -> [Chan (InMessage Peer)] -> IO ()
relay inp sfsRef monitor loopback outps = loop
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
            case msg of
                MsgNewBlock block -> do
                    let bh = getHash block :: BlockHash
                    sfs <- readIORef sfsRef
                    bp <- runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (resolveBlock bh)
                    -- when (isNothing bp) $ error "Block is missing!"
                    writeChan monitor (Left (bh, block, bpState <$> bp))
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgBlockReceived sfsRef block)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> delayed $
                        writeChan outp (MsgFinalizationReceived sfsRef bs)
                MsgFinalizationRecord fr -> do
                    writeChan monitor (Right fr)
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgFinalizationRecordReceived sfsRef fr)
                MsgMissingBlock src bh -> do
                    mb <- Get.getBlockData src bh
                    case mb of
                        Just (NormalBlock bb) -> writeChan loopback (MsgBlockReceived src bb)
                        _ -> return ()
                MsgMissingFinalization src fin -> do
                    mf <- case fin of
                        Left bh -> Get.getBlockFinalization src bh
                        Right fi -> Get.getIndexedFinalization src fi
                    forM_ mf $ \fr -> writeChan loopback (MsgFinalizationRecordReceived src fr)
            loop

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
    let n = 10
    let bns = [1..n]
    let bakeShare = (1.0 / (fromInteger $ toInteger n))
    bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.5
                (Map.fromList [(i, b) | (i, (b, _)) <- bis])
    let fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _, _)) <- bis]
    now <- truncate <$> getPOSIXTime
    let gen = GenesisData now 1 bps fps
    let iState = Example.initialState nAccounts
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bix, (_, bid)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bix ++ ".log"
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    appendFile logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        (cin, cout, out) <- makeRunner logM bid gen iState
        _ <- forkIO $ sendTransactions cin trans
        return (cin, cout, out)) bis
    monitorChan <- newChan
    mapM_ (\((cin, cout, stateRef), cs) -> forkIO $ relay cout stateRef monitorChan cin ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
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

