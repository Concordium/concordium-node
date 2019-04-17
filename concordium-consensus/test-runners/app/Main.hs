{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import GHC.Stack
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time.Clock.POSIX
import System.IO
import Data.IORef
import Data.String
import Lens.Micro.Platform
import Data.Maybe

import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instances

import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Birk.Bake
import Concordium.Types
import Concordium.Runner
import Concordium.Show
import Concordium.Logger
import Concordium.Skov.Monad
import Concordium.MonadImplementation

import qualified Data.HashMap.Strict as HashMap

import Data.List(intercalate)

import Concordium.Scheduler.Utils.Init.Example(makeTransaction, initialState)
import Concordium.Types

import Concordium.Crypto.SHA256

nAccounts :: Int
nAccounts = 2

transactions :: StdGen -> [Transaction]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        contr i = ContractAddress (fromIntegral $ i `mod` nAccounts) 0
        trs n (a : b : rs) = makeTransaction (a `mod` 9 /= 0) (contr b) n : trs (n+1) rs

sendTransactions :: Chan InMessage -> [Transaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived t)
        -- r <- randomRIO (5000, 15000)
        threadDelay 50000
        sendTransactions chan ts

makeBaker :: BakerId -> LotteryPower -> IO (BakerInfo, BakerIdentity)
makeBaker bid lot = do
        ek@(VRF.KeyPair _ epk) <- VRF.newKeyPair
        sk@(Sig.KeyPair _ spk) <- Sig.newKeyPair
        return (BakerInfo epk spk lot, BakerIdentity bid sk spk ek epk)

relay :: HasCallStack => Chan OutMessage -> IORef SkovFinalizationState -> Chan (Either (BlockHash, Block, Maybe BlockState) FinalizationRecord) -> [Chan InMessage] -> IO ()
relay inp sfsRef monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            case msg of
                MsgNewBlock block -> do
                    let bh = getHash block :: BlockHash
                    sfs <- readIORef sfsRef
                    bp <- runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (resolveBlock bh)
                    -- when (isNothing bp) $ error "Block is missing!"
                    writeChan monitor (Left (bh, block, bpState <$> bp))
                    forM_ outps $ \outp -> forkIO $ do
                        --factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived block)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationReceived bs)
                MsgFinalizationRecord fr -> do
                    writeChan monitor (Right fr)
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationRecordReceived fr)
            loop

removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re l [] = []

gsToString :: BlockState -> String
gsToString gs = intercalate "\\l" . map show $ keys
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, imodel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nAccounts-1)

main :: IO ()
main = do
    let n = 5
    let bns = [1..n]
    let bakeShare = (1.0 / (fromInteger $ toInteger n))
    bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.5
                (Map.fromList [(i, b) | (i, (b, _)) <- bis])
    let fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _, _)) <- bis]
    now <- truncate <$> getPOSIXTime
    let gen = GenesisData now 1 bps fps
    let iState = initialState nAccounts
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bix, (_, bid)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bix ++ ".log"
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    appendFile logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        (cin, cout, out) <- makeRunner logM bid gen iState
        forkIO $ sendTransactions cin trans
        return (cin, cout, out)) bis
    monitorChan <- newChan
    mapM_ (\((_,cout, stateRef), cs) -> forkIO $ relay cout stateRef monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let loop = do
            readChan monitorChan >>= \case
                Left (bh, block, gs') -> do
                    let ts = blockTransactions block
                    let stateStr = case gs' of
                                    Nothing -> ""
                                    Just gs -> gsToString gs
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ stateStr ++ "\\l\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized block) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop

