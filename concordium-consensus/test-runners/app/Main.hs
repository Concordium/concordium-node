{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time.Clock.POSIX
import System.IO

import Data.String

import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Types
import Concordium.Runner
import Concordium.Show

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Data.HashMap.Strict as HashMap

import Data.List(intercalate)

import Acorn.Utils.Init.Example(update)
import Acorn.Types(lState, instances, BlockResult(..), instances)

import Data.Maybe(fromJust)

nAccounts = 2

transactions :: StdGen -> [Transaction]
transactions gen = trs 0 (randoms gen)
    where
        trs n (a : b : c : d : f : g : rs) = let (meta, payload) = update f (g `mod` fromIntegral nAccounts)
                                             in (Transaction (TransactionNonce a b c d) meta payload) : trs (n+1) rs

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

relay :: Chan OutMessage -> Chan (Either Block FinalizationRecord) -> [Chan InMessage] -> IO ()
relay inp monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            case msg of
                MsgNewBlock block -> do
                    writeChan monitor (Left block)
                    forM_ outps $ \outp -> forkIO $ do
                        --factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived block)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationReceived bs)
                MsgFinalizationRecord fr -> do
                    writeChan monitor (Right fr)
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 1
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

gsToString gs = let keys = map (\n -> (n, lState $ fromJust (HashMap.lookup (fromString ("Tid-" ++ show n)) (instances gs)))) $ enumFromTo 0 (nAccounts-1)
                in intercalate "\\l" . map show $ keys

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
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bix, (_, bid)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bix ++ ".log"
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    appendFile logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        (cin, cout, out) <- makeRunner logM bid gen
        forkIO $ sendTransactions cin trans
        return (cin, cout, out)) bis
    monitorChan <- newChan
    mapM_ (\((_,cout, _), cs) -> forkIO $ relay cout monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let iState = initState nAccounts
    let loop gsMap = do
            readChan monitorChan >>= \case
                Left block -> do
                    let bh = hashBlock block
                    let (Just ts) = (toTransactions (blockData block))
                    let gs = Map.findWithDefault iState (blockPointer block) gsMap
                    let BlockSuccess _ gs' = executeBlockForState ts gs
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ gsToString gs' ++ "\\l\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized block) ++ " [style=dotted];"
                    hFlush stdout
                    loop (Map.insert bh gs' gsMap)
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop gsMap
    loop Map.empty

