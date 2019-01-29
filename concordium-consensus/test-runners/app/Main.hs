{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time.Clock.POSIX

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Types
import Concordium.Runner
import Concordium.Show

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(intercalate)

import Init(update)
import Interpreter(lState, instances)
import Scheduler(BlockResult(BlockSuccess))

import Data.Maybe(fromJust)

transactions :: StdGen -> [Transaction]
transactions gen = trs 0 (randoms gen)
    where
        trs n (a : b : c : d : f : g : rs) = let (meta, payload) = update f (g `mod` 10)
                                             in (Transaction (TransactionNonce a b c d) meta payload) : trs (n+1) rs
        mkSender n = BS.pack $ "Sender: " ++ show n
        mkAddress n = BS.pack $ show (n `mod` 2)

sendTransactions :: Chan InMessage -> [Transaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived t)
        r <- randomRIO (50000, 150000)
        threadDelay r
        sendTransactions chan ts

makeBaker :: BakerId -> LotteryPower -> IO (BakerInfo, BakerIdentity)
makeBaker bid lot = do
        (esk, epk) <- VRF.newKeypair
        (ssk, spk) <- Sig.newKeypair
        return (BakerInfo epk spk lot, BakerIdentity bid ssk esk)

relay :: Chan OutMessage -> Chan Block -> [Chan InMessage] -> IO ()
relay inp monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            case msg of
                MsgNewBlock block -> do
                    writeChan monitor block
                    forM_ outps $ \outp -> forkIO $ do
                        r <- (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived block)
            loop

removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re l [] = []

gsToString gs = let keys = map (\n -> (n, lState $ fromJust (Map.lookup ("Tid-" ++ show n) (instances gs)))) $ enumFromTo 0 9
                in intercalate "\\l" . map show $ keys

main :: IO ()
main = do
    let n = 10
    let bns = [1..n]
    let bakeShare = (1.0 / (fromInteger $ toInteger n))
    bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.5
                (Map.fromList [(i, b) | (i, (b, _)) <- bis])
    let fps = FinalizationParameters (Map.empty)
    now <- truncate <$> getPOSIXTime
    let gen = GenesisData now 1 bps fps
    trans <- transactions <$> newStdGen
    chans <- mapM (\(_, (_, bid)) -> do
        (cin, cout) <- makeRunner bid gen
        forkIO $ sendTransactions cin trans
        return (cin, cout)) bis
    monitorChan <- newChan
    mapM_ (\((_,cout), cs) -> forkIO $ relay cout monitorChan (fst <$> cs)) (removeEach chans)
    let iState = initState 10
    let loop gsMap = do
            block <- readChan monitorChan
            let bh = hashBlock block
            case toTransactions (blockData block) of
              Just ts -> let gs = Map.findWithDefault iState (blockPointer block) gsMap
                         in case executeBlock ts gs of
                              BlockSuccess _ gs' -> do
                                putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ "\\l" ++ gsToString gs' ++ "\\l\"];"
                                putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                                -- putStrLn (showsBlock block "")
                                loop (Map.insert bh gs' gsMap)
    loop Map.empty
