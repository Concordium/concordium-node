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

import Data.String

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Types
import Concordium.Runner
import Concordium.Show

import Data.Map(Map)
import qualified Data.Map as Map

import Interpreter.CallContract as I

import Data.List(intercalate)

transactions :: StdGen -> [Transaction]
transactions gen = trs 0 (randoms gen)
    where
      
        trs n (a : b : c : d : f : g : rs) =
          (Transaction (TransactionNonce a b c d) (Metadata (mkSender n)) (Update (mkAddress f) (mkMessage g))) : trs (n+1) rs
        mkSender n = BS.pack $ "Sender: " ++ show n
        mkAddress n = BS.pack $ show (n `mod` 2)
        mkMessage n = if n `mod` 9 == 0 then Decrement else Increment

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
        return (BakerInfo epk spk lot, BakerIdentity bid ssk spk esk epk)

relay :: Chan OutMessage -> Chan (Either Block FinalizationRecord) -> [Chan InMessage] -> IO ()
relay inp monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            case msg of
                MsgNewBlock block -> do
                    writeChan monitor (Left block)
                    forM_ outps $ \outp -> forkIO $ do
                        factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived block)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> forkIO $ do
                        factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^2) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationReceived bs)
                MsgFinalizationRecord fr -> do
                    writeChan monitor (Right fr)
                    forM_ outps $ \outp -> forkIO $ do
                        factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
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

gsToString = intercalate "\\l" . map (\(i, s) -> "(" ++ BS.unpack i ++ ", " ++ show s ++ ")") . Map.toList . instances

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
    trans <- transactions <$> newStdGen
    chans <- mapM (\(_, (_, bid)) -> do
        (cin, cout, out) <- makeRunner bid gen
        forkIO $ sendTransactions cin trans
        return (cin, cout, out)) bis
    monitorChan <- newChan
    mapM_ (\((_,cout, _), cs) -> forkIO $ relay cout monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let iState = initState 2
    let loop gsMap = do
            readChan monitorChan >>= \case
                Left block -> do
                    let bh = hashBlock block
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ "\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized block) ++ " [style=dotted];"
                Right fr ->
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
            --putStrLn (showsBlock block "")
            loop
    loop


    

