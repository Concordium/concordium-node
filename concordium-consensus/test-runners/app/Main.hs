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

transactions :: StdGen -> [Transaction]
transactions gen = trs 0 (randoms gen)
    where
        trs n (a : b : c : d : rs) = (Transaction (TransactionNonce a b c d) (BS.pack ("Transaction " ++ show n))) : trs (n+1) rs

sendTransactions :: Chan InMessage -> [Transaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived t)
        r <- randomRIO (500000, 1500000)
        threadDelay r
        sendTransactions chan ts

makeBaker :: BakerId -> LotteryPower -> IO (BakerInfo, BakerIdentity)
makeBaker bid lot = do
        (esk, epk) <- VRF.newKeypair
        (ssk, spk) <- Sig.newKeypair
        return (BakerInfo epk spk lot, BakerIdentity bid ssk esk)

relay :: Chan OutMessage -> [Chan InMessage] -> IO ()
relay inp outps = loop
    where
        loop = do
            msg <- readChan inp
            case msg of
                MsgNewBlock block -> forM_ outps $ \outp -> writeChan outp (MsgBlockReceived block)
            loop

main :: IO ()
main = do
    (b1, bid1) <- makeBaker 1 0.5
    (b2, bid2) <- makeBaker 2 0.5
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.5
                (Map.fromList [(1, b1), (2, b2)])
    let fps = FinalizationParameters (Map.empty)
    now <- truncate <$> getPOSIXTime
    let gen = GenesisData now 10 bps fps
    (b1in, b1out) <- makeRunner bid1 gen
    (b2in, b2out) <- makeRunner bid2 gen
    trans <- transactions <$> newStdGen
    forkIO $ sendTransactions b1in trans
    forkIO $ sendTransactions b2in trans
    monitorChan <- newChan
    forkIO $ relay b1out [b2in, monitorChan]
    forkIO $ relay b2out [b1in, monitorChan]
    let loop = do
            r <- readChan monitorChan
            case r of
                MsgBlockReceived block -> putStrLn (showsBlock block "")
                _ -> return ()
            loop
    loop


    

