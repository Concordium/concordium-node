{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Time.Clock.POSIX
import System.IO

import Data.String

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Types
import Concordium.Runner
import Concordium.Show

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Data.HashMap.Strict as HashMap

import Data.List(intercalate)

import Concordium.Scheduler.Utils.Init.Example(makeTransaction, initialState)
-- import Concordium.Scheduler.Types(lState, instances, instances)
import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization

import Data.Maybe(fromJust)

import Data.Serialize
import Data.Serialize.Get
import Network.Simple.TCP

import Concordium.Getters as G

import Debug.Trace

main :: IO ()
main = return ()

{-

nAccounts = 2

sendTransaction :: Chan InMessage -> BS.ByteString -> IO ()
sendTransaction chan st = do
  let mt = runGet (do h <- get
                      b <- get
                      return (h, b)) st
  case mt of
    Left err -> fail $ "Error decoding transaction: " ++ err
    Right (h, b) -> do putStrLn "Decoded transaction with header"
                       print h
                       let sha = (SHA256.hash st)
                       print sha
                       writeChan chan (MsgTransactionReceived (Transaction (TransactionNonce sha) h b ))


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

gsToString gs = let keys = map (\n -> (n, lState $ (instances gs) HashMap.! ContractAddress (fromIntegral n) 0)) $ enumFromTo 0 (nAccounts-1)
                in intercalate "\\l" . map show $ keys

receiveMessage :: Socket -> IO (Maybe BS.ByteString)
receiveMessage ch = do
  msl <- recv ch 4
  case msl of
    Nothing -> return Nothing
    Just sl -> case runGet getWord32be sl of
                 Left err -> fail $ "Cannot decode length: " ++ err
                 Right len -> do recv ch (fromIntegral len)

sendMessage :: Socket -> BS.ByteString -> IO ()
sendMessage ch bs = let l = runPut (putWord32be (fromIntegral (BS.length bs)))
                    in sendLazy ch $ BSL.fromChunks [l, bs]

main :: IO ()
main = do
    let n = 5
    let bns = [1..n]
    let bakeShare = (1.0 / (fromInteger $ toInteger n))
    bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
    let bps = BirkParameters (BS.pack "LeadershipElectionNonce") 0.8
                (Map.fromList [(i, b) | (i, (b, _)) <- bis])
    let fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _, _)) <- bis]
    now <- truncate <$> getPOSIXTime
    let gen = GenesisData now 1 bps fps
    chans <- mapM (\(n, (_, bid)) -> do
        (cin, cout, out) <- makeRunner (\_ _ _ -> return ()) bid gen

        forkIO $ serve (Host "127.0.0.1") (show (1330 - n)) $ \(connectionSocket, _) -> do
          maddr <- receiveMessage connectionSocket
          case maddr of
            Nothing -> return ()
            Just maddr -> case runGet get maddr of
                            Left _ -> return ()
                            Right caddr -> do r <- G.getLastFinalContractInfo out caddr
                                              case r of
                                                Nothing -> return ()
                                                Just x -> sendMessage connectionSocket (runPut (put x))

        forkIO $ serve (Host "127.0.0.1") (show (1335 - n)) $ \(connectionSocket, _) -> do
          maddr <- receiveMessage connectionSocket
          case maddr of
            Nothing -> return ()
            Just maddr -> case runGet get maddr of
                            Left _ -> return ()
                            Right caddr -> do r <- G.getLastFinalAccountInfo out caddr
                                              case r of
                                                Nothing -> return ()
                                                Just x -> sendMessage connectionSocket (runPut (put x))

        forkIO $ serve (Host "127.0.0.1") (show (1345 - n)) $ (\(connectionSocket, remoteAddr) -> do
           sendMessage connectionSocket . runPut . put =<< (G.getLastFinalInstances out))

        forkIO $ serve (Host "127.0.0.1") (show (1340 - n)) $ (\(connectionSocket, remoteAddr) ->
           sendMessage connectionSocket . runPut . put =<< (G.getLastFinalAccountList out))

        forkIO $ serve (Host "127.0.0.1") (show $ 1345 + n) $ \(connectionSocket, remoteAddr) -> do
          do r <- receiveMessage connectionSocket
             case r of
               Just r' -> sendTransaction cin r'
               Nothing -> return ()
        return (cin, cout, out)) bis
    monitorChan <- newChan
    mapM_ (\((_,cout, _), cs) -> forkIO $ relay cout monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let iState = initialState nAccounts
    let loop gsMap = do
            readChan monitorChan >>= \case
                Left block -> do
                    let bh = hashBlock block
                    let (Just ts) = (toTransactions (blockData block))
                    when (length ts > 0) $ do
                        print (Concordium.Types.blockBaker block)
                        print bh
                      
                    -- let gs = Map.findWithDefault iState (blockPointer block) gsMap
                    -- let Right gs' = executeBlockForState ts (makeChainMeta (blockSlot block) undefined undefined) gs
                    -- putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ gsToString gs' ++ "\\l\"];"
                    -- putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    -- putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized block) ++ " [style=dotted];"
                    -- hFlush stdout
                    loop Map.empty
                Right fr -> do
                  -- putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop gsMap
    loop Map.empty

-}
