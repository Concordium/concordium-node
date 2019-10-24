{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, CPP #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX
import System.IO
import Lens.Micro.Platform
import Data.Serialize
import qualified Data.Map as Map

import Concordium.TimeMonad
import Concordium.Types.HashableTo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.SeedState
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instances
import Concordium.GlobalState.BlockState(BlockPointerData(..))
import Concordium.GlobalState.Implementation.TreeState
import Concordium.GlobalState.Implementation.BlockState
import Concordium.GlobalState.Implementation.Block (BakedBlock, Block(NormalBlock), getBlock)
import Concordium.GlobalState.Implementation

import Concordium.Types
import Concordium.Runner
import Concordium.Logger
import Concordium.Skov

import Concordium.Scheduler.Utils.Init.Example as Example

import Concordium.Startup

import Foreign.ForeignPtr

nContracts :: Int
nContracts = 2

transactions :: StdGen -> [BareTransaction]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        contr i = ContractAddress (fromIntegral $ i `mod` nContracts) 0
        trs n (a : b : rs) = Example.makeTransaction (a `mod` 9 /= 0) (contr b) n : trs (n+1) rs
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Chan (InMessage a) -> [BareTransaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived $ runPut $ put t)
        -- r <- randomRIO (5000, 15000)
        threadDelay 500000
        sendTransactions chan ts
sendTransactions _ _ = return ()

relay :: Chan (OutMessage src) -> MVar SkovBufferedHookedState -> Chan (Either (BlockHash, BakedBlock, Maybe BlockState) FinalizationRecord) -> [Chan (InMessage ())] -> IO ()
relay inp sfsRef monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            now <- currentTime
            case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            sfs <- readMVar sfsRef
                            bp <- runSilentLogger $ flip evalSkovQueryM (sfs ^. skov) (resolveBlock bh)
                            -- when (isNothing bp) $ error "Block is missing!"
                            writeChan monitor (Left (bh, block, bpState <$> bp))
                        _ -> return ()
                    forM_ outps $ \outp -> forkIO $ do
                        --factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived () blockBS)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationReceived () bs)
                MsgFinalizationRecord fr -> do
                    case runGet get fr of
                        Right fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.1 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationRecordReceived () fr)
                _ -> return ()
            loop

removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

gsToString :: BlockState -> String
gsToString gs = (show (currentSeed (gs ^.  blockBirkParameters ^. birkSeedState))) ++ 
                    "\n current: " ++ showBakers ( (gs ^. blockBirkParameters ^. birkCurrentBakers)) ++
                    "\n prev   : " ++ showBakers ( (gs ^. blockBirkParameters ^. birkPrevEpochBakers)) ++
                    "\n lottery: " ++ showBakers ( (gs ^. blockBirkParameters ^. birkLotteryBakers))
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nContracts-1)
        showBakers bs = show [ _bakerStake binfo | (_, binfo) <- Map.toList (_bakerMap bs)]

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

main :: IO ()
main = do
    let n = 5
    now <- truncate <$> getPOSIXTime
    let (gen, bis) = makeGenesisData (now) n 1 0.5 0 dummyCryptographicParameters dummyIdentityProviders []
    let iState = Example.initialState (genesisBirkParameters gen) (genesisCryptographicParameters gen) (genesisAccounts gen) [] nContracts
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bakerId, (bid, _)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bakerId ++ ".log"

        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    appendFile logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        let logTransferFile = "transfer-log-" ++ show now ++ "-" ++ show bakerId ++ ".transfers"
        let logT bh slot reason = do
              appendFile logTransferFile (show (bh, slot, reason))
              appendFile logTransferFile "\n"
#ifdef RUST
        gsptr <- makeEmptyGlobalState gen
        (cin, cout, out) <- makeAsyncRunner logM (Just logT) bid defaultRuntimeParameters gen iState gsptr
#else
        (cin, cout, out) <- makeAsyncRunner logM (Just logT) bid defaultRuntimeParameters gen iState
#endif
        _ <- forkIO $ sendTransactions cin trans
        return (cin, cout, out)) (zip [(0::Int) ..] bis)
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
