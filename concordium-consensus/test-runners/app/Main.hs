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

import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Types.HashableTo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Basic.Block
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
#ifdef RUST
import qualified Concordium.GlobalState.Implementation as Rust
#endif

import Concordium.Types
import Concordium.Runner
import Concordium.Logger
import Concordium.Skov
import Concordium.Getters
import Concordium.Afgjort.Finalize (FinalizationPseudoMessage(..),FinalizationInstance(..))
import Concordium.Birk.Bake

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
        threadDelay 50000
        sendTransactions chan ts
sendTransactions _ _ = return ()

relay :: Chan (OutMessage src) -> SyncRunner ActiveConfig -> Chan (Either (BlockHash, BakedBlock, [Instance]) FinalizationRecord) -> [Chan (InMessage ())] -> IO ()
relay inp sr monitor outps = loop
    where
        loop = do
            msg <- readChan inp
            now <- currentTime
            case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
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
        bInsts bh = do
            bst <- resolveBlock bh
            case bst of
                Nothing -> return []
                Just bs -> getContractInstanceList (bpState bs)

removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

{-
gsToString :: BlockState -> String
gsToString gs = show (gs ^.  blockBirkParameters ^. seedState )
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nContracts-1)
-}

dummyIdentityProviders :: [IdentityProviderData]
dummyIdentityProviders = []

genesisState :: GenesisData -> Basic.BlockState
genesisState genData = Basic.initialState
                       (genesisBirkParameters genData)
                       (genesisCryptographicParameters genData)
                       (genesisAccounts genData ++ genesisSpecialBetaAccounts genData)
                       (genesisIdentityProviders genData)
                       (genesisMintPerSlot genData)


#ifdef RUST
type TreeConfig = DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = do
    gsptr <- Rust.makeEmptyGlobalState genData
    return $ DTDBConfig rt genData (genesisState genData) gsptr
#else
type TreeConfig = MemoryTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ MTDBConfig rt genData (genesisState genData)
#endif

type ActiveConfig = SkovConfig TreeConfig (BufferedFinalization ThreadTimer) HookLogHandler


main :: IO ()
main = do
    let n = 10
    now <- truncate <$> getPOSIXTime
    let (gen, bis) = makeGenesisData (now + 10) n 1 0.5 0 dummyCryptographicParameters dummyIdentityProviders []
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
        gsconfig <- makeGlobalStateConfig defaultRuntimeParameters gen
        let
            finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)) gen
            hconfig = HookLogHandler (Just logT)
            config = SkovConfig gsconfig finconfig hconfig
        (cin, cout, sr) <- makeAsyncRunner logM bid config
        _ <- forkIO $ sendTransactions cin trans
        return (cin, cout, sr)) (zip [(0::Int) ..] bis)
    monitorChan <- newChan
    mapM_ (\((_,cout, sr), cs) -> forkIO $ relay cout sr monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let loop = do
            readChan monitorChan >>= \case
                Left (bh, block, gs') -> do
                    let ts = blockTransactions block
                    let stateStr = show gs'

                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ stateStr ++ "\\l\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized block) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop
