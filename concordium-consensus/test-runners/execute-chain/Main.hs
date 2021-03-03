{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Control.Exception
-- import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Sequence as Seq
import Data.Serialize
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import System.Environment
import System.IO
import System.Clock

import Concordium.Logger
import Concordium.TimerMonad
import Concordium.Types.Execution (tsEnergyCost)
-- import Concordium.Types.HashableTo
import Concordium.Types.ProtocolVersion

import Concordium.GlobalState
-- import Concordium.GlobalState.Basic.BlockState (getBlockState)
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState

-- import Concordium.GlobalState.Paired
import Concordium.GlobalState.Parameters
-- import Concordium.GlobalState.Persistent.BlobStore
-- import Concordium.GlobalState.Persistent.BlockState (hpbsHash, makePersistent)
import Concordium.GlobalState.TreeState
import Concordium.Kontrol (currentTimestamp)
import Concordium.Kontrol.BestBlock
import Concordium.Skov

-- |Protocol version
type PV = 'P1

-- type TreeConfig = DiskTreeDiskBlockConfig PV
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData PV -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ DTDBConfig rt genData

type TreeConfig = MemoryTreeDiskBlockConfig PV
makeGlobalStateConfig :: RuntimeParameters -> GenesisData PV -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ MTDBConfig rt genData

-- type TreeConfig = MemoryTreeMemoryBlockConfig PV
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData PV -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ MTMBConfig rt genData

--uncomment if wanting paired config
-- type TreeConfig = PairGSConfig (MemoryTreeMemoryBlockConfig PV) (DiskTreeDiskBlockConfig PV)
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData PV -> IO TreeConfig
-- makeGlobalStateConfig rp genData =
--     return $ PairGSConfig (MTMBConfig rp genData, DTDBConfig rp genData)

type ActiveConfig = SkovConfig PV TreeConfig (BufferedFinalization ThreadTimer) LogUpdateHandler

parseArgs :: [String] -> IO (GenesisData PV, FilePath)
parseArgs [gdPath, blocksPath] = do
    gdfile <- LBS.readFile gdPath
    gd <- case runGetLazy getVersionedGenesisData gdfile of
        Left err -> error err
        Right gd -> return gd
    -- blocks <- LBS.readFile blocksPath
    return (gd, blocksPath)
parseArgs _ = error "Expected exactly two arguments: genesis data file, and blocks to execute file"

main :: IO ()
main = do
    (genesisData, blocks) <- parseArgs =<< getArgs
    now <- currentTimestamp
    logFile <- openFile ("consensus-" ++ show now ++ ".log") WriteMode
    let logM src lvl msg = {- when (lvl == LLInfo) $ -} do
            hPutStrLn logFile $ show lvl ++ " - " ++ show src ++ ": " ++ msg
            hFlush logFile
    gsconfig <-
        makeGlobalStateConfig
            defaultRuntimeParameters
                { rpTreeStateDir = "data/treestate-" ++ show now,
                  rpBlockStateFile = "data/blockstate-" ++ show now
                }
            genesisData
    let config = SkovConfig gsconfig NoFinalization LogUpdateHandler
    (skovContext, skovState0) <- runLoggerT (initialiseSkov config) logM
    -- t <- getCurrentTime
    stateRef <- newIORef skovState0
    startTime <- getTime Monotonic
    -- cRef <- newIORef (0, startTime)
    {- bracket (createBlobStore ("data/dummy-" ++ show now ++ ".dat")) destroyBlobStore $ \tempBS -> -}
    do
        {-
        let importBlock pb = do
                (c0, t0) <- readIORef cRef
                t1 <- liftIO $ getTime Monotonic
                let c1 = c0+1
                liftIO . putStrLn $ show c1 ++ "\t" ++ show (toNanoSecs $ diffTimeSpec t1 t0)
                writeIORef cRef (c0+1, t1)
                return (Success :: ImportingResult ())
        -}
        let importBlock pb = do
                ss <- readIORef stateRef
                let storeGetHeight = do
                        t0 <- liftIO $ getTime Monotonic
                        ur <- storeBlock pb
                        bb <- bestBlock
                        bs <- queryBlockState bb
                        outcomes <- getOutcomes bs
                        specialOutcomes <- getSpecialOutcomes bs
                        let energyCost = sum . fmap tsEnergyCost $ outcomes
                        stateHash <- getStateHash bs
                        {-
                        bss <- serializeBlockState bs
                        case runGetLazy getBlockStateV0 bss of
                            Left err -> error err
                            Right bs' -> do
                                unless (stateHash == getHash bs') $ error "State hash mismatch after (de)serialization"
                                bs'' <- liftIO $ runReaderT (makePersistent bs') tempBS
                                unless (stateHash == hpbsHash bs'') $ error "State hash mismatch after conversion to persistent"
                        -}
                        ts <- getSlotTime . blockSlot $ bb
                        h <- getCurrentHeight
                        t1 <- liftIO $ getTime Monotonic

                        liftIO . putStrLn $
                            "Height: " ++ show h
                                ++ "\tExecution time: " ++ show (toNanoSecs $ diffTimeSpec t1 t0)
                                ++ "\tState hash: "
                                ++ show stateHash
                                ++ " ["
                                ++ show ts
                                ++ "]  \tTransactions: "
                                ++ show (Vec.length outcomes)
                                ++ " + "
                                ++ show (Seq.length specialOutcomes)
                                ++ " special\tEnergy: "
                                ++ show energyCost
                        return ur
                (ur, ss') <- runLoggerT (runSkovT storeGetHeight (SkovPassiveHandlers (return ())) skovContext ss) logM
                if ur == ResultSuccess
                    then do
                        writeIORef stateRef ss'
                        return Success
                    else return $ OtherError ur
        -- res <- readBlocksV1 blocks t logM Runner importBlock
        res <- readBlocks blocks importBlock
        t1 <- getTime Monotonic
        print $ toNanoSecs $ diffTimeSpec t1 startTime
        print res

readBlocks :: FilePath -> (PendingBlock -> IO (ImportingResult a)) -> IO (ImportingResult a)
readBlocks fp continuation = do
        h <- openFile fp ReadMode
        tm <- getCurrentTime
        version <- BS.hGet h 1
        if version /= "\x01" then
            return SerializationFail
        else
            let loop = do
                    lbs <- BS.hGet h 8
                    if BS.null lbs then
                        return Success
                    else case runGet getWord64be lbs of
                        Left _ -> return SerializationFail
                        Right l -> do
                            bbs <- BS.hGet h (fromIntegral l)
                            case deserializePendingBlock SP1 bbs tm of
                                Left _ -> return SerializationFail
                                Right block -> continuation block >>= \case
                                    Success -> loop
                                    r -> return r
            in loop