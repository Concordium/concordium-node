module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock.POSIX
import System.IO
import Data.Serialize
import Data.Word
import Control.Exception
import System.Directory
import qualified Data.Map as Map
import Data.IORef

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.TimerMonad
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instance
import Concordium.Types.AnonymityRevokers
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
import Concordium.GlobalState.Paired
import Concordium.Kontrol (currentTimestamp)

import Concordium.Logger
import Concordium.Types
import Concordium.Runner
import Concordium.Skov
import Concordium.Getters
import Concordium.Afgjort.Finalize (FinalizationInstance(..))
import Concordium.Birk.Bake
import System.Environment
import Concordium.GlobalState.TreeState
import Concordium.Kontrol.BestBlock
import Concordium.Types.Execution (tsEnergyCost)
import Control.Monad.IO.Class
import Concordium.GlobalState.Basic.BlockState (getBlockStateV0)
import Concordium.GlobalState.Persistent.BlobStore
import Control.Monad.Reader
import Concordium.GlobalState.Persistent.BlockState (hpbsHash, makePersistent)

-- type TreeConfig = DiskTreeDiskBlockConfig
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ DTDBConfig rt genData

-- type TreeConfig = MemoryTreeDiskBlockConfig
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ MTDBConfig rt genData

--type TreeConfig = MemoryTreeMemoryBlockConfig
--makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
--makeGlobalStateConfig rt genData = return $ MTMBConfig rt genData

--uncomment if wanting paired config
type TreeConfig = PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rp genData =
   return $ PairGSConfig (MTMBConfig rp genData, DTDBConfig rp genData)

type ActiveConfig = SkovConfig TreeConfig (BufferedFinalization ThreadTimer) LogUpdateHandler

parseArgs :: [String] -> IO (GenesisData, LBS.ByteString)
parseArgs [gdPath, blocksPath] = do
      gdfile <- LBS.readFile gdPath
      gd <- case runGetLazy getExactVersionedGenesisData gdfile of
         Left err -> error err
         Right gd -> return gd
      blocks <- LBS.readFile blocksPath
      return (gd, blocks)
parseArgs _ = error "Expected exactly two arguments: genesis data, and blocks"

main :: IO ()
main = do
   (genesisData, blocks) <- parseArgs =<< getArgs 
   now <- currentTimestamp
   logFile <- openFile ("consensus-" ++ show now ++ ".log") WriteMode
   let logM src lvl msg = {- when (lvl == LLInfo) $ -} do
                              hPutStrLn logFile $ show lvl ++ " - " ++ show src ++ ": " ++ msg
                              hFlush logFile

   gsconfig <- makeGlobalStateConfig (defaultRuntimeParameters { rpTreeStateDir = "data/treestate-" ++ show now, rpBlockStateFile = "data/blockstate-" ++ show now }) genesisData --dbConnString
   let config = SkovConfig gsconfig NoFinalization LogUpdateHandler
   (skovContext, skovState0) <- runLoggerT (initialiseSkov config) logM
   t <- getCurrentTime
   stateRef <- newIORef skovState0
   bracket (createBlobStore ("data/dummy-" ++ show now ++ ".dat")) destroyBlobStore $ \tempBS -> do
      let importBlock pb = do
            ss <- readIORef stateRef
            let storeGetHeight = do
                  ur <- storeBlock pb
                  bb <- bestBlock
                  bs <- queryBlockState bb
                  energyCost <- sum . fmap tsEnergyCost <$> getOutcomes bs
                  stateHash <- getStateHash bs
                  bss <- serializeBlockState bs
                  case runGetLazy getBlockStateV0 bss of
                     Left err -> error err
                     Right bs' -> do
                        unless (stateHash == getHash bs') $ error "State hash mismatch after (de)serialization"
                        bs'' <- liftIO $ runReaderT (makePersistent bs') tempBS
                        unless (stateHash == hpbsHash bs'') $ error "State hash mismatch after conversion to persistent"
                  ts <- getSlotTime . blockSlot $ bb
                  h <- getCurrentHeight
                  liftIO . putStrLn $ "Height: " ++ show h ++ "\tState hash: " ++ show stateHash ++ " [" ++ show ts ++ "]\tEnergy: " ++ show energyCost
                  return ur
            (ur, ss') <- runLoggerT (runSkovT storeGetHeight (SkovPassiveHandlers (return ())) skovContext ss) logM
            if ur == ResultSuccess then do
               writeIORef stateRef ss'
               return Success
            else
               return $ OtherError ur

      res <- readBlocksV1 blocks t logM Runner importBlock
      print res
      
   return ()