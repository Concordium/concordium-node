{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Full received-block cleanup, FFI decoding, rewards, outcome serialization, and state hashing.
-- Each sample executes three blocks from the same parent; divide times by three for per-block cost.
-- Setup and explicit GC are outside timing.
-- This measures warm persistent roots without disk flushing, not cold database performance.
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Criterion.Main
import qualified Data.ByteString as Bytes
import Data.Foldable (toList)
import qualified Data.Serialize as Serialize
import System.Mem (performGC)

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.KonsensusV1.Scheduler as Block
import Concordium.Logger (runSilentLogger)
import Concordium.Scheduler.Types
import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.KonsensusV1.AutoCancelExpiredLocksFixture

-- Force the persistent fixture before Criterion receives it.
newtype Fixture = Fixture (Block.BlockExecutionData 'P11)

instance NFData Fixture where
    rnf fixture = fixture `seq` ()

prepare :: Int -> Helpers.PersistentBSM 'P11 Fixture
prepare count = do
    (fixture, _, _) <- makeFixture count False
    executionData <- withEnergyLimit (fromIntegral count * emptyCancellationCost) fixture
    parentHash <- BS.getStateHash $ Block.bedParentState executionData
    _ <- liftIO $ evaluate $ Bytes.length (Serialize.encode parentHash)
    return $ Fixture executionData

runBlocks :: Fixture -> Helpers.PersistentBSM 'P11 ()
runBlocks (Fixture executionData) = replicateM_ 3 $ do
    (state, _) <- requireExecuted =<< Block.executeBlockState executionData []
    outcomes <- BS.getSpecialOutcomes state
    outcomeHash <- BS.getTransactionOutcomesHash state
    stateHash <- BS.getStateHash state
    _ <-
        liftIO $
            evaluate $
                Bytes.length (Serialize.encode outcomeHash)
                    + Bytes.length (Serialize.encode stateHash)
                    + sum [Bytes.length $ Serialize.encode outcome | outcome <- toList outcomes]
    return ()

main :: IO ()
main = withBenchmarks [10_000, 30_000, 60_000] defaultMain
  where
    -- Keep each fixture's independent persistent context alive until Criterion finishes.
    withBenchmarks :: [Int] -> ([Benchmark] -> IO ()) -> IO ()
    withBenchmarks [] use = use []
    withBenchmarks (count : rest) use = Helpers.runTestBlockState @'P11 $ do
        context <- Helpers.PersistentBSM ask
        let run action = runSilentLogger $ Blob.runBlobStoreT (BS.runPersistentBlockStateMonad $ Helpers._runPersistentBSM action) context
            benchmark = env (run $ prepare count) $ \fixture ->
                bench ("empty-remove/" ++ show count) $
                    perRunEnv performGC $
                        \() -> run (runBlocks fixture)
        liftIO $ withBenchmarks rest $ \others -> use (benchmark : others)
