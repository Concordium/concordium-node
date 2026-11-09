{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Full received-block cleanup, FFI decoding, rewards, outcome serialization, and state hashing.
-- Each sample executes three blocks; divide reported times by three for per-block cost.
-- Setup, signing, and explicit GC are outside timing.
-- This measures warm persistent roots without disk flushing, not cold database performance.
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
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
data Fixture = Fixture (Block.BlockExecutionData 'P11) [BlockItem]

instance NFData Fixture where
    rnf fixture = fixture `seq` ()

prepare :: Int -> Bool -> Helpers.PersistentBSM 'P11 Fixture
prepare count funded = do
    (fixture, _, nonce) <- makeFixture count funded
    -- A 601-NRG limit prioritizes the 501-NRG transfer, leaving 100 NRG to scan
    -- every funded lock but not pay its 600-NRG cancellation cost.
    executionData <- withEnergyLimit (if funded then Helpers.simpleTransferCost + emptyCancellationCost else fromIntegral count * emptyCancellationCost) fixture
    transfers <- liftIO $ mapM makeTransfer [nonce .. nonce + 2]
    parentHash <- BS.getStateHash $ Block.bedParentState executionData
    _ <-
        liftIO $
            evaluate $
                Bytes.length (Serialize.encode parentHash)
                    + Bytes.length (Serialize.encode $ Block.bedBlockNonce executionData)
                    + sum [Bytes.length $ Serialize.runPut $ putBlockItemV0 tx | tx <- transfers]
    return $ Fixture executionData transfers

runBlocks :: Bool -> Fixture -> Helpers.PersistentBSM 'P11 ()
runBlocks funded (Fixture executionData transfers) = go (Block.bedParentState executionData) transfers
  where
    go _ [] = return ()
    go parent (tx : rest) = do
        let transactions = if funded then [(tx, verification)] else []
        (state, _) <- requireExecuted =<< Block.executeBlockState executionData{Block.bedParentState = parent} transactions
        outcomes <- BS.getSpecialOutcomes state
        outcomeHash <- BS.getTransactionOutcomesHash state
        stateHash <- BS.getStateHash state
        _ <-
            liftIO $
                evaluate $
                    Bytes.length (Serialize.encode outcomeHash)
                        + Bytes.length (Serialize.encode stateHash)
                        + sum [Bytes.length $ Serialize.encode outcome | outcome <- toList outcomes]
        go (if funded then state else Block.bedParentState executionData) rest

main :: IO ()
main = withBenchmarks [(count, funded) | funded <- [False, True], count <- [10_000, 30_000, 60_000]] defaultMain
  where
    -- Keep each fixture's independent persistent context alive until Criterion finishes.
    withBenchmarks :: [(Int, Bool)] -> ([Benchmark] -> IO ()) -> IO ()
    withBenchmarks [] use = use []
    withBenchmarks ((count, funded) : rest) use = Helpers.runTestBlockState @'P11 $ do
        context <- Helpers.PersistentBSM ask
        let run action = runSilentLogger $ Blob.runBlobStoreT (BS.runPersistentBlockStateMonad $ Helpers._runPersistentBSM action) context
            scenario = if funded then "funded-skip" else "empty-remove"
            benchmark = env (run $ prepare count funded) $ \fixture ->
                bench (scenario ++ "/" ++ show count) $
                    perRunEnv performGC $
                        \() -> run (runBlocks funded fixture)
        liftIO $ withBenchmarks rest $ \others -> use (benchmark : others)
