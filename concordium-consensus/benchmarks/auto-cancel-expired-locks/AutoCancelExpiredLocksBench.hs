{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Full received-block cleanup, FFI decoding, rewards, outcome serialization, and state hashing.
-- Each sample executes three blocks; divide reported times by three for per-block cost.
-- Setup, signing, explicit GC, backlog queries, and assertions are outside timing.
-- This measures warm persistent roots without disk flushing, not cold database performance.
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Criterion.Main
import qualified Data.ByteString as Bytes
import Data.Foldable (toList)
import Data.IORef
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import System.Mem (performGC)
import Test.HUnit

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.KonsensusV1.Scheduler as Block
import Concordium.Logger (runSilentLogger)
import Concordium.Scheduler.ProtocolLevelTokens.Queries (queryLockList)
import Concordium.Scheduler.Types
import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.KonsensusV1.AutoCancelExpiredLocksFixture

-- Setup validates and forces the persistent fixture before Criterion receives it.
data Fixture = Fixture (Block.BlockExecutionData 'P11) [BlockItem]

instance NFData Fixture where
    rnf fixture = fixture `seq` ()

type BlockResult = (BS.HashedPersistentBlockState 'P11, Energy, Seq.Seq SpecialTransactionOutcome)

prepare :: Int -> Bool -> Helpers.PersistentBSM 'P11 Fixture
prepare count funded = do
    (fixture, _, nonce) <- makeFixture count funded
    locks <- queryLockList $ Block.bedParentState fixture
    liftIO $ assertEqual "Initial backlog count" count (length locks)
    -- Leave enough energy to scan funded locks, but not to cancel their single reference.
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

runBlocks :: Bool -> Fixture -> Helpers.PersistentBSM 'P11 [BlockResult]
runBlocks funded (Fixture executionData transfers) = go (Block.bedParentState executionData) transfers
  where
    go _ [] = return []
    go parent (tx : rest) = do
        let transactions = if funded then [(tx, verification)] else []
        (state, energy) <- requireExecuted =<< Block.executeBlockState executionData{Block.bedParentState = parent} transactions
        outcomes <- BS.getSpecialOutcomes state
        outcomeHash <- BS.getTransactionOutcomesHash state
        stateHash <- BS.getStateHash state
        _ <-
            liftIO $
                evaluate $
                    Bytes.length (Serialize.encode outcomeHash)
                        + Bytes.length (Serialize.encode stateHash)
                        + sum [Bytes.length $ Serialize.encode outcome | outcome <- toList outcomes]
        later <- go (if funded then state else Block.bedParentState executionData) rest
        return $ (state, energy, outcomes) : later

checkBlocks :: Int -> Bool -> [BlockResult] -> Helpers.PersistentBSM 'P11 ()
checkBlocks count funded results = do
    liftIO $ assertEqual "Three actual block executions" 3 (length results)
    forM_ results $ \(state, energy, outcomes) -> do
        locks <- queryLockList state
        liftIO $ do
            assertEqual "Exact block energy" (if funded then Helpers.simpleTransferCost else fromIntegral count * emptyCancellationCost) energy
            assertEqual "Persistent backlog count after block" (if funded then count else 0) (length locks)
            assertEqual "Special outcome count" (if funded then 1 else count + 1) (Seq.length outcomes)

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
                    perRunEnvWithCleanup
                        (performGC >> newIORef [])
                        (\results -> readIORef results >>= run . checkBlocks count funded)
                        (\results -> run (runBlocks funded fixture) >>= writeIORef results)
        liftIO $ withBenchmarks rest $ \others -> use (benchmark : others)
