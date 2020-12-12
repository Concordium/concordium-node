{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.TransactionExpirySpec where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class
import Lens.Micro.Platform

import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 200000000000 Acc.emptyAccounts

baker :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 0

-- A list of transactions all of which are valid unless they are expired.
-- Ideally, this should include all payload types to ensure that expiry is handled for
-- all types of transactions.
-- TODO: Add other transaction types.
transactions :: Types.TransactionExpiryTime -> [TransactionJSON]
transactions t = [TJSON { payload = Transfer { toaddress = alesAccount, amount = 10000 }
                        , metadata = makeHeaderWithExpiry alesAccount 1 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = AddBaker {
                              bElectionVerifyKey = baker ^. _1 . bakerInfo . bakerElectionVerifyKey,
                              bElectionSecretKey = baker ^. _2,
                              bSignVerifyKey = baker ^. _1 . bakerInfo . bakerSignatureVerifyKey,
                              bSignSecretKey = baker ^. _3,
                              bAggregateVerifyKey = baker ^. _1 . bakerInfo . bakerAggregationVerifyKey,
                              bAggregateSecretKey = baker ^. _4,
                              bInitialStake = 1000000,
                              bRestakeEarnings = True
                            }
                        , metadata = makeHeaderWithExpiry alesAccount 2 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = UpdateBakerStake 2000000
                        , metadata = makeHeaderWithExpiry alesAccount 3 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = UpdateBakerKeys {
                              bElectionVerifyKey = baker ^. _1 . bakerInfo . bakerElectionVerifyKey,
                              bElectionSecretKey = baker ^. _2,
                              bSignVerifyKey = baker ^. _1 . bakerInfo . bakerSignatureVerifyKey,
                              bSignSecretKey = baker ^. _3,
                              bAggregateVerifyKey = baker ^. _1 . bakerInfo . bakerAggregationVerifyKey,
                              bAggregateSecretKey = baker ^. _4
                            }
                        , metadata = makeHeaderWithExpiry alesAccount 4 100000 t
                        , keys = [(0, alesKP)]
                         }
                 ,TJSON { payload = UpdateBakerRestakeEarnings False
                        , metadata = makeHeaderWithExpiry alesAccount 5 1000000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = RemoveBaker
                      , metadata = makeHeaderWithExpiry alesAccount 6 100000 t
                      , keys = [(0, alesKP)]
                      }
                 ]

expiryTime :: Types.TransactionExpiryTime
expiryTime = 1

slotTime :: Types.Timestamp
slotTime = Types.transactionTimeToTimestamp expiryTime

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [Types.Transaction])

testExpiryTime :: Types.TransactionExpiryTime -> IO TestResult
testExpiryTime expiry = do
    ts <- processUngroupedTransactions $ transactions expiry
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize ts)
            dummyChainMeta { Types.slotTime = slotTime }
            maxBound
            maxBound
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (getResults ftAdded, ftFailed, ftUnprocessed)

checkExpiryTimeResult :: Types.TransactionExpiryTime -> TestResult -> Assertion
checkExpiryTimeResult expiry (added, fails, unprocs) = do
  assertEqual "No unprocessed transactions." [] unprocs
  if expiryTime <= expiry
     -- transactions haven't expired, so they should all succeed
  then do
    assertEqual "No failed transactions." [] fails
    assertBool "All added transactions succeed." $ all fSucs added
     -- transactions expired and they should all fail
  else do
    assertEqual "No transactions added." [] added
    assertBool "All failed transactions expired." $ all fFails fails

  where fFails (_, Types.ExpiredTransaction) = True
        fFails _ = False

        fSucs (_, Types.TxSuccess{}) = True
        fSucs _ = False

tests :: Spec
tests =
  describe "Transaction expiry test:" $ do
    specify "Valid transactions of all payloads with expiry after slot time pass" $
      testExpiry $ expiryTime + 1
    specify "Same transactions with expiry set to slot time pass" $
       testExpiry $ expiryTime
    specify "Same transactions with expiry set before slot time fail" $
       testExpiry $ expiryTime - 1
  where testExpiry expiry = testExpiryTime expiry >>= checkExpiryTimeResult expiry
