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
initialBlockState = blockStateWithAlesAccount 2000000000 Acc.emptyAccounts

baker :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 alesAccount

-- A list of transactions all of which are valid unless they are expired.
-- This list includes all payload types to ensure that expiry is handled for
-- all types of transactions.
transactions :: Types.TransactionExpiryTime -> [TransactionJSON]
transactions t = [TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                        , metadata = makeHeaderWithExpiry alesAccount 1 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = AddBaker (baker ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                             (baker ^. _2)
                                             (baker ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                             (baker ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                             (baker ^. _4)
                                             (baker ^. _3)
                                             alesAccount
                                             alesKP
                        , metadata = makeHeaderWithExpiry alesAccount 2 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = UpdateBakerAccount 0 alesAccount alesKP
                        , metadata = makeHeaderWithExpiry alesAccount 3 100000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 3)) (BlockSig.signKey (bakerSignKey 3))
                        , metadata = makeHeaderWithExpiry alesAccount 4 100000 t
                        , keys = [(0, alesKP)]
                         }
                 ,TJSON { payload = DelegateStake 0
                        , metadata = makeHeaderWithExpiry alesAccount 5 1000000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = UndelegateStake
                        , metadata = makeHeaderWithExpiry alesAccount 6 1000000 t
                        , keys = [(0, alesKP)]
                        }
                 ,TJSON { payload = RemoveBaker 0
                      , metadata = makeHeaderWithExpiry alesAccount 7 100000 t
                      , keys = [(0, alesKP)]
                      }
                 ]

expiryTime :: Types.TransactionExpiryTime
expiryTime = 1

slotTime :: Types.Timestamp
slotTime = fromIntegral (Types.expiry expiryTime) * 1000

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [Types.Transaction])

testExpiryTime :: Types.TransactionExpiryTime -> IO TestResult
testExpiryTime expiry = do
    ts <- processUngroupedTransactions $ transactions expiry
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize ts)
            dummySpecialBetaAccounts
            Types.dummyChainMeta { Types.slotTime = slotTime }
            maxBound
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate of
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
