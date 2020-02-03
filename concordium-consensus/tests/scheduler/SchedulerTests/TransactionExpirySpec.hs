{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.TransactionExpirySpec where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Lens.Micro.Platform

import Acorn.Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Bakers
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 200000 Acc.emptyAccounts 200000

baker :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 alesAccount

-- A list of transactions all of which are valid unless they are expired.
-- This list includes all payload types to ensure that expiry is handled for
-- all types of transactions.
transactions :: Types.TransactionExpiryTime -> [TransactionJSON]
transactions t = [TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                        , metadata = makeHeaderWithExpiry alesAccount 1 1000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = DeployCredential cdi1
                        , metadata = makeHeaderWithExpiry alesAccount 2 10000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = AddBaker (baker ^. _1 . bakerElectionVerifyKey)
                                             (baker ^. _2)
                                             (baker ^. _1 . bakerSignatureVerifyKey)
                                             (baker ^. _1 . bakerAggregationVerifyKey)
                                             (baker ^. _3)
                                             alesAccount
                                             alesKP
                        , metadata = makeHeaderWithExpiry alesAccount 3 10000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = UpdateBakerAccount 0 alesAccount alesKP
                        , metadata = makeHeaderWithExpiry alesAccount 4 10000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 3)) (BlockSig.signKey (bakerSignKey 3))
                        , metadata = makeHeaderWithExpiry alesAccount 5 10000 t
                        , keypair = alesKP
                         }
                 ,TJSON { payload = DelegateStake 0
                        , metadata = makeHeaderWithExpiry alesAccount 6 100000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = UndelegateStake
                        , metadata = makeHeaderWithExpiry alesAccount 7 100000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = RemoveBaker 0 "<dummy proof>"
                      , metadata = makeHeaderWithExpiry alesAccount 8 10000 t
                      , keypair = alesKP
                      }
                 ,TJSON { payload = DeployModule "FibContract"
                        , metadata = makeHeaderWithExpiry alesAccount 9 10000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = InitContract { amount = 123
                                                 , contractName = "Fibonacci"
                                                 , moduleName = "FibContract"
                                                 , parameter = "Unit.Unit"
                                                 }
                        , metadata = makeHeaderWithExpiry alesAccount 10 100000 t
                        , keypair = alesKP
                        }
                 ,TJSON { payload = Update { amount = 0
                                           , address = Types.ContractAddress 0 0
                                           , moduleName = "FibContract"
                                           , message = "Fib 30"
                                           }
                        , metadata = makeHeaderWithExpiry alesAccount 11 100000 t
                        , keypair = alesKP
                        }
                 ]

slotTime :: Types.Timestamp
slotTime = 1

testExpiryTime ::
    Types.TransactionExpiryTime ->
    PR.Context UA
       IO
       ([(Types.BareTransaction, Types.ValidResult)],
        [(Types.BareTransaction, Types.FailureKind)],
        [Types.BareTransaction])
testExpiryTime expiry = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source
    ts <- processUngroupedTransactions $ transactions expiry
    let ((Sch.FilteredTransactions{..}, _), gstate) =
          Types.runSI (Sch.filterTransactions dummyBlockSize (Types.Energy maxBound) ts)
            dummySpecialBetaAccounts
            Types.dummyChainMeta { Types.slotTime = slotTime }
            initialBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (ftAdded, ftFailed, ftUnprocessed)

checkExpiryTimeResult :: Types.TransactionExpiryTime ->
                         ([(Types.BareTransaction, Types.ValidResult)],
                          [(Types.BareTransaction, Types.FailureKind)],
                          [Types.BareTransaction]) ->
                         Bool
checkExpiryTimeResult (Types.TransactionExpiryTime expiry) (added, fails, unprocs) =
    null unprocs &&
        if slotTime <= expiry
        -- transactions haven't expired, so they should all succeed
        then check fails added (\case (_, Types.TxSuccess{}) -> True
                                      _ -> False)
        -- transactions expired and they should all fail
        else check added fails (\case (_, Types.ExpiredTransaction) -> True
                                      _ -> False)
    where check nulls ts f = null nulls && all f ts

tests :: Spec
tests =
  describe "Transaction expiry test:" $ do
    specify "Valid transactions of all payloads with expiry after slot time pass" $
      testExpiry $ Types.TransactionExpiryTime $ slotTime + 1
    specify "Same transactions with expiry set to slot time pass" $
       testExpiry $ Types.TransactionExpiryTime slotTime
    specify "Same transactions with expiry set before slot time fail" $
       testExpiry $ Types.TransactionExpiryTime $ slotTime - 1
  where testExpiry expiry = PR.evalContext Init.initialContextData (testExpiryTime expiry)
            `shouldReturnP` checkExpiryTimeResult expiry
