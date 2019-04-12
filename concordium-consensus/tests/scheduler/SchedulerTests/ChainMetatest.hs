{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.ChainMetatest where

import Test.Hspec

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
import System.Random

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Acorn.Core as Core

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Instances as Ins
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI


initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState
    { blockAccounts = Acc.putAccount (Types.Account alesAccount 1 100000 alesACI) Acc.emptyAccounts
    , blockModules = (let (_, _, gs) = Init.baseState in Mod.Modules gs) }

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = DeployModule "ChainMetaTest"
           , metadata = Types.Header {sender = alesAccount
                                     ,nonce = 1
                                     ,gasAmount = 10000
                                     }
           }
    ,TJSON { payload = InitContract {amount = 100
                                    ,contractName = "Simple"
                                    ,moduleName = "ChainMetaTest"
                                    ,parameter = "Unit.Unit"
                                    }
           , metadata = Types.Header {sender = alesAccount
                                     ,nonce = 2
                                     ,gasAmount = 10000
                                     }
           }
    ]


testChainMeta ::
  PR.Context
    IO
    ([(Types.MessageTy, Types.ValidResult)],
     [(Types.MessageTy, Types.FailureKind)],
     [(Types.ContractAddress, Types.Instance)])
testChainMeta = do
    source <- liftIO $ TIO.readFile "test/contracts/ChainMetaTest.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gs) = Types.runSI (Sch.makeValidBlock transactions)
                                         chainMeta
                                         initialBlockState
    return (suc, fails, Ins.toList (blockInstances gs))

checkChainMetaResult :: ([(a1, Types.ValidResult)], [b], [(a3, Instance)]) -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the 
  where 
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc
    checkLocalState (Types.Instance{..}) = do
      case imodel of
        Types.VConstructor _ [Types.VLiteral (Core.Word64 8)  -- NB: These should match those in chainMeta
                             ,Types.VLiteral (Core.Word64 13)
                             ,Types.VLiteral (Core.Word64 10)] -> True
        _ -> False                                                          

tests :: SpecWith ()
tests = 
  describe "Chain metadata in transactions." $ do
    specify "Reading chain metadata." $ do
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
