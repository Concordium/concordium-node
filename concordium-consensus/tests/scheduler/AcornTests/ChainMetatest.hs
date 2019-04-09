{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module AcornTests.ChainMetatest where

import Test.Hspec

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
import System.Random

import qualified Acorn.Types as Types
import qualified Acorn.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Acorn.Scheduler as Sch
import qualified Acorn.Core as Core

import qualified Data.HashMap.Strict as Map

import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

initialGlobalState :: Types.GlobalState
initialGlobalState = (Types.GlobalState Map.empty (Map.fromList [(alesAccount, Types.Account alesAccount 1 100000 alesACI)])
                      (let (_, _, gs) = Init.baseState in gs))

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10

testChainMeta ::
  PR.Context
    IO
    ([(Types.MessageTy, Types.ValidResult)],
     [(Types.MessageTy, Types.FailureKind)],
     Map.HashMap Types.ContractAddress Types.Instance)
testChainMeta = do
    source <- liftIO $ TIO.readFile "test/contracts/ChainMetaTest.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactionsText <- liftIO $ BSL.readFile "test/transactions/chainmetatransactions.json"
    transactions <- PR.processTransactions transactionsText
    let ((suc, fails), gs) = Types.runSI (Sch.makeValidBlock transactions)
                                         chainMeta
                                         initialGlobalState
    return (suc, fails, Types.instances gs)

checkChainMetaResult ::
  ([(a, Types.ValidResult)], [b], Map.HashMap Types.ContractAddress Types.Instance) -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  Map.size instances == 1 && -- only a single contract instance should be created
  Map.member (Types.ContractAddress 0 0) instances && -- and it should have the 0, 0 index
  checkLocalState (instances Map.! Types.ContractAddress 0 0) -- and the local state should match the 
  where 
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc
    checkLocalState (Types.Instance{..}) = do
      case lState of
        Types.VConstructor _ [Types.VLiteral (Core.Word64 8)  -- NB: These should match those in chainMeta
                             ,Types.VLiteral (Core.Word64 13)
                             ,Types.VLiteral (Core.Word64 10)] -> True
        _ -> False                                                          

tests :: SpecWith ()
tests = 
  describe "Chain metadata in transactions." $ do
    specify "Reading chain metadata." $ do
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
