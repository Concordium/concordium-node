{-| This module tests the upgrading feature which was implemented as part of P5.
    In particular it tests that an initialized instance which has been deployed
    in P5 can upgrade it's underlying module i.e. the artifact via the host function
    'upgrade'

    * Test case 1
        The scenario checks that a contract A which is initialized with some state can 
        be upgraded to a new module. The new module contains a view function that returns 
        the state stored in the prior version is still available.

    * Test case 2.. todo.

-}
module SchedulerTests.SmartContracts.V1.Upgrading (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, putWord64le, putByteString, putWord16le, encode)
import qualified Data.Text as T

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils


initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

upgrading0SourceFile :: FilePath
upgrading0SourceFile = "./testdata/contracts/v1/upgrading_0.wasm"

upgrading1SourceFile :: FilePath
upgrading1SourceFile = "./testdata/contracts/v1/upgrading_1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV5]
testCases = undefined -- TODO

tests :: Spec
tests = describe "V1: Upgrade" $ do
    mkSpecs testCases

  