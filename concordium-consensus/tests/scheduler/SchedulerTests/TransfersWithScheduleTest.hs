{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.TransfersWithScheduleTest where

import Control.Monad
import Lens.Micro.Platform
import Test.Hspec
import qualified Test.HUnit as HUnit

import qualified  Concordium.Crypto.SignatureScheme as Sig
import            Concordium.GlobalState.Account
import            Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import            Concordium.GlobalState.Basic.BlockState
import            Concordium.GlobalState.Basic.BlockState.Account
import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import qualified Data.ByteString.Short as BSS
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import qualified  Data.Map as Map
import            SchedulerTests.TestUtils
import Data.Maybe
import Concordium.Types.DummyData (thomasAccount)

initialBlockState :: BlockState PV
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

cdiKeys :: CredentialDeploymentInformation -> CredentialPublicKeys
cdiKeys cdi = credPubKeys (NormalACWP cdi)

vk :: Sig.KeyPair -> Sig.VerifyKey
vk = Sig.correspondingVerifyKey


testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Transfers with schedule"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions = [
        -- trying to do a scheduled transfer with memo - should get rejected since we have protocol version 1
        ( Runner.TJSON  { payload = Runner.TransferWithScheduleAndMemo thomasAccount (Memo $ BSS.pack [0,1,2,3]) [(1, 10),(2, 11),(3, 12)],
                          metadata = makeDummyHeader alesAccount 1 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (Reject SerializationFailure
          , emptySpec
          )
        ),
        -- make a scheduled transfer
        ( Runner.TJSON  { payload = Runner.TransferWithSchedule thomasAccount [(1, 10),(2, 11),(3, 12)],
                          metadata = makeDummyHeader alesAccount 2 100000,
                          keys = [(0,[(0, alesKP)])]
                        }
        , (SuccessE [TransferredWithSchedule alesAccount thomasAccount [(1, 10),(2, 11),(3, 12)]]
          , emptySpec
          )
        )
      ]
    }
  ]



tests :: Spec
tests = describe "TransfersWithSchedule" $
  mkSpecs testCases
