{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.UpdateElectionDifficultySpec where
{-
import Test.Hspec

import Lens.Micro.Platform
import qualified Data.HashSet as Set

import qualified Concordium.Scheduler.Types as Types
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    100000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000) Acc.emptyAccounts)

-- | The initial election difficulty (NOTE: This value is used for verification,
-- the difficulty is not set from this value).
initialElectionDifficulty :: Types.ElectionDifficulty
initialElectionDifficulty = 0.5

specialBetaAccounts :: Set.HashSet Types.AccountAddress
specialBetaAccounts = Set.fromList [alesAccount]


testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Successful updates"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState,tpSpecialAccounts=specialBetaAccounts}
    , tcModules = []
    , tcTransactions =
        let nonces = [1..]
            legalElectionDifficulties = [0, 0.0001, 0.5, 0.99999]
        in
          zip nonces legalElectionDifficulties <&> \(n, d) ->
          ( TJSON { payload = UpdateElectionDifficulty d
                  , metadata = makeDummyHeader alesAccount n 10000
                  , keys = [(0, alesKP)]
                  }
          , ( SuccessE [Types.ElectionDifficultyUpdated d]
            , \bs -> specify "Correct value in birk parameters" $
                     bs ^. blockBirkParameters . birkElectionDifficulty `shouldBe` d)
          )
     }
  , TestCase
    { tcName = "Rejected updates"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState,tpSpecialAccounts=specialBetaAccounts}
    , tcModules = []
    , tcTransactions =
        let nonces = [1..]
            legalElectionDifficulty = 0.527583
            illegalElectionDifficulties = [-0.00001, -0.5, -0.99999, -1, -2, -1000,
                                            1, 1.000001, 1.5, 2.0, 3000]
            electionDifficultyNotChanged bs =
              specify "Election difficulty not changed" $
              bs ^. blockBirkParameters . birkElectionDifficulty `shouldBe` initialElectionDifficulty
            -- given a list of nonces make transactions
            mkBodies = illegalElectionDifficulties <&> \d n ->
                           ( TJSON { payload = UpdateElectionDifficulty d
                                   , metadata = makeDummyHeader alesAccount n 10000
                                   , keys = [(0, alesKP)]
                                   }
                           , (Reject Types.SerializationFailure, electionDifficultyNotChanged)
                           )
            lastTransactionResultPair =
                (TJSON { payload = UpdateElectionDifficulty legalElectionDifficulty
                      , metadata = makeDummyHeader thomasAccount 1 10000
                      , keys = [(0, thomasKP)]
                      }
                , (Reject Types.NotFromSpecialAccount, electionDifficultyNotChanged)
                )
        in zipWith ($) mkBodies nonces ++ [lastTransactionResultPair]

    }
  ]


-- NEXT: Testing can be enhanced by combining ok and failing transactions into one
-- run in randomized order, checking that after a failed transaction, the election difficulty is still the old.
tests :: Spec
tests = describe "UpdateElectionDifficulty" $
  mkSpecs testCases
-}