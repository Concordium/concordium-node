{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.UpdateAccountKeys where

import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Test.Hspec
import qualified Test.HUnit as HUnit
import System.Random

import            Concordium.Crypto.DummyData
import qualified  Concordium.Crypto.SignatureScheme as Sig
import            Concordium.GlobalState.Basic.BlockState.Account as Acc
import            Concordium.GlobalState.Basic.BlockState
import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import            Concordium.Types.DummyData
import            SchedulerTests.TestUtils

initialBlockState :: BlockState
initialBlockState = createBlockState $
                    putAccountWithRegIds (mkAccountMultipleKeys [vk kp0, vk kp1] 2 alesAccount 100000000)
                    Acc.emptyAccounts

initialBlockState2 :: BlockState
initialBlockState2 = createBlockState $
                    putAccountWithRegIds (mkAccountMultipleKeys [vk kp0, vk kp1, vk kp2, vk kp3, vk kp4] 2 alesAccount 100000000)
                    Acc.emptyAccounts


mkKeyPair :: Int -> Sig.KeyPair
mkKeyPair i = uncurry Sig.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen i)

kp0, kp1, kp2, kp3, kp4 :: Sig.KeyPair
kp0 = mkKeyPair 0
kp1 = mkKeyPair 1
kp2 = mkKeyPair 2
kp3 = mkKeyPair 3
kp4 = mkKeyPair 4

vk :: Sig.KeyPair -> Sig.VerifyKey
vk = Sig.correspondingVerifyKey

testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Account key updates"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcModules = []
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [(0, vk kp2)],
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE [AccountKeysUpdated alesAccount [(0, vk kp2)]]
          , checkKeys [(0, vk kp2), (1, vk kp1)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [(0, vk kp0), (1, vk kp1)],
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (1, kp1)] -- wrong signing keys
                        }
        , ( Fail IncorrectSignature
          , checkKeys [(0, vk kp2), (1, vk kp1)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [(0, vk kp3), (1, vk kp4)],
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp2), (1, kp1)]
                        }
        , ( SuccessE [AccountKeysUpdated alesAccount [(0, vk kp3), (1, vk kp4)]]
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [(1, vk kp0), (1, vk kp1)], -- duplicated index
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp3), (1, kp4)]
                        }
        , ( Reject $ DuplicateKeyIndex 1
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [(2, vk kp0)], -- keyindex not in use
                          metadata = makeDummyHeader alesAccount 4 10000,
                          keys = [(0, kp3), (1, kp4)]
                        }
        , ( Reject $ NonexistentAccountKey 2
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.UpdateAccountKeys [], -- technically allowed, but should have no effect
                          metadata = makeDummyHeader alesAccount 5 10000,
                          keys = [(0, kp3), (1, kp4)]
                        }
        , ( SuccessE $ [AccountKeysUpdated alesAccount []]
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      ]
    }
  , TestCase
    { tcName = "Adding account keys"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcModules = []
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.AddAccountKeys [(2, vk kp2)] Nothing,
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [AccountKeysAdded alesAccount [(2, vk kp2)]]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.AddAccountKeys [(3, vk kp3)] (Just 3),
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (2, kp2)]
                        }
        , ( SuccessE $ [ AccountKeysAdded alesAccount [(3, vk kp3)]
                       , AccountKeysSignThresholdUpdated alesAccount 3 ]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.AddAccountKeys [(3, vk kp4)] (Just 3),
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp0), (2, kp2), (1, kp1)]
                        }
        , ( Reject $ KeyIndexAlreadyInUse 3
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.AddAccountKeys [(4, vk kp4), (4, vk kp4)] (Just 3),
                          metadata = makeDummyHeader alesAccount 4 10000,
                          keys = [(0, kp0), (2, kp2), (1, kp1)]
                        }
        , ( Reject $ DuplicateKeyIndex 4
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.AddAccountKeys [(4, vk kp4)] (Just 5),
                          metadata = makeDummyHeader alesAccount 5 10000,
                          keys = [(0, kp0), (2, kp2), (1, kp1)]
                        }
        , ( SuccessE $ [ AccountKeysAdded alesAccount [(4, vk kp4)]
                       , AccountKeysSignThresholdUpdated alesAccount 5 ]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3), (4, vk kp4)] 5
          )
        )
      , ( Runner.TJSON  { payload = Runner.AddAccountKeys [] (Just 6),
                          metadata = makeDummyHeader alesAccount 6 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2), (3, kp3), (4, kp4)]
                        }
        , ( Reject $ InvalidAccountKeySignThreshold
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3), (4, vk kp4)] 5
          )
        )
      ]
    }
  , TestCase
    { tcName = "Removing account keys"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState2} -- ales has 5 keys in this one
    , tcModules = []
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [3, 4] Nothing,
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [AccountKeysRemoved alesAccount [3, 4]]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [1, 2] Nothing, -- removes more keys than allowed (threshold = 2)
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (2, kp2)]
                        }
        , ( Reject $ InvalidAccountKeySignThreshold
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [] (Just 3),
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [ AccountKeysRemoved alesAccount []
                       , AccountKeysSignThresholdUpdated alesAccount 3 ]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [4] (Just 1),
                          metadata = makeDummyHeader alesAccount 4 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2)]
                        }
        , ( Reject $ NonexistentAccountKey 4
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [1, 1] (Just 1),
                          metadata = makeDummyHeader alesAccount 5 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2)]
                        }
        , ( Reject $ DuplicateKeyIndex 1
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 3
          )
        )
      , ( Runner.TJSON  { payload = Runner.RemoveAccountKeys [0] (Just 2),
                          metadata = makeDummyHeader alesAccount 6 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2)]
                        }
        , ( SuccessE $ [ AccountKeysRemoved alesAccount [0]
                       , AccountKeysSignThresholdUpdated alesAccount 2 ]
          , checkKeys [(1, vk kp1), (2, vk kp2)] 2
          )
        )
      ]
    }
  ]
    where
      -- Prompts the blockstate for ales account keys and checks that they match the expected ones.
      checkKeys expectedKeys expectedThreshold = (\bs -> specify "Correct value in birk parameters" $
        case getAccount alesAccount (bs ^. blockAccounts) of
          Nothing -> HUnit.assertFailure "Account not found"
          Just account -> checkAccountKeys expectedKeys expectedThreshold (account ^. accountVerificationKeys))

-- Checks that the keys in the AccountKeys matches the ones in the list, that there isn't
-- any other keys than these in the AccountKeys and that the signature threshold matches.
checkAccountKeys :: [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> ID.AccountKeys -> HUnit.Assertion
checkAccountKeys keys threshold actualKeys@ID.AccountKeys{..} = do
  HUnit.assertEqual "Signature Threshold Matches" threshold akThreshold
  HUnit.assertEqual "Account keys should have same number of keys" (length keys) (length akKeys)
  forM_ keys (\(idx, key) -> case getAccountKey idx actualKeys of
    Nothing -> HUnit.assertFailure $ "Found no key at index " ++ show idx
    Just actualKey -> HUnit.assertEqual ("Key at index " ++ (show idx) ++ " should be equal") key actualKey)


tests :: Spec
tests = describe "UpdateElectionDifficulty" $
  mkSpecs testCases
