{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.UpdateAccountKeys where

import Control.Monad
import Lens.Micro.Platform
import Test.Hspec
import qualified Test.HUnit as HUnit
import System.Random

import            Concordium.Crypto.DummyData
import qualified  Concordium.Crypto.SignatureScheme as Sig
import            Concordium.GlobalState.Account
import            Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import            Concordium.GlobalState.Basic.BlockState
import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import            Concordium.Types.DummyData
import qualified  Data.Set as Set
import qualified  Data.Map as Map
import            SchedulerTests.TestUtils

-- |Protocol version
type PV = 'P0

initialBlockState :: BlockState PV
initialBlockState = createBlockState $
                    Acc.putAccountWithRegIds (mkAccountMultipleKeys [vk kp0, vk kp1] 2 alesAccount 10000000000)
                    Acc.emptyAccounts

initialBlockState2 :: BlockState PV
initialBlockState2 = createBlockState $
                    Acc.putAccountWithRegIds (mkAccountMultipleKeys [vk kp0, vk kp1, vk kp2, vk kp3, vk kp4] 2 alesAccount 10000000000)
                    Acc.emptyAccounts

-- Makes a random ED25519 keypair, using the integer to feed the randomization.
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
    , tcTransactions = [
        -- correctly update a keypair
        ( Runner.TJSON  { payload = Runner.UpdateAccountKeys $ Map.fromList [(0, vk kp2)],
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE [AccountKeysUpdated]
          , checkKeys [(0, vk kp2), (1, vk kp1)] 2
          )
        )
      , -- Now, using the old keys should fail, since they were updated
        ( Runner.TJSON  { payload = Runner.UpdateAccountKeys $ Map.fromList [(0, vk kp0), (1, vk kp1)],
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (1, kp1)] -- wrong signing keys
                        }
        , ( Fail IncorrectSignature
          , checkKeys [(0, vk kp2), (1, vk kp1)] 2
          )
        )
      , -- Using the new keys should work
        ( Runner.TJSON  { payload = Runner.UpdateAccountKeys $ Map.fromList [(0, vk kp3), (1, vk kp4)],
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp2), (1, kp1)]
                        }
        , ( SuccessE [AccountKeysUpdated]
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      , -- Updating a key index that is not in use should fail
        ( Runner.TJSON  { payload = Runner.UpdateAccountKeys $ Map.fromList [(2, vk kp0)], -- keyindex not in use
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp3), (1, kp4)]
                        }
        , ( Reject $ NonExistentAccountKey
          , checkKeys [(0, vk kp3), (1, vk kp4)] 2
          )
        )
      ]
    }
  , TestCase
    { tcName = "Adding account keys"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions = [
        -- Correctly add account key
        ( Runner.TJSON  { payload = Runner.AddAccountKeys (Map.fromList [(2, vk kp2)]) Nothing,
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [AccountKeysAdded]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , -- Correctly add account key, signing with the one added in the previous transaction
        -- and correctly update the signature threshold
        ( Runner.TJSON  { payload = Runner.AddAccountKeys (Map.fromList [(3, vk kp3)]) (Just 3),
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (2, kp2)]
                        }
        , ( SuccessE $ [ AccountKeysAdded, AccountKeysSignThresholdUpdated]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3)] 3
          )
        )
      , -- Should fail to add an account key to an index that is already in use
      ( Runner.TJSON  { payload = Runner.AddAccountKeys (Map.fromList [(3, vk kp4)]) (Just 3),
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp0), (2, kp2), (1, kp1)]
                        }
        , ( Reject $ KeyIndexAlreadyInUse
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3)] 3
          )
        )
      , -- Should allow updating the threshold past what is allowed given the current keys
        -- if more keys are added as part of the transaction
        ( Runner.TJSON  { payload = Runner.AddAccountKeys (Map.fromList [(4, vk kp4)]) (Just 5),
                          metadata = makeDummyHeader alesAccount 4 10000,
                          keys = [(0, kp0), (2, kp2), (1, kp1)]
                        }
        , ( SuccessE $ [ AccountKeysAdded, AccountKeysSignThresholdUpdated]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2), (3, vk kp3), (4, vk kp4)] 5
          )
        )
      , -- Should fail to update the threshold in such a way that it exceeds the total number
        -- of keys.
        ( Runner.TJSON  { payload = Runner.AddAccountKeys Map.empty (Just 6),
                          metadata = makeDummyHeader alesAccount 5 10000,
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
    , tcTransactions = [
        -- Correctly remove keys 3 and 4.
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys (Set.fromList [3, 4]) Nothing,
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [AccountKeysRemoved]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , -- Should fail to remove keys that makes the threshold exceed the total number of keys
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys (Set.fromList [1, 2]) Nothing, -- removes more keys than allowed (threshold = 2)
                          metadata = makeDummyHeader alesAccount 2 10000,
                          keys = [(0, kp0), (2, kp2)]
                        }
        , ( Reject $ InvalidAccountKeySignThreshold
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 2
          )
        )
      , -- Should allow updating the threshold so that it doesn't exceed the number of keys
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys Set.empty (Just 3),
                          metadata = makeDummyHeader alesAccount 3 10000,
                          keys = [(0, kp0), (1, kp1)]
                        }
        , ( SuccessE $ [ AccountKeysRemoved, AccountKeysSignThresholdUpdated]
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 3
          )
        )
      , -- Should fail to remove a key at an index for which no key is defined
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys (Set.fromList [4]) (Just 1),
                          metadata = makeDummyHeader alesAccount 4 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2)]
                        }
        , ( Reject $ NonExistentAccountKey
          , checkKeys [(0, vk kp0), (1, vk kp1), (2, vk kp2)] 3
          )
        )
      , -- Should succeed in reducing the threshold and removing a key in the same transaction
        ( Runner.TJSON  { payload = Runner.RemoveAccountKeys (Set.fromList [0]) (Just 2),
                          metadata = makeDummyHeader alesAccount 5 10000,
                          keys = [(0, kp0), (1, kp1), (2, kp2)]
                        }
        , ( SuccessE $ [ AccountKeysRemoved, AccountKeysSignThresholdUpdated]
          , checkKeys [(1, vk kp1), (2, vk kp2)] 2
          )
        )
      ]
    }
  ]
    where
      -- Prompts the blockstate for ales account keys and checks that they match the expected ones.
      checkKeys expectedKeys expectedThreshold = (\bs -> specify "Correct account keys" $
        case Acc.getAccount alesAccount (bs ^. blockAccounts) of
          Nothing -> HUnit.assertFailure $ "Account with id '" ++ show alesAccount ++ "' not found"
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
