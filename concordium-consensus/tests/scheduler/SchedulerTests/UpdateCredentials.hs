{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.UpdateCredentials where

import Control.Monad
import Lens.Micro.Platform
import Test.Hspec
import qualified Test.HUnit as HUnit
import System.Random

import            Concordium.Crypto.DummyData
import            Concordium.ID.DummyData
import qualified  Concordium.Crypto.SignatureScheme as Sig
import            Concordium.GlobalState.Account
import            Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import            Concordium.GlobalState.Basic.BlockState
import            Concordium.GlobalState.Basic.BlockState.Account
import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import            Concordium.ID.Parameters(GlobalContext)
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import            Concordium.Types.DummyData
import qualified  Data.Set as Set
import qualified  Data.Map as Map
import            SchedulerTests.TestUtils
import qualified  Data.Hashable as IntHash
import            Concordium.Crypto.FFIDataTypes

initialBlockState :: BlockState
initialBlockState = createBlockState $
                    Acc.putAccountWithRegIds ((newAccount dummyCryptographicParameters cdi8address cdi8ac) & accountAmount .~ 10000000000)
                    Acc.emptyAccounts

cdiKeys :: CredentialDeploymentInformation -> CredentialPublicKeys
cdiKeys cdi = credPubKeys (NormalACWP cdi)

vk :: Sig.KeyPair -> Sig.VerifyKey
vk = Sig.correspondingVerifyKey

cdi8ID = credId (NormalACWP cdi8)
cdi8address = addressFromRegId $ cdi8ID
cdi8kp0 = keys cdi8keys Map.! 0
cdi8kp1 = keys cdi8keys Map.! 1
cdi8ac = maybe (error "Should be safe") id $ values $ NormalACWP cdi8

cdi9ID = credId (NormalACWP cdi9)
cdi9kp0 = keys cdi9keys Map.! 0
cdi9kp1 = keys cdi9keys Map.! 1

cdi10ID = credId (NormalACWP cdi10)
cdi10kp0 = keys cdi10keys Map.! 0
cdi10kp1 = keys cdi10keys Map.! 1

cdi11ID = credId (NormalACWP cdi11)
cdi11kp0 = keys cdi11keys Map.! 0
cdi11kp1 = keys cdi11keys Map.! 1

testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Account Credential updates"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions = [
        -- correctly update a keypair
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 1 cdi9) [] 1,
                          metadata = makeDummyHeader cdi8address 1 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])]
                        }
        , (SuccessE [CredentialsUpdated cdi8address [cdi9ID] [] 1] 
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 1
          )
        )
        , -- Correctly update threshold
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [] 2,
                          metadata = makeDummyHeader cdi8address 2 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])]
                        }
        , (SuccessE [CredentialsUpdated cdi8address [] [] 2] 
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2
          )
        )
      , -- Now, using the only one set of keys should fail, since threshold were updated
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 2 cdi10) [] 2,
                          metadata = makeDummyHeader cdi8address 3 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])] -- not enough credential holders signing since threshold is now 2
                        }
        , ( Fail IncorrectSignature
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2
          )
        )
      , -- Should reject since we try to add cdi10 at index 1
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 1 cdi10) [] 2,
                          metadata = makeDummyHeader cdi8address 3 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])] -- now two credential holders are signing
                        }
        , ( Reject KeyIndexAlreadyInUse
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2
          )
        )
      , -- Now, using the new keys should work
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 2 cdi10) [] 2,
                          metadata = makeDummyHeader cdi8address 4 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])] -- now two credential holders are signing
                        }
        , ( SuccessE [CredentialsUpdated cdi8address [cdi10ID] [] 2] 
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Updating threshold to 3
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [] 3,
                          metadata = makeDummyHeader cdi8address 5 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])] 
                        }
        , ( SuccessE [CredentialsUpdated cdi8address [] [] 3] 
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)] 3
          )
        )
      , -- Trying to remove cdi9 -  should reject since threshold is 3
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [cdi9ID] 3,
                          metadata = makeDummyHeader cdi8address 6 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Reject InvalidAccountThreshold -- The credential is valid but this is the reject reason returned by the scheduler TODO: Fix this in scheduler
          , checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)] 3
          )
        )
      , -- Trying to remove cdi9 -  should succeed since threshold is changed to 2
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [cdi9ID] 2,
                          metadata = makeDummyHeader cdi8address 7 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( SuccessE [CredentialsUpdated cdi8address [] [cdi9ID] 2]
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Trying to sign with cdi9's keys -  should fail since cdi9 was removed
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [] 2,
                          metadata = makeDummyHeader cdi8address 8 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Fail IncorrectSignature
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Trying to remove cdi8 -  should reject since it is the first credential
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [cdi8ID] 1, -- notice that we also change the threshold to 1, otherwise it would fail even if cdi8 could be removed. 
                          metadata = makeDummyHeader cdi8address 8 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Reject RemoveFirstCredential
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Trying to remove cdi9 that is already removed -  should reject
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.empty) [cdi9ID] 2,
                          metadata = makeDummyHeader cdi8address 9 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Reject $ NonExistentCredIDs [cdi9ID]
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Trying to add cdi10 whos credID is already in use - should reject
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 1 cdi10) [] 2,
                          metadata = makeDummyHeader cdi8address 10 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Reject $ DuplicateCredIDs [cdi10ID]
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Trying to add cdi9 whos credID has been used earlier - should reject
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 1 cdi9) [] 2,
                          metadata = makeDummyHeader cdi8address 11 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( Reject $ DuplicateCredIDs [cdi9ID]
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2
          )
        )
      , -- Adding cdi11 at index 2 should succeed since we are deleting cdi10 
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 2 cdi11) [cdi10ID] 2,
                          metadata = makeDummyHeader cdi8address 12 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])] 
                        }
        , ( SuccessE [CredentialsUpdated cdi8address [cdi11ID] [cdi10ID] 2]
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi11)] 2
          )
        )
      , -- Trying to add cdi7 which is not a cdi for deploying to cdi8 - should reject since for this purpose, cdi7 is invalid
        ( Runner.TJSON  { payload = Runner.UpdateCredentials (Map.singleton 3 cdi7) [] 2,
                          metadata = makeDummyHeader cdi8address 13 50000,
                          keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi11kp0), (1, cdi11kp1)])] 
                        }
        , ( Reject InvalidCredentials
          , checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi11)] 2
          )
        )
      ]
    }
  ]
    where
      -- Prompts the blockstate for account keys and checks that they match the expected ones.
      checkKeys expectedKeys expectedThreshold = (\bs -> specify "Correct account keys" $
        case Acc.getAccount cdi8address (bs ^. blockAccounts) of
          Nothing -> HUnit.assertFailure $ "Account with id '" ++ show cdi8address ++ "' not found"
          Just account -> do 
            checkAccountKeys expectedKeys expectedThreshold $ account ^. accountVerificationKeys
            checkAllCredentialKeys expectedKeys $ account ^. accountCredentials)

-- Checks that the keys in the AccountInformation matches the ones in the list, that there isn't
-- any other keys than these in the AccountInformation and that the signature threshold matches.
checkAccountKeys :: [(ID.CredentialIndex, ID.CredentialPublicKeys)] -> ID.AccountThreshold -> ID.AccountInformation -> HUnit.Assertion
checkAccountKeys keys threshold actualKeys@ID.AccountInformation{..} = do
  HUnit.assertEqual "Account Threshold Matches" threshold aiThreshold
  HUnit.assertEqual "Account keys should have same number of keys" (length keys) (length aiCredentials)
  forM_ keys (\(idx, key) -> case Map.lookup idx aiCredentials of
    Nothing -> HUnit.assertFailure $ "Found no key at index " ++ show idx
    Just actualKey -> HUnit.assertEqual ("Key at index " ++ (show idx) ++ " should be equal") key actualKey)

-- Checks the keys inside the relevant credentials are correct
checkAllCredentialKeys :: [(ID.CredentialIndex, ID.CredentialPublicKeys)] -> Map.Map ID.CredentialIndex ID.AccountCredential -> HUnit.Assertion
checkAllCredentialKeys keys credentials = do
  HUnit.assertEqual "Account keys should have same number of keys" (length keys) (length credentials)
  let keysInCredentials = fmap credPubKeys credentials
  forM_ keys (\(idx, key) -> case Map.lookup idx keysInCredentials of
    Nothing -> HUnit.assertFailure $ "Found no credential with index " ++ show idx
    Just actualKey -> HUnit.assertEqual ("Key at index " ++ (show idx) ++ " should be equal") key actualKey)



tests :: Spec
tests = describe "UpdateCredentials" $
  mkSpecs testCases
