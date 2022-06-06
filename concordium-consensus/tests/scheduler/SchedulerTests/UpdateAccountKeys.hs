{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module SchedulerTests.UpdateAccountKeys where

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
import            Concordium.GlobalState.DummyData
import            Concordium.ID.Types as ID
import            Concordium.Scheduler.DummyData
import qualified  Concordium.Scheduler.Runner as Runner
import            Concordium.Scheduler.Types
import            Concordium.Types.DummyData
import qualified  Data.Map as Map
import            SchedulerTests.TestUtils


initialBlockState :: BlockState PV1
initialBlockState = createBlockState $
                    Acc.putAccountWithRegIds (mkAccountMultipleKeys [vk kp0, vk kp1] 2 alesAccount 10000000000)
                    Acc.emptyAccounts

initialBlockState2 :: BlockState PV1
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

alesCid :: CredentialRegistrationID
alesCid = dummyRegId dummyCryptographicParameters alesAccount

testCases :: [TestCase PV1]
testCases =
  [ TestCase
    { tcName = "Credential key updates"
    , tcParameters = (defaultParams @PV1){tpInitialBlockState=initialBlockState}
    , tcTransactions = [
        -- correctly update a keypair
        ( Runner.TJSON  { payload = Runner.UpdateCredentialKeys alesCid $ makeCredentialPublicKeys [vk kp2, vk kp1] 2,
                          metadata = makeDummyHeader alesAccount 1 10000,
                          keys = [(0, [(0, kp0), (1, kp1)])]
                        }
        , ( SuccessE [CredentialKeysUpdated alesCid]
          , checkKeys [(0, vk kp2), (1, vk kp1)] 2
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
          Just account -> do
            checkCredentialKeys expectedKeys expectedThreshold (aiCredentials (account ^. accountVerificationKeys) Map.! 0)
            checkKeysInCredential expectedKeys expectedThreshold ((account ^. accountCredentials) Map.! 0))

-- Checks that the keys in the AccountKeys matches the ones in the list, that there isn't
-- any other keys than these in the AccountKeys and that the signature threshold matches.
checkCredentialKeys :: [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> ID.CredentialPublicKeys -> HUnit.Assertion
checkCredentialKeys keys threshold ID.CredentialPublicKeys{..} = do
  HUnit.assertEqual "Signature Threshold Matches" threshold credThreshold
  HUnit.assertEqual "Account keys should have same number of keys" (length keys) (length credKeys)
  forM_ keys (\(idx, key) -> case Map.lookup idx credKeys of
    Nothing -> HUnit.assertFailure $ "Found no key at index " ++ show idx
    Just actualKey -> HUnit.assertEqual ("Key at index " ++ show idx ++ " should be equal") key actualKey)

checkKeysInCredential :: [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> ID.AccountCredentialRaw -> HUnit.Assertion
checkKeysInCredential keys threshold credential = checkCredentialKeys keys threshold $ credPubKeys credential

tests :: Spec
tests = do
  describe "UpdateCredentialKeys" $
    mkSpecs testCases
