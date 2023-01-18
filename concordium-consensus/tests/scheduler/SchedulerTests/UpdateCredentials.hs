{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.UpdateCredentials (tests) where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState = do
    account <-
        BS.newAccount dummyCryptographicParameters cdi8address cdi8ac
            >>= BS.addAccountAmount 10_000_000_000

    Helpers.createTestBlockStateWithAccounts [account]

cdiKeys :: CredentialDeploymentInformation -> CredentialPublicKeys
cdiKeys cdi = credPubKeys (NormalACWP cdi)

cdi8ID :: CredentialRegistrationID
cdi8ID = credId $ credential ac8

cdi8address :: AccountAddress
cdi8address = addressFromRegId cdi8ID
cdi8kp0 :: Sig.KeyPair
cdi8kp0 = keys cdi8keys Map.! 0
cdi8kp1 :: Sig.KeyPair
cdi8kp1 = keys cdi8keys Map.! 1

cdi8ac :: AccountCredential
cdi8ac = fromMaybe (error "Should be safe") $ values $ credential ac8

cdi7'' :: CredentialDeploymentInformation
cdi7'' = case credential cdi7 of
    NormalACWP cdi -> cdi
    _ -> error "cdi7 should be a normal credential. Something went wrong with test case generation."
cdi8 :: CredentialDeploymentInformation
cdi8 = case credential ac8 of
    NormalACWP cdi -> cdi
    _ -> error "cdi8 should be a normal credential. Something went wrong with test case generation."

cdi9ID :: CredentialRegistrationID
cdi9ID = credId (NormalACWP cdi9)
cdi9kp0 :: Sig.KeyPair
cdi9kp0 = keys cdi9keys Map.! 0
cdi9kp1 :: Sig.KeyPair
cdi9kp1 = keys cdi9keys Map.! 1

cdi10ID :: CredentialRegistrationID
cdi10ID = credId (NormalACWP cdi10)
cdi10kp0 :: Sig.KeyPair
cdi10kp0 = keys cdi10keys Map.! 0
cdi10kp1 :: Sig.KeyPair
cdi10kp1 = keys cdi10keys Map.! 1

cdi11ID :: CredentialRegistrationID
cdi11ID = credId (NormalACWP cdi11)
cdi11kp0 :: Sig.KeyPair
cdi11kp0 = keys cdi11keys Map.! 0
cdi11kp1 :: Sig.KeyPair
cdi11kp1 = keys cdi11keys Map.! 1

updateAccountCredentialTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
updateAccountCredentialTest _ pvString =
    specify (pvString ++ ": Credential key updates") $
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ -- correctly update a keypair
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 1 cdi9) [] 1,
                      metadata = makeDummyHeader cdi8address 1 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 1 state
                return $ do
                    Helpers.assertSuccessWithEvents
                        [CredentialsUpdated cdi8address [cdi9ID] [] 1]
                        result
                    doCheckKeys
            },
          -- Correctly update threshold
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [] 2,
                      metadata = makeDummyHeader cdi8address 2 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2 state
                return $ do
                    Helpers.assertSuccessWithEvents [CredentialsUpdated cdi8address [] [] 2] result
                    doCheckKeys
            },
          -- Now, using the only one set of keys should fail, since threshold were updated
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 2 cdi10) [] 2,
                      metadata = makeDummyHeader cdi8address 3 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)])] -- not enough credential holders signing since threshold is now 2
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2 state
                return $ do
                    Helpers.assertFailureWithReason IncorrectSignature result
                    doCheckKeys
            },
          -- Should reject since we try to add cdi10 at index 1
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 1 cdi10) [] 2,
                      metadata = makeDummyHeader cdi8address 3 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])] -- now two credential holders are signing
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (1, cdiKeys cdi9)] 2 state
                return $ do
                    Helpers.assertRejectWithReason KeyIndexAlreadyInUse result
                    doCheckKeys
            },
          -- Now, using the new keys should work
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 2 cdi10) [] 2,
                      metadata = makeDummyHeader cdi8address 4 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])] -- now two credential holders are signing
                    },
              taaAssertion = \result state -> do
                doCheckKeys <-
                    checkKeys
                        [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)]
                        2
                        state
                return $ do
                    Helpers.assertSuccessWithEvents
                        [CredentialsUpdated cdi8address [cdi10ID] [] 2]
                        result
                    doCheckKeys
            },
          -- Updating threshold to 3
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [] 3,
                      metadata = makeDummyHeader cdi8address 5 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <-
                    checkKeys
                        [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)]
                        3
                        state
                return $ do
                    Helpers.assertSuccessWithEvents [CredentialsUpdated cdi8address [] [] 3] result
                    doCheckKeys
            },
          -- Trying to remove cdi9 -  should reject since threshold is 3
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [cdi9ID] 3,
                      metadata = makeDummyHeader cdi8address 6 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <-
                    checkKeys
                        [(0, cdiKeys cdi8), (1, cdiKeys cdi9), (2, cdiKeys cdi10)]
                        3
                        state
                return $ do
                    Helpers.assertRejectWithReason InvalidAccountThreshold result
                    doCheckKeys
            },
          -- Trying to remove cdi9 -  should succeed since threshold is changed to 2
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [cdi9ID] 2,
                      metadata = makeDummyHeader cdi8address 7 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertSuccessWithEvents
                        [CredentialsUpdated cdi8address [] [cdi9ID] 2]
                        result
                    doCheckKeys
            },
          -- Trying to sign with cdi9's keys -  should fail since cdi9 was removed
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [] 2,
                      metadata = makeDummyHeader cdi8address 8 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (1, [(0, cdi9kp0), (1, cdi9kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertFailureWithReason IncorrectSignature result
                    doCheckKeys
            },
          -- Trying to remove cdi8 -  should reject since it is the first credential
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [cdi8ID] 1, -- notice that we also change the threshold to 1, otherwise it would fail even if cdi8 could be removed.
                      metadata = makeDummyHeader cdi8address 8 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertRejectWithReason RemoveFirstCredential result
                    doCheckKeys
            },
          -- Trying to remove cdi9 that is already removed -  should reject
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials Map.empty [cdi9ID] 2,
                      metadata = makeDummyHeader cdi8address 9 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertRejectWithReason (NonExistentCredIDs [cdi9ID]) result
                    doCheckKeys
            },
          -- Trying to add cdi10 whos credID is already in use - should reject
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 1 cdi10) [] 2,
                      metadata = makeDummyHeader cdi8address 10 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertRejectWithReason (DuplicateCredIDs [cdi10ID]) result
                    doCheckKeys
            },
          -- Trying to add cdi9 whos credID has been used earlier - should reject
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 1 cdi9) [] 2,
                      metadata = makeDummyHeader cdi8address 11 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi10)] 2 state
                return $ do
                    Helpers.assertRejectWithReason (DuplicateCredIDs [cdi9ID]) result
                    doCheckKeys
            },
          -- Adding cdi11 at index 2 should succeed since we are deleting cdi10
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 2 cdi11) [cdi10ID] 2,
                      metadata = makeDummyHeader cdi8address 12 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi10kp0), (1, cdi10kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi11)] 2 state
                return $ do
                    Helpers.assertSuccessWithEvents [CredentialsUpdated cdi8address [cdi11ID] [cdi10ID] 2] result
                    doCheckKeys
            },
          -- Trying to add cdi7 which is not a cdi for deploying to cdi8 - should reject since for this purpose, cdi7 is invalid
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.UpdateCredentials (Map.singleton 3 cdi7'') [] 2,
                      metadata = makeDummyHeader cdi8address 13 100_000,
                      keys = [(0, [(0, cdi8kp0), (1, cdi8kp1)]), (2, [(0, cdi11kp0), (1, cdi11kp1)])]
                    },
              taaAssertion = \result state -> do
                doCheckKeys <- checkKeys [(0, cdiKeys cdi8), (2, cdiKeys cdi11)] 2 state
                return $ do
                    Helpers.assertRejectWithReason InvalidCredentials result
                    doCheckKeys
            }
        ]

    checkKeys :: [(ID.CredentialIndex, ID.CredentialPublicKeys)] -> ID.AccountThreshold -> BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    checkKeys expectedKeys expectedThreshold state = do
        maybeAccount <- BS.bsoGetAccount state cdi8address
        case maybeAccount of
            Nothing -> return $ assertFailure $ "Account with address '" ++ show cdi8address ++ "' not found"
            Just (_, account) -> do
                accountInformation <- BS.accountVerificationKeys account
                credentials <- BS.accountCredentials account
                return $ do
                    checkAccountKeys expectedKeys expectedThreshold accountInformation
                    checkAllCredentialKeys expectedKeys credentials

-- Checks that the keys in the AccountInformation matches the ones in the list, that there isn't
-- any other keys than these in the AccountInformation and that the signature threshold matches.
checkAccountKeys :: [(ID.CredentialIndex, ID.CredentialPublicKeys)] -> ID.AccountThreshold -> ID.AccountInformation -> Assertion
checkAccountKeys keys threshold ID.AccountInformation{..} = do
    assertEqual "Account Threshold Matches" threshold aiThreshold
    assertEqual "Account keys should have same number of keys" (length keys) (length aiCredentials)
    forM_
        keys
        ( \(idx, key) -> case Map.lookup idx aiCredentials of
            Nothing -> assertFailure $ "Found no key at index " ++ show idx
            Just actualKey -> assertEqual ("Key at index " ++ show idx ++ " should be equal") key actualKey
        )

-- Checks the keys inside the relevant credentials are correct
checkAllCredentialKeys :: [(ID.CredentialIndex, ID.CredentialPublicKeys)] -> Map.Map ID.CredentialIndex ID.RawAccountCredential -> Assertion
checkAllCredentialKeys keys credentials = do
    assertEqual "Account keys should have same number of keys" (length keys) (length credentials)
    let keysInCredentials = fmap credPubKeys credentials
    forM_
        keys
        ( \(idx, key) -> case Map.lookup idx keysInCredentials of
            Nothing -> assertFailure $ "Found no credential with index " ++ show idx
            Just actualKey -> assertEqual ("Key at index " ++ show idx ++ " should be equal") key actualKey
        )

tests :: Spec
tests =
    describe "UpdateCredentials" $
        sequence_ $
            Helpers.forEveryProtocolVersion updateAccountCredentialTest
