{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.UpdateAccountKeys (tests) where

import Control.Monad
import qualified Data.Map as Map
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.DummyData
import Concordium.ID.Types as ID
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests = do
    describe "UpdateCredentialKeys" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                credentialKeyUpdateTest spv pvString

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

keyPair2 :: SigScheme.KeyPair
keyPair2 = Helpers.keyPairFromSeed 2

verifyKey :: SigScheme.KeyPair -> SigScheme.VerifyKey
verifyKey = SigScheme.correspondingVerifyKey

credentialRegistrationId0 :: CredentialRegistrationID
credentialRegistrationId0 = dummyRegId dummyCryptographicParameters accountAddress0

account0 ::
    (Types.IsAccountVersion av, Blob.MonadBlobStore m) => m (BS.PersistentAccount av)
account0 = do
    account <- Helpers.makeTestAccountFromSeed 10_000_000_000 0
    let threshold = 2
    let credentialPublicKeys =
            makeCredentialPublicKeys
                [verifyKey keyPair0, verifyKey keyPair1]
                threshold
    BS.updateAccountCredentialKeys 0 credentialPublicKeys account

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [account0]

credentialKeyUpdateTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
credentialKeyUpdateTest _ pvString =
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
                    { payload =
                        Runner.UpdateCredentialKeys
                            credentialRegistrationId0
                            $ makeCredentialPublicKeys
                                [verifyKey keyPair2, verifyKey keyPair1]
                                2,
                      metadata = makeDummyHeader accountAddress0 1 10_000,
                      keys = [(0, [(0, keyPair0), (1, keyPair1)])]
                    },
              taaAssertion = \Helpers.SchedulerResult{..} state -> do
                doCheckKeys <- checkKeys state [(0, verifyKey keyPair2), (1, verifyKey keyPair1)] 2
                return $ do
                    case Helpers.getResults $ Sch.ftAdded srTransactions of
                        [(_, Types.TxSuccess events)] ->
                            assertEqual
                                "The correct update event is produced"
                                [CredentialKeysUpdated credentialRegistrationId0]
                                events
                        other -> assertFailure $ "Transaction rejected unexpectedly: " ++ show other
                    doCheckKeys
            }
        ]
    checkKeys :: BS.PersistentBlockState pv -> [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> Helpers.PersistentBSM pv Assertion
    checkKeys state expectedKeys expectedThreshold = do
        maybeAccount <- BS.bsoGetAccount state accountAddress0
        case maybeAccount of
            Nothing -> return $ assertFailure $ "Account with address '" ++ show accountAddress0 ++ "' not found"
            Just (_, account) -> do
                accountInformation <- BS.accountVerificationKeys account
                credentials <- BS.accountCredentials account
                return $ do
                    checkCredentialKeys expectedKeys expectedThreshold (aiCredentials accountInformation Map.! 0)
                    checkKeysInCredential expectedKeys expectedThreshold (credentials Map.! 0)

-- Checks that the keys in the AccountKeys matches the ones in the list, that there isn't
-- any other keys than these in the AccountKeys and that the signature threshold matches.
checkCredentialKeys :: [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> ID.CredentialPublicKeys -> Assertion
checkCredentialKeys keys threshold ID.CredentialPublicKeys{..} = do
    assertEqual "Signature Threshold Matches" threshold credThreshold
    assertEqual "Account keys should have same number of keys" (length keys) (length credKeys)
    forM_
        keys
        ( \(idx, key) -> case Map.lookup idx credKeys of
            Nothing -> assertFailure $ "Found no key at index " ++ show idx
            Just actualKey -> assertEqual ("Key at index " ++ show idx ++ " should be equal") key actualKey
        )

checkKeysInCredential :: [(ID.KeyIndex, AccountVerificationKey)] -> ID.SignatureThreshold -> ID.RawAccountCredential -> Assertion
checkKeysInCredential keys threshold credential = checkCredentialKeys keys threshold $ credPubKeys credential
