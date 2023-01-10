{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Concordium.Scheduler.DummyData {-# WARNING "This module should not be used in production code." #-} where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.Map.Strict as Map
import Data.Time
import qualified System.Random as Random

import Concordium.Common.Version
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import Concordium.ID.DummyData
import Concordium.ID.Types
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import Concordium.Types.DummyData

-- |Maximum possible expiry of a message.
maxExpiry :: TransactionExpiryTime
maxExpiry = TransactionTime maxBound

-- Make a header assuming there is only one key on the account, its index is 0
{-# WARNING makeHeaderWithExpiry "Do not use in production." #-}
makeHeaderWithExpiry :: AccountAddress -> Nonce -> Energy -> TransactionExpiryTime -> Runner.TransactionHeader
makeHeaderWithExpiry = Runner.TransactionHeader

-- NB: In order for tests to work, the slot time (currently set to 0)
-- must be <= than transaction expiry time (currently also set to 0
-- in `dummyTransactionExpiryTime`)
{-# WARNING makeDummyHeader "Do not use in production." #-}
makeDummyHeader :: AccountAddress -> Nonce -> Energy -> Runner.TransactionHeader
makeDummyHeader a n e = makeHeaderWithExpiry a n e dummyMaxTransactionExpiryTime

{-# WARNING cdi1 "Do not use in production." #-}
cdi1 :: AccountCreation
cdi1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-1.json" >>= embedFile)
{-# WARNING cdi2 "Do not use in production." #-}
cdi2 :: AccountCreation
cdi2 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-2.json" >>= embedFile)
{-# WARNING cdi3 "Do not use in production." #-}
cdi3 :: AccountCreation
cdi3 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-3.json" >>= embedFile)

-- credential 4 should have the same reg id as credential 3, so should be rejected
{-# WARNING cdi4 "Do not use in production." #-}
cdi4 :: AccountCreation
cdi4 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-4.json" >>= embedFile)

-- Credentials 5 and 6 should have the same account address
{-# WARNING cdi5 "Do not use in production." #-}
cdi5 :: AccountCreation
cdi5 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-5.json" >>= embedFile)

-- {-# WARNING cdi6 "Do not use in production." #-}
-- cdi6 :: AccountCreation
-- cdi6 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-6.json" >>= embedFile)
{-# WARNING cdi7 "Do not use in production." #-}
cdi7 :: AccountCreation
cdi7 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-7.json" >>= embedFile)

readAccountCreation :: BSL.ByteString -> AccountCreation
readAccountCreation bs =
    case AE.eitherDecode bs of
        Left err -> error $ "Cannot read account creation " ++ err
        Right d -> if vVersion d == 0 then vValue d else error "Incorrect account creation version."

newtype SigningKeys = SigningKeys
    { keys :: (Map.Map KeyIndex Sig.KeyPair) -- [(KeyIndex, Sig.KeyPair)]
    }

instance AE.FromJSON SigningKeys where
    parseJSON = AE.withObject "SigningKeys" $ \v -> do
        keys <- v AE..: "keys"
        return SigningKeys{..}

{-# WARNING readSigningKeys "Do not use in production." #-}
readSigningKeys :: BSL.ByteString -> SigningKeys
readSigningKeys bs =
    case AE.eitherDecode bs of
        Left err -> error $ "Cannot read credential because " ++ err
        Right d -> d

{-# WARNING ac8 "Do not use in production." #-}
ac8 :: AccountCreation
ac8 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-8.json" >>= embedFile)
{-# WARNING cdi8keys "Do not use in production." #-}
cdi8keys :: SigningKeys
cdi8keys = readSigningKeys . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-private-keys-8.json" >>= embedFile)
{-# WARNING cdi9 "Do not use in production." #-}
cdi9 :: CredentialDeploymentInformation
cdi9 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-9.json" >>= embedFile)
{-# WARNING cdi9keys "Do not use in production." #-}
cdi9keys :: SigningKeys
cdi9keys = readSigningKeys . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-private-keys-9.json" >>= embedFile)
{-# WARNING cdi10 "Do not use in production." #-}
cdi10 :: CredentialDeploymentInformation
cdi10 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-10.json" >>= embedFile)
{-# WARNING cdi10keys "Do not use in production." #-}
cdi10keys :: SigningKeys
cdi10keys = readSigningKeys . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-private-keys-10.json" >>= embedFile)
{-# WARNING cdi11 "Do not use in production." #-}
cdi11 :: CredentialDeploymentInformation
cdi11 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-11.json" >>= embedFile)
{-# WARNING cdi11keys "Do not use in production." #-}
cdi11keys :: SigningKeys
cdi11keys = readSigningKeys . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-private-keys-11.json" >>= embedFile)

-- {-# WARNING cdi12 "Do not use in production." #-}
-- cdi12 :: CredentialDeploymentInformation
-- cdi12 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-11.json" >>= embedFile)
-- {-# WARNING cdi13 "Do not use in production." #-}
-- cdi13 :: CredentialDeploymentInformation
-- cdi13 = readCredential . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-12.json" >>= embedFile)

{-# WARNING icdi1 "Do not use in production." #-}
icdi1 :: AccountCreation
icdi1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-1.json" >>= embedFile)
{-# WARNING icdi2 "Do not use in production." #-}
icdi2 :: AccountCreation
icdi2 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-2.json" >>= embedFile)
{-# WARNING icdi3 "Do not use in production." #-}
icdi3 :: AccountCreation
icdi3 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-3.json" >>= embedFile)
{-# WARNING icdi4 "Do not use in production." #-}
icdi4 :: AccountCreation
icdi4 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-4.json" >>= embedFile)

{-# WARNING dummyBlockSize "Do not use in production." #-}
dummyBlockSize :: Integer
dummyBlockSize = 10000000000

{-# WARNING dummyBlockTimeout "Do not use in production." #-}
dummyBlockTimeout :: UTCTime
dummyBlockTimeout = read "2100-09-23 13:27:13.257285424 UTC"

dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata{slotTime = 0}

dummyMaxCredentials :: CredentialsPerBlockLimit
dummyMaxCredentials = 10

-- |Generate an account credential with a single keypair and sufficiently late expiry date.
makeTestCredential ::
    Sig.VerifyKey ->
    AccountAddress ->
    AccountCredential
makeTestCredential key accountAddress =
    dummyCredential
        dummyCryptographicParameters
        accountAddress
        key
        dummyMaxValidTo
        dummyCreatedAt

-- |Generate an account credential with a single keypair and sufficiently late expiry date
-- deterministically from a seed.
makeTestCredentialFromSeed :: Int -> AccountCredential
makeTestCredentialFromSeed seed =
    let keyPair = keyPairFromSeed seed
        address = accountAddressFromSeed seed
    in  makeTestCredential (Sig.correspondingVerifyKey keyPair) address

-- |Generate an account providing a verify key, account address and the initial balance.
-- The generated account have a single credential and single keypair, which has sufficiently late
-- expiry date.
makeTestAccount ::
    (IsAccountVersion av, Blob.MonadBlobStore m) =>
    Sig.VerifyKey ->
    AccountAddress ->
    Amount ->
    m (BS.PersistentAccount av)
makeTestAccount key accountAddress amount = do
    let credential = makeTestCredential key accountAddress
    account <- BS.newAccount dummyCryptographicParameters accountAddress credential
    BS.addAccountAmount amount account

-- |Generate a test account keypair deterministically from a seed.
-- Note this is also used internally by `makeTestAccountFromSeed` and providing the same seed
-- results in the same keypair as used for the generated account.
keyPairFromSeed :: Int -> Sig.KeyPair
keyPairFromSeed =
    uncurry Sig.KeyPairEd25519
        . fst
        . randomEd25519KeyPair
        . Random.mkStdGen

-- |Generate an account address deterministically from a seed.
-- Note this is also used internally by `makeTestAccountFromSeed` and providing the same seed
-- results in the same account address as used for the generated account.
accountAddressFromSeed :: Int -> AccountAddress
accountAddressFromSeed = accountAddressFrom

-- |Generate a test account with the provided amount as balance. The generated account have a single
-- credential and single keypair, which has sufficiently late expiry date. The keypair and address
-- is generated deterministically from a seed.
makeTestAccountFromSeed ::
    (IsAccountVersion av, Blob.MonadBlobStore m) =>
    Amount ->
    Int ->
    m (BS.PersistentAccount av)
makeTestAccountFromSeed amount seed =
    let keyPair = keyPairFromSeed seed
        address = accountAddressFromSeed seed
    in  makeTestAccount (Sig.correspondingVerifyKey keyPair) address amount
