{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.DummyData where

import qualified Data.Hashable as IntHash
import qualified Data.PQueue.Prio.Max as Queue
import qualified Data.ByteString.Lazy as BSL
import qualified Data.FixedByteString as FBS
import Concordium.Crypto.SHA256(Hash(..), hash)
import Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types hiding (accountAddress)
import Concordium.ID.Account
import Concordium.ID.Types
import qualified Concordium.Crypto.Ed25519Signature as Ed25519

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Environment as Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as OrdMap
import qualified Data.Aeson as AE

import Lens.Micro.Platform

import System.IO.Unsafe
import System.Random

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

-- Make a header assuming there is only one key on the account, its index is 0
makeHeaderWithExpiry :: AccountAddress -> Nonce -> Energy -> TransactionExpiryTime -> Runner.TransactionHeader
makeHeaderWithExpiry = Runner.TransactionHeader

makeHeader :: AccountAddress -> Nonce -> Energy -> Runner.TransactionHeader
makeHeader a n e = makeHeaderWithExpiry a n e dummyTransactionExpiryTime

alesKP :: KeyPair
alesKP = uncurry Sig.KeyPairEd25519 . fst $ Ed25519.randomKeyPair (mkStdGen 1)

alesVK :: VerifyKey
alesVK = correspondingVerifyKey alesKP

alesAccount :: AccountAddress
alesAccount = accountAddressFrom 1

thomasKP :: KeyPair
thomasKP = uncurry Sig.KeyPairEd25519 . fst $ Ed25519.randomKeyPair (mkStdGen 2)

thomasVK :: VerifyKey
thomasVK = correspondingVerifyKey thomasKP

thomasAccount :: AccountAddress
thomasAccount = accountAddressFrom 2

accountAddressFrom :: Int -> AccountAddress
accountAddressFrom n = fst (randomAccountAddress (mkStdGen n))

accountVFKeyFrom :: Int -> VerifyKey
accountVFKeyFrom = correspondingVerifyKey . uncurry Sig.KeyPairEd25519 . fst . Ed25519.randomKeyPair . mkStdGen

-- This credential value is invalid and does not satisfy the invariants normally expected of credentials.
-- Should only be used when only the existence of a credential is needed in testing, but the credential
-- will neither be serialized, nor inspected.
dummyCredential :: AccountAddress -> CredentialExpiryTime -> CredentialDeploymentValues
dummyCredential aaddr pExpiry  = CredentialDeploymentValues
    {
      cdvAccount = ExistingAccount aaddr,
      cdvRegId = dummyRegId aaddr,
      cdvIpId = IP_ID 0,
      cdvThreshold = Threshold 2,
      cdvArData = [],
      cdvPolicy = Policy {
        pAttributeListVariant = 0,
        pItems = OrdMap.empty,
        ..
        },
      ..
    }

-- Derive a dummy registration id from an account address. This hashes the
-- account address derived from the verification key, and uses it as a seed of a
-- random number generator.
dummyRegId :: AccountAddress -> CredentialRegistrationID
dummyRegId addr = RegIdCred . FBS.pack $ bytes
  where bytes = take (FBS.fixedLength (undefined :: RegIdSize)) . randoms . mkStdGen $ IntHash.hash addr

-- This generates an account without any credentials
-- late expiry date, but is otherwise not well-formed.
mkAccountNoCredentials :: VerifyKey -> AccountAddress -> Amount -> Account
mkAccountNoCredentials key addr amnt = newAccount (makeSingletonAC key) addr & (accountAmount .~ amnt)

-- This generates an account with a single credential and single keypair, which has sufficiently
-- late expiry date, but is otherwise not well-formed.
mkAccount :: VerifyKey -> AccountAddress -> Amount -> Account
mkAccount key addr amnt = mkAccountNoCredentials key addr amnt &
                           (accountCredentials .~ (Queue.singleton dummyExpiryTime (dummyCredential addr dummyExpiryTime)))

dummyExpiryTime :: CredentialExpiryTime
dummyExpiryTime = 1

dummyTransactionExpiryTime :: TransactionExpiryTime
dummyTransactionExpiryTime = 0

dummySlotTime :: Timestamp
dummySlotTime = 0

emptyBirkParameters :: BirkParameters
emptyBirkParameters = BirkParameters {
  _birkElectionDifficulty = 0.5,
  _birkCurrentBakers = emptyBakers,
  _birkPrevEpochBakers = emptyBakers,
  _birkLotteryBakers = emptyBakers,
  _birkSeedState = genesisSeedState (hash "NONCE") 360
  }

bakerElectionKey :: Int -> BakerElectionPrivateKey
bakerElectionKey n = fst (VRF.randomKeyPair (mkStdGen n))

bakerSignKey :: Int -> BakerSignPrivateKey
bakerSignKey n = fst (BlockSig.randomKeyPair (mkStdGen n))

bakerAggregationKey :: Int -> BakerAggregationPrivateKey
bakerAggregationKey n = fst (Bls.randomSecretKey (mkStdGen n))

-- |Make a baker deterministically from a given seed and with the given reward account.
-- Uses 'bakerElectionKey' and 'bakerSignKey' with the given seed to generate the keys.
-- The baker has 0 lottery power.
-- mkBaker :: Int -> AccountAddress -> (BakerInfo
mkBaker :: Int -> AccountAddress -> (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
mkBaker seed acc = (BakerInfo {
  _bakerElectionVerifyKey = VRF.publicKey electionKey,
  _bakerSignatureVerifyKey = BlockSig.verifyKey sk,
  _bakerAggregationVerifyKey = Bls.derivePublicKey blssk,
  _bakerStake = 0,
  _bakerAccount = acc
  }, VRF.privateKey electionKey, BlockSig.signKey sk, blssk)
  where electionKey = bakerElectionKey seed
        sk = bakerSignKey seed
        blssk = bakerAggregationKey seed

readCredential :: FilePath -> IO CredentialDeploymentInformation
readCredential fp = do
  bs <- BSL.readFile fp
  case AE.eitherDecode bs of
    Left err -> fail $ "Cannot read credential from file " ++ fp ++ " because " ++ err
    Right d -> return d

{-# NOINLINE cdi1 #-}
{-# NOINLINE cdi2 #-}
{-# NOINLINE cdi3 #-}
{-# NOINLINE cdi4 #-}
{-# NOINLINE cdi5 #-}
{-# NOINLINE cdi6 #-}
{-# NOINLINE cdi7 #-}
cdi1 :: CredentialDeploymentInformation
cdi1 = unsafePerformIO (readCredential "testdata/credential-1.json")
cdi2 :: CredentialDeploymentInformation
cdi2 = unsafePerformIO (readCredential "testdata/credential-2.json")
cdi3 :: CredentialDeploymentInformation
cdi3 = unsafePerformIO (readCredential "testdata/credential-3.json")
-- credential 4 should have the same reg id as credential 3, so should be rejected
cdi4 :: CredentialDeploymentInformation
cdi4 = unsafePerformIO (readCredential "testdata/credential-4.json")
-- Credentials 5 and 6 should have the same account address
cdi5 :: CredentialDeploymentInformation
cdi5 = unsafePerformIO (readCredential "testdata/credential-5.json")
cdi6 :: CredentialDeploymentInformation
cdi6 = unsafePerformIO (readCredential "testdata/credential-6.json")
cdi7 :: CredentialDeploymentInformation
cdi7 = unsafePerformIO (readCredential "testdata/credential-7.json")

accountAddressFromCred :: CredentialDeploymentInformation -> AccountAddress
accountAddressFromCred = credentialAccountAddress . cdiValues

{-# NOINLINE dummyIdentityProviders #-}
dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders =
  case unsafePerformIO (eitherReadIdentityProviders <$> BSL.readFile "testdata/identity_providers.json") of
    Left err -> error $ "Could not load identity provider test data: " ++ err
    Right ips -> IdentityProviders (HM.fromList (map (\r -> (ipIdentity r, r)) ips))

dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (readCryptographicParameters <$> BSL.readFile "testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

blockSize :: Integer
blockSize = 10000000000

dummySpecialBetaAccounts :: Types.SpecialBetaAccounts
dummySpecialBetaAccounts = Types.emptySpecialBetaAccounts
