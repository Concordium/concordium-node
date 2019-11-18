{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.DummyData where

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
import qualified Data.Aeson as AE

import System.IO.Unsafe
import System.Random

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

makeHeader :: KeyPair -> Nonce -> Energy -> Runner.TransactionHeader
makeHeader kp = Runner.TransactionHeader (Sig.correspondingVerifyKey kp)

alesKP :: KeyPair
alesKP = uncurry Sig.KeyPairEd25519 . fst $ Ed25519.randomKeyPair (mkStdGen 1)

alesVK :: VerifyKey
alesVK = correspondingVerifyKey alesKP

alesAccount :: AccountAddress
alesAccount = accountAddress alesVK

thomasKP :: KeyPair
thomasKP = uncurry Sig.KeyPairEd25519 . fst $ Ed25519.randomKeyPair (mkStdGen 2)

thomasVK :: VerifyKey
thomasVK = correspondingVerifyKey thomasKP

thomasAccount :: AccountAddress
thomasAccount = accountAddress thomasVK

accountAddressFrom :: Int -> AccountAddress
accountAddressFrom n = accountAddress (accountVFKeyFrom n)

accountVFKeyFrom :: Int -> VerifyKey
accountVFKeyFrom = correspondingVerifyKey . uncurry Sig.KeyPairEd25519 . fst . Ed25519.randomKeyPair . mkStdGen

mkAccount ::AccountVerificationKey -> Amount -> Account
mkAccount vfKey amnt = (newAccount vfKey) {_accountAmount = amnt}

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
accountAddressFromCred cdi = accountAddress (cdvVerifyKey (cdiValues cdi))

{-# NOINLINE dummyIdentityProviders #-}
dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders =
  case unsafePerformIO (eitherReadIdentityProviders <$> BSL.readFile "testdata/identity-providers.json") of
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
