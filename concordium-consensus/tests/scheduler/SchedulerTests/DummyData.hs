{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.DummyData where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.FixedByteString as FBS
import Concordium.Crypto.SHA256(Hash(..))
import Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import Concordium.Types hiding (accountAddress)
import Concordium.GlobalState.Transactions
import Concordium.ID.Account
import Concordium.ID.Types
import Concordium.Crypto.Ed25519Signature

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Bakers
import qualified Data.Aeson as AE

import qualified Data.HashMap.Strict as HM

import System.IO.Unsafe
import System.Random

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

makeHeader :: Sig.KeyPair -> Nonce -> Energy -> TransactionHeader
makeHeader kp nonce amount = makeTransactionHeader Sig.Ed25519 (Sig.verifyKey kp) nonce amount blockPointer


alesKP :: KeyPair
alesKP = fst (randomKeyPair (mkStdGen 1))

alesVK :: VerifyKey
alesVK = verifyKey alesKP

alesAccount :: AccountAddress
alesAccount = accountAddress alesVK Ed25519

thomasKP :: KeyPair
thomasKP = fst (randomKeyPair (mkStdGen 2))

thomasVK :: VerifyKey
thomasVK = verifyKey thomasKP

thomasAccount :: AccountAddress
thomasAccount = accountAddress thomasVK Ed25519

accountAddressFrom :: Int -> AccountAddress
accountAddressFrom n = accountAddress (accountVFKeyFrom n) Ed25519

accountVFKeyFrom :: Int -> VerifyKey
accountVFKeyFrom = verifyKey . fst . randomKeyPair . mkStdGen 

mkAccount ::AccountVerificationKey -> Amount -> Account
mkAccount vfKey amnt = (newAccount vfKey Ed25519) {_accountAmount = amnt}


-- |Make a dummy credential deployment information from an account registration
-- id and sequential registration id. All the proofs are dummy values, and there
-- is no anoymity revocation data.
mkDummyCDI :: AccountVerificationKey -> Int -> CredentialDeploymentInformation
mkDummyCDI vfKey nregId =
    CredentialDeploymentInformation {
        cdiValues = CredentialDeploymentValues {
            cdvVerifyKey = vfKey
            ,cdvSigScheme = Ed25519
            ,cdvRegId = let d = show nregId
                            l = length d
                            pad = replicate (48-l) '0'
                        in RegIdCred (FBS.pack . map (fromIntegral . fromEnum) $ (pad ++ d))
            ,cdvIpId = IP_ID 0
            ,cdvPolicy = Policy 0 0 []
            ,cdvArData = AnonymityRevocationData {
                ardName = ARName 13,
                ardIdCredPubEnc = undefined -- FIXME
                }
            },
          cdiProofs = Proofs "proof"
        }

emptyBirkParameters :: BirkParameters
emptyBirkParameters = BirkParameters {
  _birkLeadershipElectionNonce = "",
  _birkElectionDifficulty = 0.5,
  _birkBakers = emptyBakers
  }

bakerElectionKey :: Int -> BakerElectionPrivateKey
bakerElectionKey n = fst (VRF.randomKeyPair (mkStdGen n))

bakerSignKey :: Int -> BakerSignPrivateKey
bakerSignKey n = fst (BlockSig.randomKeyPair (mkStdGen n))


-- |Make a baker deterministically from a given seed and with the given reward account.
-- Uses 'bakerElectionKey' and 'bakerSignKey' with the given seed to generate the keys.
-- The baker has 0 lottery power.
mkBaker :: Int -> AccountAddress -> BakerInfo
mkBaker seed acc = BakerInfo {
  _bakerElectionVerifyKey = VRF.publicKey (bakerElectionKey seed),
  _bakerSignatureVerifyKey = BlockSig.verifyKey (bakerSignKey seed),
  _bakerStake = 0,
  _bakerAccount = acc
  }

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
accountAddressFromCred cdi = accountAddress (cdvVerifyKey (cdiValues cdi)) (cdvSigScheme (cdiValues cdi))

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
