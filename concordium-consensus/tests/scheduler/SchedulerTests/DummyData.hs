{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.DummyData where

import qualified Data.ByteString.Char8 as BS
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
import Concordium.GlobalState.Bakers

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


dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters = CryptographicParameters {
  elgamalGenerator = ElgamalGenerator "",
  attributeCommitmentKey = PedersenKey ""
  }

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
            ,cdvIpId = IP_ID "ip_id"
            ,cdvPolicy = Policy 0 0 []
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
