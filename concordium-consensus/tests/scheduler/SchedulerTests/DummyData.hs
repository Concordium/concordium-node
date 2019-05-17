module SchedulerTests.DummyData where

import qualified Data.FixedByteString as FBS
import Concordium.Crypto.SHA256(Hash(..))
import Concordium.Crypto.SignatureScheme as Sig
import Concordium.Types hiding (accountAddress)
import Concordium.GlobalState.Transactions
import Concordium.ID.Types
import Concordium.ID.Account
import Concordium.Crypto.Ed25519Signature

import System.Random

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

makeHeader :: Sig.KeyPair -> Nonce -> Amount -> TransactionHeader
makeHeader kp nonce amount = makeTransactionHeader Sig.Ed25519 (Sig.verifyKey kp) nonce amount blockPointer


alesKP :: KeyPair
alesKP = fst (randomKeyPair (mkStdGen 1))

alesACI :: AccountCreationInformation
alesACI = createAccount (verifyKey alesKP)

alesAccount :: AccountAddress
alesAccount = accountAddress alesACI


thomasKP :: KeyPair
thomasKP = fst (randomKeyPair (mkStdGen 2))

thomasACI :: AccountCreationInformation
thomasACI = createAccount (verifyKey thomasKP)

thomasAccount :: AccountAddress
thomasAccount = accountAddress thomasACI



