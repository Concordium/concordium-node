{-# LANGUAGE TemplateHaskell #-}

module Concordium.GlobalState.Account where

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Utils
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data PersistingAccountData = PersistingAccountData {
  _accountAddress :: !AccountAddress
  ,_accountEncryptionKey :: !AccountEncryptionKey
  ,_accountVerificationKeys :: !AccountKeys
  ,_accountCredentials :: ![CredentialDeploymentValues]
  -- ^Credentials; most recent first
  ,_accountMaxCredentialValidTo :: !CredentialValidTo
  ,_accountStakeDelegate :: !(Maybe BakerId)
  ,_accountInstances :: !(Set.Set ContractAddress)
} deriving (Show, Eq)

makeClassy ''PersistingAccountData

-- | Encrypted amounts stored on an account.
data AccountEncryptedAmount = AccountEncryptedAmount {
  -- | Starting index for encrypted amounts.
  _startIndex :: !EncryptedAmountAggIndex,
  -- | Amounts starting at @startIndex@. They are assumed to be numbered sequentially.
  _encryptedAmounts :: !(Seq.Seq EncryptedAmount)
} deriving(Eq, Show)

-- |Initial encrypted amount on a newly created account.
initialAccountEncryptedAmount :: AccountEncryptedAmount
initialAccountEncryptedAmount = AccountEncryptedAmount{
  _startIndex = 0,
  _encryptedAmounts = Seq.empty
}

instance Serialize AccountEncryptedAmount where
  put AccountEncryptedAmount{..} = 
    put _startIndex <>
    putWord32be (fromIntegral (Seq.length _encryptedAmounts)) <>
    mapM_ put _encryptedAmounts
  
  get = do 
    _startIndex <- get
    len <- getWord32be
    _encryptedAmounts <- Seq.fromList <$> replicateM (fromIntegral len) get
    return AccountEncryptedAmount{..}

makeLenses ''AccountEncryptedAmount

-- | Add an encrypted amount to the end of the list.
addEncryptedAmount :: EncryptedAmount -> AccountEncryptedAmount -> AccountEncryptedAmount
addEncryptedAmount newAmount accEncAmount = accEncAmount & encryptedAmounts %~ (Seq.|> newAmount)

-- | Drop the encrypted amount with indices up to the given one, and add the new amount at the end.
-- If the new index is before the 
replaceUpTo :: EncryptedAmountAggIndex -> EncryptedAmount -> AccountEncryptedAmount -> AccountEncryptedAmount
replaceUpTo newIndex newAmount AccountEncryptedAmount{..} = 
  AccountEncryptedAmount{
    _startIndex = newStartIndex,
    _encryptedAmounts = newEncryptedAmounts
  }
  where (newStartIndex, toDrop) = 
          if newIndex > _startIndex 
          then (newIndex, fromIntegral (newIndex - _startIndex)) 
          else (_startIndex, 0)
        newEncryptedAmounts = Seq.drop toDrop _encryptedAmounts Seq.|> newAmount

instance Serialize PersistingAccountData where
  put PersistingAccountData{..} = put _accountAddress <>
                                  put _accountEncryptionKey <>
                                  put _accountVerificationKeys <>
                                  put _accountCredentials <> -- The order is significant for hash computation
                                  put _accountStakeDelegate <>
                                  put (Set.toAscList _accountInstances)
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    _accountVerificationKeys <- get
    _accountCredentials <- get
    when (null _accountCredentials) $ fail "Account has no credentials"
    let _accountMaxCredentialValidTo = maximum (pValidTo . cdvPolicy <$> _accountCredentials)
    _accountStakeDelegate <- get
    _accountInstances <- Set.fromList <$> get
    return PersistingAccountData{..}

-- TODO To avoid recomputing the hash for the persisting account data each time we update an account
-- we might want to explicitly store its hash, too.
makeAccountHash :: Nonce -> Amount -> AccountEncryptedAmount -> PersistingAccountData -> Hash.Hash
makeAccountHash n a eas pd = Hash.hashLazy $ runPutLazy $
  put n >> put a >> put eas >> put pd

{-# INLINE addCredential #-}
addCredential :: HasPersistingAccountData d => CredentialDeploymentValues -> d -> d
addCredential cdv = (accountCredentials %~ (cdv:))
  . (accountMaxCredentialValidTo %~ max (pValidTo (cdvPolicy cdv)))

{-# INLINE setKey #-}
-- |Set a at a given index to a given value. The value of 'Nothing' will remove the key.
setKey :: HasPersistingAccountData d => KeyIndex -> Maybe VerifyKey -> d -> d
setKey idx key = accountVerificationKeys %~ (\ks -> ks { akKeys = akKeys ks & at' idx .~ key })

{-# INLINE setThreshold #-}
-- |Set the signature threshold.
setThreshold :: HasPersistingAccountData d => SignatureThreshold -> d -> d
setThreshold thr = accountVerificationKeys %~ (\ks -> ks { akThreshold = thr })

data EncryptedAmountUpdate = 
  -- |Replace encrypted amounts less than the given index,
  -- by appending the new amount at the end of the list of encrypted amounts.
  -- This is used when sending an encrypted amount, as well as when transferring
  -- from the encrypted to public balance.
  ReplaceUpTo {
    aggIndex :: !EncryptedAmountAggIndex,
    newAmount :: !EncryptedAmount
  }
  -- |Add an encrypted amount to the end of the list of encrypted amounts.
  -- This is used when transfering from public to encrypted balance, as well
  -- as when receiving an encrypted amount.
  | Add {
    newAmount :: !EncryptedAmount
  }
  -- |Do nothing to the encrypted amount.
  | Empty

data AccountKeysUpdate =
    RemoveKeys !(Set.Set KeyIndex) -- Removes the keys at the specified indexes from the account
  | SetKeys !(Map.Map KeyIndex AccountVerificationKey) -- Sets keys at the specified indexes to the specified key

-- |An update to an account state.
data AccountUpdate = AccountUpdate {
  -- |Address of the affected account.
  _auAddress :: !AccountAddress
  -- |Optionally a new account nonce.
  ,_auNonce :: !(Maybe Nonce)
  -- |Optionally an update to the account amount.
  ,_auAmount :: !(Maybe AmountDelta)
  -- |Optionally an update to the encrypted amounts.
  ,_auEncrypted :: !EncryptedAmountUpdate
  -- |Optionally a new credential.
  ,_auCredential :: !(Maybe CredentialDeploymentValues)
  -- |Optionally an update to the account keys
  ,_auKeysUpdate :: !(Maybe AccountKeysUpdate)
  -- |Optionally update the signature threshold
  ,_auSignThreshold :: !(Maybe SignatureThreshold)
}
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountAddress -> AccountUpdate
emptyAccountUpdate addr = AccountUpdate addr Nothing Nothing Empty Nothing Nothing Nothing

-- |Optionally add a credential to an account.
{-# INLINE updateCredential #-}
updateCredential :: (HasPersistingAccountData d) => Maybe CredentialDeploymentValues -> d -> d
updateCredential = maybe id addCredential

-- |Optionally update the verification keys and signature threshold for an account.
{-# INLINE updateAccountKeys #-}
updateAccountKeys :: (HasPersistingAccountData d) => Maybe AccountKeysUpdate -> Maybe SignatureThreshold -> d -> d
updateAccountKeys Nothing Nothing = id
updateAccountKeys mKeysUpd mNewThreshold = accountVerificationKeys %~ \AccountKeys{..} ->
    AccountKeys {
      akKeys = maybe akKeys (update akKeys) mKeysUpd,
      akThreshold = fromMaybe akThreshold mNewThreshold
    }
  where
    update oldKeys (RemoveKeys indices) = Set.foldl' (flip Map.delete) oldKeys indices
    update oldKeys (SetKeys keys) = Map.foldlWithKey (\m idx key -> Map.insert idx key m) oldKeys keys
