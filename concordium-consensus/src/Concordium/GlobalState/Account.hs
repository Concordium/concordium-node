{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Account where

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Utils
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Transactions(AccountInformation)
import Concordium.Types.HashableTo
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.GlobalState.BakerInfo

-- FIXME: Figure out where to put this constant.
maxNumIncoming :: Int
maxNumIncoming = 32

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data PersistingAccountData = PersistingAccountData {
  _accountAddress :: !AccountAddress
  ,_accountEncryptionKey :: !AccountEncryptionKey
  ,_accountVerificationKeys :: !AccountInformation
  ,_accountCredentials :: !(Map.Map KeyIndex AccountCredential)
  -- ^Credentials; most recent first
  ,_accountMaxCredentialValidTo :: !CredentialValidTo
  ,_accountInstances :: !(Set.Set ContractAddress)
} deriving (Show, Eq)

makeClassy ''PersistingAccountData

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount :: EncryptedAmount -> AccountEncryptedAmount -> AccountEncryptedAmount
addIncomingEncryptedAmount newAmount old =
  case _aggregatedAmount old of
    Nothing -> -- we have to aggregate if we have >= maxNumIncoming amounts on the sequence
      if Seq.length (_incomingEncryptedAmounts old) >= maxNumIncoming then
        -- irrefutable because of check above
        let ~(x Seq.:<| y Seq.:<| rest) = _incomingEncryptedAmounts old in
          old{_incomingEncryptedAmounts = rest Seq.|> newAmount,
              _startIndex = _startIndex old + 1,
              _aggregatedAmount = Just (x <> y, 2)
             }
      else
        old & incomingEncryptedAmounts %~ (Seq.|> newAmount)
    Just (e, n) -> -- we have to aggregate always
      -- irrefutable because of check above
      let ~(x Seq.:<| rest) = _incomingEncryptedAmounts old in
        old {_incomingEncryptedAmounts = rest Seq.|> newAmount,
             _startIndex = _startIndex old + 1,
             _aggregatedAmount = Just (e <> x, n + 1)
            }

-- | Drop the encrypted amount with indices up to (but not including) the given one, and add the new amount at the end.
-- This is used when an account is transfering from from an encrypted balance, and the newly added
-- amount is the remaining balance that was not used.
--
-- As mentioned above, the whole 'selfBalance' must always be used in any
-- outgoing action of the account.
replaceUpTo :: EncryptedAmountAggIndex -> EncryptedAmount -> AccountEncryptedAmount -> AccountEncryptedAmount
replaceUpTo newIndex newAmount AccountEncryptedAmount{..} =
  AccountEncryptedAmount{
    _selfAmount = newAmount,
    _startIndex = newStartIndex,
    _incomingEncryptedAmounts = newEncryptedAmounts,
    _aggregatedAmount = newAggregatedAmount
  }
  where (newStartIndex, toDrop, dropAggregated) =
          if newIndex > _startIndex
          then
            if isNothing _aggregatedAmount
            then
              (newIndex, fromIntegral (newIndex - _startIndex), False)
            else
              (newIndex, fromIntegral (newIndex - _startIndex) - 1, True)
          else (_startIndex, 0, False)
        newEncryptedAmounts = Seq.drop toDrop _incomingEncryptedAmounts
        newAggregatedAmount = if dropAggregated then Nothing else _aggregatedAmount

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount :: EncryptedAmount -> AccountEncryptedAmount -> AccountEncryptedAmount
addToSelfEncryptedAmount newAmount = selfAmount %~ (<> newAmount)

instance Serialize PersistingAccountData where
  put PersistingAccountData{..} = put _accountAddress <>
                                  put _accountEncryptionKey <>
                                  -- put _accountVerificationKeys <>
                                  put _accountCredentials <> -- The order is significant for hash computation
                                  put (Set.toAscList _accountInstances)
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    _accountVerificationKeys <- get
    _accountCredentials <- get
    when (null _accountCredentials) $ fail "Account has no credentials"
    let _accountMaxCredentialValidTo = maximum (validTo <$> _accountCredentials)
    _accountInstances <- Set.fromList <$> get
    return PersistingAccountData{..}


-- |Pending changes to the baker associated with an account.
-- Changes are effective on the actual bakers, two epochs after the specified epoch,
-- however, the changes will be made to the 'AccountBaker' at the specified epoch.
data BakerPendingChange
  = NoChange
  -- ^There is no change pending to the baker.
  | ReduceStake !Amount !Epoch
  -- ^The stake will be decreased to the given amount.
  | RemoveBaker !Epoch
  -- ^The baker will be removed.
  deriving (Eq, Ord, Show)

instance Serialize BakerPendingChange where
  put NoChange = putWord8 0
  put (ReduceStake amt epoch) = putWord8 1 >> put amt >> put epoch
  put (RemoveBaker epoch) = putWord8 2 >> put epoch

  get = getWord8 >>= \case
    0 -> return NoChange
    1 -> ReduceStake <$> get <*> get
    2 -> RemoveBaker <$> get
    _ -> fail "Invalid BakerPendingChange"

-- |A baker associated with an account.
data AccountBaker = AccountBaker {
  _stakedAmount :: !Amount,
  _stakeEarnings :: !Bool,
  _accountBakerInfo :: !BakerInfo,
  _bakerPendingChange :: !BakerPendingChange
} deriving (Eq, Show)

makeLenses ''AccountBaker

instance Serialize AccountBaker where
  put AccountBaker{..} = do
    put _stakedAmount
    put _stakeEarnings
    put _accountBakerInfo
    put _bakerPendingChange
  get = do
    _stakedAmount <- get
    _stakeEarnings <- get
    _accountBakerInfo <- get
    _bakerPendingChange <- get
    -- If there is a pending reduction, check that it is actually a reduction.
    case _bakerPendingChange of
      ReduceStake amt _
        | amt > _stakedAmount -> fail "Pending stake reduction is not a reduction in stake"
      _ -> return ()
    return AccountBaker{..}

instance HashableTo AccountBakerHash AccountBaker where
  getHash AccountBaker{..}
    = makeAccountBakerHash
        _stakedAmount
        _stakeEarnings
        _accountBakerInfo
        _bakerPendingChange

type AccountBakerHash = Hash.Hash

-- |Make an 'AccountBakerHash' for a baker.
makeAccountBakerHash :: Amount -> Bool -> BakerInfo -> BakerPendingChange -> AccountBakerHash
makeAccountBakerHash amt stkEarnings binfo bpc = Hash.hashLazy $ runPutLazy $
  put amt >> put stkEarnings >> put binfo >> put bpc

-- |An 'AccountBakerHash' that is used when an account has no baker.
-- This is defined as the hash of the empty string.
nullAccountBakerHash :: AccountBakerHash
nullAccountBakerHash = Hash.hash ""

-- TODO To avoid recomputing the hash for the persisting account data each time we update an account
-- we might want to explicitly store its hash, too.
makeAccountHash :: Nonce -> Amount -> AccountEncryptedAmount -> AccountReleaseScheduleHash -> PersistingAccountData -> AccountBakerHash -> Hash.Hash
makeAccountHash n a eas ars pd abh = Hash.hashLazy $ runPutLazy $
  put n >> put a >> put eas >> put ars >> put pd >> put abh

-- {-# INLINE setKey #-}
-- -- |Set a at a given index to a given value. The value of 'Nothing' will remove the key.
-- setKey :: HasPersistingAccountData d => KeyIndex -> Maybe VerifyKey -> d -> d
-- setKey idx key = accountVerificationKeys %~ (\ks -> ks { akKeys = akKeys ks & at' idx .~ key })

-- {-# INLINE setThreshold #-}
-- -- |Set the signature threshold.
-- setThreshold :: HasPersistingAccountData d => SignatureThreshold -> d -> d
-- setThreshold thr = accountVerificationKeys %~ (\ks -> ks { akThreshold = thr })

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
  -- This is used when receiving an encrypted amount.
  | Add {
    newAmount :: !EncryptedAmount
  }
  -- |Add an encrypted amount to the self balance, aggregating to what is already there.
  -- This is used when an account is transferring from public to secret balance.
  | AddSelf {
    newAmount :: !EncryptedAmount
    }
  deriving(Eq, Show)

data AccountKeysUpdate =
    RemoveKeys !(Set.Set KeyIndex) -- Removes the keys at the specified indexes from the account
  | SetKeys !(Map.Map KeyIndex AccountVerificationKey) -- Sets keys at the specified indexes to the specified key
  deriving(Eq)

data CredentialsUpdate = CredentialsUpdate {
  -- |Remove credentials with these indices.
  cuRemove :: ![KeyIndex],
  -- |Add credentials with these indices.
  cuAdd :: !(Map.Map KeyIndex AccountCredential)
  } deriving(Eq)

-- |An update to an account state.
data AccountUpdate = AccountUpdate {
  -- |Address of the affected account.
  _auAddress :: !AccountAddress
  -- |Optionally a new account nonce.
  ,_auNonce :: !(Maybe Nonce)
  -- |Optionally an update to the account amount.
  ,_auAmount :: !(Maybe AmountDelta)
  -- |Optionally an update the encrypted amount.
  ,_auEncrypted :: !(Maybe EncryptedAmountUpdate)
  -- |Optionally a new credential.
  ,_auCredentials :: !(Maybe CredentialsUpdate)
  -- |Optionally an update to the account keys
  ,_auKeysUpdate :: !(Maybe AccountKeysUpdate)
  -- |Optionally update the account signature threshold,
  -- i.e., how many credentials need to sign the transaction.
  ,_auAccountThreshold :: !(Maybe AccountThreshold)
  -- |Optionally update the locked stake on the account.
  ,_auReleaseSchedule :: !(Maybe [([(Timestamp, Amount)], TransactionHash)])
} deriving(Eq)
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountAddress -> AccountUpdate
emptyAccountUpdate addr = AccountUpdate addr Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |Optionally add a credential to an account.
updateCredentials :: (HasPersistingAccountData d) => Maybe CredentialsUpdate -> d -> d
updateCredentials Nothing = id
updateCredentials (Just CredentialsUpdate{..}) = (accountCredentials %~ Map.union cuAdd . removeKeys)
    . (accountMaxCredentialValidTo %~ max (maximum (validTo <$> cuAdd))) -- FIXME:  This line is not correct since we've removed some data.
  where removeKeys = flip (foldl' (flip Map.delete)) cuRemove

-- |Optionally update the verification keys and signature threshold for an account.
-- {-# INLINE updateAccountKeys #-}
-- updateAccountKeys :: (HasPersistingAccountData d) => Maybe AccountKeysUpdate -> Maybe SignatureThreshold -> d -> d
-- updateAccountKeys Nothing Nothing = id
-- updateAccountKeys mKeysUpd mNewThreshold = accountVerificationKeys %~ \AccountKeys{..} ->
--     AccountKeys {
--       akKeys = maybe akKeys (update akKeys) mKeysUpd,
--       akThreshold = fromMaybe akThreshold mNewThreshold
--     }
--   where
--     update oldKeys (RemoveKeys indices) = Set.foldl' (flip Map.delete) oldKeys indices
--     update oldKeys (SetKeys keys) = Map.foldlWithKey (\m idx key -> Map.insert idx key m) oldKeys keys
