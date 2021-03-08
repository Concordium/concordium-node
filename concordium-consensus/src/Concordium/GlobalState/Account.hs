{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.GlobalState.Account where

import Control.Monad
import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Utils.Serialization
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.GlobalState.BakerInfo

-- FIXME: Figure out where to put this constant.
maxNumIncoming :: Int
maxNumIncoming = 32

-- |A list of credential IDs that have been removed from an account.
data RemovedCredentials
    = EmptyRemovedCredentials
    | RemovedCredential !CredentialRegistrationID !RemovedCredentials
    deriving (Eq)

-- |Convert a 'RemovedCredentials' to a list of 'CredentialRegistrationID's.
removedCredentialsToList :: RemovedCredentials -> [CredentialRegistrationID]
removedCredentialsToList EmptyRemovedCredentials = []
removedCredentialsToList (RemovedCredential cred rest) = cred : removedCredentialsToList rest

instance Serialize RemovedCredentials where
  put rc = do
    let l = removedCredentialsToList rc
    putLength (length l)
    mapM_ put l
  get = do
    len <- getLength
    l <- replicateM len get
    return $! foldr RemovedCredential EmptyRemovedCredentials l

-- |The hash of 'EmptyRemovedCredentials'.
emptyRemovedCredentialsHash :: Hash.Hash
emptyRemovedCredentialsHash = Hash.hash "E"
{-# NOINLINE emptyRemovedCredentialsHash #-}

-- |Function for determining the hash of a 'RemovedCredential'.
removedCredentialHash :: CredentialRegistrationID -> Hash.Hash -> Hash.Hash
removedCredentialHash cred hrest = Hash.hash $ "R" <> encode cred <> Hash.hashToByteString hrest

instance HashableTo Hash.Hash RemovedCredentials where
  getHash EmptyRemovedCredentials = emptyRemovedCredentialsHash
  getHash (RemovedCredential cred rest) = removedCredentialHash cred (getHash rest)

-- |Update hashed remove credentials with a new removed credentials.
addRemovedCredential :: CredentialRegistrationID -> Hashed RemovedCredentials -> Hashed RemovedCredentials
addRemovedCredential cred hrc = Hashed (RemovedCredential cred (hrc ^. unhashed)) (removedCredentialHash cred (getHash hrc))

-- |Hashed 'EmptyRemovedCredentials'.
emptyHashedRemovedCredentials :: Hashed RemovedCredentials
emptyHashedRemovedCredentials = makeHashed EmptyRemovedCredentials
{-# NOINLINE emptyHashedRemovedCredentials #-}

-- |Data about an account that is unlikely to change very frequently.
data PersistingAccountData (pv :: ProtocolVersion) = PersistingAccountData {
  -- |Address of the account
  _accountAddress :: !AccountAddress
  -- |Account encryption key (for encrypted amounts)
  ,_accountEncryptionKey :: !AccountEncryptionKey
  -- |Account signature verification keys. Except for the threshold,
  -- these are derived from the account credentials, and are provided
  -- for convenience.
  ,_accountVerificationKeys :: !AccountInformation
  -- |Current credentials. This map is always non-empty and (presently)
  -- will have a credential at index 0 that cannot be changed.
  ,_accountCredentials :: !(Map.Map CredentialIndex AccountCredential)
  -- |Maximum "valid to" date of the current credentials. This is
  -- provided as a convenience for determining whether an account has
  -- a currently valid credential, and is derived from the credential map.
  ,_accountMaxCredentialValidTo :: !CredentialValidTo
  -- |Credential IDs of removed credentials.
  ,_accountRemovedCredentials :: !(Hashed RemovedCredentials)
}

makeClassy ''PersistingAccountData

instance (IsProtocolVersion pv) => Eq (PersistingAccountData pv) where
  pad1 == pad2 =
    _accountAddress pad1 == _accountAddress pad2
    && _accountEncryptionKey pad1 == _accountEncryptionKey pad2
    && _accountVerificationKeys pad1 == _accountVerificationKeys pad2
    && _accountCredentials pad1 == _accountCredentials pad2
    && _accountMaxCredentialValidTo pad1 == _accountMaxCredentialValidTo pad2
    && _accountRemovedCredentials pad1 == _accountRemovedCredentials pad2

instance (IsProtocolVersion pv) => Show (PersistingAccountData pv) where
  show PersistingAccountData{..} = "PersistingAccountData {" ++
    "_accountAddress = " ++ show _accountAddress ++ ", " ++
    "_accountEncryptionKey = " ++ show _accountEncryptionKey ++ ", " ++
    "_accountVerificationKeys = " ++ show _accountVerificationKeys ++ ", " ++
    "_accountCredentials = " ++ show _accountCredentials ++ ", " ++
    "_accountRemovedCredentials = " ++ show (removedCredentialsToList $ _unhashed _accountRemovedCredentials) ++ "}"

type PersistingAccountDataHash = Hash.Hash

-- |Hashing of 'PersistingAccountData'.
-- 
-- * Only the 'aiThreshold' field of '_accountVerificationKeys' is stored, since
--   this is sufficient to recover it from '_accountCredentials'.
--
-- * '_accountMaxCredentialValidTo' is omitted, since it can also be recovered
--   from '_accountCredentials'.
instance HashableTo PersistingAccountDataHash (PersistingAccountData pv) where
  getHash PersistingAccountData{..} = Hash.hashLazy $ runPutLazy $ do
    put _accountAddress
    put _accountEncryptionKey
    put (aiThreshold _accountVerificationKeys)
    putSafeMapOf put put _accountCredentials
    put (_hashed _accountRemovedCredentials)

instance Monad m => MHashableTo m PersistingAccountDataHash (PersistingAccountData pv)

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

-- |Serialization for 'PersistingAccountData'. This is mainly to support
-- the (derived) 'BlobStorable' instance.
-- Account serialization does not use this.
instance IsProtocolVersion pv => Serialize (PersistingAccountData pv) where
  put PersistingAccountData{..} = do
    put _accountAddress
    put _accountEncryptionKey
    put (aiThreshold _accountVerificationKeys)
    putSafeMapOf put put _accountCredentials
    put (_accountRemovedCredentials ^. unhashed)
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    threshold <- get
    _accountCredentials <- getSafeMapOf get get
    let _accountVerificationKeys = getAccountInformation threshold _accountCredentials
    let _accountMaxCredentialValidTo = maximum (validTo <$> _accountCredentials)
    _accountRemovedCredentials <- makeHashed <$> get
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

-- |Function for computing the hash of an account for protocol version P1.
makeAccountHashP1 :: Nonce -> Amount -> AccountEncryptedAmount -> AccountReleaseScheduleHash -> PersistingAccountDataHash -> AccountBakerHash -> Hash.Hash
makeAccountHashP1 n a eas arsh padh abh = Hash.hashLazy $ runPutLazy $ do
  put n
  put a
  put eas
  put arsh
  put padh
  put abh

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
  -- |Optionally update the locked stake on the account.
  ,_auReleaseSchedule :: !(Maybe [([(Timestamp, Amount)], TransactionHash)])
} deriving(Eq)
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountAddress -> AccountUpdate
emptyAccountUpdate addr = AccountUpdate addr Nothing Nothing Nothing Nothing

updateAccountInformation :: AccountThreshold -> Map.Map CredentialIndex AccountCredential -> [CredentialIndex] -> AccountInformation -> AccountInformation
updateAccountInformation threshold addCreds remove (AccountInformation oldCredKeys _) =
  let addCredKeys = fmap credPubKeys addCreds
      removeKeys = flip (foldl' (flip Map.delete)) remove
      newCredKeys = Map.union addCredKeys . removeKeys $ oldCredKeys
  in
  AccountInformation {
    aiCredentials = newCredKeys,
    aiThreshold = threshold
  }

-- |Add or remove credentials on an account.
-- The caller must ensure the following, which are not checked:
--
-- * Any credential index that is removed must already exist.
-- * Any credential index that is added must not exist after the removals take effect.
-- * At least one credential remains after all removals and additions.
-- * Any new threshold is at most the number of accounts remaining (and at least 1).
updateCredentials :: (HasPersistingAccountData d pv) => [CredentialIndex] -> Map.Map CredentialIndex AccountCredential -> AccountThreshold -> d -> d
updateCredentials cuRemove cuAdd cuAccountThreshold d =
  -- maximum is safe here since there must always be at least one credential on the account.
  d' & (accountMaxCredentialValidTo .~ maximum (validTo <$> allCredentials))
  where removeKeys = flip (foldl' (flip Map.delete)) cuRemove
        d' = d & (accountCredentials %~ Map.union cuAdd . removeKeys)
               & (accountVerificationKeys %~ updateAccountInformation cuAccountThreshold cuAdd cuRemove)
               & (accountRemovedCredentials %~ flip (foldl' (flip (addRemovedCredential . removedCredentialId))) cuRemove)
        allCredentials = Map.elems (d' ^. accountCredentials)
        removedCredentialId cix = credId $ Map.findWithDefault (error "Removed credential key not found") cix (d ^. accountCredentials)
        

-- |Update the keys of the given account credential.
updateCredKeyInAccountCredential :: AccountCredential -> CredentialPublicKeys -> AccountCredential
updateCredKeyInAccountCredential (InitialAC icdv) keys = InitialAC (icdv{icdvAccount=keys})
updateCredKeyInAccountCredential (NormalAC cdv comms) keys = NormalAC (cdv{cdvPublicKeys=keys}) comms

-- |Optionally update the verification keys and signature threshold for an account.
-- Precondition: The credential with given credential index exists.
updateCredentialKeys :: (HasPersistingAccountData d pv) => CredentialIndex -> CredentialPublicKeys -> d -> d
updateCredentialKeys credIndex credKeys d =
  case (Map.lookup credIndex (d ^. accountCredentials), Map.lookup credIndex (aiCredentials (d ^. accountVerificationKeys))) of
    (Just oldCred, Just _) ->
      let updateCred = Map.insert credIndex (updateCredKeyInAccountCredential oldCred credKeys)
          updateKeys = Map.insert credIndex credKeys
          updateAi ai@AccountInformation{..} = ai{aiCredentials = updateKeys aiCredentials}
      in d & (accountCredentials %~ updateCred) & (accountVerificationKeys %~ updateAi)
    _ -> d -- do nothing. This is safe, but should not happen if the precondition is satisfied.

-- |Flags used for serializing an account in V0 format.
data AccountSerializationFlags = AccountSerializationFlags {
    -- |Whether the account address is serialized explicity,
    -- or derived from the last credential.
    asfExplicitAddress :: Bool,
    -- |Whether the encryption key is serialized explicity,
    -- or derived from the cryptographic parameters and last
    -- credential.
    asfExplicitEncryptionKey :: Bool,
    -- |Whether the account has more than one credential.
    asfMultipleCredentials :: Bool,
    -- |Whether the account's encrypted amount is serialized
    -- explicity, or is the default (empty) value.
    asfExplicitEncryptedAmount :: Bool,
    -- |Whether the account's release schedule is serialized
    -- explicitly, or is the default (empty) value.
    asfExplicitReleaseSchedule :: Bool,
    -- |Whether the account has a baker.
    asfHasBaker :: Bool,
    -- |Whether the account threshold is 1.
    asfThresholdIsOne :: Bool,
    -- |Whether the account has removed credentials.
    asfHasRemovedCredentials :: Bool
  }

instance Serialize AccountSerializationFlags where
  put AccountSerializationFlags{..} = putWord8 $
          cbit 0 asfExplicitAddress
          .|. cbit 1 asfExplicitEncryptionKey
          .|. cbit 2 asfMultipleCredentials
          .|. cbit 3 asfExplicitEncryptedAmount
          .|. cbit 4 asfExplicitReleaseSchedule
          .|. cbit 5 asfHasBaker
          .|. cbit 6 asfThresholdIsOne
          .|. cbit 7 asfHasRemovedCredentials
    where
      cbit n b = if b then bit n else 0
  get = do
    flags <- getWord8
    let asfExplicitAddress = testBit flags 0
        asfExplicitEncryptionKey = testBit flags 1
        asfMultipleCredentials = testBit flags 2
        asfExplicitEncryptedAmount = testBit flags 3
        asfExplicitReleaseSchedule = testBit flags 4
        asfHasBaker = testBit flags 5
        asfThresholdIsOne = testBit flags 6
        asfHasRemovedCredentials = testBit flags 7
    return AccountSerializationFlags{..}
