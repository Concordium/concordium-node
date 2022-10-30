{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.GlobalState.Account where

import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import Data.Maybe
import Data.Foldable
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Utils.Serialization
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution
import Concordium.Constants
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV0 as ARSV0
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as ARSV1

-- |A list of credential IDs that have been removed from an account.
data RemovedCredentials
    = EmptyRemovedCredentials
    | RemovedCredential !RawCredentialRegistrationID !RemovedCredentials
    deriving (Eq)

-- |Convert a 'RemovedCredentials' to a list of 'CredentialRegistrationID's.
removedCredentialsToList :: RemovedCredentials -> [RawCredentialRegistrationID]
removedCredentialsToList EmptyRemovedCredentials = []
removedCredentialsToList (RemovedCredential cred rest) = cred : removedCredentialsToList rest

instance Show RemovedCredentials where
  show = show . removedCredentialsToList

instance Serialize RemovedCredentials where
  put rc = do
    let l = removedCredentialsToList rc
    putLength (length l)
    mapM_ put l
  get = do
    len <- getLength
    Vec.foldr RemovedCredential EmptyRemovedCredentials <$>
      Vec.replicateM len get
 
-- |The hash of 'EmptyRemovedCredentials'.
emptyRemovedCredentialsHash :: Hash.Hash
emptyRemovedCredentialsHash = Hash.hash "E"
{-# NOINLINE emptyRemovedCredentialsHash #-}

-- |Function for determining the hash of a 'RemovedCredential'.
removedCredentialHash :: RawCredentialRegistrationID -> Hash.Hash -> Hash.Hash
removedCredentialHash cred hrest = Hash.hash $ "R" <> encode cred <> Hash.hashToByteString hrest

instance HashableTo Hash.Hash RemovedCredentials where
  getHash EmptyRemovedCredentials = emptyRemovedCredentialsHash
  getHash (RemovedCredential cred rest) = removedCredentialHash cred (getHash rest)

-- |Update hashed remove credentials with a new removed credentials.
addRemovedCredential :: RawCredentialRegistrationID -> Hashed RemovedCredentials -> Hashed RemovedCredentials
addRemovedCredential cred hrc = Hashed (RemovedCredential cred (hrc ^. unhashed)) (removedCredentialHash cred (getHash hrc))

-- |Hashed 'EmptyRemovedCredentials'.
emptyHashedRemovedCredentials :: Hashed RemovedCredentials
emptyHashedRemovedCredentials = makeHashed EmptyRemovedCredentials
{-# NOINLINE emptyHashedRemovedCredentials #-}

-- |Data about an account that is unlikely to change very frequently.
data PersistingAccountData = PersistingAccountData {
  -- |Address of the account
  _accountAddress :: !AccountAddress
  -- |Account encryption key (for encrypted amounts). This is stored as a "Raw"
  -- encryption key for two reasons. First, it takes up around 1/3 of the space
  -- of a deserialized key and second it is much faster to load from a byte
  -- array since expensive validity checks are not needed. This raw key will
  -- always be possible to convert to an 'AccountEncryptionKey' since only valid
  -- keys are stored. When this process is needed the cost of conversion is
  -- dominated by other costs.
  ,_accountEncryptionKey :: !RawAccountEncryptionKey
  -- |Account signature verification keys. Except for the threshold,
  -- these are derived from the account credentials, and are provided
  -- for convenience.
  ,_accountVerificationKeys :: !AccountInformation
  -- |Current credentials. This map is always non-empty and (presently)
  -- will have a credential at index 'initialCredentialIndex' (0) that cannot be changed.
  ,_accountCredentials :: !(Map.Map CredentialIndex RawAccountCredential)
  -- |Credential IDs of removed credentials.
  ,_accountRemovedCredentials :: !(Hashed RemovedCredentials)
}
  deriving (Eq, Show)

makeClassy ''PersistingAccountData

newtype PersistingAccountDataHash = PersistingAccountDataHash {thePersistingAccountDataHash :: Hash.Hash}
  deriving (Eq, Ord, Show, Serialize)

-- |Hashing of 'PersistingAccountData'.
-- 
-- * Only the 'aiThreshold' field of '_accountVerificationKeys' is stored, since
--   this is sufficient to recover it from '_accountCredentials'.
--
instance HashableTo PersistingAccountDataHash PersistingAccountData where
  getHash PersistingAccountData{..} = PersistingAccountDataHash $ Hash.hashLazy $ runPutLazy $ do
    put _accountAddress
    put _accountEncryptionKey
    put (aiThreshold _accountVerificationKeys)
    putSafeMapOf put put _accountCredentials
    put (_hashed _accountRemovedCredentials)

instance Monad m => MHashableTo m PersistingAccountDataHash PersistingAccountData

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

newtype EncryptedAmountHash = EncryptedAmountHash {theEncryptedAmountHash :: Hash.Hash}
  deriving newtype (Eq, Ord, Show, Serialize)

instance HashableTo EncryptedAmountHash AccountEncryptedAmount where
  getHash = EncryptedAmountHash . getHash . encode

instance Monad m => MHashableTo m EncryptedAmountHash AccountEncryptedAmount

-- |The 'EncryptedAmountHash' of 'initialAccountEncryptedAmount'.
initialAccountEncryptedAmountHash :: EncryptedAmountHash
{-# NOINLINE initialAccountEncryptedAmountHash #-}
initialAccountEncryptedAmountHash = getHash initialAccountEncryptedAmount

-- |Serialization for 'PersistingAccountData'. This is mainly to support
-- the (derived) 'BlobStorable' instance.
-- Account serialization does not use this.
instance Serialize PersistingAccountData where
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
    _accountRemovedCredentials <- makeHashed <$> get
    return PersistingAccountData{..}

-- |The hash of an account.  The type is parametrised by the account version as the structure of
-- accounts (and hence the hashing scheme) depend on the account version.
newtype AccountHash (av :: AccountVersion) = AccountHash {theAccountHash :: Hash.Hash}
  deriving newtype (Eq, Ord, Show, Serialize)

-- |Inputs for computing the hash of an account.
data AccountHashInputsV0 (av :: AccountVersion) =
  AccountHashInputsV0 {
    ahiNextNonce :: !Nonce,
    ahiAccountAmount :: !Amount,
    ahiAccountEncryptedAmount :: !AccountEncryptedAmount,
    ahiAccountReleaseScheduleHash :: !ARSV0.AccountReleaseScheduleHashV0,
    ahiPersistingAccountDataHash :: !PersistingAccountDataHash,
    ahiAccountStakeHash :: !(AccountStakeHash av)
  }

-- |Generate the hash for an account (for 'AccountV0' or 'AccountV1'), given the
-- 'AccountHashInputsV0'. 'makeAccountHash' should be used in preference to this function.
makeAccountHashV0 :: AccountHashInputsV0 av -> Hash.Hash
makeAccountHashV0 AccountHashInputsV0{..} = Hash.hashLazy $ runPutLazy $ do
  put ahiNextNonce
  put ahiAccountAmount
  put ahiAccountEncryptedAmount
  put ahiAccountReleaseScheduleHash
  put ahiPersistingAccountDataHash
  put ahiAccountStakeHash

-- |This comprises the hashes of the seldom-updated parts of an account that are used to compute an
-- 'AccountMerkleHash', namely the hashes of the persisting account data, account stake,
-- encrypted amount, and account release schedule.
data AccountMerkleHashInputs (av :: AccountVersion) where
  AccountMerkleHashInputsV2 :: {
    -- |Hash of the persisting account data.
    amhi2PersistingAccountDataHash :: !PersistingAccountDataHash,
    -- |Hash of the account stake.
    amhi2AccountStakeHash :: !(AccountStakeHash av),
    -- |Hash of the account's encrypted amount.
    amhi2EncryptedAmountHash :: !EncryptedAmountHash,
    -- |Hash of the account's release schedule.
    amhi2AccountReleaseScheduleHash :: !ARSV1.AccountReleaseScheduleHashV1
  } -> AccountMerkleHashInputs 'AccountV2

-- |The Merkle hash derived from the seldom-updated parts of an account, namely the persisting
-- account data, account stake, encrypted amount, and account release schedule.
-- This is used to derive the hash of an account for 'AccountV2'.
newtype AccountMerkleHash (av :: AccountVersion) = AccountMerkleHash {theMerkleHash :: Hash.Hash}
  deriving (Eq, Ord, Show, Serialize)

instance HashableTo (AccountMerkleHash av) (AccountMerkleHashInputs av) where
  getHash AccountMerkleHashInputsV2{..} =
    AccountMerkleHash $ Hash.hashOfHashes
      (Hash.hashOfHashes
        (thePersistingAccountDataHash amhi2PersistingAccountDataHash)
        (theAccountStakeHash amhi2AccountStakeHash))
      (Hash.hashOfHashes
        (theEncryptedAmountHash amhi2EncryptedAmountHash)
        (ARSV1.theAccountReleaseScheduleHashV1 amhi2AccountReleaseScheduleHash))


data AccountHashInputsV2 (av :: AccountVersion) =
  AccountHashInputsV2 {
    ahi2NextNonce :: !Nonce,
    ahi2AccountBalance :: !Amount,
    ahi2StakedBalance :: !Amount,
    ahi2MerkleHash :: !(AccountMerkleHash av)
  }

-- |Generate the hash for an account (for 'AccountV2'), given the
-- 'AccountHashInputsV2'. 'makeAccountHash' should be used in preference to this function.
makeAccountHashV2 :: AccountHashInputsV2 av -> Hash.Hash
makeAccountHashV2 AccountHashInputsV2{..} = Hash.hashLazy $ runPutLazy $ do
  putShortByteString "AC02"
  put ahi2NextNonce
  put ahi2AccountBalance
  put ahi2StakedBalance
  put ahi2MerkleHash

-- |Inputs for computing the 'AccountHash' for an account.
data AccountHashInputs (av :: AccountVersion) where
  AHIV0 :: AccountHashInputsV0 'AccountV0 -> AccountHashInputs 'AccountV0
  AHIV1 :: AccountHashInputsV0 'AccountV1 -> AccountHashInputs 'AccountV1
  AHIV2 :: AccountHashInputsV2 'AccountV2 -> AccountHashInputs 'AccountV2

makeAccountHash :: AccountHashInputs av -> AccountHash av
{-# INLINE makeAccountHash #-}
makeAccountHash (AHIV0 ahi) = AccountHash $ makeAccountHashV0 ahi
makeAccountHash (AHIV1 ahi) = AccountHash $ makeAccountHashV0 ahi
makeAccountHash (AHIV2 ahi) = AccountHash $ makeAccountHashV2 ahi

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
  -- |Index of the affected account.
  _auIndex :: !AccountIndex
  -- |Optionally a new account nonce.
  ,_auNonce :: !(Maybe Nonce)
  -- |Optionally an update to the account amount.
  ,_auAmount :: !(Maybe AmountDelta)
  -- |Optionally an update the encrypted amount.
  ,_auEncrypted :: !(Maybe EncryptedAmountUpdate)
  -- |Optionally update the locked stake on the account by adding scheduled releases.
  -- Each entry in the list MUST have a non-empty list of releases.
  ,_auReleaseSchedule :: !(Maybe [([(Timestamp, Amount)], TransactionHash)])
} deriving (Eq, Show)
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountIndex ->  AccountUpdate
emptyAccountUpdate ai = AccountUpdate ai Nothing Nothing Nothing Nothing

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
updateCredentials :: (HasPersistingAccountData d) => [CredentialIndex] -> Map.Map CredentialIndex AccountCredential -> AccountThreshold -> d -> d
updateCredentials cuRemove cuAdd cuAccountThreshold d =
  d & (accountCredentials %~ Map.union (Map.map toRawAccountCredential cuAdd) . removeKeys)
               & (accountVerificationKeys %~ updateAccountInformation cuAccountThreshold cuAdd cuRemove)
               & (accountRemovedCredentials %~ flip (foldl' (flip (addRemovedCredential . removedCredentialId))) cuRemove)
  where removeKeys = flip (foldl' (flip Map.delete)) cuRemove
        removedCredentialId :: CredentialIndex -> RawCredentialRegistrationID
        removedCredentialId cix = credId $ Map.findWithDefault (error "Removed credential key not found") cix (d ^. accountCredentials)
        

-- |Update the keys of the given account credential.
updateCredKeyInAccountCredential :: AccountCredential' credTy -> CredentialPublicKeys -> AccountCredential' credTy
updateCredKeyInAccountCredential (InitialAC icdv) keys = InitialAC (icdv{icdvAccount=keys})
updateCredKeyInAccountCredential (NormalAC cdv comms) keys = NormalAC (cdv{cdvPublicKeys=keys}) comms

-- |Optionally update the verification keys and signature threshold for an account.
-- Precondition: The credential with given credential index exists.
updateCredentialKeys :: (HasPersistingAccountData d) => CredentialIndex -> CredentialPublicKeys -> d -> d
updateCredentialKeys credIndex credKeys d =
  case (Map.lookup credIndex (d ^. accountCredentials), Map.lookup credIndex (aiCredentials (d ^. accountVerificationKeys))) of
    (Just oldCred, Just _) ->
      let updateCred = Map.insert credIndex (updateCredKeyInAccountCredential oldCred credKeys)
          updateKeys = Map.insert credIndex credKeys
          updateAi ai@AccountInformation{..} = ai{aiCredentials = updateKeys aiCredentials}
      in d & (accountCredentials %~ updateCred) & (accountVerificationKeys %~ updateAi)
    _ -> d -- do nothing. This is safe, but should not happen if the precondition is satisfied.

-- |Flags used for serializing an account in V0 format. This forms
-- part of the serialization of the account.
--
-- These flags determine whether certain values are explicitly recorded
-- in the serialization of the account. When the values are not
-- explicitly recorded, they have a sensible default, which is likely
-- to be common.
--
-- The purpose of this structure is to pack multiple flags into a single
-- byte of the serialization.
data AccountSerializationFlags = AccountSerializationFlags {
    -- |Whether the account address is serialized explicitly,
    -- or derived from the initial credential.
    asfExplicitAddress :: Bool,
    -- |Whether the encryption key is serialized explicitly,
    -- or derived from the cryptographic parameters and 
    -- initial credential.
    asfExplicitEncryptionKey :: Bool,
    -- |Whether the account has more than one credential.
    asfMultipleCredentials :: Bool,
    -- |Whether the account's encrypted amount is serialized
    -- explicitly, or is the default (empty) value.
    asfExplicitEncryptedAmount :: Bool,
    -- |Whether the account's release schedule is serialized
    -- explicitly, or is the default (empty) value.
    asfExplicitReleaseSchedule :: Bool,
    -- |Whether the account has a baker or delegation.
    asfHasBakerOrDelegation :: Bool,
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
          .|. cbit 5 asfHasBakerOrDelegation
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
        asfHasBakerOrDelegation = testBit flags 5
        asfThresholdIsOne = testBit flags 6
        asfHasRemovedCredentials = testBit flags 7
    return AccountSerializationFlags{..}

-- * Account update and query structures

-- |An update to a 'BakerPool', indicating the components that are to be replaced.
data BakerPoolInfoUpdate = BakerPoolInfoUpdate
    { updOpenForDelegation :: !(Maybe OpenStatus),
      updMetadataURL :: !(Maybe UrlText),
      updTransactionFeeCommission :: !(Maybe AmountFraction),
      updBakingRewardCommission :: !(Maybe AmountFraction),
      updFinalizationRewardCommission :: !(Maybe AmountFraction)
    }
    deriving (Eq)

-- |A 'BakerPoolInfoUpdate' that makes no changes.
emptyBakerPoolInfoUpdate :: BakerPoolInfoUpdate
emptyBakerPoolInfoUpdate =
    BakerPoolInfoUpdate
        { updOpenForDelegation = Nothing,
          updMetadataURL = Nothing,
          updTransactionFeeCommission = Nothing,
          updBakingRewardCommission = Nothing,
          updFinalizationRewardCommission = Nothing
        }

-- |Use a 'BakerPoolInfoUpdate' to update a 'BakerPoolInfo'.
applyBakerPoolInfoUpdate :: BakerPoolInfoUpdate -> BakerPoolInfo -> BakerPoolInfo
applyBakerPoolInfoUpdate
    BakerPoolInfoUpdate{..}
    BakerPoolInfo{_poolCommissionRates = CommissionRates{..}, ..} =
        BakerPoolInfo
            { _poolOpenStatus = fromMaybe _poolOpenStatus updOpenForDelegation,
              _poolMetadataUrl = fromMaybe _poolMetadataUrl updMetadataURL,
              _poolCommissionRates =
                CommissionRates
                    { _finalizationCommission = fromMaybe _finalizationCommission updFinalizationRewardCommission,
                      _bakingCommission = fromMaybe _bakingCommission updBakingRewardCommission,
                      _transactionCommission = fromMaybe _transactionCommission updTransactionFeeCommission
                    }
            }

-- |Details about the stake associated with an account.
-- Compared to 'AccountStake' this omits the 'BakerInfoEx' and the 'DelegatorId'.
-- It is more efficient to query these details on an account than to get the full baker on
-- an account, as it avoids loading the baker keys and pool parameters where possible.
data StakeDetails av
    = StakeDetailsNone
    | StakeDetailsBaker
        { sdStakedCapital :: !Amount,
          sdRestakeEarnings :: !Bool,
          sdPendingChange :: !(StakePendingChange av)
        }
    | StakeDetailsDelegator
        { sdStakedCapital :: !Amount,
          sdRestakeEarnings :: !Bool,
          sdPendingChange :: !(StakePendingChange av),
          sdDelegationTarget :: !DelegationTarget
        }
