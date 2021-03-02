{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.Account where

import Control.Monad
import Data.Bits
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Utils
import Concordium.Utils.Serialization
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.GlobalState.BakerInfo

-- FIXME: Figure out where to put this constant.
maxNumIncoming :: Int
maxNumIncoming = 32

-- |Type family for the instances stored as part of an account's
-- persisting data. As of 'P1', this is no longer stored and is
-- the '()'.
type family AccountInstances (pv :: ProtocolVersion) where
  AccountInstances 'P0 = Set.Set ContractAddress
  AccountInstances _ = ()

emptyAccountInstances :: SProtocolVersion pv -> AccountInstances pv
emptyAccountInstances pv = case pv of
  SP0 -> Set.empty
  SP1 -> ()

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data PersistingAccountData (pv :: ProtocolVersion) = PersistingAccountData {
  _accountAddress :: !AccountAddress
  ,_accountEncryptionKey :: !AccountEncryptionKey
  ,_accountVerificationKeys :: !AccountKeys
  ,_accountCredentials :: !(NonEmpty AccountCredential)
  -- ^Credentials; most recent first
  ,_accountMaxCredentialValidTo :: !CredentialValidTo
  ,_accountInstances :: !(AccountInstances pv)
}

makeClassy ''PersistingAccountData

instance (IsProtocolVersion pv) => Eq (PersistingAccountData pv) where
  pad1 == pad2 =
    _accountAddress pad1 == _accountAddress pad2
    && _accountEncryptionKey pad1 == _accountEncryptionKey pad2
    && _accountVerificationKeys pad1 == _accountVerificationKeys pad2
    && _accountCredentials pad1 == _accountCredentials pad2
    && _accountMaxCredentialValidTo pad1 == _accountMaxCredentialValidTo pad2
    && case protocolVersion :: SProtocolVersion pv of
        SP0 -> _accountInstances pad1 == _accountInstances pad2
        _ -> True

instance (IsProtocolVersion pv) => Show (PersistingAccountData pv) where
  show PersistingAccountData{..} = "PersistingAccountData {" ++
    "_accountAddress = " ++ show _accountAddress ++ ", " ++
    "_accountEncryptionKey = " ++ show _accountEncryptionKey ++ ", " ++
    "_accountVerificationKeys = " ++ show _accountVerificationKeys ++ ", " ++
    "_accountCredentials = " ++ show _accountCredentials ++ "}"

type PersistingAccountDataHash = Hash.Hash

instance HashableTo PersistingAccountDataHash (PersistingAccountData pv) where
  getHash PersistingAccountData{..} = Hash.hashLazy $ runPutLazy $ do
    put _accountAddress
    put _accountEncryptionKey
    put _accountVerificationKeys
    putLength (NE.length _accountCredentials)
    mapM_ put _accountCredentials

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
-- the (derived) 'BlobStorable' instance, but is also used in the 'P0'
-- account hashing. V0 account serialization does not use this.
instance IsProtocolVersion pv => Serialize (PersistingAccountData pv) where
  put PersistingAccountData{..} = do
    put _accountAddress
    put _accountEncryptionKey
    put _accountVerificationKeys
    putLength (length _accountCredentials)
    mapM_ put _accountCredentials
    case protocolVersion @pv of
      SP0 -> put (Set.toAscList _accountInstances)
      _ -> return ()
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    _accountVerificationKeys <- get
    numCredentials <- getLength
    when (numCredentials < 1) $ fail "Account has no credentials"
    _accountCredentials <- (:|) <$> get <*> replicateM (numCredentials - 1) get
    let _accountMaxCredentialValidTo = maximum (validTo <$> _accountCredentials)
    _accountInstances <- case protocolVersion @pv of
      SP0 -> Set.fromList <$> get
      SP1 -> return ()
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
makeAccountHashP0 :: Nonce -> Amount -> AccountEncryptedAmount -> AccountReleaseScheduleHash -> PersistingAccountData 'P0 -> AccountBakerHash -> Hash.Hash
makeAccountHashP0 n a eas ars pd abh = Hash.hashLazy $ runPutLazy $
  put n >> put a >> put eas >> put ars >> put pd >> put abh

makeAccountHashP1 :: Nonce -> Amount -> AccountEncryptedAmount -> AccountReleaseScheduleHash -> PersistingAccountDataHash -> AccountBakerHash -> Hash.Hash
makeAccountHashP1 n a eas arsh padh abh = Hash.hashLazy $ runPutLazy $ do
  put n
  put a
  put eas
  put arsh
  put padh
  put abh

{-# INLINE addCredential #-}
addCredential :: HasPersistingAccountData d pv => AccountCredential -> d -> d
addCredential cdv = (accountCredentials %~ NE.cons cdv)
  . (accountMaxCredentialValidTo %~ max (validTo cdv))

{-# INLINE setKey #-}
-- |Set a at a given index to a given value. The value of 'Nothing' will remove the key.
setKey :: HasPersistingAccountData d pv => KeyIndex -> Maybe VerifyKey -> d -> d
setKey idx key = accountVerificationKeys %~ (\ks -> ks { akKeys = akKeys ks & at' idx .~ key })

{-# INLINE setThreshold #-}
-- |Set the signature threshold.
setThreshold :: HasPersistingAccountData d pv => SignatureThreshold -> d -> d
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
  ,_auCredential :: !(Maybe AccountCredential)
  -- |Optionally an update to the account keys
  ,_auKeysUpdate :: !(Maybe AccountKeysUpdate)
  -- |Optionally update the signature threshold
  ,_auSignThreshold :: !(Maybe SignatureThreshold)
  -- |Optionally update the locked stake on the account.
  ,_auReleaseSchedule :: !(Maybe [([(Timestamp, Amount)], TransactionHash)])
} deriving(Eq)
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountAddress -> AccountUpdate
emptyAccountUpdate addr = AccountUpdate addr Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |Optionally add a credential to an account.
{-# INLINE updateCredential #-}
updateCredential :: (HasPersistingAccountData d pv) => Maybe AccountCredential -> d -> d
updateCredential = maybe id addCredential

-- |Optionally update the verification keys and signature threshold for an account.
{-# INLINE updateAccountKeys #-}
updateAccountKeys :: (HasPersistingAccountData d pv) => Maybe AccountKeysUpdate -> Maybe SignatureThreshold -> d -> d
updateAccountKeys Nothing Nothing = id
updateAccountKeys mKeysUpd mNewThreshold = accountVerificationKeys %~ \AccountKeys{..} ->
    AccountKeys {
      akKeys = maybe akKeys (update akKeys) mKeysUpd,
      akThreshold = fromMaybe akThreshold mNewThreshold
    }
  where
    update oldKeys (RemoveKeys indices) = Set.foldl' (flip Map.delete) oldKeys indices
    update oldKeys (SetKeys keys) = Map.foldlWithKey (\m idx key -> Map.insert idx key m) oldKeys keys

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
    asfHasBaker :: Bool
  }

instance Serialize AccountSerializationFlags where
  put AccountSerializationFlags{..} = putWord8 $
          cbit 0 asfExplicitAddress
          .|. cbit 1 asfExplicitEncryptionKey
          .|. cbit 2 asfMultipleCredentials
          .|. cbit 3 asfExplicitEncryptedAmount
          .|. cbit 4 asfExplicitReleaseSchedule
          .|. cbit 5 asfHasBaker
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
    return AccountSerializationFlags{..}
