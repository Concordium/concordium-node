{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.GlobalState.Basic.BlockState.Account(
  module Concordium.GlobalState.Account,
  module Concordium.GlobalState.Basic.BlockState.Account
) where

import Control.Monad
import Data.Maybe
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import Concordium.Utils.Serialization
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types
import Concordium.ID.Parameters
import Concordium.Types.HashableTo
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.Types
import Concordium.Types.Accounts

-- |Type for how a 'PersitingAccountData' value is stored as part of
-- an account. This is stored with its hash.
type AccountPersisting pv = Hashed (PersistingAccountData pv)

-- |Make an 'AccountPersisting' value from a 'PersistingAccountData' value.
makeAccountPersisting :: PersistingAccountData pv -> AccountPersisting pv
makeAccountPersisting = makeHashed
{-# INLINE makeAccountPersisting #-}

-- |Show an 'AccountPersisting' value.
showAccountPersisting :: SProtocolVersion pv -> AccountPersisting pv -> String
showAccountPersisting spv = case spv of
  SP1 -> show

-- |An (in-memory) account.
data Account (pv :: ProtocolVersion) = Account {
  _accountPersisting :: !(AccountPersisting pv)
  -- ^Account data that seldom changes. Stored separately for efficient
  -- memory use and hashing.
  ,_accountNonce :: !Nonce
  -- ^The next sequence number of a transaction on this account.
  ,_accountAmount :: !Amount
  -- ^This account amount contains all the amount owned by the account,
  -- excluding encrypted amounts. In particular, the available amount is
  -- @accountAmount - totalLockedUpBalance accountReleaseSchedule@.
  ,_accountEncryptedAmount :: !AccountEncryptedAmount
  -- ^The encrypted amount
  ,_accountReleaseSchedule :: !AccountReleaseSchedule
  -- ^Locked-up amounts and their release schedule.
  ,_accountBaker :: !(Maybe AccountBaker)
  -- ^The baker associated with the account (if any).
  }

makeLenses ''Account

instance IsProtocolVersion pv => Eq (Account pv) where
  a1 == a2 =
    _accountPersisting a1 == _accountPersisting a2
    && _accountNonce a1 == _accountNonce a2
    && _accountEncryptedAmount a1 == _accountEncryptedAmount a2
    && _accountBaker a1 == _accountBaker a2 

instance (IsProtocolVersion pv) => HasPersistingAccountData (Account pv) pv where
  persistingAccountData = accountPersisting . unhashed

instance IsProtocolVersion pv => Show (Account pv) where
  show Account{..} = "Account {" ++
    "_accountPersisting = " ++ showAccountPersisting (protocolVersion @pv) _accountPersisting ++
    ", _accountNonce = " ++ show _accountNonce ++
    ", _accountAmount = " ++ show _accountAmount ++
    ", _accountEncryptedAmount = " ++ show _accountEncryptedAmount ++
    ", _accountReleaseSchedule = " ++ show _accountReleaseSchedule ++
    ", _accountBaker = " ++ show _accountBaker ++
    "}"


-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
serializeAccount :: (IsProtocolVersion pv) => GlobalContext -> S.Putter (Account pv)
serializeAccount cryptoParams acct@Account{..} = do
    S.put flags
    when asfExplicitAddress $ S.put _accountAddress
    when asfExplicitEncryptionKey $ S.put _accountEncryptionKey
    unless asfThresholdIsOne $ S.put (aiThreshold _accountVerificationKeys)
    putCredentials
    when asfHasRemovedCredentials $ S.put (_accountRemovedCredentials ^. unhashed)
    S.put _accountNonce
    S.put _accountAmount
    when asfExplicitEncryptedAmount $ S.put _accountEncryptedAmount
    when asfExplicitReleaseSchedule $ S.put _accountReleaseSchedule
    mapM_ S.put _accountBaker
  where
    PersistingAccountData {..} = acct ^. persistingAccountData
    flags = AccountSerializationFlags {..}
    initialCredId = credId (Map.findWithDefault
            (error "Account missing initial credential")
            initialCredentialIndex
            _accountCredentials
          )
    asfExplicitAddress = _accountAddress /= addressFromRegId initialCredId
    asfExplicitEncryptionKey = _accountEncryptionKey /= makeEncryptionKey cryptoParams initialCredId
    (asfMultipleCredentials, putCredentials) = case Map.toList _accountCredentials of
      [(i, cred)] | i == initialCredentialIndex -> (False, S.put cred)
      _ -> (True, putSafeMapOf S.put S.put _accountCredentials)
    asfExplicitEncryptedAmount = _accountEncryptedAmount /= initialAccountEncryptedAmount
    asfExplicitReleaseSchedule = _accountReleaseSchedule /= emptyAccountReleaseSchedule
    asfHasBaker = isJust _accountBaker
    asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
    asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials


-- |Deserialize an account.
-- The serialization format may depend on the protocol version.
deserializeAccount :: forall pv. IsProtocolVersion pv => GlobalContext -> S.Get (Account pv)
deserializeAccount cryptoParams = do
    AccountSerializationFlags{..} <- S.get
    preAddress <- if asfExplicitAddress then Just <$> S.get else return Nothing
    preEncryptionKey <- if asfExplicitEncryptionKey then Just <$> S.get else return Nothing
    threshold <- if asfThresholdIsOne then return 1 else S.get
    let getCredentials
          | asfMultipleCredentials = do
              creds <- getSafeMapOf S.get S.get
              case Map.lookup initialCredentialIndex creds of
                Nothing -> fail $ "Account has no credential with index " ++ show initialCredentialIndex
                Just cred -> return (creds, credId cred)
          | otherwise = do
              cred <- S.get
              return (Map.singleton initialCredentialIndex cred, credId cred)
    (_accountCredentials, initialCredId) <- getCredentials
    _accountRemovedCredentials <- if asfHasRemovedCredentials then makeHashed <$> S.get else return emptyHashedRemovedCredentials
    let _accountVerificationKeys = getAccountInformation threshold _accountCredentials
    let _accountAddress = fromMaybe (addressFromRegId initialCredId) preAddress
        _accountEncryptionKey = fromMaybe (makeEncryptionKey cryptoParams initialCredId) preEncryptionKey
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- if asfExplicitEncryptedAmount then S.get else return initialAccountEncryptedAmount
    _accountReleaseSchedule <- if asfExplicitReleaseSchedule then S.get else return emptyAccountReleaseSchedule
    _accountBaker <- if asfHasBaker then Just <$> S.get else return Nothing
    let _accountPersisting = makeAccountPersisting @pv PersistingAccountData {..}
    return Account{..}

instance (IsProtocolVersion pv) => HashableTo Hash.Hash (Account pv) where
  getHash Account{..} = case protocolVersion @pv of
      SP1 -> makeAccountHashP1 _accountNonce _accountAmount _accountEncryptedAmount (getHash _accountReleaseSchedule) (getHash _accountPersisting) bkrHash
    where
      bkrHash = maybe nullAccountBakerHash getHash _accountBaker



-- |Create an empty account with the given public key, address and credentials.
newAccountMultiCredential :: forall pv. (IsProtocolVersion pv)
  => GlobalContext  -- ^Cryptographic parameters, needed to derive the encryption key from the credentials.
  -> AccountThreshold -- ^The account threshold, how many credentials need to sign..
  -> AccountAddress -- ^Address of the account to be created.
  -> Map.Map CredentialIndex AccountCredential -- ^Initial credentials on the account. NB: It is assumed that this map has a value at index 'initialCredentialIndex' (0).
  -> Account pv
newAccountMultiCredential cryptoParams threshold _accountAddress cs = Account {
        _accountPersisting = makeAccountPersisting @pv PersistingAccountData {
        _accountEncryptionKey = makeEncryptionKey cryptoParams (credId (cs Map.! initialCredentialIndex)),
        _accountCredentials = cs,
        _accountVerificationKeys = getAccountInformation threshold cs,
        _accountRemovedCredentials = emptyHashedRemovedCredentials,
        ..
        },
        _accountNonce = minNonce,
        _accountAmount = 0,
        _accountEncryptedAmount = initialAccountEncryptedAmount,
        _accountReleaseSchedule = emptyAccountReleaseSchedule,
        _accountBaker = Nothing
    }

-- |Create an empty account with the given public key, address and credential.
newAccount :: (IsProtocolVersion pv) => GlobalContext -> AccountAddress -> AccountCredential -> Account pv
newAccount cryptoParams _accountAddress credential
    = newAccountMultiCredential cryptoParams 1 _accountAddress (Map.singleton initialCredentialIndex credential)
