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
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.Basic.BlockState.Account(
  module Concordium.GlobalState.Account,
  module Concordium.GlobalState.Basic.BlockState.Account
) where

import Control.Monad
import Data.Coerce
import Data.Maybe
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Utils.Serialization
import Concordium.ID.Types
import Concordium.ID.Parameters
import Concordium.Types.HashableTo
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.P4 as P4

-- |Type for how a 'PersistingAccountData' value is stored as part of
-- an account. This is stored with its hash.
type AccountPersisting = Hashed PersistingAccountData

-- |Make an 'AccountPersisting' value from a 'PersistingAccountData' value.
makeAccountPersisting :: PersistingAccountData -> AccountPersisting
makeAccountPersisting = makeHashed
{-# INLINE makeAccountPersisting #-}

-- |An (in-memory) account.
data Account (av :: AccountVersion) = Account {
  _accountPersisting :: !AccountPersisting
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
  ,_accountStaking :: !(AccountStake av)
  -- ^The baker or delegation associated with the account (if any).
  }
  deriving (Eq, Show)

makeLenses ''Account

accountBaker :: SimpleGetter (Account av) (Maybe (AccountBaker av))
accountBaker = to g
  where
    g Account{_accountStaking = AccountStakeBaker bkr} = Just bkr
    g _ = Nothing

-- |Get the baker from an account, on the basis that it is known to be a baker
unsafeAccountBaker :: HasCallStack => SimpleGetter (Account av) (AccountBaker av)
unsafeAccountBaker = accountBaker . non (error "Invariant violation: account was expected to be a baker")

accountDelegator :: SimpleGetter (Account av) (Maybe (AccountDelegation av))
accountDelegator = to g
  where
    g Account{_accountStaking = AccountStakeDelegate del} = Just del
    g _ = Nothing

unsafeAccountDelegator :: HasCallStack => SimpleGetter (Account av) (AccountDelegation av)
unsafeAccountDelegator = accountDelegator . non (error "Invariant violation: account was expected to be a delegator")

instance HasPersistingAccountData (Account av) where
  persistingAccountData = accountPersisting . unhashed

-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
serializeAccount :: IsAccountVersion av => GlobalContext -> S.Putter (Account av)
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
    when asfHasBakerOrDelegation $ putAccountStake _accountStaking
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
    asfHasBakerOrDelegation = case _accountStaking of
        AccountStakeNone -> False
        _ -> True
    asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
    asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials

-- |Deserialize an account.
-- The serialization format may depend on the protocol version.
deserializeAccount :: forall oldpv pv. IsProtocolVersion oldpv
    => StateMigrationParameters oldpv pv
    -> GlobalContext
    -> S.Get (Account (AccountVersionFor pv))
deserializeAccount migration cryptoParams = do
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
    _accountStaking <- if asfHasBakerOrDelegation
      then
        case migration of
          StateMigrationParametersTrivial -> getAccountStake
          StateMigrationParametersP3ToP4 mp ->
            P4.migrateAccountStakeV0toV1 mp <$> getAccountStake
      else
        return AccountStakeNone
    let _accountPersisting = makeAccountPersisting PersistingAccountData {..}
    return Account{..}

instance IsAccountVersion av => HashableTo (AccountHash av) (Account av) where
  getHash Account{..} = makeAccountHash $ AccountHashInputs {
      ahiNextNonce = _accountNonce ,
      ahiAccountAmount = _accountAmount ,
      ahiAccountEncryptedAmount = _accountEncryptedAmount ,
      ahiAccountReleaseScheduleHash = getHash _accountReleaseSchedule,
      ahiPersistingAccountDataHash = getHash _accountPersisting,
      ahiAccountStakeHash = getAccountStakeHash _accountStaking
    }

instance forall av. IsAccountVersion av => HashableTo Hash.Hash (Account av) where
  getHash = coerce @(AccountHash av) . getHash

-- |Create an empty account with the given public key, address and credentials.
newAccountMultiCredential :: forall av. (IsAccountVersion av)
  => GlobalContext  -- ^Cryptographic parameters, needed to derive the encryption key from the credentials.
  -> AccountThreshold -- ^The account threshold, how many credentials need to sign..
  -> AccountAddress -- ^Address of the account to be created.
  -> Map.Map CredentialIndex AccountCredential -- ^Initial credentials on the account. NB: It is assumed that this map has a value at index 'initialCredentialIndex' (0).
  -> Account av
newAccountMultiCredential cryptoParams threshold _accountAddress cs = Account {
        _accountPersisting = makeAccountPersisting PersistingAccountData {
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
        _accountStaking = AccountStakeNone
    }

-- |Create an empty account with the given public key, address and credential.
newAccount :: (IsAccountVersion av) => GlobalContext -> AccountAddress -> AccountCredential -> Account av
newAccount cryptoParams _accountAddress credential
    = newAccountMultiCredential cryptoParams 1 _accountAddress (Map.singleton initialCredentialIndex credential)
