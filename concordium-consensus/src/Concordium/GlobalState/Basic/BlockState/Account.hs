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
import Concordium.Types.Migration
import Concordium.Genesis.Data

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

-- |A traversal for accessing the 'AccountBaker' record of an account if it has one.
-- This can be used for getting the baker (e.g. with '(^?)') or updating it (if it already exists)
-- but not for setting it unless the account already has a baker.
accountBaker :: Traversal' (Account av) (AccountBaker av)
accountBaker f = g
  where
    g acct@Account{_accountStaking = AccountStakeBaker bkr} =
        (\bkr' -> acct{_accountStaking = AccountStakeBaker bkr'}) <$> f bkr
    g acct = pure acct

-- |Get the baker from an account, on the basis that it is known to be a baker
unsafeAccountBaker :: HasCallStack => SimpleGetter (Account av) (AccountBaker av)
unsafeAccountBaker = singular accountBaker

-- |A traversal for accessing the 'AccountDelegation' record of an account if it has one.
-- This can be used for getting the delegation (e.g. with '(^?)') or updating it (if it already
-- exists) but not for setting it unless the account already has a baker.
accountDelegator :: Traversal' (Account av) (AccountDelegation av)
accountDelegator f = g
  where
    g acct@Account{_accountStaking = AccountStakeDelegate del} =
        (\del' -> acct{_accountStaking = AccountStakeDelegate del'}) <$> f del
    g acct = pure acct

-- |Get the delegator on an account, on the basis that it is known to be a delegator.
unsafeAccountDelegator :: HasCallStack => SimpleGetter (Account av) (AccountDelegation av)
unsafeAccountDelegator = singular accountDelegator

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
    asfExplicitAddress = _accountAddress /= addressFromRegIdRaw initialCredId
    -- There is an opportunity for improvement here. We do not have to convert
    -- the raw key to a structured one. We can check the equality directly on
    -- the byte representation (in fact equality is defined on those). However
    -- that requires a bit of work to expose the right raw values from
    -- cryptographic parameters.
    asfExplicitEncryptionKey = unsafeEncryptionKeyFromRaw _accountEncryptionKey /= makeEncryptionKey cryptoParams (unsafeCredIdFromRaw initialCredId)
    (asfMultipleCredentials, putCredentials) = case Map.toList _accountCredentials of
      [(i, cred)] | i == initialCredentialIndex -> (False, S.put cred)
      _ -> (True, putSafeMapOf S.put S.put _accountCredentials)
    asfExplicitEncryptedAmount = _accountEncryptedAmount /= initialAccountEncryptedAmount
    asfExplicitReleaseSchedule = _accountReleaseSchedule /= emptyAccountReleaseSchedule
    asfHasBakerOrDelegation = _accountStaking /= AccountStakeNone
    asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
    asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials

-- |Deserialize an account.
-- The serialization format may depend on the protocol version, and maybe migrated from one version
-- to another, using the 'StateMigrationParameters' provided.
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
    let _accountAddress = fromMaybe (addressFromRegIdRaw initialCredId) preAddress
        -- There is an opportunity for improvement here. We do not have to convert
        -- the raw credId to a structured one. We can directly construct the 
        -- However that requires a bit of work to expose the right raw values from
        -- cryptographic parameters.
        _accountEncryptionKey = fromMaybe (toRawEncryptionKey (makeEncryptionKey cryptoParams (unsafeCredIdFromRaw initialCredId))) preEncryptionKey
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- if asfExplicitEncryptedAmount then S.get else return initialAccountEncryptedAmount
    _accountReleaseSchedule <- if asfExplicitReleaseSchedule then S.get else return emptyAccountReleaseSchedule
    _accountStaking <- if asfHasBakerOrDelegation
      then
        migrateAccountStake migration <$> getAccountStake
      else
        return AccountStakeNone
    let _accountPersisting = makeAccountPersisting PersistingAccountData {..}
    return Account{..}

instance IsAccountVersion av => HashableTo (AccountHash av) (Account av) where
  getHash Account{..} = makeAccountHash $ AccountHashInputs {
      ahiNextNonce = _accountNonce,
      ahiAccountAmount = _accountAmount,
      ahiAccountEncryptedAmount = _accountEncryptedAmount,
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
        _accountEncryptionKey = toRawEncryptionKey (makeEncryptionKey cryptoParams (credId (cs Map.! initialCredentialIndex))),
        _accountCredentials = toRawAccountCredential <$> cs,
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
