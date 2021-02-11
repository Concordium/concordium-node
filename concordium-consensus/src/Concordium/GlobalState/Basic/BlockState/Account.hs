{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Concordium.GlobalState.Basic.BlockState.Account(
  module Concordium.GlobalState.Account,
  module Concordium.GlobalState.Basic.BlockState.Account
) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Serialize as S
import Lens.Micro.Platform

import Concordium.Utils.Serialization
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types
import Concordium.ID.Parameters
import Concordium.Types.HashableTo
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import Concordium.Types

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data Account = Account {
  _accountPersisting :: !PersistingAccountData
  ,_accountNonce :: !Nonce
  ,_accountAmount :: !Amount -- ^This account amount contains all the amount owned by the account,
                            --  this particularly means that the available amount is equal to
                            -- @accountAmount - totalLockedUpBalance accountReleaseSchedule@.
  ,_accountEncryptedAmount :: !AccountEncryptedAmount
  ,_accountReleaseSchedule :: !AccountReleaseSchedule
  ,_accountBaker :: !(Maybe AccountBaker)
  } deriving(Show, Eq)

makeLenses ''Account

instance HasPersistingAccountData Account where
  persistingAccountData = accountPersisting

-- TODO: Account serialization should be improved.
-- (This particular serialization is only used for genesis.)
instance S.Serialize Account where
  put Account{..} = S.put _accountPersisting <>
                    S.put _accountNonce <>
                    S.put _accountAmount <>
                    S.put _accountEncryptedAmount <>
                    S.put _accountReleaseSchedule <>
                    S.put _accountBaker
  get = do
    _accountPersisting <- S.get
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- S.get
    _accountReleaseSchedule <- S.get
    _accountBaker <- S.get
    -- Check that the account balance is sufficient to meet any staked amount.
    case _accountBaker of
      Just AccountBaker{..}
        | _stakedAmount > _accountAmount -> fail "Staked amount exceeds balance"
      _ -> return ()
    return Account{..}

-- |Serialize an account in V0 format.
-- This format allows accounts to be stored in a reduced format by
-- elliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
-- This format does not store the smart contract instances, which are
-- implied by the instance table.
putAccountV0 :: GlobalContext -> S.Putter Account
putAccountV0 cryptoParams Account{_accountPersisting = PersistingAccountData {..}, ..} = do
    S.put flags
    when asfExplicitAddress $ S.put _accountAddress
    when asfExplicitEncryptionKey $ S.put _accountEncryptionKey
    S.put _accountVerificationKeys
    putCredentials
    S.put _accountNonce
    S.put _accountAmount
    when asfExplicitEncryptedAmount $ S.put _accountEncryptedAmount
    when asfExplicitReleaseSchedule $ S.put _accountReleaseSchedule
    mapM_ S.put _accountBaker
  where
    flags = AccountSerializationFlags {..}
    initialRegId = regId (NE.last _accountCredentials)
    asfExplicitAddress = _accountAddress /= addressFromRegId initialRegId
    asfExplicitEncryptionKey = _accountEncryptionKey /= makeEncryptionKey cryptoParams initialRegId
    (asfMultipleCredentials, putCredentials) = case _accountCredentials of
      cred :| [] -> (False, S.put cred)
      creds -> (True, putLength (length creds) >> mapM_ S.put creds)
    asfExplicitEncryptedAmount = _accountEncryptedAmount /= initialAccountEncryptedAmount
    asfExplicitReleaseSchedule = _accountReleaseSchedule /= emptyAccountReleaseSchedule
    asfHasBaker = isJust _accountBaker


-- |Deserialize an account in V0 format.
-- The instances are initialized as empty, and must be corrected
-- by processing the instance table.
getAccountV0 :: GlobalContext -> S.Get Account
getAccountV0 cryptoParams = do
    AccountSerializationFlags{..} <- S.get
    preAddress <- if asfExplicitAddress then Just <$> S.get else return Nothing
    preEncryptionKey <- if asfExplicitEncryptionKey then Just <$> S.get else return Nothing
    _accountVerificationKeys <- S.get
    let getCredentials
          | asfMultipleCredentials = do
              numCredentials <- getLength
              when (numCredentials < 1) $ fail "Account has no credentials"
              (:|) <$> S.get <*> replicateM (numCredentials - 1) S.get
          | otherwise = pure <$> S.get
    _accountCredentials <- getCredentials
    let _accountMaxCredentialValidTo = maximum (validTo <$> _accountCredentials)
    let initialRegId = regId (NE.last _accountCredentials)
        _accountAddress = fromMaybe (addressFromRegId initialRegId) preAddress
        _accountEncryptionKey = fromMaybe (makeEncryptionKey cryptoParams initialRegId) preEncryptionKey
    let _accountInstances = Set.empty
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- if asfExplicitEncryptedAmount then S.get else return initialAccountEncryptedAmount
    _accountReleaseSchedule <- if asfExplicitReleaseSchedule then S.get else return emptyAccountReleaseSchedule
    _accountBaker <- if asfHasBaker then Just <$> S.get else return Nothing
    let _accountPersisting = PersistingAccountData {..}
    return Account{..}

instance HashableTo Hash.Hash Account where
  getHash Account{..} = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount (getHash _accountReleaseSchedule) _accountPersisting bkrHash
    where
      bkrHash = maybe nullAccountBakerHash getHash _accountBaker

-- |Create an empty account with the given public key, address and credentials.
newAccountMultiCredential :: GlobalContext -> AccountKeys -> AccountAddress -> NonEmpty AccountCredential -> Account
newAccountMultiCredential cryptoParams _accountVerificationKeys _accountAddress cs = Account {
        _accountPersisting = PersistingAccountData {
        _accountEncryptionKey = makeEncryptionKey cryptoParams (regId (NE.last cs)),
        _accountCredentials = cs,
        _accountMaxCredentialValidTo = maximum (validTo <$> cs),
        _accountInstances = Set.empty,
        ..
        },
        _accountNonce = minNonce,
        _accountAmount = 0,
        _accountEncryptedAmount = initialAccountEncryptedAmount,
        _accountReleaseSchedule = emptyAccountReleaseSchedule,
        _accountBaker = Nothing
    }

-- |Create an empty account with the given public key, address and credential.
newAccount :: GlobalContext -> AccountKeys -> AccountAddress -> AccountCredential -> Account
newAccount cryptoParams _accountVerificationKeys _accountAddress credential
    = newAccountMultiCredential cryptoParams _accountVerificationKeys _accountAddress (credential :| [])
