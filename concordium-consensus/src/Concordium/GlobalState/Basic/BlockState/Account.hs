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

import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

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

instance HashableTo Hash.Hash Account where
  getHash Account{..} = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount (getHash _accountReleaseSchedule) _accountPersisting bkrHash
    where
      bkrHash = maybe nullAccountBakerHash getHash _accountBaker



-- |Create an empty account with the given public key, address and credentials.
newAccountMultiCredential ::
  GlobalContext  -- ^Cryptographic parameters, needed to derive the encryption key from the credentials.
  -> AccountThreshold -- ^The account threshold, how many credentials need to sign..
  -> AccountAddress -- ^Address of the account to be created.
  -> Map.Map CredentialIndex AccountCredential -- ^Initial credentials on the account. NB: It is assumed that this map has a value at index 0.
  -> Account
newAccountMultiCredential cryptoParams threshold _accountAddress cs = Account {
        _accountPersisting = PersistingAccountData {
        _accountEncryptionKey = makeEncryptionKey cryptoParams (credId (cs Map.! 0)),
        _accountCredentials = cs,
        _accountMaxCredentialValidTo = maximum (validTo <$> cs),
        _accountVerificationKeys = getAccountInformation threshold cs,
        ..
        },
        _accountNonce = minNonce,
        _accountAmount = 0,
        _accountEncryptedAmount = initialAccountEncryptedAmount,
        _accountReleaseSchedule = emptyAccountReleaseSchedule,
        _accountBaker = Nothing
    }

-- |Create an empty account with the given public key, address and credential.
newAccount :: GlobalContext -> AccountAddress -> AccountCredential -> Account
newAccount cryptoParams _accountAddress credential
    = newAccountMultiCredential cryptoParams 1 _accountAddress (Map.singleton 0 credential)
