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

import qualified Data.Set as Set
import qualified Data.Serialize as S
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types
import Concordium.ID.Parameters
import Concordium.Types.HashableTo
import Concordium.GlobalState.Account
import Concordium.GlobalState.BakerInfo

import Concordium.Types

data AccountBaker = AccountBaker {
  _stakedAmount :: !Amount,
  _stakeEarnings :: !Bool,
  _accountBakerInfo :: !BakerInfo,
  _bakerPendingChange :: !BakerPendingChange
} deriving (Eq, Show)

makeLenses ''AccountBaker

instance S.Serialize AccountBaker where
  put AccountBaker{..} = do
    S.put _stakedAmount
    S.put _stakeEarnings
    S.put _accountBakerInfo
    S.put _bakerPendingChange
  get = do
    _stakedAmount <- S.get
    _stakeEarnings <- S.get
    _accountBakerInfo <- S.get
    _bakerPendingChange <- S.get
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

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data Account = Account {
  _accountPersisting :: !PersistingAccountData
  ,_accountNonce :: !Nonce
  ,_accountAmount :: !Amount
  ,_accountEncryptedAmount :: !AccountEncryptedAmount
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
                    S.put _accountBaker
  get = do
    _accountPersisting <- S.get
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- S.get
    _accountBaker <- S.get
    -- Check that the account balance is sufficient to meet any staked amount.
    case _accountBaker of
      Just AccountBaker{..}
        | _stakedAmount > _accountAmount -> fail "Staked amount exceeds balance"
      _ -> return ()
    return Account{..}

instance HashableTo Hash.Hash Account where
  getHash Account{..} = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount _accountPersisting bkrHash
    where
      bkrHash = case _accountBaker of
        Nothing -> nullAccountBakerHash
        Just bkr -> getHash bkr

-- |Create an empty account with the given public key, encryption key, address and credential.
newAccount :: GlobalContext -> AccountKeys -> AccountAddress -> CredentialDeploymentValues -> Account
newAccount cryptoParams _accountVerificationKeys _accountAddress credential = Account {
        _accountPersisting = PersistingAccountData {
        _accountEncryptionKey = makeEncryptionKey cryptoParams (cdvRegId credential),
        _accountCredentials = [credential],
        _accountMaxCredentialValidTo = pValidTo (cdvPolicy credential),
        _accountInstances = Set.empty,
        ..
        },
        _accountNonce = minNonce,
        _accountAmount = 0,
        _accountEncryptedAmount = initialAccountEncryptedAmount,
        _accountBaker = Nothing
    }
