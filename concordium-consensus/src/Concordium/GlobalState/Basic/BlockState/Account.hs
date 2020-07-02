{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.GlobalState.Basic.BlockState.Account where

import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Max as Queue
import qualified Data.Serialize as S
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Account

import Concordium.Types

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data Account = Account {
  _accountAddress :: !AccountAddress
  ,_accountNonce :: !Nonce
  ,_accountAmount :: !Amount
  ,_accountEncryptedAmount :: ![EncryptedAmount]
  ,_accountEncryptionKey :: !AccountEncryptionKey
  ,_accountVerificationKeys :: !AccountKeys
  ,_accountCredentials :: !(Queue.MaxPQueue CredentialValidTo CredentialDeploymentValues)
  ,_accountStakeDelegate :: !(Maybe BakerId)
  ,_accountInstances :: !(Set.Set ContractAddress)
  } deriving(Show, Eq)

makeLenses ''Account

instance S.Serialize Account where
  put Account{..} = S.put _accountAddress <>
                    S.put _accountNonce <>
                    S.put _accountAmount <>
                    S.put _accountEncryptedAmount <>
                    S.put _accountEncryptionKey <>
                    S.put _accountVerificationKeys <>
                    S.put (Queue.elemsU _accountCredentials) <> -- we do not care whether the output is ordered or not
                    S.put _accountStakeDelegate <>
                    S.put (Set.toAscList _accountInstances)
  get = do
    _accountAddress <- S.get
    _accountNonce <- S.get
    _accountAmount <- S.get
    _accountEncryptedAmount <- S.get
    _accountEncryptionKey <- S.get
    _accountVerificationKeys <- S.get
    preAccountCredentials <- Queue.fromList . map (\cdv -> (pValidTo (cdvPolicy cdv), cdv)) <$> S.get
    let _accountCredentials = Queue.seqSpine preAccountCredentials preAccountCredentials
    _accountStakeDelegate <- S.get
    _accountInstances <- Set.fromList <$> S.get
    return Account{..}

instance HashableTo Hash.Hash Account where
  getHash Account{..} = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount PersistingAccountData{..}

-- |Create an empty account with the given public key.
newAccount :: AccountKeys -> AccountAddress -> CredentialRegistrationID -> Account
newAccount _accountVerificationKeys _accountAddress regId = Account {
        _accountNonce = minNonce,
        _accountAmount = 0,
        _accountEncryptedAmount = [],
        _accountEncryptionKey = makeEncryptionKey regId,
        _accountCredentials = Queue.empty,
        _accountStakeDelegate = Nothing,
        _accountInstances = Set.empty,
        ..
    }
