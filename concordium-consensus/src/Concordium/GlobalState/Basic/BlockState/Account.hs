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

data Account = Account {
  -- |Address of the account.
  _accountAddress :: !AccountAddress
  -- |Next available nonce for this account.
  ,_accountNonce :: !Nonce
  -- |Current public account balance.
  ,_accountAmount :: !Amount
  -- |List of encrypted amounts on the account.
  ,_accountEncryptedAmount :: ![EncryptedAmount]
  -- |Encryption key with which the encrypted amount on this account must be
  -- encrypted. Other accounts use it to send encrypted amounts to this account,
  -- if the key exists.
  ,_accountEncryptionKey :: !AccountEncryptionKey
  -- |The key used to verify transaction signatures, it records the signature scheme used as well.
  ,_accountVerificationKeys :: !AccountKeys
  -- |For now the only operation we need with a credential is to check whether
  -- there are any credentials that are valid, and validity only depends on expiry.
  -- A Max priority queue allows us to efficiently check for existence of such credentials,
  -- as well as listing of all valid credentials, and efficient insertion of new credentials.
  -- The priority is the expiry time of the credential.
  ,_accountCredentials :: !(Queue.MaxPQueue CredentialValidTo CredentialDeploymentValues)
  -- |The baker to which this account's stake is delegated (if any).
  ,_accountStakeDelegate :: !(Maybe BakerId)
  -- |The set of instances belonging to this account.
  -- TODO: Revisit choice of datastructure.  Additions and removals
  -- are expected to be rare.  The set is traversed when stake delegation
  -- changes.
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
