{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.GlobalState.Account where

import Control.Monad
import qualified Data.Set as Set
import Data.Serialize
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types
import Concordium.Types

-- |See 'Concordium.GlobalState.BlockState.AccountOperations' for documentation
data PersistingAccountData = PersistingAccountData {
  _accountAddress :: !AccountAddress
  ,_accountEncryptionKey :: !AccountEncryptionKey
  ,_accountVerificationKeys :: !AccountKeys
  ,_accountCredentials :: ![CredentialDeploymentValues]
  -- ^Credentials; most recent first
  ,_accountMaxCredentialValidTo :: !CredentialValidTo
  ,_accountStakeDelegate :: !(Maybe BakerId)
  ,_accountInstances :: !(Set.Set ContractAddress)
} deriving (Show, Eq)

makeClassy ''PersistingAccountData

instance Serialize PersistingAccountData where
  put PersistingAccountData{..} = put _accountAddress <>
                                  put _accountEncryptionKey <>
                                  put _accountVerificationKeys <>
                                  put _accountCredentials <> -- The order is significant for hash computation
                                  put _accountStakeDelegate <>
                                  put (Set.toAscList _accountInstances)
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    _accountVerificationKeys <- get
    _accountCredentials <- get
    when (null _accountCredentials) $ fail "Account has no credentials"
    let _accountMaxCredentialValidTo = maximum (pValidTo . cdvPolicy <$> _accountCredentials)
    _accountStakeDelegate <- get
    _accountInstances <- Set.fromList <$> get
    return PersistingAccountData{..}

-- TODO To avoid recomputing the hash for the persisting account data each time we update an account
-- we might want to explicitly store its hash, too.
makeAccountHash :: Nonce -> Amount -> [EncryptedAmount] -> PersistingAccountData -> Hash.Hash
makeAccountHash n a eas pd = Hash.hashLazy $ runPutLazy $
  put n >> put a >> put eas >> put pd

{-# INLINE addCredential #-}
addCredential :: HasPersistingAccountData d => CredentialDeploymentValues -> d -> d
addCredential cdv = (accountCredentials %~ (cdv:))
  . (accountMaxCredentialValidTo %~ max (pValidTo (cdvPolicy cdv)))
