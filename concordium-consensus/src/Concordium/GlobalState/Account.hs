{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.GlobalState.Account where

import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Max as Queue
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
  ,_accountCredentials :: !(Queue.MaxPQueue CredentialValidTo CredentialDeploymentValues)
  ,_accountStakeDelegate :: !(Maybe BakerId)
  ,_accountInstances :: !(Set.Set ContractAddress)
} deriving (Show, Eq)

makeLenses ''PersistingAccountData

instance Serialize PersistingAccountData where
  put PersistingAccountData{..} = put _accountAddress <>
                                  put _accountEncryptionKey <>
                                  put _accountVerificationKeys <>
                                  put (Queue.elemsU _accountCredentials) <> -- we do not care whether the output is ordered or not
                                  put _accountStakeDelegate <>
                                  put (Set.toAscList _accountInstances)
  get = do
    _accountAddress <- get
    _accountEncryptionKey <- get
    _accountVerificationKeys <- get
    preAccountCredentials <- Queue.fromList . map (\cdv -> (pValidTo (cdvPolicy cdv), cdv)) <$> get
    let _accountCredentials = Queue.seqSpine preAccountCredentials preAccountCredentials
    _accountStakeDelegate <- get
    _accountInstances <- Set.fromList <$> get
    return PersistingAccountData{..}

makeAccountHash :: Nonce -> Amount -> [EncryptedAmount] -> PersistingAccountData -> Hash.Hash
makeAccountHash n a eas pd = Hash.hashLazy $ runPutLazy $
  put n >> put a >> put eas >> put pd
