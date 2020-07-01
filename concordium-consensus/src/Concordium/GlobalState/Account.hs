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

-- TODO (MRA) add comment
data PersistingAccountData = PersistingAccountData {
  -- |Address of the account.
  _accountAddress :: !AccountAddress
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
