{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonoLocalBinds #-}

module Concordium.GlobalState.Persistent.Account where

import Control.Monad.IO.Class (MonadIO)
import Data.Serialize
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.GlobalState.Account

data PersistentAccount = PersistentAccount {
  -- |Next available nonce for this account.
  _accountNonce :: !Nonce
  -- |Current public account balance.
  ,_accountAmount :: !Amount
  -- |List of encrypted amounts on the account.
  -- TODO (MRA) create bufferedref list
  ,_accountEncryptedAmount :: ![EncryptedAmount]
  ,_persistingData :: !(BufferedRef PersistingAccountData)
  -- TODO (MRA) explain that we need this for the merkle tree because we can't create the hash based on a pointer to PersistingAccountData
  ,_accountHash :: !Hash.Hash
  } deriving Show

makeLenses ''PersistentAccount

instance (MonadBlobStore m BlobRef, MonadIO m) => BlobStorable m BlobRef PersistentAccount where
    storeUpdate p PersistentAccount{..} = do
        (pAccData, accData) <- storeUpdate p _persistingData
        let persistentAcc = PersistentAccount {
                _persistingData = accData,
                ..
            }
        let putAccs = do
                    put _accountNonce
                    put _accountAmount
                    put _accountEncryptedAmount
                    pAccData
        return (putAccs, persistentAcc)
    store p a = fst <$> storeUpdate p a
    load p = do
        mAccNonce <- load p
        mAccAmount <- load p
        mAccEncryptedAmount <- load p
        mAccData <- load p
        return $ do
          _accountNonce <- mAccNonce
          _accountAmount <- mAccAmount
          _accountEncryptedAmount <- mAccEncryptedAmount
          pData <- mAccData
          _persistingData <- makeBufferedRef pData
          let _accountHash = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount pData
          return PersistentAccount {..}

instance HashableTo Hash.Hash PersistentAccount where
  getHash = _accountHash

makePersistentAccount :: MonadIO m => Transient.Account -> m PersistentAccount
makePersistentAccount Transient.Account{..} = do
  let pdata = PersistingAccountData{..}
      _accountHash = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount pdata
  _persistingData <- makeBufferedRef pdata
  return PersistentAccount {..}

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^. accountAddress@ returns the account's address.
(^^.) :: (MonadIO m, MonadBlobStore m BlobRef)
      => PersistentAccount
      -> Getting b PersistingAccountData b
      -> m b
acc ^^. l = (^. l) <$> loadBufferedRef (acc ^. persistingData)

{-# INLINE (^^.) #-}
infixl 8 ^^.

-- TODO (MRA) check if the value is different and leave pointer unchanged if it isn't?
-- |Set a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountStakeDelegate .~~ Nothing@ sets the
-- account's stake delegate to 'Nothing'.
(.~~) :: (MonadIO m, MonadBlobStore m BlobRef)
      => ASetter PersistingAccountData PersistingAccountData a b
      -> b
      -> PersistentAccount
      -> m PersistentAccount
(.~~) l v acc = do
  pData <- loadBufferedRef (acc ^. persistingData)
  newPData <- makeBufferedRef $ pData & l .~ v
  return $ acc & persistingData .~ newPData

{-# INLINE (.~~) #-}
infixr 4 .~~

-- TODO (MRA) factor out common code between these two functions
-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) :: (MonadIO m, MonadBlobStore m BlobRef)
      => ASetter PersistingAccountData PersistingAccountData a b
      -> (a -> b)
      -> PersistentAccount
      -> m PersistentAccount
(%~~) l f acc = do
  pData <- loadBufferedRef (acc ^. persistingData)
  newPData <- makeBufferedRef $ pData & l %~ f
  return $ acc & persistingData .~ newPData

{-# INLINE (%~~) #-}
infixr 4 %~~
