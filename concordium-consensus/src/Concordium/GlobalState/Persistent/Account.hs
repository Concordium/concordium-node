{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, BangPatterns, RecordWildCards, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures, FlexibleInstances, QuantifiedConstraints, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Concordium.GlobalState.Persistent.Account where

import qualified Data.Set as Set
import Lens.Micro.Platform
import Data.Serialize
import GHC.Generics
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

import Concordium.Types
import qualified Concordium.GlobalState.Persistent.AccountTable as AT
import Concordium.GlobalState.Persistent.AccountTable (AccountIndex, AccountTable)
import qualified Concordium.ID.Types as ID
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Account as Transient

instance (MonadBlobStore m ref) => BlobStorable m ref AccountIndex
instance Trie.FixedTrieKey AccountAddress

data Accounts = Accounts {
    accountMap :: !(Trie.TrieN (BufferedBlobbed BlobRef) AccountAddress AccountIndex),
    accountTable :: !AccountTable,
    accountRegIds :: !(Nullable (Set.Set ID.CredentialRegistrationID)),
    accountRegIdHistory :: !RegIdHistory
}

makePersistent :: Transient.Accounts -> Accounts
makePersistent (Transient.Accounts amap atbl aregids) = Accounts {..}
    where
        accountMap = runIdentity (Trie.fromList (Map.toList amap))
        accountTable = AT.makePersistent atbl
        accountRegIds = Some aregids
        accountRegIdHistory = RegIdHistory (Set.toList aregids) Null

instance Show Accounts where
    show a = show (accountTable a)

-- |This history of used registration ids, consisting of a list of (uncommitted) ids, and a pointer
-- to a further (committed) history.
data RegIdHistory = RegIdHistory ![ID.CredentialRegistrationID] !(Nullable (BlobRef RegIdHistory))
    deriving (Generic)

instance Serialize RegIdHistory

-- This is probably not ideal, but some performance analysis is probably required to find a good
-- compromise.
instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef RegIdHistory

loadRegIds :: forall m. (MonadBlobStore m BlobRef) => Accounts -> m (Set.Set ID.CredentialRegistrationID, Accounts)
loadRegIds a@Accounts{accountRegIds = Some regids} = return (regids, a)
loadRegIds a@Accounts{accountRegIds = Null, ..} = do
        regids <- Set.fromList <$> loadRegIdHist accountRegIdHistory
        return (regids, a {accountRegIds = Some regids})
    where
        loadRegIdHist :: RegIdHistory -> m [ID.CredentialRegistrationID]
        loadRegIdHist (RegIdHistory l Null) = return l
        loadRegIdHist (RegIdHistory l (Some ref)) = (l ++) <$> (loadRegIdHist =<< loadRef ref)

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef Accounts where
    storeUpdate p Accounts{..} = do
        (pMap, accountMap') <- storeUpdate p accountMap
        (pTable, accountTable') <- storeUpdate p accountTable
        (pRIH, accountRegIdHistory') <- case accountRegIdHistory of
            RegIdHistory [] r -> return (put r, accountRegIdHistory)
            rih -> do
                rRIH <- storeRef rih
                return (put (Some rRIH), RegIdHistory [] (Some rRIH))
        let newAccounts = Accounts{
                accountMap = accountMap',
                accountTable = accountTable',
                accountRegIdHistory = accountRegIdHistory',
                ..
            }
        return (pMap >> pTable >> pRIH, newAccounts)
    store p a = fst <$> storeUpdate p a
    load p = do
        maccountMap <- load p
        maccountTable <- load p
        mrRIH <- load p
        return $ do
            accountMap <- maccountMap
            accountTable <- maccountTable
            rRIH <- mrRIH
            return $ Accounts {accountRegIds = Null, accountRegIdHistory = RegIdHistory [] rRIH, ..}

emptyAccounts :: Accounts
emptyAccounts = Accounts Trie.empty AT.empty (Some Set.empty) (RegIdHistory [] Null)

putAccount :: (MonadBlobStore m BlobRef) => Account -> Accounts -> m Accounts
putAccount !acct accts0 = do
        (isFresh, newAccountMap) <- Trie.adjust addToAM addr (accountMap accts0)
        newAccountTable <- case isFresh of
            Nothing -> snd <$> AT.append acct (accountTable accts0)
            Just ai -> AT.update (const (return ((), acct))) ai (accountTable accts0) <&> \case
                Nothing -> error $ "Account table corruption: missing account at index " ++ show ai
                Just ((), newAT) -> newAT
        return $! accts0 {accountMap = newAccountMap, accountTable = newAccountTable}
    where
        addr = acct ^. accountAddress
        acctIndex = AT.nextAccountIndex (accountTable accts0)
        addToAM Nothing = return (Nothing, Trie.Insert acctIndex)
        addToAM (Just v) = return (Just v, Trie.NoChange)

putNewAccount :: (MonadBlobStore m BlobRef) => Account -> Accounts -> m (Bool, Accounts)
putNewAccount !acct accts0 = do
        (isFresh, newAccountMap) <- Trie.adjust addToAM addr (accountMap accts0)
        if isFresh then do
            (_, newAccountTable) <- AT.append acct (accountTable accts0)
            return (True, accts0 {accountMap = newAccountMap, accountTable = newAccountTable})
        else
            return (False, accts0)
    where
        addr = acct ^. accountAddress
        acctIndex = AT.nextAccountIndex (accountTable accts0)
        addToAM Nothing = return (True, Trie.Insert acctIndex)
        addToAM (Just v) = return (False, Trie.NoChange)


exists :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m Bool
exists addr Accounts{..} = isJust <$> Trie.lookup addr accountMap

getAccount :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m (Maybe Account)
getAccount addr Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> AT.lookup ai accountTable

unsafeGetAccount :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m Account
unsafeGetAccount addr accts = getAccount addr accts <&> \case
        Just acct -> acct
        Nothing -> error $ "unsafeGetAccount : Account " ++ show addr ++ " does not exist."

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: (MonadBlobStore m BlobRef) => ID.CredentialRegistrationID -> Accounts -> m (Bool, Accounts)
regIdExists rid accts0 = do
        (regids, accts) <- loadRegIds accts0
        return (rid `Set.member` regids, accts)

recordRegId :: (MonadBlobStore m BlobRef) => ID.CredentialRegistrationID -> Accounts -> m Accounts
recordRegId rid accts0 = do
        (regids, accts1) <- loadRegIds accts0
        let (RegIdHistory l r) = accountRegIdHistory accts1
        return $! accts1 {
                accountRegIds = Some (Set.insert rid regids),
                accountRegIdHistory = RegIdHistory (rid:l) r
                }

updateAccount :: (MonadBlobStore m BlobRef) => (Account -> m (a, Account)) -> AccountAddress -> Accounts -> m (Maybe a, Accounts)
updateAccount fupd addr a0@Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> AT.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, at') -> return (Just res, a0 {accountTable = at'})

accountAddresses :: (MonadBlobStore m BlobRef) => Accounts -> m [AccountAddress]
accountAddresses = Trie.keys . accountMap