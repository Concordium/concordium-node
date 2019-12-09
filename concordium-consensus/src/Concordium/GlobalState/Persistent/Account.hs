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
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient

-- |Representation of the set of accounts on the chain.
-- Each account has an 'AccountIndex' which is the order
-- in which it was created.
--
-- The operations on 'Accounts', when used correctly, maintain the following invariants:
--
-- * Every @(address, index)@ pair in 'accountMap' has a corresponding account
--   in 'accountTable' with the given index and address.
-- * Every @(index, account)@ pair in 'accountTable' has a corresponding entry
--   in 'accountMap', which maps the account address to @index@.
-- * The 'accountMap' only ever increases: no accounts are removed, and account
--   indexes do not change.
-- * 'accountRegIds' is either @Null@ or a set that is equivalent to the set of
--   registration ids represented by 'accountRegIdHistory'.
-- * The set represented by 'accountRegIdHistory' only ever grows.
--
-- Note that the operations *do not* enforce a correspondence between 'accountRegIds'
-- and the credentials used by the accounts in 'accountTable'.
-- The data integrity of accounts is also not enforced by these operations.
--
-- This implementation uses disk-backed structures for implementation.
data Accounts = Accounts {
    -- |Unique index of accounts by 'AccountAddress'
    accountMap :: !(Trie.TrieN (BufferedBlobbed BlobRef) AccountAddress AccountIndex),
    -- |Hashed Markel-tree of the accounts
    accountTable :: !AccountTable,
    -- |Optional cached set of used 'ID.CredentialRegistrationID's
    accountRegIds :: !(Nullable (Set.Set ID.CredentialRegistrationID)),
    -- |Persisted representation of the set of used 'ID.CredentialRegistrationID's
    accountRegIdHistory :: !RegIdHistory
}

-- |Convert a (non-persistent) 'Transient.Accounts' to a (persistent) 'Accounts'.
-- The new object is not yet stored on disk.
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

-- |Load the registration ids.  If 'accountRegIds' is @Null@, then 'accountRegIdHistory'
-- is used (reading from disk as necessary) to determine it, in which case 'accountRegIds'
-- is updated with the determined value.
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

-- |An 'Accounts' with no accounts.
emptyAccounts :: Accounts
emptyAccounts = Accounts Trie.empty AT.empty (Some Set.empty) (RegIdHistory [] Null)

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index
-- and recording it in 'accountMap'.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account.
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

-- |Add a new account. Returns @False@ and leaves the accounts unchanged if
-- there is already an account with the same address.
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
        addToAM (Just _) = return (False, Trie.NoChange)

-- |Determine if an account with the given address exists.
exists :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m Bool
exists addr Accounts{..} = isJust <$> Trie.lookup addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m (Maybe Account)
getAccount addr Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> AT.lookup ai accountTable

-- |Retrieve an account with the given address.
-- An account with the address is required to exist.
unsafeGetAccount :: (MonadBlobStore m BlobRef) => AccountAddress -> Accounts -> m Account
unsafeGetAccount addr accts = getAccount addr accts <&> \case
        Just acct -> acct
        Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: (MonadBlobStore m BlobRef) => ID.CredentialRegistrationID -> Accounts -> m (Bool, Accounts)
regIdExists rid accts0 = do
        (regids, accts) <- loadRegIds accts0
        return (rid `Set.member` regids, accts)

-- |Record an account registration ID as used.
recordRegId :: (MonadBlobStore m BlobRef) => ID.CredentialRegistrationID -> Accounts -> m Accounts
recordRegId rid accts0 = do
        (regids, accts1) <- loadRegIds accts0
        let (RegIdHistory l r) = accountRegIdHistory accts1
        return $! accts1 {
                accountRegIds = Some (Set.insert rid regids),
                accountRegIdHistory = RegIdHistory (rid:l) r
                }

-- |Perform an update to an account with the given address.
-- Does nothing (returning @Nothing@) if the account does not exist.
-- This should not be used to alter the address of an account (which is
-- disallowed).
updateAccount :: (MonadBlobStore m BlobRef) => (Account -> m (a, Account)) -> AccountAddress -> Accounts -> m (Maybe a, Accounts)
updateAccount fupd addr a0@Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> AT.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, at') -> return (Just res, a0 {accountTable = at'})

-- |Get a list of all account addresses.
accountAddresses :: (MonadBlobStore m BlobRef) => Accounts -> m [AccountAddress]
accountAddresses = Trie.keys . accountMap