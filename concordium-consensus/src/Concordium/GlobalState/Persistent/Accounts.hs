{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Concordium.GlobalState.Persistent.Accounts where

import qualified Data.Set as Set
import Lens.Micro.Platform
import Data.Serialize
import GHC.Generics
import Data.Maybe
import qualified Data.Map.Strict as Map

import Concordium.Types
import Concordium.Utils.Serialization.Put
import Concordium.GlobalState.Persistent.Account
import qualified Concordium.ID.Types as ID
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Account hiding (replaceUpTo, addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Transient

import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as Transient
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.Types.HashableTo
import Data.Foldable (foldrM, foldl', foldlM)
import Concordium.ID.Parameters

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
data Accounts (pv :: ProtocolVersion) = Accounts {
    -- |Unique index of accounts by 'AccountAddress'
    accountMap :: !(Trie.TrieN (BufferedBlobbed BlobRef) AccountAddress AccountIndex),
    -- |Hashed Merkle-tree of the accounts
    accountTable :: !(LFMBTree AccountIndex HashedBufferedRef (PersistentAccount pv)),
    -- |Optional cached set of used 'ID.CredentialRegistrationID's
    accountRegIds :: !(Nullable (Set.Set ID.CredentialRegistrationID)),
    -- |Persisted representation of the set of used 'ID.CredentialRegistrationID's
    accountRegIdHistory :: !RegIdHistory
}

-- |Convert a (non-persistent) 'Transient.Accounts' to a (persistent) 'Accounts'.
-- The new object is not yet stored on disk.
makePersistent :: (MonadBlobStore m, IsProtocolVersion pv) => Transient.Accounts pv -> m (Accounts pv)
makePersistent (Transient.Accounts amap atbl aregids) = do
    accountTable <- L.fromAscList =<< mapM (makePersistentAccount . snd) (Transient.toList atbl)
    accountMap <- Trie.fromList (Map.toList amap)
    return Accounts {..}
    where
        accountRegIds = Some aregids
        accountRegIdHistory = RegIdHistory (Set.toList aregids) Null

instance (IsProtocolVersion pv) => Show (Accounts pv) where
    show a = show (accountTable a)

instance (MonadBlobStore m, IsProtocolVersion pv) => MHashableTo m H.Hash (Accounts pv) where
  getHashM Accounts {..} = getHashM accountTable

-- |This history of used registration ids, consisting of a list of (uncommitted) ids, and a pointer
-- to a further (committed) history. Committed here means written to persistent storage.
data RegIdHistory = RegIdHistory ![ID.CredentialRegistrationID] !(Nullable (BlobRef RegIdHistory))
    deriving (Generic)

instance Serialize RegIdHistory

-- This is probably not ideal, but some performance analysis is probably required to find a good
-- compromise.
instance MonadBlobStore m => BlobStorable m RegIdHistory

-- |Load the registration ids.  If 'accountRegIds' is @Null@, then 'accountRegIdHistory'
-- is used (reading from disk as necessary) to determine it, in which case 'accountRegIds'
-- is updated with the determined value.
loadRegIds :: forall m pv. MonadBlobStore m => Accounts pv -> m (Set.Set ID.CredentialRegistrationID, Accounts pv)
loadRegIds a@Accounts{accountRegIds = Some regids} = return (regids, a)
loadRegIds a@Accounts{accountRegIds = Null, ..} = do
        regids <- Set.fromList <$> loadRegIdHist accountRegIdHistory
        return (regids, a {accountRegIds = Some regids})
    where
        loadRegIdHist :: RegIdHistory -> m [ID.CredentialRegistrationID]
        loadRegIdHist (RegIdHistory l Null) = return l
        loadRegIdHist (RegIdHistory l (Some ref)) = (l ++) <$> (loadRegIdHist =<< loadRef ref)

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (Accounts pv) where
    storeUpdate Accounts{..} = do
        (pMap, accountMap') <- storeUpdate accountMap
        (pTable, accountTable') <- storeUpdate accountTable
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
    store a = fst <$> storeUpdate a
    load = do
        maccountMap <- load
        maccountTable <- load
        mrRIH <- load
        return $ do
            accountMap <- maccountMap
            accountTable <- maccountTable
            rRIH <- mrRIH
            return $ Accounts {accountRegIds = Null, accountRegIdHistory = RegIdHistory [] rRIH, ..}

instance (MonadBlobStore m, IsProtocolVersion pv) => Cacheable m (Accounts pv) where
    cache accts0 = do
        (_, accts@Accounts{..}) <- loadRegIds accts0
        acctMap <- cache accountMap
        acctTable <- cache accountTable
        return accts{
            accountMap = acctMap,
            accountTable = acctTable
        }

-- |An 'Accounts' with no accounts.
emptyAccounts :: Accounts pv
emptyAccounts = Accounts Trie.empty L.empty (Some Set.empty) (RegIdHistory [] Null)

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index
-- and recording it in 'accountMap'.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account.
putAccount :: (MonadBlobStore m, IsProtocolVersion pv) => PersistentAccount pv -> Accounts pv -> m (Accounts pv)
putAccount !acct accts0 = do
        addr <- acct ^^. accountAddress
        (isFresh, newAccountMap) <- Trie.adjust addToAM addr (accountMap accts0)
        newAccountTable <- case isFresh of
            Nothing -> snd <$> L.append acct (accountTable accts0)
            Just ai -> L.update (const (return ((), acct))) ai (accountTable accts0) <&> \case
                Nothing -> error $ "Account table corruption: missing account at index " ++ show ai
                Just ((), newAT) -> newAT
        return $! accts0 {accountMap = newAccountMap, accountTable = newAccountTable}
    where
        acctIndex = fromIntegral $ L.size (accountTable accts0)
        addToAM Nothing = return (Nothing, Trie.Insert acctIndex)
        addToAM (Just v) = return (Just v, Trie.NoChange)

-- |Add a new account. Returns @False@ and leaves the accounts unchanged if
-- there is already an account with the same address.
putNewAccount :: (MonadBlobStore m, IsProtocolVersion pv) => PersistentAccount pv -> Accounts pv -> m (Bool, Accounts pv)
putNewAccount !acct accts0 = do
        addr <- acct ^^. accountAddress
        (isFresh, newAccountMap) <- Trie.adjust addToAM addr (accountMap accts0)
        if isFresh then do
            (_, newAccountTable) <- L.append acct (accountTable accts0)
            return (True, accts0 {accountMap = newAccountMap, accountTable = newAccountTable})
        else
            return (False, accts0)
    where
        acctIndex = fromIntegral $ L.size (accountTable accts0)
        addToAM Nothing = return (True, Trie.Insert acctIndex)
        addToAM (Just _) = return (False, Trie.NoChange)

-- |Determine if an account with the given address exists.
exists :: MonadBlobStore m => AccountAddress -> Accounts pv -> m Bool
exists addr Accounts{..} = isJust <$> Trie.lookup addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: (MonadBlobStore m, IsProtocolVersion pv) => AccountAddress -> Accounts pv -> m (Maybe (PersistentAccount pv))
getAccount addr Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> L.lookup ai accountTable

-- |Get the account at a given index (if any).
getAccountIndex :: MonadBlobStore m => AccountAddress -> Accounts pv -> m (Maybe AccountIndex)
getAccountIndex addr Accounts{..} = Trie.lookup addr accountMap

-- |Retrieve an account and its index from a given address.
-- Returns @Nothing@ if no such account exists.
getAccountWithIndex :: (MonadBlobStore m, IsProtocolVersion pv) => AccountAddress -> Accounts pv -> m (Maybe (AccountIndex, PersistentAccount pv))
getAccountWithIndex addr Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> fmap (ai, ) <$> L.lookup ai accountTable

-- |Retrieve the account at a given index.
indexedAccount :: (MonadBlobStore m, IsProtocolVersion pv) => AccountIndex -> Accounts pv -> m (Maybe (PersistentAccount pv))
indexedAccount ai Accounts{..} = L.lookup ai accountTable

-- |Retrieve an account with the given address.
-- An account with the address is required to exist.
unsafeGetAccount :: (MonadBlobStore m, IsProtocolVersion pv) => AccountAddress -> Accounts pv -> m (PersistentAccount pv)
unsafeGetAccount addr accts = getAccount addr accts <&> \case
        Just acct -> acct
        Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: MonadBlobStore m => ID.CredentialRegistrationID -> Accounts pv -> m (Bool, Accounts pv)
regIdExists rid accts0 = do
        (regids, accts) <- loadRegIds accts0
        return (rid `Set.member` regids, accts)

-- |Record an account registration ID as used.
recordRegId :: MonadBlobStore m => ID.CredentialRegistrationID -> Accounts pv -> m (Accounts pv)
recordRegId rid accts0 = do
        (regids, accts1) <- loadRegIds accts0
        let (RegIdHistory l r) = accountRegIdHistory accts1
        return $! accts1 {
                accountRegIds = Some (Set.insert rid regids),
                accountRegIdHistory = RegIdHistory (rid:l) r
                }

recordRegIds :: MonadBlobStore m => [ID.CredentialRegistrationID] -> Accounts pv -> m (Accounts pv)
recordRegIds rids accts0 = do
        (regids, accts1) <- loadRegIds accts0
        let (RegIdHistory l r) = accountRegIdHistory accts1
        return $! accts1 {
                accountRegIds = Some (Set.union regids (Set.fromAscList rids)),
                accountRegIdHistory = RegIdHistory (rids++l) r
                }

-- |Perform an update to an account with the given address.
-- Does nothing (returning @Nothing@) if the account does not exist.
-- This should not be used to alter the address of an account (which is
-- disallowed).
updateAccounts :: (MonadBlobStore m, IsProtocolVersion pv) => (PersistentAccount pv -> m (a, PersistentAccount pv)) -> AccountAddress -> Accounts pv -> m (Maybe a, Accounts pv)
updateAccounts fupd addr a0@Accounts{..} = Trie.lookup addr accountMap >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> L.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, act') -> return (Just res, a0 {accountTable = act'})

-- |Perform an update to an account with the given index.
-- Does nothing (returning @Nothing@) if the account does not exist.
-- This should not be used to alter the address of an account (which is
-- disallowed).
updateAccountsAtIndex :: (MonadBlobStore m, IsProtocolVersion pv) => (PersistentAccount pv -> m (a, PersistentAccount pv)) -> AccountIndex -> Accounts pv -> m (Maybe a, Accounts pv)
updateAccountsAtIndex fupd ai a0@Accounts{..} = L.update fupd ai accountTable >>= \case
        Nothing -> return (Nothing, a0)
        Just (res, act') -> return (Just res, a0 {accountTable = act'})

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv) => AccountUpdate -> PersistentAccount pv -> m (PersistentAccount pv)
updateAccount !upd !acc = do
  rData <- loadBufferedRef (acc ^. accountReleaseSchedule)
  (stakeDelta, releaseSchedule) <- case upd ^. auReleaseSchedule of
        Just l -> (amountToDelta $ foldl' (+) 0 (concatMap (\(values, _) -> map snd values) l),) <$> foldlM (flip addReleases) rData l
        Nothing -> return (amountToDelta 0, rData)
  encAmount <- loadBufferedRef (acc ^. accountEncryptedAmount)
  let updateSingle Add{..} = addIncomingEncryptedAmount newAmount
      updateSingle ReplaceUpTo{..} = replaceUpTo aggIndex newAmount
      updateSingle AddSelf{..} = addToSelfEncryptedAmount newAmount
  newEncryptedAmount <- foldrM updateSingle encAmount (upd ^. auEncrypted)
  newEncryptedAmountRef <- makeBufferedRef newEncryptedAmount
  releaseScheduleRef <- makeBufferedRef releaseSchedule
  let newAccWithoutHash = acc & accountNonce %~ setMaybe (upd ^. auNonce)
                                                    & accountAmount %~ applyAmountDelta (upd ^. auAmount . non 0)
                                                    & accountAmount %~ applyAmountDelta stakeDelta
                                                    & accountReleaseSchedule .~ releaseScheduleRef
                                                    & accountEncryptedAmount .~ newEncryptedAmountRef
  rehashAccount newAccWithoutHash
  where setMaybe (Just x) _ = x
        setMaybe Nothing y = y

-- |Get a list of all account addresses.
accountAddresses :: MonadBlobStore m => Accounts pv -> m [AccountAddress]
accountAddresses = Trie.keys . accountMap

-- |Serialize accounts in V0 format.
serializeAccounts :: (MonadBlobStore m, MonadPut m, IsProtocolVersion pv) => GlobalContext -> Accounts pv -> m ()
serializeAccounts cryptoParams accts = do
        liftPut $ putWord64be $ L.size (accountTable accts)
        L.mmap_ (serializeAccount cryptoParams) (accountTable accts)
