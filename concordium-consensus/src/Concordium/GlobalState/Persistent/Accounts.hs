{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.Accounts where

import Control.Monad
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
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Account hiding (replaceUpTo, addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Transient

import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as Transient
import qualified Concordium.GlobalState.AccountMap as AccountMap
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.Types.HashableTo
import Data.Foldable (foldrM, foldl', foldlM)
import Concordium.ID.Parameters

-- |Type alias for the cache to use for accounts.
type AccountCache (av :: AccountVersion) = FIFOCache (PersistentAccount av)

-- |Construct a new 'AccountCache' with the given size.
newAccountCache :: Int -> IO (AccountCache av)
newAccountCache = newCache

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
    accountMap :: !(AccountMap.PersistentAccountMap pv),
    -- |Hashed Merkle-tree of the accounts
    accountTable :: !(LFMBTree' AccountIndex HashedBufferedRef (EagerlyHashedCachedRef (AccountCache (AccountVersionFor pv)) (PersistentAccount (AccountVersionFor pv)))),
    -- accountTable :: !(LFMBTree' AccountIndex HashedBufferedRef (BufferedRef (PersistentAccount (AccountVersionFor pv)))),
    -- |Optional cached set of used 'ID.CredentialRegistrationID's
    accountRegIds :: !(Nullable (Map.Map ID.RawCredentialRegistrationID AccountIndex)),
    -- |Persisted representation of the map from registration ids to account indices.
    accountRegIdHistory :: !(Trie.TrieN BufferedFix ID.RawCredentialRegistrationID AccountIndex)
}

type SupportsPersistentAccount pv m = (IsProtocolVersion pv, MonadBlobStore m, MonadCache (AccountCache (AccountVersionFor pv)) m)

-- |Convert a (non-persistent) 'Transient.Accounts' to a (persistent) 'Accounts'.
-- The new object is not yet stored on disk.
makePersistent :: SupportsPersistentAccount pv m => Transient.Accounts pv -> m (Accounts pv)
makePersistent (Transient.Accounts amap atbl aregids) = do
    accountTable <- L.fromAscList =<< mapM (makePersistentAccount . snd) (Transient.toList atbl)
    accountMap <- AccountMap.toPersistent amap
    accountRegIdHistory <- Trie.fromList (Map.toList aregids)
    return Accounts {accountRegIds = Some aregids,..}

instance (IsProtocolVersion pv) => Show (Accounts pv) where
    show a = show (accountTable a)

instance SupportsPersistentAccount pv m => MHashableTo m H.Hash (Accounts pv) where
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
loadRegIds :: forall m pv. MonadBlobStore m => Accounts pv -> m (Map.Map ID.RawCredentialRegistrationID AccountIndex, Accounts pv)
loadRegIds a@Accounts{accountRegIds = Some regids} = return (regids, a)
loadRegIds a@Accounts{accountRegIds = Null, ..} = do
        regids <- Trie.toMap accountRegIdHistory
        return (regids, a {accountRegIds = Some regids})

instance (SupportsPersistentAccount pv m) => BlobStorable m (Accounts pv) where
    storeUpdate Accounts{..} = do
        (pMap, accountMap') <- storeUpdate accountMap
        (pTable, accountTable') <- storeUpdate accountTable
        (pRegIdHistory, regIdHistory') <- storeUpdate accountRegIdHistory
        let newAccounts = Accounts{
                accountMap = accountMap',
                accountTable = accountTable',
                accountRegIdHistory = regIdHistory',
                ..
            }
        return (pMap >> pTable >> pRegIdHistory, newAccounts)
    store a = fst <$> storeUpdate a
    load = do
        maccountMap <- load
        maccountTable <- load
        mrRIH <- load
        return $ do
            accountMap <- maccountMap
            accountTable <- maccountTable
            accountRegIdHistory <- mrRIH
            return $ Accounts {accountRegIds = Null,..}

instance SupportsPersistentAccount pv m => Cacheable m (Accounts pv) where
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
emptyAccounts = Accounts AccountMap.empty L.empty (Some Map.empty) Trie.empty

-- |Add a new account. Returns @Just idx@ if the new account is fresh, i.e., the address does not exist,
-- or @Nothing@ in case the account already exists. In the latter case there is no change to the accounts structure.
putNewAccount :: SupportsPersistentAccount pv m => PersistentAccount (AccountVersionFor pv) -> Accounts pv -> m (Maybe AccountIndex, Accounts pv)
putNewAccount !acct accts0 = do
        addr <- acct ^^. accountAddress
        (existingAccountId, newAccountMap) <- AccountMap.maybeInsert addr acctIndex (accountMap accts0)
        if isNothing existingAccountId then do
            (_, newAccountTable) <- L.append acct (accountTable accts0)
            return (Just acctIndex, accts0 {accountMap = newAccountMap, accountTable = newAccountTable})
        else
            return (Nothing, accts0)
    where
        acctIndex = fromIntegral $ L.size (accountTable accts0)


-- |Determine if an account with the given address exists.
exists :: (IsProtocolVersion pv, MonadBlobStore m) => AccountAddress -> Accounts pv -> m Bool
exists addr Accounts{..} = AccountMap.isAddressAssigned addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: SupportsPersistentAccount pv m => AccountAddress -> Accounts pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
getAccount addr Accounts{..} = AccountMap.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> L.lookup ai accountTable

-- |Retrieve an account associated with the given credential registration ID.
-- Returns @Nothing@ if no such account exists.
getAccountByCredId :: SupportsPersistentAccount pv m => ID.RawCredentialRegistrationID -> Accounts pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountByCredId cid accs@Accounts{accountRegIds = Null,..} = Trie.lookup cid accountRegIdHistory  >>= \case
        Nothing -> return Nothing
        Just ai -> fmap (ai, ) <$> indexedAccount ai accs
getAccountByCredId cid accs@Accounts{accountRegIds = Some cachedIds} =
    case Map.lookup cid cachedIds of
        Nothing -> return Nothing
        Just ai -> fmap (ai, ) <$> indexedAccount ai accs


-- |Get the account at a given index (if any).
getAccountIndex :: (IsProtocolVersion pv, MonadBlobStore m) => AccountAddress -> Accounts pv -> m (Maybe AccountIndex)
getAccountIndex addr Accounts{..} = AccountMap.lookup addr accountMap

-- |Retrieve an account and its index from a given address.
-- Returns @Nothing@ if no such account exists.
getAccountWithIndex :: SupportsPersistentAccount pv m => AccountAddress -> Accounts pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountWithIndex addr Accounts{..} = AccountMap.lookup addr accountMap >>= \case
        Nothing -> return Nothing
        Just ai -> fmap (ai, ) <$> L.lookup ai accountTable

-- |Retrieve the account at a given index.
indexedAccount :: SupportsPersistentAccount pv m => AccountIndex -> Accounts pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
indexedAccount ai Accounts{..} = L.lookup ai accountTable

-- |Retrieve an account with the given address.
-- An account with the address is required to exist.
unsafeGetAccount :: SupportsPersistentAccount pv m => AccountAddress -> Accounts pv -> m (PersistentAccount (AccountVersionFor pv))
unsafeGetAccount addr accts = getAccount addr accts <&> \case
        Just acct -> acct
        Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."

-- |Check whether the given account address would clash with any existing account address.
-- The meaning of "clash" depends on the protocol version.
addressWouldClash :: (IsProtocolVersion pv, MonadBlobStore m) => AccountAddress -> Accounts pv -> m Bool
addressWouldClash addr Accounts{..} = AccountMap.addressWouldClash addr accountMap

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @Just ai@ if the registration ID already exists, and @ai@ is the index of the account it is or was associated with.
regIdExists :: MonadBlobStore m => ID.CredentialRegistrationID -> Accounts pv -> m (Maybe AccountIndex, Accounts pv)
regIdExists rid accts0 = do
        (regids, accts) <- loadRegIds accts0
        return (ID.toRawCredRegId rid `Map.lookup` regids, accts)

-- |Record an account registration ID as used.
recordRegId :: MonadBlobStore m => ID.CredentialRegistrationID -> AccountIndex -> Accounts pv -> m (Accounts pv)
recordRegId rid idx accts0 = do
        accountRegIdHistory' <- Trie.insert (ID.toRawCredRegId rid) idx (accountRegIdHistory accts0)
        return $! accts0 {
                accountRegIds = Map.insert (ID.toRawCredRegId rid) idx <$> accountRegIds accts0,
                accountRegIdHistory = accountRegIdHistory'
                }

recordRegIds :: MonadBlobStore m => [(ID.CredentialRegistrationID, AccountIndex)] -> Accounts pv -> m (Accounts pv)
recordRegIds rids accts0 = foldM (\accts (cid, idx) -> recordRegId cid idx accts) accts0 rids

-- |Perform an update to an account with the given address.
-- Does nothing (returning @Nothing@) if the account does not exist.
-- If the account does exist then the first component of the return value is @Just@
-- and the index of the updated account is returned, together with whatever value
-- was produced by the supplied update function.
--
-- This should not be used to alter the address of an account (which is
-- disallowed).
updateAccounts :: SupportsPersistentAccount pv m => (PersistentAccount (AccountVersionFor pv) -> m (a, PersistentAccount (AccountVersionFor pv))) -> AccountAddress -> Accounts pv -> m (Maybe (AccountIndex, a), Accounts pv)
updateAccounts fupd addr a0@Accounts{..} = AccountMap.lookup addr accountMap >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> L.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, act') -> return (Just (ai, res), a0 {accountTable = act'})

-- |Perform an update to an account with the given index.
-- Does nothing (returning @Nothing@) if the account does not exist.
-- This should not be used to alter the address of an account (which is
-- disallowed).
updateAccountsAtIndex :: SupportsPersistentAccount pv m => (PersistentAccount (AccountVersionFor pv) -> m (a, PersistentAccount (AccountVersionFor pv))) -> AccountIndex -> Accounts pv -> m (Maybe a, Accounts pv)
updateAccountsAtIndex fupd ai a0@Accounts{..} = L.update fupd ai accountTable >>= \case
        Nothing -> return (Nothing, a0)
        Just (res, act') -> return (Just res, a0 {accountTable = act'})

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: forall m av. (MonadBlobStore m, IsAccountVersion av) => AccountUpdate -> PersistentAccount av -> m (PersistentAccount av)
updateAccount !upd !acc = do
  rData <- refLoad (acc ^. accountReleaseSchedule)
  (stakeDelta, releaseSchedule) <- case upd ^. auReleaseSchedule of
        Just l -> (amountToDelta $ foldl' (+) 0 (concatMap (\(values, _) -> map snd values) l),) <$> foldlM (flip addReleases) rData l
        Nothing -> return (amountToDelta 0, rData)
  encAmount <- refLoad (acc ^. accountEncryptedAmount)
  let updateSingle Add{..} = addIncomingEncryptedAmount newAmount
      updateSingle ReplaceUpTo{..} = replaceUpTo aggIndex newAmount
      updateSingle AddSelf{..} = addToSelfEncryptedAmount newAmount
  newEncryptedAmount <- foldrM updateSingle encAmount (upd ^. auEncrypted)
  newEncryptedAmountRef <- refMake newEncryptedAmount
  releaseScheduleRef <- refMake releaseSchedule
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
accountAddresses = AccountMap.addresses . accountMap

-- |Serialize accounts in V0 format.
serializeAccounts :: (SupportsPersistentAccount pv m, MonadPut m) => GlobalContext -> Accounts pv -> m ()
serializeAccounts cryptoParams accts = do
        liftPut $ putWord64be $ L.size (accountTable accts)
        L.mmap_ (serializeAccount cryptoParams) (accountTable accts)

-- |Fold over the account table in ascending order of account index.
foldAccounts :: SupportsPersistentAccount pv m => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccounts f a accts = L.mfold f a (accountTable accts)

-- |Fold over the account table in ascending order of account index.
foldAccountsDesc :: SupportsPersistentAccount pv m => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccountsDesc f a accts = L.mfoldDesc f a (accountTable accts)