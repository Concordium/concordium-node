-- here because of the 'SupportsPersistentAccount' constraint is a bit too coarse right now.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
--  * Adding accounts
--  When an account is added (via ‘putNewAccount’) then it is first added to the ‘DifferenceMap’,
--  it is kept in memory for the block until it either gets finalized or pruned.
--  If a block is pruned then the retaining pointers are dropped and thus the block and associated ‘DifferenceMap’ is evicted from memory.
--
--  A thawed block is behind a  ‘BufferedRef’, this ‘BufferedRef’ is written to disk upon finalization
--  (or certification for consensus version 1).
--  This in return invokes ‘storeUpdate’ for all intermediate references for the block state for the particular block.
--  When the accounts structure is being written to disk so is the ‘DifferenceMap’,
--  i.e. the contents of the ‘DifferenceMap’ is being written to the lmdb backed account map.
--
--  * Startup flow
--  When a consensus runner starts up it can either be via an existing state or
--  from a fresh state (i.e. via a provided genesis configuration)
--
--  In the latter case then when starting up it is checked whether the lmdb backed account map is populated or not.
--  If the map is not populated then it is being populated by traversing the account table
--  and writing all @AccountAddress -> AccountIndex@ mappings into
--  the lmdb store in one transaction and then it proceeds as normal.
--  On the other hand, if the lmdb backed account map is already populated then the startup procedure will skip the populating step.
--
--  When starting up from a fresh genesis configuration then as part of creating the genesis state,
--  then the difference map is being built containing all accounts present in the genesis configuration.
--  When the genesis block is being written to disk, then so is the ‘DifferenceMap’
--  via the ‘storeUpdate’ implementation of the accounts structure.
--
--  * Rollbacks
--  For consensus version 0 no actions are required when rolling back blocks.
--  That is because we only ever store finalized blocks in this consensus version,
--  then there is no need to actually roll back any of the account present in the lmdb backed account map (as the accounts are finalized).
--
--  For consensus version 1 we also store certified blocks in addition to the finalized blocks.
--  Thus we have to roll back accounts that have been added to a certified block that is being rolled back.
--  We do not need to roll back accounts that have been added as part of finalized blocks in this consensus version as explained above for consensus version 0.
--
--  General flow
--  The account map resides in its own lmdb database and functions across protocol versions.
--  There is a ‘DifferenceMap’ associated with each block.
--  For frozen blocks this is simply empty, while for thawed blocks it may or may not
--  contain @ AccountAddress -> AccountIndex@ mappings depending on whether an account has been added for that particular block.
--
--  The lmdb backed account map consists of a single lmdb store indexed by AccountAddresses and values are the associated ‘AccountIndex’ for each account.
--
--  (The ‘DifferenceMap’ consists of a @Map AccountAddress AccountIndes@ which retains the accounts that have been added to the chain for the associated block.
--  Moreover the ‘DifferenceMap’ potentially retains a pointer to a so-called parent ‘DifferenceMap’.
--  I.e. @Maybe DifferenceMap@. If this is @Nothing@ then it means that the parent block is certified or finalized.
--  If the parent map yields a ‘DifferenceMap’ then the parent block is not persisted yet, and so the ‘DifferenceMap’ uses this parent map
--  for keeping track of non persisted accounts for supporting e.g. queries via the ‘AccountAddress’.
module Concordium.GlobalState.Persistent.Accounts where

import Control.Monad.Reader
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Serialize

import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.CachedRef
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.Utils.Serialization.Put

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.AccountMap as OldMap
import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.ID.Parameters
import Concordium.Types.HashableTo

-- | Representation of the set of accounts on the chain.
--  Each account has an 'AccountIndex' which is the order
--  in which it was created.
--
--  The operations on 'Accounts', when used correctly, maintain the following invariants:
--
--  * Every @(address, index)@ pair in the LMDB account map and difference map has a corresponding account
--    in 'accountTable' with the given index and address.
--  * Every @(index, account)@ pair in 'accountTable' has a corresponding entry
--    in 'accountMap', which maps the account address to @index@.
--  * The 'accountMap' only ever increases: no accounts are removed, and account
--    indexes do not change.
--  * 'accountRegIds' is either @Null@ or a set that is equivalent to the set of
--    registration ids represented by 'accountRegIdHistory'.
--  * The set represented by 'accountRegIdHistory' only ever grows.
--
--  Note that the operations *do not* enforce a correspondence between 'accountRegIds'
--  and the credentials used by the accounts in 'accountTable'.
--  The data integrity of accounts is also not enforced by these operations.
--
--  This implementation uses disk-backed structures for implementation.
--
--  Accounts are cached using an 'AccountCache'. This caches accounts keyed by the 'BlobRef' at
--  which the 'PersistentAccount' is stored, and uses a FIFO eviction strategy. The benefit of this
--  is that it is relatively simple and transparent. The downside is that it would most likely be
--  beneficial to evict old versions of an account from the cache when a new one is created, which
--  is not the case with this caching mechanism.
--
--  An alternative would be to key the cache by the account index. However, this is less convenient
--  since it requires the key to be available when loading the account from the reference, and
--  hence the current solution was chosen. Caching by account index (probably with an LRU strategy)
--  would likely be a more effective strategy over all.
data Accounts (pv :: ProtocolVersion) = Accounts
    { -- | Hashed Merkle-tree of the accounts
      accountTable :: !(LFMBTree' AccountIndex HashedBufferedRef (AccountRef (AccountVersionFor pv))),
      -- | Persisted representation of the map from registration ids to account indices.
      accountRegIdHistory :: !(Trie.TrieN UnbufferedFix ID.RawCredentialRegistrationID AccountIndex),
      -- | An in-memory difference map used keeping track of accounts
      --  added in live blocks.
      --  This is empty for a frozen block state.
      accountDiffMap :: !DiffMap.DifferenceMap
    }

instance (IsProtocolVersion pv) => Show (Accounts pv) where
    show accts = "Accounts: " <> show (accountTable accts) <> "DiffMap: " <> show (accountDiffMap accts)

-- | A constraint that ensures a monad @m@ supports the persistent account operations.
--  This essentially requires that the monad support 'MonadBlobStore', and 'MonadCache' for
--  the account cache and 'MonadAccountMapStore' for the persistent account map.
type SupportsPersistentAccount pv m =
    ( IsProtocolVersion pv,
      MonadBlobStore m,
      MonadCache (AccountCache (AccountVersionFor pv)) m,
      LMDBAccountMap.MonadAccountMapStore m
    )

instance (SupportsPersistentAccount pv m) => MHashableTo m H.Hash (Accounts pv) where
    getHashM Accounts{..} = getHashM accountTable

instance (SupportsPersistentAccount pv m) => BlobStorable m (Accounts pv) where
    storeUpdate Accounts{..} = do
        (pTable, accountTable') <- storeUpdate accountTable
        (pRegIdHistory, regIdHistory') <- storeUpdate accountRegIdHistory
        LMDBAccountMap.insert (Map.toList $ DiffMap.flatten accountDiffMap)
        let newAccounts =
                Accounts
                    { accountTable = accountTable',
                      accountRegIdHistory = regIdHistory',
                      accountDiffMap = DiffMap.empty $ Just accountDiffMap
                    }

        -- put an empty 'OldMap.PersistentAccountMap'.
        -- In earlier versions of the node the above mentioned account map was used,
        -- but this is now superseded by the 'LMDBAccountMap.MonadAccountMapStore'.
        -- We put this (0 :: Int) here to remain backwards compatible as this simply indicates an empty map.
        -- This should be revised as part of a future protocol update when the database layout can be changed.
        return (put (0 :: Int) >> pTable >> pRegIdHistory, newAccounts)
    load = do
        -- load the persistent account map and throw it away. We always put an empty one in,
        -- but that has not always been the case. But the 'OldMap.PersistentAccountMap' is now superseded by
        -- the LMDBAccountMap.MonadAccountMapStore.
        -- This should be revised as part of a future protocol update when the database layout can be changed.
        void (load :: Get (m (OldMap.PersistentAccountMap pv)))
        maccountTable <- load
        mrRIH <- load
        return $ do
            accountTable <- maccountTable
            accountRegIdHistory <- mrRIH
            let accountDiffMap = DiffMap.empty Nothing
            return $ Accounts{..}

instance (SupportsPersistentAccount pv m, av ~ AccountVersionFor pv) => Cacheable1 m (Accounts pv) (PersistentAccount av) where
    liftCache cch accts@Accounts{..} = do
        acctTable <- liftCache (liftCache @_ @(HashedCachedRef (AccountCache av) (PersistentAccount av)) cch) accountTable
        return
            accts{accountTable = acctTable}

emptyAccounts :: Maybe DiffMap.DifferenceMap -> Accounts pv
emptyAccounts Nothing = Accounts L.empty Trie.empty $ DiffMap.empty Nothing
emptyAccounts successorDiffMap = Accounts L.empty Trie.empty $ DiffMap.empty successorDiffMap

-- | Add a new account. Returns @Just idx@ if the new account is fresh, i.e., the address does not exist,
--  or @Nothing@ in case the account already exists. In the latter case there is no change to the accounts structure.
putNewAccount :: (SupportsPersistentAccount pv m) => PersistentAccount (AccountVersionFor pv) -> Accounts pv -> m (Maybe AccountIndex, Accounts pv)
putNewAccount !acct a0@Accounts{..} = do
    addr <- accountCanonicalAddress acct
    exists addr a0 >>= \case
        True -> return (Nothing, a0)
        False -> do
            (accIdx, newAccountTable) <- L.append acct accountTable
            let newDiffMap = DiffMap.insert addr accIdx accountDiffMap
            return (Just accIdx, a0{accountTable = newAccountTable, accountDiffMap = newDiffMap})

-- | Construct an 'Accounts' from a list of accounts. Inserted in the order of the list.
fromList :: (SupportsPersistentAccount pv m) => [PersistentAccount (AccountVersionFor pv)] -> m (Accounts pv)
fromList = foldlM insert $ emptyAccounts Nothing
  where
    insert accounts account = snd <$> putNewAccount account accounts

-- | Determine if an account with the given address exists.
exists :: (SupportsPersistentAccount pv m) => AccountAddress -> Accounts pv -> m Bool
exists addr accts = isJust <$> getAccountIndex addr accts

-- | Retrieve an account associated with the given credential registration ID.
--  Returns @Nothing@ if no such account exists.
getAccountByCredId :: (SupportsPersistentAccount pv m) => ID.RawCredentialRegistrationID -> Accounts pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountByCredId cid accs@Accounts{..} =
    Trie.lookup cid (accountRegIdHistory) >>= \case
        Nothing -> return Nothing
        Just ai -> fmap (ai,) <$> indexedAccount ai accs

-- | Get the account at a given index (if any).
getAccountIndex :: (SupportsPersistentAccount pv m) => AccountAddress -> Accounts pv -> m (Maybe AccountIndex)
getAccountIndex addr Accounts{..} =
    case DiffMap.lookup addr accountDiffMap of
        Just accIdx -> return $ Just accIdx
        Nothing ->
            LMDBAccountMap.lookup addr >>= \case
                Nothing -> return Nothing
                Just accIdx -> return $ Just accIdx

-- | Retrieve an account with the given address.
--  Returns @Nothing@ if no such account exists.
getAccount :: (SupportsPersistentAccount pv m) => AccountAddress -> Accounts pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
getAccount addr accts = fmap snd <$> getAccountWithIndex addr accts

-- | Retrieve an account and its index from a given address.
--  Returns @Nothing@ if no such account exists.
getAccountWithIndex :: forall pv m. (SupportsPersistentAccount pv m) => AccountAddress -> Accounts pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountWithIndex addr accts =
    getAccountIndex addr accts >>= \case
        Nothing -> return Nothing
        Just ai -> do
            mAcc <- L.lookup ai $ accountTable accts
            return $ (ai,) <$> mAcc

-- | Retrieve the account at a given index.
indexedAccount :: (SupportsPersistentAccount pv m) => AccountIndex -> Accounts pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
indexedAccount ai Accounts{..} = L.lookup ai accountTable

-- | Check that an account registration ID is not already on the chain.
--  See the foundation (Section 4.2) for why this is necessary.
--  Return @Just ai@ if the registration ID already exists, and @ai@ is the index of the account it is or was associated with.
regIdExists :: (MonadBlobStore m) => ID.CredentialRegistrationID -> Accounts pv -> m (Maybe AccountIndex)
regIdExists rid accts = Trie.lookup (ID.toRawCredRegId rid) (accountRegIdHistory $ accts)

-- | Record an account registration ID as used.
recordRegId :: (MonadBlobStore m) => ID.CredentialRegistrationID -> AccountIndex -> Accounts pv -> m (Accounts pv)
recordRegId rid idx accts0 = do
    accountRegIdHistory' <- Trie.insert (ID.toRawCredRegId rid) idx (accountRegIdHistory accts0)
    return $!
        accts0
            { accountTable = accountTable accts0,
              accountRegIdHistory = accountRegIdHistory'
            }

recordRegIds :: (MonadBlobStore m) => [(ID.CredentialRegistrationID, AccountIndex)] -> Accounts pv -> m (Accounts pv)
recordRegIds rids accts0 = foldM (\accts (cid, idx) -> recordRegId cid idx accts) accts0 rids

-- | Get the account registration ids map. This loads the entire map from the blob store, and so
--  should generally be avoided if this is not necessary.
loadRegIds :: forall m pv. (MonadBlobStore m) => Accounts pv -> m (Map.Map ID.RawCredentialRegistrationID AccountIndex)
loadRegIds accts = Trie.toMap (accountRegIdHistory accts)

-- | Perform an update to an account with the given address.
--  Does nothing (returning @Nothing@) if the account does not exist.
--  If the account does exist then the first component of the return value is @Just@
--  and the index of the updated account is returned, together with whatever value
--  was produced by the supplied update function.
--
--  This should not be used to alter the address of an account (which is
--  disallowed).
updateAccounts ::
    (SupportsPersistentAccount pv m) =>
    ( PersistentAccount (AccountVersionFor pv) ->
      m (a, PersistentAccount (AccountVersionFor pv))
    ) ->
    AccountAddress ->
    Accounts pv ->
    m (Maybe (AccountIndex, a), Accounts pv)
updateAccounts fupd addr a0@Accounts{..} = do
    getAccountIndex addr a0 >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> update ai
  where
    update ai =
        L.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, act') -> return (Just (ai, res), a0{accountTable = act'})

-- | Perform an update to an account with the given index.
--  Does nothing (returning @Nothing@) if the account does not exist.
--  This should not be used to alter the address of an account (which is
--  disallowed).
updateAccountsAtIndex :: (SupportsPersistentAccount pv m) => (PersistentAccount (AccountVersionFor pv) -> m (a, PersistentAccount (AccountVersionFor pv))) -> AccountIndex -> Accounts pv -> m (Maybe a, Accounts pv)
updateAccountsAtIndex fupd ai a0@Accounts{..} =
    L.update fupd ai accountTable >>= \case
        Nothing -> return (Nothing, a0)
        Just (res, act') -> return (Just res, a0{accountTable = act'})

-- | Perform an update to an account with the given index.
--  Does nothing if the account does not exist.
--  This should not be used to alter the address of an account (which is
--  disallowed).
updateAccountsAtIndex' :: (SupportsPersistentAccount pv m) => (PersistentAccount (AccountVersionFor pv) -> m (PersistentAccount (AccountVersionFor pv))) -> AccountIndex -> Accounts pv -> m (Accounts pv)
updateAccountsAtIndex' fupd ai = fmap snd . updateAccountsAtIndex fupd' ai
  where
    fupd' = fmap ((),) . fupd

-- | Get a list of all account addresses and their assoicated account indices.
allAccounts :: (SupportsPersistentAccount pv m) => Accounts pv -> m [(AccountAddress, AccountIndex)]
allAccounts accounts = do
    persistedAccs <- Map.fromList <$> LMDBAccountMap.all
    return $ Map.toList $ persistedAccs `Map.union` DiffMap.flatten (accountDiffMap accounts)

-- | Get a list of all account addresses.
accountAddresses :: (SupportsPersistentAccount pv m) => Accounts pv -> m [AccountAddress]
accountAddresses accounts = map fst <$> allAccounts accounts

-- | Serialize accounts in V0 format.
serializeAccounts :: (SupportsPersistentAccount pv m, MonadPut m) => GlobalContext -> Accounts pv -> m ()
serializeAccounts cryptoParams Accounts{..} = do
    liftPut $ putWord64be $ L.size accountTable
    L.mmap_ (serializeAccount cryptoParams) accountTable

-- | Fold over the account table in ascending order of account index.
foldAccounts :: (SupportsPersistentAccount pv m) => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccounts f a accts = L.mfold f a (accountTable accts)

-- | Fold over the account table in descending order of account index.
foldAccountsDesc :: (SupportsPersistentAccount pv m) => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccountsDesc f a accts = L.mfoldDesc f a (accountTable accts)

-- | Get all account addresses and their assoicated 'AccountIndex' via the account table in ascending order
--  of account index.
-- Note. This function should only be used when querying a historical block. When querying with respect to the "best block" then
-- use 'allAccounts'.
allAccountsViaTable :: (SupportsPersistentAccount pv m) => Accounts pv -> m [(AccountAddress, AccountIndex)]
allAccountsViaTable accts = do
    addresses <-
        foldAccountsDesc
            ( \accum pacc -> do
                !addr <- accountCanonicalAddress pacc
                return $ addr : accum
            )
            []
            accts
    return $! zip addresses [0 ..]

-- | If the LMDB account map is not already initialized, then this function populates the LMDB account map via the provided provided 'AccountsAndDiffMap'.
--  Otherwise, this function does nothing.
tryPopulateLMDBStore :: (SupportsPersistentAccount pv m) => Accounts pv -> m ()
tryPopulateLMDBStore accts = do
    isInitialized <- LMDBAccountMap.isInitialized
    unless isInitialized (void $ LMDBAccountMap.insert =<< allAccountsViaTable accts)

-- | See documentation of @migratePersistentBlockState@.
migrateAccounts ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      SupportMigration m t,
      SupportsPersistentAccount oldpv m,
      SupportsPersistentAccount pv (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    Accounts oldpv ->
    t m (Accounts pv)
migrateAccounts migration Accounts{..} = do
    newAccountTable <- L.migrateLFMBTree (migrateHashedCachedRef' (migratePersistentAccount migration)) accountTable
    -- The account registration IDs are not cached. There is a separate cache
    -- that is purely in-memory and just copied over.
    newAccountRegIds <- Trie.migrateUnbufferedTrieN return accountRegIdHistory
    return $!
        Accounts
            { accountTable = newAccountTable,
              accountRegIdHistory = newAccountRegIds,
              accountDiffMap = DiffMap.empty Nothing
            }
