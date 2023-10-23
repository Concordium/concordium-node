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

module Concordium.GlobalState.Persistent.Accounts where

import Control.Monad.State
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
--  * Every @(address, index)@ pair in 'accountMap' has a corresponding account
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
      accountRegIdHistory :: !(Trie.TrieN UnbufferedFix ID.RawCredentialRegistrationID AccountIndex)
    }

-- | An 'Accounts' with an assoicated ' DiffMap.DifferenceMap'.
--  The 'DiffMap.DifferenceMap' includes a mapping @AccountAddress -> AccountIndex@ for accounts
--  which have been added to a block which have not been persisted yet (certified or finalized).
-- 
--  For blocks which have been persisted the 'DiffMap.DifferenceMap' is @Nothing@ as potential new 
--  accounts have been written to the lmdb account map.
data AccountsAndDiffMap (pv :: ProtocolVersion) = AccountsAndDiffMap
    { -- | The persistent accounts and what is stored on disk.
      aadAccounts :: !(Accounts pv),
      -- | An in-memory difference map used keeping track of accounts
      --  added in live blocks.
      --  This is empty for a frozen block state.
      aadDiffMap :: !DiffMap.DifferenceMap
    }

instance (IsProtocolVersion pv) => Show (AccountsAndDiffMap pv) where
    show aad = "Accounts: " <> show (aadAccounts aad) <> "DiffMap: " <> show (aadDiffMap aad)

-- | A constraint that ensures a monad @m@ supports the persistent account operations.
--  This essentially requires that the monad support 'MonadBlobStore', and 'MonadCache' for
--  the account cache and 'MonadAccountMapStore' for the persistent account map.
type SupportsPersistentAccount pv m =
    ( IsProtocolVersion pv,
      MonadBlobStore m,
      MonadCache (AccountCache (AccountVersionFor pv)) m,
      LMDBAccountMap.MonadAccountMapStore m
    )

instance (IsProtocolVersion pv) => Show (Accounts pv) where
    show a = show (accountTable a)

instance (SupportsPersistentAccount pv m) => MHashableTo m H.Hash (AccountsAndDiffMap pv) where
    getHashM AccountsAndDiffMap{..} = getHashM $ accountTable aadAccounts

instance (SupportsPersistentAccount pv m) => BlobStorable m (AccountsAndDiffMap pv) where
    storeUpdate AccountsAndDiffMap{aadAccounts = Accounts{..},..} = do
        (pTable, accountTable') <- storeUpdate accountTable
        (pRegIdHistory, regIdHistory') <- storeUpdate accountRegIdHistory
        LMDBAccountMap.insert (Map.toList $ DiffMap.flatten aadDiffMap)
        let newAccounts =
                AccountsAndDiffMap
                    { aadAccounts =
                        Accounts
                            { accountTable = accountTable',
                              accountRegIdHistory = regIdHistory'
                            },
                      aadDiffMap = DiffMap.empty $ Just aadDiffMap
                    }
        return (pTable >> pRegIdHistory, newAccounts)
    load = do
        maccountTable <- load
        mrRIH <- load
        return $ do
            accountTable <- maccountTable
            accountRegIdHistory <- mrRIH
            return $ AccountsAndDiffMap{aadAccounts = Accounts{..}, aadDiffMap = DiffMap.empty Nothing}

instance (SupportsPersistentAccount pv m, av ~ AccountVersionFor pv) => Cacheable1 m (AccountsAndDiffMap pv) (PersistentAccount av) where
    liftCache cch aad@AccountsAndDiffMap{aadAccounts = accts@Accounts{..}} = do
        acctTable <- liftCache (liftCache @_ @(HashedCachedRef (AccountCache av) (PersistentAccount av)) cch) accountTable
        return
            aad{aadAccounts = accts{accountTable = acctTable}}

emptyAccounts :: Accounts pv
emptyAccounts = Accounts L.empty Trie.empty

-- | Creates an empty 'AccountsAndDifferenceMap'.
--  The difference map will inherit the difference map of the provided provided 'AccountsAndDiffMap' if supplied.
emptyAccountsAndDiffMap :: Maybe (AccountsAndDiffMap pv) -> AccountsAndDiffMap pv
emptyAccountsAndDiffMap Nothing = AccountsAndDiffMap emptyAccounts $ DiffMap.empty Nothing
emptyAccountsAndDiffMap (Just successor) = AccountsAndDiffMap emptyAccounts $ DiffMap.empty (Just $ aadDiffMap successor)

-- | Add a new account. Returns @Just idx@ if the new account is fresh, i.e., the address does not exist,
--  or @Nothing@ in case the account already exists. In the latter case there is no change to the accounts structure.
putNewAccount :: (SupportsPersistentAccount pv m) => PersistentAccount (AccountVersionFor pv) -> AccountsAndDiffMap pv -> m (Maybe AccountIndex, AccountsAndDiffMap pv)
putNewAccount !acct a0@AccountsAndDiffMap{aadAccounts = accts0@Accounts{..}, ..} = do
    addr <- accountCanonicalAddress acct
    exists addr a0 >>= \case
        True -> return (Nothing, a0)
        False -> do
            (accIdx, newAccountTable) <- L.append acct accountTable
            let newDiffMap = DiffMap.insert addr accIdx aadDiffMap
            return (Just accIdx, a0{aadAccounts = accts0{accountTable = newAccountTable}, aadDiffMap = newDiffMap})

-- | Construct an 'Accounts' from a list of accounts. Inserted in the order of the list.
fromList :: (SupportsPersistentAccount pv m) => [PersistentAccount (AccountVersionFor pv)] -> m (AccountsAndDiffMap pv)
fromList = foldlM insert $ emptyAccountsAndDiffMap Nothing
  where
    insert accounts account = snd <$> putNewAccount account accounts

-- | Determine if an account with the given address exists.
exists :: (SupportsPersistentAccount pv m) => AccountAddress -> AccountsAndDiffMap pv -> m Bool
exists addr accts = isJust <$> getAccountIndex addr accts

-- | Retrieve an account associated with the given credential registration ID.
--  Returns @Nothing@ if no such account exists.
getAccountByCredId :: (SupportsPersistentAccount pv m) => ID.RawCredentialRegistrationID -> AccountsAndDiffMap pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountByCredId cid accs@AccountsAndDiffMap{..} =
    Trie.lookup cid (accountRegIdHistory aadAccounts) >>= \case
        Nothing -> return Nothing
        Just ai -> fmap (ai,) <$> indexedAccount ai accs

-- | Get the account at a given index (if any).
getAccountIndex :: (SupportsPersistentAccount pv m) => AccountAddress -> AccountsAndDiffMap pv -> m (Maybe AccountIndex)
getAccountIndex addr AccountsAndDiffMap{..} =
    case DiffMap.lookup addr aadDiffMap of
        Just accIdx -> return $ Just accIdx
        Nothing ->
            LMDBAccountMap.lookup addr >>= \case
                Nothing -> return Nothing
                Just accIdx -> return $ Just accIdx

-- | Retrieve an account with the given address.
--  Returns @Nothing@ if no such account exists.
getAccount :: (SupportsPersistentAccount pv m) => AccountAddress -> AccountsAndDiffMap pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
getAccount addr accts = fmap snd <$> getAccountWithIndex addr accts

-- | Retrieve an account and its index from a given address.
--  Returns @Nothing@ if no such account exists.
getAccountWithIndex :: forall pv m. (SupportsPersistentAccount pv m) => AccountAddress -> AccountsAndDiffMap pv -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
getAccountWithIndex addr accts =
    getAccountIndex addr accts >>= \case
        Nothing -> return Nothing
        Just ai -> do
            mAcc <- L.lookup ai $ accountTable (aadAccounts accts)
            return $ (ai,) <$> mAcc

-- | Retrieve the account at a given index.
indexedAccount :: (SupportsPersistentAccount pv m) => AccountIndex -> AccountsAndDiffMap pv -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
indexedAccount ai AccountsAndDiffMap{..} = L.lookup ai (accountTable aadAccounts)

-- | Check that an account registration ID is not already on the chain.
--  See the foundation (Section 4.2) for why this is necessary.
--  Return @Just ai@ if the registration ID already exists, and @ai@ is the index of the account it is or was associated with.
regIdExists :: (MonadBlobStore m) => ID.CredentialRegistrationID -> AccountsAndDiffMap pv -> m (Maybe AccountIndex)
regIdExists rid accts = Trie.lookup (ID.toRawCredRegId rid) (accountRegIdHistory $ aadAccounts accts)

-- | Record an account registration ID as used.
recordRegId :: (MonadBlobStore m) => ID.CredentialRegistrationID -> AccountIndex -> AccountsAndDiffMap pv -> m (AccountsAndDiffMap pv)
recordRegId rid idx accts0 = do
    accountRegIdHistory' <- Trie.insert (ID.toRawCredRegId rid) idx (accountRegIdHistory (aadAccounts accts0))
    return $!
        accts0
            { aadAccounts =
                Accounts
                    { accountTable = accountTable $ aadAccounts accts0,
                      accountRegIdHistory = accountRegIdHistory'
                    }
            }

recordRegIds :: (MonadBlobStore m) => [(ID.CredentialRegistrationID, AccountIndex)] -> AccountsAndDiffMap pv -> m (AccountsAndDiffMap pv)
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
    AccountsAndDiffMap pv ->
    m (Maybe (AccountIndex, a), AccountsAndDiffMap pv)
updateAccounts fupd addr a0@AccountsAndDiffMap{aadAccounts = accs0@Accounts{..}} = do
    getAccountIndex addr a0 >>= \case
        Nothing -> return (Nothing, a0)
        Just ai -> update ai
  where
    update ai =
        L.update fupd ai accountTable >>= \case
            Nothing -> return (Nothing, a0)
            Just (res, act') -> return (Just (ai, res), a0{aadAccounts = accs0{accountTable = act'}})

-- | Perform an update to an account with the given index.
--  Does nothing (returning @Nothing@) if the account does not exist.
--  This should not be used to alter the address of an account (which is
--  disallowed).
updateAccountsAtIndex :: (SupportsPersistentAccount pv m) => (PersistentAccount (AccountVersionFor pv) -> m (a, PersistentAccount (AccountVersionFor pv))) -> AccountIndex -> AccountsAndDiffMap pv -> m (Maybe a, AccountsAndDiffMap pv)
updateAccountsAtIndex fupd ai a0@AccountsAndDiffMap{aadAccounts = accs0@Accounts{..}} =
    L.update fupd ai accountTable >>= \case
        Nothing -> return (Nothing, a0)
        Just (res, act') -> return (Just res, a0{aadAccounts = accs0{accountTable = act'}})

-- | Perform an update to an account with the given index.
--  Does nothing if the account does not exist.
--  This should not be used to alter the address of an account (which is
--  disallowed).
updateAccountsAtIndex' :: (SupportsPersistentAccount pv m) => (PersistentAccount (AccountVersionFor pv) -> m (PersistentAccount (AccountVersionFor pv))) -> AccountIndex -> AccountsAndDiffMap pv -> m (AccountsAndDiffMap pv)
updateAccountsAtIndex' fupd ai = fmap snd . updateAccountsAtIndex fupd' ai
  where
    fupd' = fmap ((),) . fupd

-- | Get a list of all account addresses and their assoicated account indices.
allAccountAddressesAndIndices :: (SupportsPersistentAccount pv m) => AccountsAndDiffMap pv -> m [(AccountAddress, AccountIndex)]
allAccountAddressesAndIndices accounts = do
    allPersistedAccounts <- LMDBAccountMap.all
    let inMemAccounts = Map.toList $ DiffMap.flatten $ aadDiffMap accounts
    return $ allPersistedAccounts ++ inMemAccounts

-- | Get a list of all account addresses.
accountAddresses :: (SupportsPersistentAccount pv m) => AccountsAndDiffMap pv -> m [AccountAddress]
accountAddresses accounts = map fst <$> allAccountAddressesAndIndices accounts

-- | Serialize accounts in V0 format.
serializeAccounts :: (SupportsPersistentAccount pv m, MonadPut m) => GlobalContext -> AccountsAndDiffMap pv -> m ()
serializeAccounts cryptoParams AccountsAndDiffMap{..} = do
    liftPut $ putWord64be $ L.size (accountTable aadAccounts)
    L.mmap_ (serializeAccount cryptoParams) (accountTable aadAccounts)

-- | Fold over the account table in ascending order of account index.
foldAccounts :: (SupportsPersistentAccount pv m) => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccounts f a accts = L.mfold f a (accountTable accts)

-- | Fold over the account table in ascending order of account index.
foldAccountsDesc :: (SupportsPersistentAccount pv m) => (a -> PersistentAccount (AccountVersionFor pv) -> m a) -> a -> Accounts pv -> m a
foldAccountsDesc f a accts = L.mfoldDesc f a (accountTable accts)

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
    AccountsAndDiffMap oldpv ->
    t m (AccountsAndDiffMap pv)
migrateAccounts migration AccountsAndDiffMap{aadAccounts = Accounts{..}} = do
    newAccountTable <- L.migrateLFMBTree (migrateHashedCachedRef' (migratePersistentAccount migration)) accountTable
    -- The account registration IDs are not cached. There is a separate cache
    -- that is purely in-memory and just copied over.
    newAccountRegIds <- Trie.migrateUnbufferedTrieN return accountRegIdHistory
    return $!
        AccountsAndDiffMap
            { aadAccounts =
                Accounts
                    { accountTable = newAccountTable,
                      accountRegIdHistory = newAccountRegIds
                    },
              aadDiffMap = DiffMap.empty Nothing
            }
