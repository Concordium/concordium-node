{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
module Concordium.GlobalState.AccountMap (
  AccountMap(..),
  PersistentAccountMap,
  PureAccountMap,

  -- * Construction
  toPersistent,
  empty,

  -- * Queries
  lookup,
  addressWouldClash,

  -- * Modifications
  insert,
  maybeInsert,

  -- * Traversals
  toList,
  toMap,
  addresses,
  isAddressAssigned,

  -- * Specializations for the pure in-memory implementation.
  lookupPure,
  addressWouldClashPure,
  insertPure,
  maybeInsertPure,
  toListPure,
  toMapPure,
  addressesPure,
  isAddressAssignedPure,

  -- * Migration
  migratePersistentAccountMap
  )

where

import Prelude hiding (lookup)
import Data.Fix
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Functor.Foldable (Base)
import Data.Bifunctor
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Trans

import Concordium.Types
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.MonadicRecursive

-- |An account map, mapping account addresses to account indices. Account
-- addresses are used externally to refer to accounts. Depending on the protocol
-- version, multiple account addresses may refer to the same account.
--
-- Currently, in protocol versions 1 and 2 each account has exactly one address.
-- In protocol version 3 an account may have up to 2^24 addresses.
newtype AccountMap (pv :: ProtocolVersion) fix = AccountMap {
  unAccountMap :: Trie.TrieN fix AccountAddress AccountIndex
  }

-- |The account map to be used in the persistent block state.
type PersistentAccountMap pv = AccountMap pv BufferedFix

migratePersistentAccountMap
    :: (BlobStorable m AccountIndex, BlobStorable (t m) AccountIndex, MonadTrans t)
    => PersistentAccountMap oldpv
    -> t m (PersistentAccountMap pv)
migratePersistentAccountMap (AccountMap am) = AccountMap <$> Trie.migrateTrieN True return am

-- |The account map that is purely in memory and used in the basic block state.
type PureAccountMap pv = AccountMap pv Fix

-- Necessary state storage instances for the persistent map. The pure one is not
-- stored so does not need the related instances.
instance MonadBlobStore m => Cacheable m (PersistentAccountMap pv) where
  cache (AccountMap am) = AccountMap <$> cache am
  {-# INLINE cache #-}

instance MonadBlobStore m => BlobStorable m (PersistentAccountMap pv) where
  store (AccountMap am) = store am
  {-# INLINE store #-}

  storeUpdate (AccountMap am) = second AccountMap <$> storeUpdate am
  {-# INLINE storeUpdate #-}

  load = fmap AccountMap <$> load
  {-# INLINE load #-}

-- |Convert a pure account map to the persistent one.
toPersistent :: MonadBlobStore m => PureAccountMap pv -> m (PersistentAccountMap pv)
toPersistent = fmap AccountMap . Trie.fromTrie . unAccountMap

-- Aliases for reducing constraint repetition in methods below.
type TrieGetContext fix m = (MRecursive m (fix (Trie.TrieF AccountAddress AccountIndex)), Base (fix (Trie.TrieF AccountAddress AccountIndex)) ~ Trie.TrieF AccountAddress AccountIndex)
type TrieModifyContext fix m = (TrieGetContext fix m, MCorecursive m (fix (Trie.TrieF AccountAddress AccountIndex)))


-- |Account map with no entries.
empty :: AccountMap pv fix
empty = AccountMap Trie.empty

mkPrefix :: AccountAddress -> [Word8]
mkPrefix = take accountAddressPrefixSize . Trie.unpackKey

-- |Retrieve the account index for the given address if the address exists.
-- The semantics of this method depends on the protocol version.
lookup :: forall pv fix m . (IsProtocolVersion pv, TrieGetContext fix m) => AccountAddress -> AccountMap pv fix -> m (Maybe AccountIndex)
lookup addr (AccountMap am) = case protocolVersion @pv of
  SP1 -> Trie.lookup addr am
  SP2 -> Trie.lookup addr am
  -- In protocol version 3, to support account aliases, we look up the account
  -- via the first 29 bytes of the address only. No new accounts can be created
  -- which would clash with an existing account on the first 29 bytes of the
  -- address. However since in principle there might be 2 or more account
  -- addresses with the same prefix created in protocol versions 1 and 2 we have
  -- a fallback. If such a situation does occur then those accounts can only be
  -- referred to by the exact address. This is what the logic below implements.
  _ -> Trie.lookupPrefix (mkPrefix addr) am >>= \case
    [] -> return Nothing
    [(_, v)] -> return (Just v)
    fs -> case filter ((== addr) . fst) fs of
      [(_, v)] -> return (Just v)
      _ -> return Nothing

-- |Check whether the given address would clash with any of the existing ones in the map.
-- The meaning of "clash" depends on the protocol version.
addressWouldClash :: forall pv fix m . (IsProtocolVersion pv, TrieGetContext fix m) => AccountAddress -> AccountMap pv fix -> m Bool
addressWouldClash addr (AccountMap am) = case protocolVersion @pv of
  SP1 -> isJust <$> Trie.lookup addr am
  SP2 -> isJust <$> Trie.lookup addr am
  _ -> not . null <$> Trie.lookupPrefix (mkPrefix addr) am

-- |Insert a new key value pair if it is fresh. If the key already exists in the
-- map no changes are made and the existing 'AccountIndex' is returned.
maybeInsert :: forall pv fix m . TrieModifyContext fix m => AccountAddress -> AccountIndex -> AccountMap pv fix -> m (Maybe AccountIndex, AccountMap pv fix)
maybeInsert addr ai (AccountMap am) = second AccountMap <$> Trie.adjust upd addr am
  where upd Nothing = pure (Nothing, Trie.Insert ai)
        upd (Just eai) = pure (Just eai, Trie.NoChange)

-- |Insert a new value for the given key. If a key is already in the map the value is replaced with the new one.
insert :: forall pv fix m . TrieModifyContext fix m => AccountAddress -> AccountIndex -> AccountMap pv fix -> m (AccountMap pv fix)
insert addr ai (AccountMap am) = AccountMap <$> Trie.insert addr ai am

-- |Get a list of all key value pairs in the account map. The order
-- of keys is unspecified.
toList :: TrieGetContext fix m => AccountMap pv fix -> m [(AccountAddress, AccountIndex)]
toList = Trie.toList . unAccountMap

-- |Transform the account map into an ordinary Data.Map. This is mainly used for testing.
toMap :: TrieGetContext fix m => AccountMap pv fix -> m (Map.Map AccountAddress AccountIndex)
toMap = Trie.toMap . unAccountMap

-- |Get a list of all addresses stored in the account map.
addresses :: TrieGetContext fix m => AccountMap pv fix -> m [AccountAddress]
addresses = Trie.keys . unAccountMap

-- |Check whether an there is an account with the specified address (which may
-- be an alias). Note that this is slightly different than 'addressWouldClash'
-- and it is in principle possible that 'isAddressAssigned' returns 'False', but
-- 'addressWouldClash' returns 'True'. This is the case for protocol version 3
-- where the first 29 bytes of the address match an existing address, there are
-- at least two addresses already with those same 29 bytes as prefix, but the
-- actual address does not match any of the existing ones.
isAddressAssigned :: forall pv fix m . (IsProtocolVersion pv, TrieGetContext fix m) => AccountAddress -> AccountMap pv fix -> m Bool
isAddressAssigned addr am = isJust <$> lookup addr am


-- * Convenient specializations of the methods for the pure in-memory account map implementation.

addressWouldClashPure :: forall pv . IsProtocolVersion pv => AccountAddress -> PureAccountMap pv -> Bool
addressWouldClashPure addr = runIdentity . addressWouldClash addr

lookupPure :: IsProtocolVersion pv => AccountAddress -> PureAccountMap pv -> Maybe AccountIndex
lookupPure addr = runIdentity . lookup addr

insertPure :: AccountAddress -> AccountIndex -> PureAccountMap pv -> PureAccountMap pv
insertPure addr ai = runIdentity . insert addr ai

maybeInsertPure :: AccountAddress -> AccountIndex -> PureAccountMap pv -> (Maybe AccountIndex, PureAccountMap pv)
maybeInsertPure addr ai = runIdentity . maybeInsert addr ai

toListPure :: PureAccountMap pv -> [(AccountAddress, AccountIndex)]
toListPure = runIdentity . toList

toMapPure :: PureAccountMap pv -> Map.Map AccountAddress AccountIndex
toMapPure = runIdentity . toMap

addressesPure :: PureAccountMap pv -> [AccountAddress]
addressesPure = runIdentity . addresses

isAddressAssignedPure :: forall pv . IsProtocolVersion pv => AccountAddress -> PureAccountMap pv -> Bool
isAddressAssignedPure addr = runIdentity . isAddressAssigned addr
