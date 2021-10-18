{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Concordium.Cache
  (
  -- Types
  Cache,
  -- Creating caches
  empty, emptyCapped,
  -- Functions
  insert, lookup, delete, size
  )
where

import Prelude hiding (lookup)
import Data.Hashable
import qualified Data.HashMap.Strict as Map

-- |A generic cache where entries consists of
-- a key @k@  and a value @v@.
--
-- The cache supports @insert@, @lookup@, @delete@ and @size@
-- Duplicate insertion overrides the old entry with the
-- new provided entry.
--
-- Optionally the 'Cache' can be capped.
-- A capped 'Cache' will never yield more entries than
-- its defined capacity.
-- If the capacity is exceeded then all of the content of
-- the cache will get expunged before inserting the new entry.
data Cache k v = C {
  _contents :: Map.HashMap k v,
  _capacity :: !(Maybe Int)
} deriving (Eq, Show)

-- |Create an empty cache
{-# INLINE empty #-}
empty :: Cache k v
empty = _empty Nothing

-- |Create an empty capped
{-# INLINE emptyCapped #-}
emptyCapped :: Int -> Cache k v
emptyCapped c = _empty (Just c)

-- |Helper function for creating a cache
{-# INLINE _empty #-}
_empty :: Maybe Int -> Cache k v
_empty cap = C{_contents = Map.empty, _capacity=cap}
 
-- |Gets the size of the cache
{-# INLINE size #-}
size :: Cache k v -> Int
size C{..} = Map.size _contents


{-# INLINE insert #-}
insert :: (Eq k, Hashable k) => k -> v -> Cache k v -> Cache k v
insert k v c@C{..}  = 
  case _capacity of
    Just cap ->
      if size c >= cap then _insert k v (_empty $ Just cap)
      else _insert k v c
    Nothing -> _insert k v c

-- |Helper function to insert entries into the underlying Map
{-# INLINE _insert #-}
_insert :: (Eq k, Hashable k) => k -> v -> Cache k v -> Cache k v
_insert k v C{..} = C{_contents=Map.insert k v _contents, _capacity=_capacity}    

{-# INLINE lookup #-}
lookup :: (Eq k, Hashable k) => k -> Cache k v -> Maybe v
lookup k C{..} = Map.lookup k _contents

{-# INLINE delete #-}                 
{-# LANGUAGE AllowAmbiguousTypes #-}
delete :: (Eq k, Hashable k) => k -> Cache k v -> Cache k v
delete k C{..} = C{_contents = Map.delete k _contents, _capacity=_capacity}
