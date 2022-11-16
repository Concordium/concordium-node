{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--    Module      : Concordium.GlobalState.LFMBTree
--    Description : Left-full Merkle Binary Tree implementation
--
--    An implementation of a Left-full Merkle Binary Tree.
module Concordium.GlobalState.Basic.BlockState.LFMBTree (
    -- * Tree type
    LFMBTree,
    size,

    -- * Construction
    empty,

    -- * Query
    lookup,

    -- * Insertion
    append,

    -- * Update
    update,

    -- * Conversion
    fromFoldable,
    fromList,
    toAscList,
    toAscPairList,
    fromListChoosingFirst,
    hashFromFoldable,

    -- * Specialized functions for @Maybe@
    lookupMaybe,
    delete,
    toAscPairListMaybes,
    fromAscListMaybes,

    -- * Traversals
    ix,

    -- * Helpers
    setBits,

    -- * Structure specification
    -- $specification
)
where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo
import Control.Monad (join)
import Data.Bits
import Data.Coerce (Coercible, coerce)
import Data.Foldable (foldl', toList)
import Data.Maybe (fromJust)
import Data.Word
import Lens.Micro ((<&>))
import Lens.Micro.Internal (Index, IxValue, Ixed (..))
import Prelude hiding (lookup)

{-
-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------
-}

-- | Helper function that returns a saturated @Bits@ value until position @h@.
setBits ::
    (Bits a, Num a) =>
    -- | Number of bits set to 1. Must be true and not more than the bit length of the expected resulting type.
    Int ->
    a
setBits h = bit (h + 1) - 1

-- | Local alias for the height of a node.
-- Leaf's height is defined as -1 and the height of a
-- node is one more than the maximum of the height of the children nodes.
type Height = Word64

{-
-------------------------------------------------------------------------------
                                Type definition
-------------------------------------------------------------------------------
-}

-- | Left-full Merkle Binary Tree
data LFMBTree k v
    = Empty
    | NonEmpty !Word64 !(T v)
    deriving (Eq, Show)

-- | Inner type of a non-empty tree
data T v
    = Node !Height !(T v) !(T v)
    | Leaf !v
    deriving (Eq, Show)

{-
-------------------------------------------------------------------------------
                                Instances
-------------------------------------------------------------------------------
-}

instance HashableTo H.Hash v => HashableTo H.Hash (T v) where
    getHash (Leaf v) = getHash v
    getHash (Node _ l r) = H.hashOfHashes (getHash l) (getHash r)

-- | The hash of a LFMBTree is defined as the hash of the string "EmptyLFMBTree" if it
-- is empty or the hash of the tree otherwise.
instance HashableTo H.Hash v => HashableTo H.Hash (LFMBTree k v) where
    getHash Empty = H.hash "EmptyLFMBTree"
    getHash (NonEmpty _ v) = getHash v

type instance Index (LFMBTree k v) = k
type instance IxValue (LFMBTree k v) = v
instance (Bits k, Ord k, Coercible k Word64) => Ixed (LFMBTree k v) where
    ix k f m = case lookup k m of
        Just v -> f v <&> (\v' -> snd . fromJust $ update (const ((), v')) k m)
        Nothing -> pure m

instance Foldable T where
    foldMap f (Leaf v) = f v
    foldMap f (Node _ l r) = foldMap f l `mappend` foldMap f r

instance Foldable (LFMBTree k) where
    foldMap _ Empty = mempty
    foldMap f (NonEmpty _ t) = foldMap f t

{-
-------------------------------------------------------------------------------
                                  Interface
-------------------------------------------------------------------------------
-}

size :: (Num k, Coercible k Word64) => LFMBTree k v -> k
size Empty = 0
size (NonEmpty s _) = coerce s

-- | Returns the empty tree
empty :: LFMBTree k v
empty = Empty

-- | Returns the value at the given key if it is present in the tree
-- or Nothing otherwise.
lookup :: (Bits k, Ord k, Coercible k Word64) => k -> LFMBTree k v -> Maybe v
lookup _ Empty = Nothing
lookup k (NonEmpty s t) =
    if k >= (coerce s)
        then Nothing -- If we try to find a key that is past the size of the tree, it will not be present
        else lookupT k t
  where
    lookupT key = \case
        Leaf v -> Just v
        Node height left right ->
            if key `testBit` fromIntegral height -- If the bit at position @height@ is set on the requested key, move to the right, otherwise move to the left.
                then lookupT key right
                else lookupT key left

-- | If a tree holds values of type @Maybe v@ then lookup should return a @Just@ if the value is present and @Nothing@ if it is not present or is a Nothing. This function implements such behavior.
lookupMaybe :: (Bits k, Ord k, Coercible k Word64) => k -> LFMBTree k (Maybe v) -> Maybe v
lookupMaybe k t = join $ lookup k t

-- | Adds a value to the tree returning the assigned key and the new tree.
append :: (Num k, Coercible k Word64) => v -> LFMBTree k v -> (k, LFMBTree k v)
append v Empty = (0, NonEmpty 1 (Leaf v))
append v (NonEmpty s t) = (coerce s, NonEmpty (s + 1) (appendT s v t))
  where
    createLeaf originalNode value f = f originalNode (Leaf value)
    appendT key value node =
        case node of
            originalNode@(Leaf _) -> createLeaf originalNode value (Node 0) -- see Note [Inserting on a leaf]
            originalNode@(Node height leftNode rightNode) ->
                -- see Note [Moving on the tree]
                let saturatedLevel = setBits (fromIntegral height)
                    modLevel = (.&. setBits (fromIntegral height - 1))
                in  if key > saturatedLevel
                        then createLeaf originalNode value (Node (height + 1))
                        else
                            let newNode = appendT (modLevel key) value rightNode
                            in  Node height leftNode newNode

{- Note [Inserting on a leaf]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Inserting on a leaf will create a new node in which
its right child is a leaf with the new value and the
left child is the original leaf
-}

{- Note [Moving on the tree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For inserting on a node, if the wanted key is bigger than the
saturated value at the current height, a new node has to be created
holding the new value as a leaf on its right child and the original
node on its left child. Otherwise, we have to insert the value into
the right subtree and the key has to be cropped to allow for this
same comparison to work recursively so we remove the highest bit using
an @and@ operation.
-}

-- | Update a value at a given key.
--
-- If the key is not present in the tree, then it resturns @Nothing@.
-- Otherwise, the value is loaded, modified with the given function and stored again.
--
-- @update@ will also recompute the hashes on the way up to the root.
update :: (Ord k, Bits k, Coercible k Word64) => (v -> (a, v)) -> k -> LFMBTree k v -> Maybe (a, LFMBTree k v)
update _ _ Empty = Nothing
update f k (NonEmpty s t) =
    if k >= (coerce s)
        then Nothing
        else
            let (a, t') = updateT k f t
            in  Just (a, NonEmpty s t')
  where
    updateT key fun node =
        case node of
            Leaf r ->
                let (a, newVal) = fun r in (a, Leaf newVal)
            (Node height left right) ->
                if key `testBit` fromIntegral height
                    then let (a, right') = updateT key fun right in (a, Node height left right')
                    else let (a, left') = updateT key fun left in (a, Node height left' right)

-- | If a tree holds values of type @Maybe v@ then deleting is done by inserting a @Nothing@ at a given position.
-- This function will return Nothing if the key is not present and otherwise it will return the updated tree.
delete :: (Ord k, Bits k, Coercible k Word64) => k -> LFMBTree k (Maybe v) -> Maybe (LFMBTree k (Maybe v))
delete k t = snd <$> update (const ((), Nothing)) k t

-- | Return the elements sorted by their keys. As there is no operation
-- for deleting elements, this list will contain all the elements starting
-- on the index 0 up to the size of the tree.
toAscList :: LFMBTree k v -> [v]
toAscList Empty = []
toAscList (NonEmpty _ t) = toListT t
  where
    toListT (Leaf v) = [v]
    toListT (Node _ l r) = toListT l ++ toListT r

-- | Return the pairs (key, value) sorted by their keys. This list will contain
-- all the elements starting on the index 0.
toAscPairList :: Coercible k Word64 => LFMBTree k v -> [(k, v)]
toAscPairList = zip (map coerce [0 :: Word64 ..]) . toList

-- | If a tree contains values of type Maybe, a `Nothing` is considered a deletion.
-- This function filters out the `Nothing` items and unwraps the `Just` items.
toAscPairListMaybes :: Coercible k Word64 => LFMBTree k (Maybe v) -> [(k, v)]
toAscPairListMaybes l = [(i, v) | (i, Just v) <- toAscPairList l]

-- | Create a tree from a 'Foldable'. The items will be inserted sequentially
-- starting on the index 0.
fromFoldable :: (Num k, Coercible k Word64, Foldable f) => f v -> LFMBTree k v
fromFoldable = foldl' (\acc e -> snd $ append e acc) empty

-- | Create a tree from a list of items. The items will be inserted sequentially
-- starting on the index 0.
fromList :: (Num k, Coercible k Word64) => [v] -> LFMBTree k v
fromList = fromFoldable
{-# INLINE fromList #-}

-- | Create a tree from a list of items choosing the first item seen when getting duplicates. The given function extracts the equatable field of the value that will be compared when checking for duplicates.
fromListChoosingFirst :: (Num k, Eq a, Coercible k Word64) => (v -> a) -> [v] -> LFMBTree k v
fromListChoosingFirst f = fromListHelper [] empty
  where
    fromListHelper _ t [] = t
    fromListHelper seen t (x : xs) =
        let key = f x
        in  if key `elem` seen
                then fromListHelper seen t xs
                else fromListHelper (seen ++ [key]) (snd $ append x t) xs

-- | Create a tree that holds the values wrapped in @Just@ when present and keeps @Nothing@s on the missing positions
fromAscListMaybes :: (Integral k, Coercible k Word64) => [(k, v)] -> LFMBTree k (Maybe v)
fromAscListMaybes l = fromList $ go l 0
  where
    go z@((i, v) : xs) idx
        | i == idx = Just v : go xs (i + 1)
        | otherwise = (replicate (fromIntegral $ i - idx) Nothing) ++ go z i
    go [] _ = []

-- | Get the hash of an LFMBTree constructed from a 'Foldable' by inserting each item sequentially
-- from index 0.
-- prop> hashFromFoldable l == getHash (fromFoldable @Word64 l)
--
-- TODO: Optimise this implementation.
hashFromFoldable :: (Foldable f, HashableTo H.Hash v) => f v -> H.Hash
hashFromFoldable = getHash . fromFoldable @Word64

{-
-------------------------------------------------------------------------------
                                Specification
-------------------------------------------------------------------------------
-}

-- $specification
--
--  The tree works like an array that carries hashes and can therefore be represented by
--  a root hash. Values are only added/updated but never deleted, therefore all the values
--  are stored using sequential indices.
--
--  To calculate the position that is being accessed, from a node that has height @h@, if
--  we move to the left child node, the @h@-ith bit of the index is 0 and if we move to the
--  right child node, the @h@-ith bit of the index is 1.
--
--  Insertions maintain this invariant and updates must recompute the hashes of the intermediate
--  nodes all the way to the root hash.
--
--  The basic behavior of this tree is better illustrated using the following insertion diagrams in
--  the node that is spawned in each step is marked with equality signs instead of dashes. A tree with
--  only one element has this shape:
--
-- @
--  +-+
--  |A|
--  +-+
--  i=0
-- @
--
--  When inserting a second element, the tree spawns a parent node that now holds both leafs:
--
-- @
--      +===+
--    +-+h=0+-+
--    | +===+ |
--  +-+       +-+
--  |A|       |B|
--  +-+       +-+
--  i=0       i=1
-- @
--
--  On the third element, the tree spawns another parent node that now holds the previous subtree on
--  its left child node and the new value on its right child node.
--
-- @
--                  +===+
--          +-------+h=1+------+
--          |       +===+      |
--      +---+                  +-+
--    +-+h=0+-+                |C|
--    | +---+ |                +-+
--  +-+       +-+              i=10
--  |A|       |B|
--  +-+       +-+
--  i=0       i=1
-- @
--
--  The fourth element can be placed next to the third one and we only spawn a node on the right subtree
--  that now holds the third and fourth elements:
--
-- @
--                  +---+
--          +-------+h=1+------+
--          |       +---+      |
--      +---+                  +===+
--    +-+h=0+-+              +-+h=0+-+
--    | +---+ |              | +===+ |
--  +-+       +-+          +-+       +-+
--  |A|       |B|          |C|       |D|
--  +-+       +-+          +-+       +-+
--  i=0       i=1          i=10      i=11
-- @
--
--  When inserting the fifth element, the tree spawns a new root node that holds the original tree on its
--  left child node and the new value on its right child node:
--
-- @
--                             +===+
--                     +-------+h=2+-------+
--                     |       +===+       |
--                 +---+                   +-+
--          +------+h=1+------+            |E|
--          |      +---+      |            +-+
--      +---+                 +---+        i=100
--    +-+h=0+-+             +-+h=0+-+
--    | +---+ |             | +---+ |
--  +-+       +-+         +-+       +-+
--  |A|       |B|         |C|       |D|
--  +-+       +-+         +-+       +-+
--  i=0       i=1         i=10      i=11
-- @
--
-- On insertions and on updates, the hashes of the nodes that have changed must be recomputed up to the
-- root hash.
