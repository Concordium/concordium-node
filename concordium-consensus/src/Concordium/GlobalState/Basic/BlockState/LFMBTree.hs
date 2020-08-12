{-# LANGUAGE OverloadedStrings #-}

-- |
--    Module      : Concordium.GlobalState.LFMBTree
--    Description : Left-full Merkle Binary Tree implementation
--
--    An implementation of a Left-full Merkle Binary Tree.
module Concordium.GlobalState.Basic.BlockState.LFMBTree
  ( -- * Tree type
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
    toAscList,
    fromAscList,
    toAscPairList,

    -- * Specialized functions for @Maybe@
    lookupMaybe,
    delete,
    toAscPairListMaybes,
    fromAscListMaybes,

    -- * Helpers
    setBits,

    -- * Structure specification
    -- $specification
  )
where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo
import Data.Bits
import Data.Foldable (foldl')
import Data.Serialize
import Data.Word
import Prelude hiding (lookup)
import Control.Monad (join)
{-
-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------
-}

-- | Helper function that returns a saturated @Bits@ value until position @h@.
setBits :: (Bits a, Num a)
  => Int -- ^ Number of bits set to 1. Must be true and not more than the bit length of the expected resulting type.
  -> a
setBits h = bit (h + 1) - 1

-- | Local alias for the height of a node.
-- Leaf's height is defined as -1 and the height of a
-- node is one more than the maximum of the height of the children nodes.
type Height = Word64

-- | Local alias for the key of the tree.
type Key = Word64

{-
-------------------------------------------------------------------------------
                                Type definition
-------------------------------------------------------------------------------
-}

-- | Left-full Merkle Binary Tree
data LFMBTree v
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

instance
  ( HashableTo H.Hash v
  ) =>
  HashableTo H.Hash (T v)
  where
  getHash (Leaf v) = getHash v
  getHash (Node _ l r) = H.hashOfHashes (getHash l) (getHash r)

-- | The hash of a LFMBTree is defined as the hash of the string "EmptyLFMBTree" if it
-- is empty or the hash of the tree otherwise.
instance
  ( HashableTo H.Hash v
  ) =>
  HashableTo H.Hash (LFMBTree v)
  where
  getHash Empty = H.hash "EmptyLFMBTree"
  getHash (NonEmpty _ v) = getHash v

instance Serialize v => Serialize (LFMBTree v) where
  put Empty = putWord64be 0
  put (NonEmpty h t) = do
    putWord64be h
    put t
  get = do
    tag <- getWord64be
    case tag of
      0 -> return Empty
      h -> NonEmpty h <$> get

instance Serialize v => Serialize (T v) where
  put (Leaf v) = do
    putWord8 0
    put v
  put (Node h l r) = do
    putWord8 1
    put h
    put l
    put r
  get = do
    tag <- getWord8
    case tag of
      0 -> Leaf <$> get
      _ -> Node <$> get <*> get <*> get

{-
-------------------------------------------------------------------------------
                                  Interface
-------------------------------------------------------------------------------
-}

size :: LFMBTree v -> Word64
size Empty = 0
size (NonEmpty s _) = s

-- | Returns the empty tree
empty :: LFMBTree v
empty = Empty

-- | Returns the value at the given key if it is present in the tree
-- or Nothing otherwise.
lookup :: Key -> LFMBTree v -> Maybe v
lookup _ Empty = Nothing
lookup k (NonEmpty s t) =
  if k >= s
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
lookupMaybe :: Key -> LFMBTree (Maybe v) -> Maybe v
lookupMaybe k t = join $ lookup k t

-- | Adds a value to the tree returning the assigned key and the new tree.
append :: v -> LFMBTree v -> (Key, LFMBTree v)
append v Empty = (0, NonEmpty 1 (Leaf v))
append v (NonEmpty s t) = (s, NonEmpty (s + 1) (appendT s v t))
  where
    createLeaf originalNode value f = f originalNode (Leaf value)
    appendT key value node =
      case node of
        originalNode@(Leaf _) -> do
          -- Inserting on a leaf will create a new node in which
          -- its right child is a leaf with the new value and the
          -- left child is the original leaf
          createLeaf originalNode value (Node 0)
        originalNode@(Node height leftNode rightNode) ->
          -- For inserting on a node, if the wanted key is bigger than the
          -- saturated value at the current height, a new node has to be created
          -- holding the new value as a leaf on its right child and the original
          -- node on its left child. Otherwise, we have to insert the value into
          -- the right subtree and the key has to be cropped to allow for this
          -- same comparison to work recursively so we remove the highest bit using
          -- an @and@ operation.
          let saturatedLevel = setBits (fromIntegral height)
              modLevel = (.&. setBits (fromIntegral height - 1))
           in if key > saturatedLevel
                then createLeaf originalNode value (Node (height + 1))
                else
                  let newNode = appendT (modLevel key) value rightNode
                   in Node height leftNode newNode

-- | Update a value at a given key.
--
-- If the key is not present in the tree, then it resturns @Nothing@.
-- Otherwise, the value is loaded, modified with the given function and stored again.
--
-- @update@ will also recompute the hashes on the way up to the root.
update :: (v -> (a, v)) -> Key -> LFMBTree v -> Maybe (a, LFMBTree v)
update _ _ Empty = Nothing
update f k (NonEmpty s t) =
  if k >= s
    then Nothing
    else
      let (a, t') = updateT k f t
       in Just (a, NonEmpty s t')
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
delete :: Key -> LFMBTree (Maybe v) -> Maybe (LFMBTree (Maybe v))
delete k t = snd <$> update (const ((), Nothing)) k t

-- | Return the elements sorted by their keys. As there is no operation
-- for deleting elements, this list will contain all the elements starting
-- on the index 0 up to the size of the tree.
toAscList :: LFMBTree v -> [v]
toAscList Empty = []
toAscList (NonEmpty _ t) = toListT t
  where
    toListT (Leaf v) = [v]
    toListT (Node _ l r) = toListT l ++ toListT r

-- | Return the pairs (key, value) sorted by their keys. This list will contain
-- all the elements starting on the index 0.
toAscPairList :: LFMBTree v -> [(Word64, v)]
toAscPairList = zip [0..] . toAscList

-- | If a tree contains values of type Maybe, a `Nothing` is considered a deletion.
-- This function filters out the `Nothing` items and unwraps the `Just` items.
toAscPairListMaybes :: LFMBTree (Maybe v) -> [(Word64, v)]
toAscPairListMaybes l = [(i, v) | (i, Just v) <- toAscPairList l]

-- | Create a tree from a list of items. The items will be inserted sequentially
-- starting on the index 0.
fromAscList :: [v] -> LFMBTree v
fromAscList = foldl' (\acc e -> snd $ append e acc) empty

-- | Create a tree that holds the values wrapped in @Just@ when present and keeps @Nothing@s on the missing positions
fromAscListMaybes :: [(Word64, v)] -> LFMBTree (Maybe v)
fromAscListMaybes l = fromAscList $ go l 0
  where go z@((i,v):xs) ix
         | i == ix = Just v : go xs (i + 1)
         | otherwise = (replicate (fromIntegral $ i - ix) Nothing) ++ go z i
        go [] _ = []
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
