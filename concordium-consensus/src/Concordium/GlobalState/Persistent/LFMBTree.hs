{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
--    Module      : Concordium.GlobalState.LFMBTree
--    Description : Left-full Merkle Binary Tree implementation
--
--    An implementation of a Left-full Merkle Binary Tree.
module Concordium.GlobalState.Persistent.LFMBTree
  ( -- * Tree type
    LFMBTree,
    size,

    -- * Construction
    empty,

    -- * Constraint aliases
    CanStoreLFMBTree,

    -- * Query
    lookup,
    lookupRef,

    -- * Insertion
    append,
    appendWithRef,

    -- * Update
    update,

    -- * Conversion
    toAscList,
    toAscPairList,
    fromAscList,

    -- * Specialized functions for @Nullable@
    lookupNullable,
    delete,
    fromAscListNullable,

    -- * Structure specification
    -- $specification
  )
where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.HashableTo
import Control.Monad
import Concordium.GlobalState.Basic.BlockState.LFMBTree (setBits)
import Data.Bits
import Data.Kind
import Data.Serialize
import Data.Word
import Prelude hiding (lookup)
import Data.Coerce (coerce, Coercible)

{-
-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------
-}

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
--
--  This type is parametrized by the reference type of
--  the stored items.
data LFMBTree k (ref :: Type -> Type) v
  -- |Empty tree
  = Empty
  -- |Non-empty tree, with the number of elements in the tree (always non-zero).
  | NonEmpty
    -- Number of elements in the tree. Always non-zero.
    !Word64
    -- The tree.
    !(T ref v)

deriving instance (Show (ref v), Show (ref (T ref v))) => (Show (LFMBTree k ref v))

-- | Inner type of a non-empty tree
data T (ref :: Type -> Type) v
  = Node !Height !(ref (T ref v)) !(ref (T ref v))
  | Leaf !(ref v)

deriving instance (Show (ref v), Show (ref (T ref v))) => (Show (T ref v))

{-
-------------------------------------------------------------------------------
                                Instances
-------------------------------------------------------------------------------
-}

instance
  ( Monad m,
    MHashableTo m H.Hash (ref v), -- references to values       must be mhashable
    MHashableTo m H.Hash (ref (T ref v)) -- references to nodes must be mhashable
  ) =>
  MHashableTo m H.Hash (T ref v)
  where
  getHashM (Leaf v) = getHashM v
  getHashM (Node _ l r) = do
    hl <- getHashM l
    hr <- getHashM r
    return $ H.hashOfHashes hl hr

-- | The hash of a LFMBTree is defined as the hash of the string "EmptyLFMBTree" if it
-- is empty or the hash of the tree otherwise.
instance
  ( Monad m,
    MHashableTo m H.Hash (ref v), -- references to values       must be mhashable
    MHashableTo m H.Hash (ref (T ref v)) -- references to nodes must be mhashable
  ) =>
  MHashableTo m H.Hash (LFMBTree k ref v)
  where
  getHashM Empty = return $ H.hash "EmptyLFMBTree"
  getHashM (NonEmpty _ v) = getHashM v

-- | Constraints that ensures a monad @m@ can store an LFMBTree (that holds references
-- of type @ref@ to values of type @v@) in references of type @ref@.
--
-- NOTE: This constraint is not intended to be used outside of this module but just be
-- fulfilled by every monad that tries to use this structure. It is just here for readability.
type CanStoreLFMBTree m ref v =
  ( MonadBlobStore m, -- Will work with MonadIOs
    BlobStorable m v,
    MHashableTo m H.Hash v, -- values                              must be storable in @BlobRef@s on the monad @m@
    BlobStorable m (ref v), -- references to values        must be storable in @Blobref@s on the monad @m@
    BlobStorable m (ref (T ref v)), -- references to nodes must be storable in @BlobRef@s on the monad @m@
    Reference m ref v, -- references to values                           must be @Reference@
    Reference m ref (ref v), -- references to references                 must be @Reference@
    Reference m ref (T ref v), -- references to nodes                    must be @Reference@
    Reference m ref (ref (T ref v)) -- references to references to nodes must be @Reference@
  )

instance CanStoreLFMBTree m ref v => BlobStorable m (T ref v) where
  store (Leaf ref) = do
    pt <- store ref
    return (putWord8 0 >> pt)
  store (Node height left right) = do
    l <- store left
    r <- store right
    return $ do
      putWord8 1
      putWord64be height
      l
      r

  storeUpdate (Leaf ref) = do
    (pt, ref') <- storeUpdate ref
    return (putWord8 0 >> pt, Leaf ref')
  storeUpdate (Node height left right) = do
    (leftp, left') <- storeUpdate left
    (rightp, right') <- storeUpdate right
    return
      ( do
          putWord8 1
          putWord64be height
          leftp
          rightp,
        Node height left' right'
      )

  load = do
    t <- getWord8
    case t of
      0 -> do
        val <- load
        return (Leaf <$> val)
      _ -> do
        h <- getWord64be
        left <- load
        right <- load
        return (Node h <$> left <*> right)

instance CanStoreLFMBTree m ref v => BlobStorable m (LFMBTree k ref v) where
  store Empty = return (putWord64be 0)
  store (NonEmpty h t) = do
    t' <- store t
    return $ putWord64be h <> t'

  storeUpdate t@Empty = return (putWord64be 0, t)
  storeUpdate (NonEmpty h t) = do
    (pt, t') <- storeUpdate t
    return
      ( putWord64be h <> pt,
        NonEmpty h t'
      )

  load = do
    s <- getWord64be
    case s of
      0 -> return . return $ Empty
      _ -> do
        t <- load
        return (NonEmpty s <$> t)

-- These instances are defined concretely because it is easier than
-- giving complex higher-order constraints.
instance (BlobStorable m v, MHashableTo m H.Hash v, Cacheable m v) => Cacheable m (T BufferedRef v) where
  cache (Node h l r) = Node h <$> cache l <*> cache r
  cache (Leaf a) = Leaf <$> cache a

instance (BlobStorable m v, MHashableTo m H.Hash v, Cacheable m v) => Cacheable m (T HashedBufferedRef v) where
  cache (Node h l r) = Node h <$> cache l <*> cache r
  cache (Leaf a) = Leaf <$> cache a

instance (Applicative m, Cacheable m (T ref v)) => Cacheable m (LFMBTree k ref v) where
  cache t@Empty = pure t
  cache (NonEmpty s t) = NonEmpty s <$> cache t

{-
-------------------------------------------------------------------------------
                                  Interface
-------------------------------------------------------------------------------
-}

size :: LFMBTree k ref v -> Word64
size Empty = 0
size (NonEmpty s _) = s

-- | Returns the empty tree
empty :: LFMBTree k ref v
empty = Empty

-- | Returns the value at the given key if it is present in the tree
-- or Nothing otherwise.
lookup :: (CanStoreLFMBTree m ref v, Ord k, Bits k, Coercible k Word64) => k -> LFMBTree k ref v -> m (Maybe v)
lookup a b = do
  r <- lookupRef a b
  case r of
    Nothing -> return Nothing
    Just v -> Just <$> refLoad v

-- | Return the reference to the value at the given key if it is present in the tree
-- or Nothing otherwise.
lookupRef :: (CanStoreLFMBTree m ref v, Ord k, Bits k, Coercible k Word64) => k -> LFMBTree k ref v -> m (Maybe (ref v))
lookupRef _ Empty = return Nothing
lookupRef k (NonEmpty s t) =
  if k >= (coerce s)
    then return Nothing -- If we try to find a key that is past the size of the tree, it will not be present
    else lookupT k t
  where
    -- lookupT :: Key -> T m ref v -> m (Maybe v)
    lookupT key = \case
      Leaf ref -> return $ Just ref
      Node height left right ->
        if key `testBit` fromIntegral height -- If the bit at position @height@ is set on the requested key, move to the right, otherwise move to the left.
          then lookupT key =<< refLoad right
          else lookupT key =<< refLoad left

-- | If a tree holds values of type @Nullable v@ then lookup should return a @Just@ if the value is present and @Nothing@ if it is not present or is a Null. This function implements such behavior.
lookupNullable :: (CanStoreLFMBTree m ref (Nullable v), Ord k, Bits k, Coercible k Word64) => k -> LFMBTree k ref (Nullable v) -> m (Maybe v)
lookupNullable k t = lookup k t >>= \case
  Just (Some v) -> return $ Just v
  _ -> return Nothing

-- | Adds a value to the tree returning the assigned key and the new tree.
append :: (CanStoreLFMBTree m ref v, Coercible k Word64, Num k) => v -> LFMBTree k ref v -> m (k, LFMBTree k ref v)
append a b = do
  (x, y, _) <- appendWithRef a b
  return (x, y)

-- | Adds a value to the tree returning the assigned key, the new tree and the created reference to the value so that it can be shared.
appendWithRef :: (CanStoreLFMBTree m ref v, Coercible k Word64, Num k) => v -> LFMBTree k ref v -> m (k, LFMBTree k ref v, ref v)
appendWithRef v Empty = do
  ref <- refMake v
  return (0, NonEmpty 1 (Leaf ref), ref)
appendWithRef v (NonEmpty s t) = do
  (t', r) <- appendT s v t Nothing
  return (coerce s, NonEmpty (s + 1) t', r)
  where
    -- createLeaf :: T m ref v -> Maybe (ref (T m ref v)) -> v -> (ref (T m ref v) -> ref (T m ref v)) -> m (T m ref v)
    createLeaf originalNode refNode value f = do
      -- Given a node, we either already have a reference (in @refNode@)
      -- or we store it (this will only happen on the top level where the
      -- first layer is not a reference)
      ref <- maybe (refMake originalNode) return refNode
      -- Create the new leaf and the reference to it's node
      refVal <- refMake value
      ref' <- refMake $ Leaf refVal
      -- Combine the original ref and the new one into a node with the given
      -- partially applied function.
      return (f ref ref', refVal)
    -- appendT :: Key -> v -> T m ref v -> Maybe (ref (T m ref v)) -> m (T m ref v)
    -- NOTE: @refNode@ is the stored reference to @node@ for it to be reused if needed
    appendT key value node refNode =
      case node of
        originalNode@(Leaf _) -> do
          -- Inserting on a leaf will create a new node in which
          -- its right child is a leaf with the new value and the
          -- left child is the original leaf
          createLeaf originalNode refNode value (Node 0)
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
                then do
                  createLeaf originalNode refNode value (Node (height + 1))
                else do
                  nextNode <- refLoad rightNode
                  (newNode, r') <- appendT (modLevel key) value nextNode (Just rightNode)
                  newRef <- refMake newNode
                  return (Node height leftNode newRef, r')

-- | Update a value at a given key.
--
-- If the key is not present in the tree, the same tree is returned.
-- Otherwise, the value is loaded, modified with the given function and stored again.
--
-- @update@ will also recompute the hashes on the way up to the root.
update :: (CanStoreLFMBTree m ref v, Ord k, Bits k, Coercible k Word64) => (v -> m (a, v)) -> k -> LFMBTree k ref v -> m (Maybe (a, LFMBTree k ref v))
update _ _ Empty = return Nothing
update f k (NonEmpty s t) =
  if k >= coerce s
    then return Nothing
    else do
      (a, t') <- updateT k f t
      return $ Just (a, NonEmpty s t')
  where
    -- updateT :: Key -> (v -> v) -> T m ref v -> m (T m ref v)
    updateT key fun node =
      case node of
        Leaf r -> do
          val <- refLoad r
          (a, newVal) <- fun val
          newRef <- refMake newVal
          return (a, Leaf newRef)
        (Node height left right) ->
          if key `testBit` fromIntegral height
            then do
              (a, right') <- updateT key fun =<< refLoad right
              right'' <- refMake right'
              return (a, Node height left right'')
            else do
              (a, left') <- updateT key fun =<< refLoad left
              left'' <- refMake left'
              return (a, Node height left'' right)

-- | If a tree holds values of type @Maybe v@ then deleting is done by inserting a @Nothing@ at a given position.
-- This function will return Nothing if the key is not present and otherwise it will return the updated tree.
delete :: (CanStoreLFMBTree m ref (Nullable v), Ord k, Bits k, Coercible k Word64) => k -> LFMBTree k ref (Nullable v) -> m (Maybe (LFMBTree k ref (Nullable v)))
delete k t = do
  v <- update (const $ return ((), Null)) k t
  return $ fmap snd v

-- | Return the elements sorted by their keys. As there is no operation
-- for deleting elements, this list will contain all the elements starting
-- on the index 0 up to the size of the tree.
toAscList :: CanStoreLFMBTree m ref v => LFMBTree k ref v -> m [v]
toAscList Empty = return []
toAscList (NonEmpty _ t) = toListT t
  where
    toListT (Leaf v) = (return :: a -> [a]) <$> refLoad v
    toListT (Node _ l r) = do
      l' <- toListT =<< refLoad l
      r' <- toListT =<< refLoad r
      return (l' ++ r')

-- | Return the pairs (key, value) sorted by their keys. This list will contain
-- all the elements starting on the index 0.
toAscPairList :: (CanStoreLFMBTree m ref v, Coercible k Word64) => LFMBTree k ref v -> m [(k, v)]
toAscPairList t = zip (map coerce [0 :: Word64 ..]) <$> toAscList t

-- | Create a tree from a list of items. The items will be inserted sequentially
-- starting on the index 0.
fromAscList :: (CanStoreLFMBTree m ref v, Num k, Coercible k Word64) => [v] -> m (LFMBTree k ref v)
fromAscList = foldM (\acc e -> snd <$> append e acc) empty

-- | Create a tree that holds the values wrapped in @Some@ when present and keeps @Null@s on the missing positions
fromAscListNullable :: (CanStoreLFMBTree m ref (Nullable v), Coercible k Word64, Integral k) => [(k, v)] -> m (LFMBTree k ref (Nullable v))
fromAscListNullable l = fromAscList $ go l 0
  where go z@((i,v):xs) ix
         | i == ix = Some v : go xs (i + 1)
         | otherwise = (replicate (fromIntegral $ i - ix) Null) ++ go z i
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
