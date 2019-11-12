{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, DerivingVia, DerivingStrategies, MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures, DeriveFoldable, DeriveTraversable, FlexibleInstances, QuantifiedConstraints, UndecidableInstances, StandaloneDeriving #-}
-- |This module provides an implementation of indexes that may be persisted to
-- disk.  Keys in the index are effectively fixed-length byte strings.  The
-- index is implemented as a Trie, where each branching node has degree 256.
--
-- TODO: It is likely desirable to replace this with a B-Tree in many applications,
-- especially where the keys are randomly distributed.
module Concordium.GlobalState.Persistent.Trie where

import qualified Data.Vector as V
import Data.List(intercalate,stripPrefix)
import Data.Word
import Data.Functor.Foldable hiding (Nil)
import Data.Bifunctor
import Control.Monad
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Either
import qualified Data.Map.Strict as Map

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.Types.Acorn.Core as Core (Name(..), ModuleRef(..))
import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types (AccountAddress)

class FixedTrieKey a where
    -- |Unpack a key to a list of bytes.
    -- The length of the list must be independent of the value.
    unpackKey :: a -> [Word8]
    default unpackKey :: (Serialize a) => a -> [Word8]
    unpackKey = BS.unpack . encode
    -- |Pack a key from a list of bytes.
    packKey :: [Word8] -> a
    default packKey :: (Serialize a) => [Word8] -> a
    packKey = fromRight (error "FixedTrieKey: deserialization failed") . decode . BS.pack

instance FixedTrieKey Word64
instance FixedTrieKey Word32
deriving via Word32 instance FixedTrieKey Core.Name
instance FixedTrieKey SHA256.Hash
deriving via SHA256.Hash instance FixedTrieKey Core.ModuleRef
instance FixedTrieKey AccountAddress


-- |Trie with keys all of same fixed length treated as lists of bytes.
-- The first parameter of 'TrieF' is the type of keys, which should
-- by an instance of 'FixedTrieKey'.
-- The second parameter is the type of values.
-- The third parameter is the recursive type argument: a type for Tries
-- is obtained by applying a fixed-point combinator to @TrieF k v@.
data TrieF k v r
    = Branch !(V.Vector (Nullable r))
    -- ^Branch on the next byte of the key (256, possibly null children)
    | Stem [Word8] !r
    -- ^The next bytes of the key are given
    | Tip !v
    -- ^A value
    deriving (Show, Functor, Foldable, Traversable)

-- |Render a 'TrieF', where the children have already been rendered
-- as 'String's.
showTrieFString :: Show v => TrieF k v String -> String
showTrieFString (Branch vec) = "[ " ++ ss (0 :: Int) (V.toList vec) ++ "]"
    where
        ss _ [] = ""
        ss i (Null:r) = ss (i+1) r
        ss i (Some v:r) = show i ++ ":" ++ v ++ "; " ++ ss (i+1) r
showTrieFString (Stem l r) = intercalate ":" (show <$> l) ++ r
showTrieFString (Tip v) = show v

instance Bifunctor (TrieF k) where
    first _ (Branch vec) = Branch vec
    first _ (Stem pref r) = Stem pref r
    first f (Tip v) = Tip (f v)
    second = fmap

instance (Serialize r, Serialize (Nullable r), Serialize v) => Serialize (TrieF k v r) where
    put (Branch vec) = do
        putWord8 1
        forM_ vec put
    put (Tip v) = putWord8 2 >> put v
    put (Stem l r) = do
        let len = length l
        if len <= 251 then
            putWord8 (3 + fromIntegral len)
        else do
            putWord8 255
            putWord64be (fromIntegral len)
        forM_ l putWord8
        put r
    get = getWord8 >>= \case
        0 -> fail "Empty trie"
        1 -> Branch . V.fromList <$> replicateM 256 get
        2 -> Tip <$> get
        v -> do
            len <- if v == 255 then do
                    fromIntegral <$> getWord64be
                else
                    return (fromIntegral (v - 3))
            Stem <$> replicateM len getWord8 <*> get

instance (BlobStorable m ref r, BlobStorable m ref (Nullable r), BlobStorable m ref v) => BlobStorable m ref (TrieF k v r) where
    storeUpdate p (Branch vec) = do
        pvec <- mapM (storeUpdate p) vec
        return (putWord8 1 >> sequence_ (fst <$> pvec), Branch (snd <$> pvec))
    storeUpdate p (Tip v) = do
        (pv, v') <- storeUpdate p v
        return (putWord8 2 >> pv, Tip v')
    storeUpdate p (Stem l r) = do
        (pr, r') <- storeUpdate p r
        let putter = do
                let len = length l
                if len <= 251 then
                    putWord8 (3 + fromIntegral len)
                else do
                    putWord8 255
                    putWord64be (fromIntegral len)
                forM_ l putWord8
                pr
        return (putter, Stem l r')
    store p t = fst <$> storeUpdate p t
    load p = getWord8 >>= \case
        0 -> fail "Empty trie"
        1 -> do
            branchms <- replicateM 256 (load p)
            return $ Branch . V.fromList <$> sequence branchms
        2 -> fmap Tip <$> load p
        v -> do
            len <- if v == 255 then do
                    fromIntegral <$> getWord64be
                else
                    return (fromIntegral (v - 3))
            l <- replicateM len getWord8
            r <- load p
            return (Stem l <$> r)


-- |@Trie k v@ is defined as a simple fixed-point of @TrieF k v@.
newtype Trie k v = Trie (TrieF k v (Trie k v)) deriving (Show)
type instance Base (Trie k v) = TrieF k v
instance Recursive (Trie k v) where
    project (Trie t) = t
instance Corecursive (Trie k v) where
    embed = Trie
instance (Monad m) => MRecursive m (Trie k v) where
    mproject = pure . project
instance (Monad m) => MCorecursive m (Trie k v) where
    membed = pure . embed

instance Functor (Trie k) where
    fmap f = hoist (first f)

-- * Trie operations
--
-- These operations are defined on a type @t@ that can be unrolled/rolled in a monad
-- @m@ to type @TrieF k v t@.  (Unrolling is provided by the 'mproject' operation of
-- 'MRecursive', and rolling is provided by the 'membed' operation of 'MCorecursive'.)
-- The rolling and unrolling can correspond to simply adding or removing constructors
-- (as for 'Trie'), but also correspond to creating and traversing disk references.

-- |Retrieve the value (if any) corresponding to a given key.
lookupF :: (MRecursive m t, Base t ~ TrieF k v, FixedTrieKey k) => k -> t -> m (Maybe v)
lookupF k = lu (unpackKey k) <=< mproject
    where
        lu [] (Tip v) = pure (Just v)
        lu _ (Tip _) = pure Nothing
        lu key (Stem pref r) = case stripPrefix pref key of
            Nothing -> pure Nothing
            Just key' -> mproject r >>= lu key'
        lu [] (Branch _) = pure Nothing
        lu (w:key') (Branch vec) = case vec V.! fromIntegral w of
            Null -> return Nothing
            Some r -> mproject r >>= lu key'

-- |Traverse the trie, applying a function to each key value pair and concatenating the
-- results monoidally.  Keys are traversed from lowest to highest in their byte-wise
-- resepresation.
mapReduceF :: (MRecursive m t, Base t ~ TrieF k v, FixedTrieKey k, Monoid a) => (k -> v -> m a) -> t -> m a
mapReduceF mfun = mr [] <=< mproject
    where
        mr keyPrefix (Tip v) = mfun (packKey keyPrefix) v
        mr keyPrefix (Stem pref r) = mr (keyPrefix ++ pref) =<< mproject r
        mr keyPrefix (Branch vec) = do
            let
                handleBranch _ Null = return mempty
                handleBranch i (Some r) = mr (keyPrefix ++ [fromIntegral i]) =<< mproject r
            mconcat . V.toList <$> V.imapM handleBranch vec

-- |Compute the common prefix and distinct suffixes of two lists.
commonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
commonPrefix [] [] = ([], [], [])
commonPrefix l1@(h1:t1) l2@(h2:t2)
    | h1 == h2 = let ~(p,r1,r2) = commonPrefix t1 t2 in (h1:p,r1,r2)
    | otherwise = ([], l1, l2)
commonPrefix l1 l2 = ([], l1, l2)

-- |Representation of an alteration to make in a map at some key.
data Alteration v
    = NoChange
    -- ^Leave the value as it was
    | Remove
    -- ^Remove the entry
    | Insert v
    -- ^Insert or replace the old value with the given one

-- |A generalised update function for updating the value of the map at a given key.
-- 'alterM' takes the key to alter and an update function.  The return value is @Nothing@
-- if the result would be an empty trie.
--
-- If the key is already present in the map, the update function is called with @Just v@;
-- otherwise it is called with @Nothing@.  The return value of the update function determines
-- whether the key is added, removed, or no change occurs.  The update operation is monadic,
-- which can allow for values to be stored as references, which are written out as part of the
-- update.  The update function can also return a value that is passed back to the caller.
--
-- Note that, while @NoChange@ is theoretically redundant it provides an important optimisation
-- when the trie will be stored on disk: no writes are needed when no change is made.
--
-- The trie implementation is intended to be persistent in that the old trie will still be valid
-- and represent the same map after an update is made.  Thus, there is no explicit reclamation of
-- storage.
alterM :: forall m t k v a. (MRecursive m t, MCorecursive m t, Base t ~ TrieF k v, FixedTrieKey k) => k -> (Maybe v -> m (a, Alteration v)) -> t -> m (a, Maybe t)
alterM k upd pt0 = do
        t0 <- mproject pt0
        let
            nochange0 res = return (res, Just pt0)
            remove0 res = return (res, Nothing)
            update0 res [] t = return (res, Just t)
            update0 res key t = do
                t' <- membed (Stem key t)
                return (res, Just t')
        aM (unpackKey k) t0 nochange0 remove0 update0
    where
        aM :: [Word8] -> TrieF k v t -> (a -> m z) -> (a -> m z) -> (a -> [Word8] -> t -> m z) -> m z
        aM [] (Tip v0) nochange remove update = upd (Just v0) >>= \case
            (res, Insert v) -> membed (Tip v) >>= update res []
            (res, Remove) -> remove res
            (res, NoChange) -> nochange res
        aM _ (Tip _) _ _ _ = error "Key too long"
        aM [] (Branch _) _ _ _ = error "Key too short"
        aM (kw:key') (Branch vec) nochange remove update = case vec V.! fromIntegral kw of
            Null -> upd Nothing >>= \case
                (res, Insert v) -> do
                    tip <- membed (Tip v)
                    stem <- if null key' then return tip else membed (Stem key' tip)
                    let vec' = vec V.// [(fromIntegral kw, Some stem)]
                    branch <- membed (Branch vec')
                    update res [] branch
                (res, _) -> nochange res
            Some r -> do
                b <- mproject r
                let
                    myupdate res [] t = do
                        let vec' = vec V.// [(fromIntegral kw, Some t)]
                        membed (Branch vec') >>= update res []
                    myupdate res skey t' = do
                        t <- membed (Stem skey t')
                        let vec' = vec V.// [(fromIntegral kw, Some t)]
                        membed (Branch vec') >>= update res []
                    myremove res = do
                        let
                            vec' = vec V.// [(fromIntegral kw, Null)]
                            nobranches _ [] = remove res
                            nobranches i (Null:vv) = nobranches (i+1) vv
                            nobranches i (Some x:vv) = onebranch i x vv
                            onebranch i x [] = update res [i] x
                            onebranch i x (Null:vv) = onebranch i x vv
                            onebranch _ _ (Some _:_) = membed (Branch vec') >>= update res []
                        nobranches 0 (V.toList vec')
                aM key' b nochange myremove myupdate
        aM key (Stem pref r) nochange remove update = case commonPrefix key pref of
                (_, key', []) -> do
                    t <- mproject r
                    let myupdate res skey t' = update res (pref ++ skey) t'
                    aM key' t nochange remove myupdate
                (_, [], (_:_)) -> error "Key too short"
                (pref', (hk:tk), (hr:tr)) -> upd Nothing >>= \case
                    (res, Insert v) -> do
                        rbranch <- if null tr then return r else membed (Stem tr r)
                        ktip <- membed (Tip v)
                        kbranch <- if null tk then return ktip else membed (Stem tk ktip)
                        branches <- membed (Branch (V.replicate 256 Null V.// [(fromIntegral hr, Some rbranch), (fromIntegral hk, Some kbranch)] ))
                        update res pref' branches
                    (res, _) -> nochange res

-- |A trie constructed using the specified fixed-point combinator.
data TrieN fix k v
    = EmptyTrieN
    -- ^The empty trie
    | TrieN !Int !(fix (TrieF k v))
    -- ^A non empty trie with its size

instance (Show v, FixShowable fix) => Show (TrieN fix k v) where
    show EmptyTrieN = "EmptyTrieN"
    show (TrieN _ t) = showFix showTrieFString t

instance (MonadBlobStore m ref, BlobStorable m ref (fix (TrieF k v)), BlobStorable m ref v, MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => BlobStorable m ref (TrieN fix k v) where
    store _ EmptyTrieN = return (put (0 :: Int))
    store p (TrieN size t) = do
        pt <- store p t
        return (put size >> pt)
    storeUpdate _ v@EmptyTrieN = return (put (0 :: Int), v)
    storeUpdate p (TrieN size t) = do
        (pt, t') <- storeUpdate p t
        return (put size >> pt, TrieN size t')
    load p = do
        size <- get
        if size == 0 then
            return (return EmptyTrieN)
        else do
            mt <- load p
            return (TrieN size <$> mt)

-- |The empty trie.
empty :: TrieN fix k v
empty = EmptyTrieN

-- |A singleton trie.
singleton :: (MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => k -> v -> m (TrieN fix k v)
singleton k v = membed (Tip v) >>= membed . (Stem (unpackKey k)) >>= return . (TrieN 1)

-- |Insert/update a given key.
insert :: (MRecursive m (fix (TrieF k v)), MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) =>
    k -> v -> TrieN fix k v -> m (TrieN fix k v)
insert k v EmptyTrieN = singleton k v
insert k v (TrieN s t) = do
    let
        alt Nothing = return (True, Insert v)
        alt _ = return (False, Insert v)
    (inc, mt') <- alterM k alt t
    case mt' of
        Just t' -> return $! TrieN (if inc then s + 1 else s) t'
        Nothing -> return EmptyTrieN

-- |Remove a given key.
delete :: (MRecursive m (fix (TrieF k v)), MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) =>
    k -> TrieN fix k v -> m (TrieN fix k v)
delete _ EmptyTrieN = return EmptyTrieN
delete k (TrieN s t) = do
    let
        alt Nothing = return (False, Remove)
        alt _ = return (True, Remove)
    (dec, mt') <- alterM k alt t
    case mt' of
        Just t' -> return $! TrieN (if dec then s - 1 else s) t'
        Nothing -> return EmptyTrieN

-- |Lookup the value at a given key.
lookup :: (MRecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => k -> TrieN fix k v -> m (Maybe v)
lookup _ EmptyTrieN = return Nothing
lookup k (TrieN _ t) = lookupF k t

-- |
adjust :: (MRecursive m (fix (TrieF k v)), MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) =>
    (Maybe v -> m (a, Alteration v)) -> k -> TrieN fix k v -> m (a, TrieN fix k v)
adjust adj k EmptyTrieN = do
        (res, alter) <- adj Nothing
        case alter of
            Insert v -> (res, ) <$> singleton k v
            _ -> return (res, EmptyTrieN)
adjust adj k (TrieN s t) = do
    let
        ur Nothing (a, r@(Insert _)) = ((s+1, a), r)
        ur Nothing (a, r) = ((s, a), r)
        ur (Just _) (a, r@(Remove)) = ((s-1, a), r)
        ur (Just _) (a, r) = ((s, a), r)
        adj' i = ur i <$> adj i
    ((s', res), mt') <- alterM k adj' t
    case mt' of
        Just t' -> return $! (res, TrieN s' t')
        Nothing -> return $! (res, EmptyTrieN)

keys :: (MRecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => TrieN fix k v -> m [k]
keys EmptyTrieN = return []
keys (TrieN _ t) = mapReduceF (\k _ -> pure [k]) t

fromTrie :: forall m fix k v. (MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v) => TrieN Fix k v -> m (TrieN fix k v)
fromTrie EmptyTrieN = return EmptyTrieN
fromTrie (TrieN s t) = do
        t' <- conv t
        return (TrieN s t')
    where
        conv :: Fix (TrieF k v) -> m (fix (TrieF k v))
        conv t0 = do
            t1 <- mapM conv (project t0)
            membed t1

fromList :: (MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => [(k,v)] -> m (TrieN fix k v)
fromList l = do
        t <- foldM (\tt (k,v) -> insert k v tt) empty l
        fromTrie t

toMap :: (MRecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k, Ord k) => TrieN fix k v -> m (Map.Map k v)
toMap EmptyTrieN = return Map.empty
toMap (TrieN _ t) = mapReduceF (\k v -> return (Map.singleton k v)) t