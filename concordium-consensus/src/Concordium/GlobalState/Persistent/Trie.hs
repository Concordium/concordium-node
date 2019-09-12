{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, DerivingVia, DerivingStrategies, MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures #-}
module Concordium.GlobalState.Persistent.Trie where

import qualified Data.Vector as V
import Data.List
import Data.Word
import Data.Functor.Foldable hiding (Nil)
import Data.Bifunctor
import Control.Monad
import Data.Serialize
import qualified Data.ByteString as BS

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore

class FixedTrieKey a where
    -- |Unpack a key to a list of bytes.
    -- The length of the list must be independent of the value.
    unpackKey :: a -> [Word8]
    default unpackKey :: (Serialize a) => a -> [Word8]
    unpackKey = BS.unpack . encode

instance FixedTrieKey Word64

-- |Trie with keys all of same fixed length treated as lists of bytes.
data TrieF k v r
    = Branch (V.Vector (Nullable r))
    | Stem [Word8] r
    | Tip !v
    deriving (Show)

instance Functor (TrieF k v) where
    fmap f (Branch vec) = Branch (fmap (fmap f) vec)
    fmap f (Stem k r) = Stem k (f r)
    fmap _ (Tip v) = Tip v

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

newtype Trie k v = Trie (TrieF k v (Trie k v)) deriving (Show)
type instance Base (Trie k v) = TrieF k v
instance Recursive (Trie k v) where
    project (Trie t) = t
instance Corecursive (Trie k v) where
    embed = Trie

instance Functor (Trie k) where
    fmap f = hoist (first f)


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
        
commonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
commonPrefix [] [] = ([], [], [])
commonPrefix l1@(h1:t1) l2@(h2:t2)
    | h1 == h2 = let ~(p,r1,r2) = commonPrefix t1 t2 in (h1:p,r1,r2)
    | otherwise = ([], l1, l2)
commonPrefix l1 l2 = ([], l1, l2)

data Alteration v = NoChange | Remove | Insert v

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


data TrieN fix k v
    = EmptyTrieN
    | TrieN !Int !(fix (TrieF k v))

empty :: TrieN fix k v
empty = EmptyTrieN

singleton :: (MCorecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => k -> v -> m (TrieN fix k v)
singleton k v = membed (Tip v) >>= membed . (Stem (unpackKey k)) >>= return . (TrieN 1)

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

lookup :: (MRecursive m (fix (TrieF k v)), Base (fix (TrieF k v)) ~ TrieF k v, FixedTrieKey k) => k -> TrieN fix k v -> m (Maybe v)
lookup _ EmptyTrieN = return Nothing
lookup k (TrieN _ t) = lookupF k t


-- data TrieN fix k v = TrieN {length :: Int, trie :: fix (TrieF k v)}

{-
lookupAlg :: Applicative m => TrieF k v ([Word8] -> m (Maybe v)) -> [Word8] -> m (Maybe v)
-- lookupAlg Nil _ = pure Nothing
lookupAlg (Tip v) [] = pure $ Just v
lookupAlg (Tip _) _ = pure Nothing
lookupAlg (Stem pref r) key = case stripPrefix pref key of
        Nothing -> pure Nothing
        Just key' -> r key'
lookupAlg (Branch _) [] = pure Nothing
lookupAlg (Branch vec) (w:r) = (vec V.! fromIntegral w) r

empty :: (Corecursive t, Base t ~ TrieF k v) => t
empty = embed Nil
-}

{-
insert :: (FixedTrieKey k, Base trie ~ TrieF k v, Recursive trie, Corecursive trie) => k -> v -> t -> t
insert k v = ins (unpackKey k) . project
    where
        ins k 
insertAlg v [] Nil = Tip v
insertAlg v k Nil = Stem k (Right [])
-}