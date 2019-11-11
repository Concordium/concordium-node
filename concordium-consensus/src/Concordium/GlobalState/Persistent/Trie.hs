{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, DerivingVia, DerivingStrategies, MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures, DeriveFoldable, DeriveTraversable, FlexibleInstances, QuantifiedConstraints, UndecidableInstances, StandaloneDeriving #-}
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
data TrieF k v r
    = Branch !(V.Vector (Nullable r))
    | Stem [Word8] !r
    | Tip !v
    deriving (Show, Functor, Foldable, Traversable)

showTrieFString :: Show v => TrieF k v String -> String
showTrieFString (Branch vec) = "[ " ++ ss (0 :: Int) (V.toList vec) ++ "]"
    where
        ss _ [] = ""
        ss i (Null:r) = ss (i+1) r
        ss i (Some v:r) = show i ++ ":" ++ v ++ "; " ++ ss (i+1) r
showTrieFString (Stem l r) = intercalate ":" (show <$> l) ++ r
showTrieFString (Tip v) = show v


{-
instance Functor (TrieF k v) where
    fmap f (Branch vec) = Branch (fmap (fmap f) vec)
    fmap f (Stem k r) = Stem k (f r)
    fmap _ (Tip v) = Tip v
-}
instance Bifunctor (TrieF k) where
    first _ (Branch vec) = Branch vec
    first _ (Stem pref r) = Stem pref r
    first f (Tip v) = Tip (f v)
    second = fmap

{-
instance Foldable (TrieF k v) where
    foldr _ b (Tip _) = b
    foldr f b (Stem _ z) = f z b
    foldr f b (Branch vec) = foldr f b [z | Some z <- V.toList vec]
-}


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