{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, DerivingVia, DerivingStrategies, MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures, DeriveFoldable, DeriveTraversable, FlexibleInstances, QuantifiedConstraints, UndecidableInstances, StandaloneDeriving #-}
module Concordium.GlobalState.Persistent.AccountTable where

import Prelude hiding (lookup)
import Data.Word
import Data.Bits
import Data.Serialize
import Data.Functor.Foldable hiding (Nil)
import Control.Monad
import Control.Exception
import Data.Functor

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as Transient

-- |Account indexes are 64-bit values.
-- This means that the level of a branch cannot exceed 64, which is used in the encoding.
type AccountIndex = Word64

data ATF r
    = Branch !Word8 !Bool H.Hash !r !r
    | Leaf H.Hash !Account
    deriving (Show, Functor, Foldable, Traversable)

instance HashableTo H.Hash (ATF r) where
    getHash (Branch _ _ h _ _) = h
    getHash (Leaf h _) = h

showATFString :: ATF String -> String
showATFString (Branch lvl _ _ l r) = "Branch(" ++ show lvl ++ ", " ++ l ++ ", " ++ r ++ ")"
showATFString (Leaf _ a) = show a

instance (BlobStorable m ref r) => BlobStorable m ref (ATF r) where
    storeUpdate p (Branch lvl fll hsh cl cr) = do
        (pcl, cl') <- storeUpdate p cl
        (pcr, cr') <- storeUpdate p cr
        let putBranch = do
                -- We know lvl <= 64, so store the full bit in the high bit.
                putWord8 $ if fll then setBit lvl 7 else lvl
                put hsh
                pcl
                pcr
        return (putBranch, Branch lvl fll hsh cl' cr')
    storeUpdate p (Leaf hsh acct) = do
        (pacct, acct') <- storeUpdate p acct
        let putLeaf = do
                putWord8 255
                put hsh
                pacct
        return (putLeaf, Leaf hsh acct')
    store p t = fst <$> storeUpdate p t
    load p = do
        lvlfll <- getWord8
        hsh <- get
        if lvlfll == 255 then do
            -- Leaf
            macct <- load p
            return (Leaf hsh <$> macct)
        else do
            let
                lvl = clearBit lvlfll 7
                fll = testBit lvlfll 7
            mcl <- load p
            mcr <- load p
            return (Branch lvl fll hsh <$> mcl <*> mcr)

nextLevel :: ATF r -> Word8
nextLevel (Branch lvl _ _ _ _) = lvl + 1
nextLevel (Leaf _ _) = 0

full :: ATF r -> Bool
full (Branch _ f _ _ _) = f
full (Leaf _ _) = True

mkLeaf :: Account -> ATF r
mkLeaf acct = Leaf (getHash acct) acct
{-# INLINE mkLeaf #-}

branchHash :: H.Hash -> H.Hash -> H.Hash
branchHash h1 h2 = H.hashShort $ H.hashToShortByteString h1 <> H.hashToShortByteString h2

lookupF :: (MRecursive m t, Base t ~ ATF) => AccountIndex -> t -> m (Maybe Account)
lookupF x = mproject >=> \case
    (Leaf _ acct) -> return $! if x == 0 then Just acct else Nothing
    (Branch (fromIntegral -> branchBit) _ _ l r) ->
        if testBit x branchBit then lookupF (clearBit x branchBit) r else lookupF x l

appendF :: (MRecursive m t, MCorecursive m t, Base t ~ ATF) => Account -> t -> m (AccountIndex, Word8, Bool, H.Hash, t)
appendF acct t = mproject t >>= \case
        (Leaf h _) -> do
            enewLeaf <- membed newLeaf
            let hsh = branchHash h newHash
            b <- membed (Branch 0 True hsh t enewLeaf)
            return (1, 1, True, hsh, b)
        (Branch lvl True hsh _ _) -> do
            enewLeaf <- membed newLeaf
            let newhsh = branchHash hsh newHash
            b <- membed (Branch (lvl + 1) False newhsh t enewLeaf)
            return (bit (fromIntegral lvl + 1), lvl+2, False, newhsh, b)
        (Branch lvl False _ l r) -> do
            (rx, nlvl, rfull, rhsh, r') <- appendF acct r
            lhsh <- getHash <$> mproject l
            let isFull = rfull && nlvl == lvl
            let newhsh = branchHash lhsh rhsh
            b <- membed (Branch lvl isFull newhsh l r')
            return (setBit rx (fromIntegral lvl), lvl + 1, isFull, newhsh, b)
    where
        newHash = getHash acct
        newLeaf = Leaf newHash acct
    
updateF :: forall m t a. (MRecursive m t, MCorecursive m t, Base t ~ ATF) => (Account -> m (a, Account)) -> AccountIndex -> t -> m (Maybe (a, H.Hash, t))
updateF upd x t = mproject t >>= \case
    (Leaf _ acct) -> if x == 0 then do
            (res, acct') <- upd acct
            let hsh' = getHash acct'
            t' <- membed (Leaf hsh' acct')
            return $ Just (res, hsh', t')
        else return Nothing
    (Branch lvl fll _ l r) -> if testBit x (fromIntegral lvl) then
            updateF upd (clearBit x (fromIntegral lvl)) r >>= \case
                Nothing -> return Nothing
                Just (res, rhsh, r') -> do
                    lhsh <- getHash <$> mproject l
                    let hsh' = branchHash lhsh rhsh
                    b <- membed (Branch lvl fll hsh' l r')
                    return $ Just (res, hsh', b)
        else
            updateF upd x l >>= \case
                Nothing -> return Nothing
                Just (res, lhsh, l') -> do
                    rhsh <- getHash <$> mproject r
                    let hsh' = branchHash lhsh rhsh
                    b <- membed (Branch lvl fll hsh' l' r)
                    return $ Just (res, hsh', b)

mapReduceF :: (MRecursive m t, Base t ~ ATF, Monoid a) => (AccountIndex -> Account -> m a) -> t -> m a
mapReduceF mfun = mr 0 <=< mproject
    where
        mr lowIndex (Leaf _ acct) = mfun lowIndex acct
        mr lowIndex (Branch lvl _ _ l r) = do
            la <- mr lowIndex =<< mproject l
            ra <- mr (setBit lowIndex (fromIntegral lvl)) =<< mproject r
            return (la <> ra)

data AccountTable = Empty | Tree !AccountIndex H.Hash (BufferedBlobbed BlobRef ATF)

nextAccountIndex :: AccountTable -> AccountIndex
nextAccountIndex Empty = 0
nextAccountIndex (Tree nai _ _) = nai

instance HashableTo H.Hash AccountTable where
    getHash Empty = minBound
    getHash (Tree _ h _) = h

instance Show AccountTable where
    show Empty = "Empty"
    show (Tree _ _ t) = showFix showATFString t

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef AccountTable where
    store _ Empty = return (put (0 :: AccountIndex))
    store p (Tree nai _ t) = do
        pt <- store p t
        return (put nai >> pt)
    storeUpdate _ v@Empty = return (put (0 :: AccountIndex), v)
    storeUpdate p (Tree nai h t) = do
        (pt, t') <- storeUpdate p t
        return (put nai >> pt, Tree nai h t')
    load p = do
        nai <- get
        if nai == 0 then
            return (return Empty)
        else
            load p <&> (\mt -> do
                t <- mt
                tt <- mproject (t :: (BufferedBlobbed BlobRef ATF))
                t' <- membed tt
                return (Tree nai (getHash tt) t'))

empty :: AccountTable
empty = Empty

lookup :: (MonadBlobStore m BlobRef) => AccountIndex -> AccountTable -> m (Maybe Account)
lookup _ Empty = return Nothing
lookup x (Tree _ _ t) = lookupF x t

append :: (MonadBlobStore m BlobRef) => Account -> AccountTable -> m (AccountIndex, AccountTable)
append acct Empty = (0,) . (Tree 1 (getHash leaf)) <$> membed leaf
    where
        leaf = mkLeaf acct
append acct (Tree nai _ t) = do
    (i, _, _, hsh, t') <- appendF acct t
    assert (i == nai) $ return (i, Tree (nai + 1) hsh t')

update :: (MonadBlobStore m BlobRef) => (Account -> m (a, Account)) -> AccountIndex -> AccountTable -> m (Maybe (a, AccountTable))
update _ _ Empty = return Nothing
update upd i (Tree nai _ t) = fmap (\(res, h, t') -> (res, Tree nai h t')) <$> updateF upd i t

toList :: (MonadBlobStore m BlobRef) => AccountTable -> m [(AccountIndex, Account)]
toList Empty = return []
toList (Tree _ _ t) = mapReduceF (\ai acct -> return [(ai, acct)]) t

makePersistent :: Transient.AccountTable -> AccountTable
makePersistent Transient.Empty = Empty
makePersistent (Transient.Tree t0) = tree (conv t0)
    where
        conv (Transient.Branch lvl fll hsh l r) = let (nxtr, _, cr) = conv r in (nxtr + 2^lvl, hsh, LBMemory (Branch lvl fll hsh ((\(_, _, t) -> t) $ conv l) cr))
        conv (Transient.Leaf hsh acct) = (1, hsh, LBMemory (Leaf hsh acct))
        tree (ai, hsh, t) = Tree ai hsh t