{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, TypeFamilies #-}
module Concordium.GlobalState.AccountTable where

import Prelude hiding (lookup)
import Data.Word
import Data.Bits
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo

type AccountIndex = Word64

data AccountTable = Empty | Tree !AT

instance HashableTo H.Hash AccountTable where
    getHash Empty = minBound
    getHash (Tree t) = getHash t

data AT
    = Branch !Word8 !Bool H.Hash !AT !AT
    | Leaf H.Hash Account

instance HashableTo H.Hash AT where
    getHash (Branch _ _ h _ _) = h
    getHash (Leaf h _) = h

nextLevel :: AT -> Word8
nextLevel (Branch lvl _ _ _ _) = lvl + 1
nextLevel (Leaf _ _) = 0

full :: AT -> Bool
full (Branch _ f _ _ _) = f
full (Leaf _ _) = True

mkLeaf :: Account -> AT
mkLeaf acct = Leaf (getHash acct) acct
{-# INLINE mkLeaf #-}

mkBranch :: Word8 -> Bool -> AT -> AT -> AT
mkBranch lvl f l r = Branch lvl f (H.hash $ H.hashToByteString (getHash l) <> H.hashToByteString (getHash r)) l r
{-# INLINE mkBranch #-}

empty :: AccountTable
empty = Empty

lookup' :: AccountIndex -> AT -> Maybe Account
lookup' 0 (Leaf _ acct) = Just acct
lookup' _ (Leaf _ _) = Nothing
lookup' x (Branch (fromIntegral -> branchBit) _ _ l r)
    | testBit x branchBit = lookup' (clearBit x branchBit) r
    | otherwise = lookup' x l

lookup :: AccountIndex -> AccountTable -> Maybe Account
lookup _ Empty = Nothing
lookup x (Tree t) = lookup' x t

append :: Account -> AccountTable -> (AccountIndex, AccountTable)
append acct Empty = (0, Tree (Leaf (getHash acct) acct))
append acct (Tree t) = (append' t) & _2 %~ Tree
    where
        append' :: AT -> (AccountIndex, AT)
        append' l@(Leaf h _) = (1, Branch 0 True (H.hash $ H.hashToByteString h <> H.hashToByteString newHash) l newLeaf)
        append' b@(Branch lvl True _ _ _) = (bit (fromIntegral lvl + 1), mkBranch (lvl + 1) False b newLeaf)
        append' (Branch lvl False _ l r) = let (i', r') = append' r in
                                                (setBit i' (fromIntegral lvl),
                                                mkBranch lvl (full r' && nextLevel r' == lvl) l r')
        newLeaf = Leaf newHash acct
        newHash = getHash acct

type instance Index AT = AccountIndex
type instance IxValue AT = Account

instance Ixed AT where
    ix i upd l@(Leaf _ acct)
        | i == 0    = mkLeaf <$> upd acct
        | otherwise = pure l
    ix i upd (Branch lvl f _ l r)
        | testBit i (fromIntegral lvl) = mkBranch lvl f l <$> ix (clearBit i (fromIntegral lvl)) upd r
        | otherwise = (\l' -> mkBranch lvl f l' r) <$> ix i upd l

type instance Index AccountTable = AccountIndex
type instance IxValue AccountTable = Account

instance Ixed AccountTable where
    ix _ _ Empty = pure Empty
    ix i upd a@(Tree t)
        | i < bit (fromIntegral (nextLevel t)) = Tree <$> ix i upd t
        | otherwise = pure a

toList :: AccountTable -> [(AccountIndex, Account)]
toList Empty = []
toList (Tree t) = toL 0 t
    where
        toL o (Leaf _ a) = [(o, a)]
        toL o (Branch lvl _ _ t1 t2) = toL o t1 ++ toL (setBit o (fromIntegral lvl)) t2