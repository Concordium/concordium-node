{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Concordium.GlobalState.Basic.BlockState.AccountTable where

import Prelude hiding (lookup)
import Data.Word
import Data.Bits
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.Types.HashableTo
import Concordium.Types

data AccountTable (pv :: ProtocolVersion) = Empty | Tree !(AT pv)

instance HashableTo H.Hash (AccountTable pv) where
    getHash Empty = H.hash "EmptyLFMBTree" -- this is the definition in the persistent implementation, I think it is acceptable to define it this way in the basic one
    getHash (Tree t) = getHash t

data AT (pv :: ProtocolVersion)
    = Branch !Word8 !Bool !H.Hash !(AT pv) !(AT pv)
    | Leaf !H.Hash (Account pv)

instance HashableTo H.Hash (AT pv) where
    getHash (Branch _ _ h _ _) = h
    getHash (Leaf h _) = h

nextLevel :: AT pv -> Word8
nextLevel (Branch lvl _ _ _ _) = lvl + 1
nextLevel (Leaf _ _) = 0

full :: AT pv -> Bool
full (Branch _ f _ _ _) = f
full (Leaf _ _) = True

mkLeaf :: IsProtocolVersion pv => Account pv -> AT pv
mkLeaf acct = Leaf (getHash acct) acct
{-# INLINE mkLeaf #-}

mkBranch :: Word8 -> Bool -> AT pv -> AT pv -> AT pv
mkBranch lvl f l r = Branch lvl f (H.hashShort $ H.hashToShortByteString (getHash l) <> H.hashToShortByteString (getHash r)) l r
{-# INLINE mkBranch #-}

empty :: AccountTable pv
empty = Empty

lookup' :: AccountIndex -> AT pv -> Maybe (Account pv)
lookup' 0 (Leaf _ acct) = Just acct
lookup' _ (Leaf _ _) = Nothing
lookup' x (Branch (fromIntegral -> branchBit) _ _ l r)
    | testBit x branchBit = lookup' (clearBit x branchBit) r
    | otherwise = lookup' x l

lookup :: AccountIndex -> AccountTable pv -> Maybe (Account pv)
lookup _ Empty = Nothing
lookup x (Tree t) = lookup' x t

append :: forall pv. IsProtocolVersion pv => Account pv -> AccountTable pv -> (AccountIndex, AccountTable pv)
append acct Empty = (0, Tree (Leaf (getHash acct) acct))
append acct (Tree t) = (append' t) & _2 %~ Tree
    where
        append' :: AT pv -> (AccountIndex, AT pv)
        append' l@(Leaf h _) = (1, Branch 0 True (H.hashShort $ H.hashToShortByteString h <> H.hashToShortByteString newHash) l newLeaf)
        append' b@(Branch lvl True _ _ _) = (bit (fromIntegral lvl + 1), mkBranch (lvl + 1) False b newLeaf)
        append' (Branch lvl False _ l r) = let (i', r') = append' r in
                                                (setBit i' (fromIntegral lvl),
                                                mkBranch lvl (full r' && nextLevel r' == lvl) l r')
        newLeaf = Leaf newHash acct
        newHash = getHash acct

-- |Get the size of an 'AccountTable'.
size :: AccountTable pv -> Word64
size Empty = 0
size (Tree t) = size' t
    where
        size' (Leaf _ _) = 1
        size' (Branch lvl True _ _ _) = bit (fromIntegral lvl + 1)
        size' (Branch lvl False _ _ r) = setBit (size' r) (fromIntegral lvl)

type instance Index (AT pv) = AccountIndex
type instance IxValue (AT pv) = Account pv

instance IsProtocolVersion pv => Ixed (AT pv) where
    ix i upd l@(Leaf _ acct)
        | i == 0    = mkLeaf <$> upd acct
        | otherwise = pure l
    ix i upd (Branch lvl f _ l r)
        | testBit i (fromIntegral lvl) = mkBranch lvl f l <$> ix (clearBit i (fromIntegral lvl)) upd r
        | otherwise = (\l' -> mkBranch lvl f l' r) <$> ix i upd l

type instance Index (AccountTable pv) = AccountIndex
type instance IxValue (AccountTable pv) = Account pv

instance IsProtocolVersion pv => Ixed (AccountTable pv) where
    ix _ _ Empty = pure Empty
    ix i upd a@(Tree t)
        | i < bit (fromIntegral (nextLevel t)) = Tree <$> ix i upd t
        | otherwise = pure a

toList :: AccountTable pv -> [(AccountIndex, Account pv)]
toList Empty = []
toList (Tree t) = toL 0 t
    where
        toL o (Leaf _ a) = [(o, a)]
        toL o (Branch lvl _ _ t1 t2) = toL o t1 ++ toL (setBit o (fromIntegral lvl)) t2
