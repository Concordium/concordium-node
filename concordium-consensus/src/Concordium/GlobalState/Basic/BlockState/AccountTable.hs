{-# LANGUAGE BangPatterns #-}
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

data AccountTable (av :: AccountVersion) = Empty | Tree !(AT av)

instance HashableTo H.Hash (AccountTable av) where
    getHash Empty = H.hash "EmptyLFMBTree" -- this is the definition in the persistent implementation, I think it is acceptable to define it this way in the basic one
    getHash (Tree t) = getHash t

data AT (av :: AccountVersion)
    = Branch !Word8 !Bool !H.Hash !(AT av) !(AT av)
    | Leaf !(AccountHash av) (Account av)

instance HashableTo H.Hash (AT av) where
    getHash (Branch _ _ h _ _) = h
    getHash (Leaf (AccountHash h) _) = h

nextLevel :: AT av -> Word8
nextLevel (Branch lvl _ _ _ _) = lvl + 1
nextLevel (Leaf _ _) = 0

full :: AT av -> Bool
full (Branch _ f _ _ _) = f
full (Leaf _ _) = True

mkLeaf :: IsAccountVersion av => Account av -> AT av
mkLeaf acct = Leaf (getHash acct) acct
{-# INLINE mkLeaf #-}

mkBranch :: Word8 -> Bool -> AT av -> AT av -> AT av
mkBranch lvl f l r = Branch lvl f (H.hashShort $ H.hashToShortByteString (getHash l) <> H.hashToShortByteString (getHash r)) l r
{-# INLINE mkBranch #-}

empty :: AccountTable av
empty = Empty

lookup' :: AccountIndex -> AT av -> Maybe (Account av)
lookup' 0 (Leaf _ acct) = Just acct
lookup' _ (Leaf _ _) = Nothing
lookup' x (Branch (fromIntegral -> branchBit) _ _ l r)
    | testBit x branchBit = lookup' (clearBit x branchBit) r
    | otherwise = lookup' x l

lookup :: AccountIndex -> AccountTable av -> Maybe (Account av)
lookup _ Empty = Nothing
lookup x (Tree t) = lookup' x t

append :: forall av. IsAccountVersion av => Account av -> AccountTable av -> (AccountIndex, AccountTable av)
append acct Empty = (0, Tree (Leaf (getHash acct) acct))
append acct (Tree t) = append' t & _2 %~ Tree
    where
        append' :: AT av -> (AccountIndex, AT av)
        append' l@(Leaf h _) = (1, Branch 0 True branchHash l newLeaf)
            where
                branchHash = H.hashShort $
                        H.hashToShortByteString (theAccountHash h)
                        <> H.hashToShortByteString (theAccountHash newHash)
        append' b@(Branch lvl True _ _ _) = (bit (fromIntegral lvl + 1), mkBranch (lvl + 1) False b newLeaf)
        append' (Branch lvl False _ l r) = let (i', r') = append' r in
                                                (setBit i' (fromIntegral lvl),
                                                mkBranch lvl (full r' && nextLevel r' == lvl) l r')
        newLeaf = Leaf newHash acct
        newHash = getHash acct

-- |Get the size of an 'AccountTable'.
size :: AccountTable av -> Word64
size Empty = 0
size (Tree t) = size' t
    where
        size' (Leaf _ _) = 1
        size' (Branch lvl True _ _ _) = bit (fromIntegral lvl + 1)
        size' (Branch lvl False _ _ r) = setBit (size' r) (fromIntegral lvl)

type instance Index (AT av) = AccountIndex
type instance IxValue (AT av) = Account av

instance IsAccountVersion av => Ixed (AT av) where
    ix i upd l@(Leaf _ acct)
        | i == 0    = mkLeaf <$> upd acct
        | otherwise = pure l
    ix i upd (Branch lvl f _ l r)
        | testBit i (fromIntegral lvl) = mkBranch lvl f l <$> ix (clearBit i (fromIntegral lvl)) upd r
        | otherwise = (\l' -> mkBranch lvl f l' r) <$> ix i upd l

type instance Index (AccountTable av) = AccountIndex
type instance IxValue (AccountTable av) = Account av

instance IsAccountVersion av => Ixed (AccountTable av) where
    ix _ _ Empty = pure Empty
    ix i upd a@(Tree t)
        | i < bit (fromIntegral (nextLevel t)) = Tree <$> ix i upd t
        | otherwise = pure a

toList :: AccountTable av -> [(AccountIndex, Account av)]
toList Empty = []
toList (Tree t) = toL 0 t
    where
        toL o (Leaf _ a) = [(o, a)]
        toL o (Branch lvl _ _ t1 t2) = toL o t1 ++ toL (setBit o (fromIntegral lvl)) t2

-- |Strict fold over the account table in increasing order of account index.
foldl' :: (a -> Account av -> a) -> a ->  AccountTable av -> a
foldl' _ a Empty = a
foldl' f !a0 (Tree t) = ft a0 t
    where
        ft a (Leaf _ acct) = f a acct
        ft a (Branch _ _ _ l r) =
            let !a1 = ft a l
                !a2 = ft a1 r
            in a2