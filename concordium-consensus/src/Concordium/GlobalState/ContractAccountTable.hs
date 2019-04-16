{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Concordium.GlobalState.ContractAccountTable where

import Prelude hiding (lookup)

import Data.Word
import Data.Bits
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import qualified Data.Serialize as S


import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

type ContractAccountIndex = Word64
newtype Version = Version Word32 deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance HashableTo H.Hash Version where
    getHash (Version v) = H.hash (S.runPut $ S.put v)

data ContractAccountTable hash account = Empty | Tree (CAT hash account)

data CAT hash account
    = Branch !Word8 !Bool hash !(CAT hash account) !(CAT hash account)
    | Leaf !account

instance Foldable (CAT hash) where
    foldr f r (Leaf a) = f a r
    foldr f r (Branch _ _ _ t1 t2) = foldr f (foldr f r t2) t1

instance Foldable (ContractAccountTable hash) where
    foldr f r Empty = r
    foldr f r (Tree t) = foldr f r t

empty :: ContractAccountTable hash account
empty = Empty

lookup :: ContractAccountIndex -> ContractAccountTable hash (Either version account) -> Maybe account
lookup _ Empty = Nothing
lookup n (Tree t) = lookup' n t

lookup' :: ContractAccountIndex -> CAT hash (Either version account) -> Maybe account
lookup' 0 (Leaf (Right a)) = Just a
lookup' _ (Leaf _) = Nothing
lookup' n (Branch b _ _ l r)
    | n < 2^b = lookup' n l
    | n < 2^(b+1) = lookup' (n - 2^b) r
    | otherwise = Nothing

instance (HashableTo hash account, Bits hash, Enum hash) => HashableTo hash (CAT hash (Either Version account)) where
    getHash (Branch _ _ h _ _) = h
    getHash (Leaf (Left (Version v))) = toEnum (1 + fromIntegral v)
    getHash (Leaf (Right a)) = getHash a

instance (HashableTo H.Hash account) => HashableTo H.Hash (ContractAccountTable H.Hash (Either Version account)) where
    getHash Empty = H.hash "SCAT"
    getHash (Tree t) = H.hash $ "SCAT" <> S.runPut (S.put $ (getHash t :: H.Hash))

type instance Index (ContractAccountTable hash account) = ContractAccountIndex
type instance IxValue (ContractAccountTable hash account) = account

type instance Index (CAT hash account) = ContractAccountIndex
type instance IxValue (CAT hash account) = account

hasVacancies :: CAT hash (Either version account) -> Bool
hasVacancies (Leaf (Left _)) = True
hasVacancies (Leaf (Right _)) = False
hasVacancies (Branch _ v _ _ _) = v

computeBranchHash :: (HashableTo H.Hash a) => Word8 -> CAT H.Hash (Either Version a) -> CAT H.Hash (Either Version a) -> H.Hash
computeBranchHash b t1 t2 = H.hash $ S.runPut $ do
        S.put b
        S.put (getHash t1 :: H.Hash)
        S.put (getHash t2 :: H.Hash)

instance (HashableTo H.Hash account) => Ixed (CAT H.Hash (Either Version account)) where
    ix i upd l@(Leaf a)
        | i == 0    = Leaf <$> upd a
        | otherwise = pure l
    ix i upd br@(Branch b _ _ t1 t2)
        | i < 2^b = mkBranch <$> (ix i upd t1) <*> pure t2
        | i < 2^(b+1) = mkBranch t1 <$> (ix (i - 2^b) upd t2)
        | otherwise = pure br
        where
            mkBranch t1' t2' = Branch b (hasVacancies t1' || hasVacancies t2') (computeBranchHash b t1' t2') t1' t2'

newContractAccount :: forall account. (HashableTo H.Hash account) => Lens (CAT H.Hash (Either Version account)) (CAT H.Hash (Either Version account)) (ContractAccountIndex, Version) account
newContractAccount mk = nca 0
    where
        nca offset (Leaf (Left v)) = Leaf . Right <$> mk (offset, v+1)
        nca offset l@(Leaf (Right _)) = mkBranch 0 l . Leaf . Right <$> mk (offset + 1, 0)
        nca offset (Branch b True _ t1 t2)
            | hasVacancies t1 = (\t1' -> mkBranch b t1' t2) <$> (nca offset t1)
            | otherwise = mkBranch b t1 <$> nca (offset + 2^b) t2 -- In this case, it should be that @hasVacancies t2@ holds or @hgt t2 < b@
        nca offset br@(Branch b False _ _ _)
            = mkBranch (b+1) br . Leaf . Right <$> mk (offset + 2^(b+1), 0)
        mkBranch b t1 t2 = Branch b (hasVacancies t1 || hasVacancies t2 || hgt t2 < b) (computeBranchHash b t1 t2) t1 t2
        hgt (Leaf _) = 0
        hgt (Branch b _ _ _ _) = b + 1



