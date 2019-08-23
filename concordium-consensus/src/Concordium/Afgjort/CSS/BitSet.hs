module Concordium.Afgjort.CSS.BitSet where

import Data.Bits

type BitSet = Integer

empty :: BitSet
empty = 0

null :: BitSet -> Bool
null = (== 0)

union :: BitSet -> BitSet -> BitSet
union = (.|.)

intersection :: BitSet -> BitSet -> BitSet
intersection = (.&.)

member :: (Enum a) => a -> BitSet -> Bool
member i s = testBit s (fromEnum i)

singleton :: (Enum a) => a -> BitSet
singleton i = bit (fromEnum i)

insert :: (Enum a) => a -> BitSet -> BitSet
insert i s = setBit s (fromEnum i)

foldl :: (Enum b) => (a -> b -> a) -> a -> BitSet -> a
foldl _ st 0 = st
foldl f st0 s0 = go 0 st0 s0
    where
        go _ st 0 = st
        go a st s = go (a+1) st' (shiftR s 1)
            where
                st' = if testBit s 0 then f st (toEnum a) else st

toAscList :: (Enum a) => BitSet -> [a]
toAscList = go 0
    where
        go _ 0 = []
        go n s
            | testBit s 0 = (toEnum n : r)
            | otherwise = r
            where
                r = go (n+1) (shiftR s 1)

toList :: (Enum a) => BitSet -> [a]
toList = toAscList

fromList :: (Enum a) => [a] -> BitSet
fromList = foldr (\b s ->  setBit s (fromEnum b)) 0

fromAscList :: (Enum a) => [a] -> BitSet
fromAscList = fromList

isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf s1 s2 = s1 .&. s2 == s1

filter :: (Enum a) => (a -> Bool) -> BitSet -> BitSet
filter f = fromList . Prelude.filter f . toList

size :: BitSet -> Int
size = popCount

difference :: BitSet -> BitSet -> BitSet
difference s1 s2 = s1 .&. complement s2