{-# LANGUAGE TypeFamilies #-}

module Concordium.Afgjort.PartyMap where

import qualified Data.Map.Strict as Map
import Lens.Micro.Internal (Index, IxValue, Ixed)
import Lens.Micro.Platform

import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.Types

data PartyMap a = PartyMap
    { weight :: !VoterPower,
      partyMap :: !(Map.Map Party a)
    }
    deriving (Eq, Ord, Show)

instance Functor PartyMap where
    fmap f pm = pm{partyMap = fmap f (partyMap pm)}

instance Foldable PartyMap where
    foldMap f = foldMap f . partyMap
    foldr f x = foldr f x . partyMap
    foldl f x = foldl f x . partyMap
    foldr1 f = foldr1 f . partyMap
    foldl1 f = foldl1 f . partyMap
    elem m = elem m . partyMap
    maximum = maximum . partyMap
    minimum = minimum . partyMap
    sum = sum . partyMap
    product = product . partyMap

instance Ixed (PartyMap a) where
    ix i = lens partyMap (\z m -> z{partyMap = m}) . ix i

type instance Index (PartyMap a) = Party
type instance IxValue (PartyMap a) = a

member :: Party -> PartyMap a -> Bool
{-# INLINE member #-}
member p = Map.member p . partyMap

insert :: Party -> VoterPower -> a -> PartyMap a -> PartyMap a
{-# INLINE insert #-}
insert p vp a m
    | member p m = m
    | otherwise =
        m
            { weight = weight m + vp,
              partyMap = Map.insert p a (partyMap m)
            }

delete :: Party -> VoterPower -> PartyMap a -> PartyMap a
{-# INLINE delete #-}
delete p vp m
    | member p m =
        m
            { weight = weight m - vp,
              partyMap = Map.delete p (partyMap m)
            }
    | otherwise = m

empty :: PartyMap a
empty = PartyMap 0 Map.empty

singleton :: Party -> VoterPower -> a -> PartyMap a
singleton p vp v = PartyMap vp (Map.singleton p v)

keysSet :: PartyMap a -> PS.PartySet
keysSet pm = PS.PartySet (weight pm) (BitSet.fromList $ Map.keys (partyMap pm))

keys :: PartyMap a -> [Party]
keys = Map.keys . partyMap

toList :: PartyMap a -> [(Party, a)]
toList = Map.toList . partyMap

fromList :: (Party -> VoterPower) -> [(Party, a)] -> PartyMap a
fromList power l = PartyMap (sum $ (power . fst) <$> l) (Map.fromList l)

union :: (Party -> VoterPower) -> PartyMap a -> PartyMap a -> PartyMap a
union pv p1 = foldr (\(p, a) -> insert p (pv p) a) p1 . toList
