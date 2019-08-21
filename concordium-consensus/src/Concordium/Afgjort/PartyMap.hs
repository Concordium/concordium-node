{-# LANGUAGE TypeFamilies #-}
module Concordium.Afgjort.PartyMap where

import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Lens.Micro.Internal(Index,IxValue,Ixed)

import Concordium.Afgjort.Types
import qualified Concordium.Afgjort.PartySet as PS
import qualified Concordium.Afgjort.CSS.BitSet as BitSet

data PartyMap a = PartyMap { 
    weight :: !VoterPower,
    partyMap :: !(Map.Map Party a)
} deriving (Eq,Ord,Show)

instance Functor PartyMap where
    fmap f pm = pm {partyMap = fmap f (partyMap pm)}

instance Ixed (PartyMap a) where
    ix i = lens partyMap (\z m -> z {partyMap = m}) . ix i

type instance Index (PartyMap a) = Party
type instance IxValue (PartyMap a) = a

member :: Party -> PartyMap a -> Bool
{-# INLINE member #-}
member p = Map.member p . partyMap

insert :: Party -> VoterPower -> a -> PartyMap a -> PartyMap a
{-# INLINE insert #-}
insert p vp a m
    | member p m = m
    | otherwise = m {
            weight = weight m + vp,
            partyMap = Map.insert p a (partyMap m)
        }

empty :: PartyMap a
empty = PartyMap 0 Map.empty

singleton :: Party -> VoterPower -> a -> PartyMap a
singleton p vp v = PartyMap vp (Map.singleton p v)

keysSet :: PartyMap a -> PS.PartySet
keysSet pm = PS.PartySet (weight pm) (BitSet.fromList $ Map.keys (partyMap pm))

keys :: PartyMap a -> [Party]
keys = Map.keys . partyMap