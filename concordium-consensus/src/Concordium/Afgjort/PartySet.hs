module Concordium.Afgjort.PartySet where

import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.Types

data PartySet = PartySet {
    weight :: !VoterPower,
    parties :: !BitSet.BitSet
} deriving (Eq,Ord)

instance Show PartySet where
    show ps = "{" ++ show (BitSet.toList (parties ps) :: [Party]) ++ "}"

empty :: PartySet
empty = PartySet 0 BitSet.empty

null :: PartySet -> Bool
null = BitSet.null . parties

insert ::
    Party -- ^Party to add
    -> VoterPower -- ^Weight of party to add
    -> PartySet -- ^Set to add to
    -> PartySet
insert party pWeight pset
    | party `BitSet.member` parties pset = pset
    | otherwise = PartySet {
            weight = weight pset + pWeight,
            parties = BitSet.insert party (parties pset)
        }

-- |Add a party to the set, and return a boolean that indicates
-- if it was already there.
insertLookup ::
    Party -- ^Party to add
    -> VoterPower -- ^Weight of party to add
    -> PartySet -- ^Set to add to
    -> (Bool, PartySet)
insertLookup party pWeight pset
    | party `BitSet.member` parties pset = (True, pset)
    | otherwise = (False, PartySet {
            weight = weight pset + pWeight,
            parties = BitSet.insert party (parties pset)
        })

union ::
    (Party -> VoterPower)
    -- ^Party weight function
    -> PartySet
    -> PartySet
    -> PartySet
union partyWeight s1 s2 = PartySet w p
    where
        w = weight s1 - (sum $ partyWeight <$> BitSet.toList (BitSet.intersection (parties s1) (parties s2))) + weight s2
        p = BitSet.union (parties s1) (parties s2)

size :: PartySet -> Int
size = BitSet.size . parties

member :: Party -> PartySet -> Bool
member p ps = BitSet.member p (parties ps)

singleton :: Party -> VoterPower -> PartySet
singleton p vp = PartySet vp (BitSet.singleton p)

toList :: PartySet -> [Party]
toList = BitSet.toList . parties

fromList :: (Party -> VoterPower) -> [Party] -> PartySet
fromList w l = PartySet (sum $ w <$> l) (BitSet.fromList l)

isSubsetOf :: PartySet -> PartySet -> Bool
isSubsetOf ps1 ps2 = BitSet.isSubsetOf (parties ps1) (parties ps2)