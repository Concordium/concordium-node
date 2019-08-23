{-# LANGUAGE RecordWildCards, TupleSections #-}
module Concordium.Afgjort.CSS.NominationSet where

{-
import qualified Data.Set as Set
import Data.Set (Set)
-}
import qualified Concordium.Afgjort.CSS.BitSet as Set
import Concordium.Afgjort.CSS.BitSet (BitSet)
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Bits
import Control.Monad

import Concordium.Afgjort.Types


-- |A nomination set records a set of parties that have seen
-- Top and a set of parties that have seen Bottom.  `nomMax`
-- should be an upper bound of the parties occurring in
-- these sets.  Either set may be empty (in which case @Nothing@
-- is used to represent it), but at least one of them should not
-- be.
data NominationSet = NominationSet {
    nomMax :: !Party,
    nomTop :: !BitSet,
    nomBot :: !BitSet
} deriving (Eq, Ord)

instance Show NominationSet where
    show NominationSet{..} = "{top: " ++ sh nomTop ++ ", bot: " ++ sh nomBot ++ "}"
        where
            sh s = show (Set.toList s :: [Party])

data NominationSetTag
    = NSEmpty
    | NSTop
    | NSBot
    | NSBoth
    deriving (Eq)

nomTag :: NominationSet -> NominationSetTag
nomTag s = case (Set.null (nomTop s), Set.null (nomBot s)) of
            (True, True) -> NSEmpty
            (False, True) -> NSTop
            (True, False) -> NSBot
            (False, False) -> NSBoth

putUntaggedNominationSet :: Putter NominationSet
putUntaggedNominationSet NominationSet{..} = do
        putParty nomMax
        unless (Set.null nomTop) $ putParties minParty (Set.toAscList nomTop)
        unless (Set.null nomBot) $ putParties minParty (Set.toAscList nomBot)
    where
        putParties curP l
            | curP <= nomMax = do
                let (curl, restl) = span (< curP + 8) l
                putPartiesByte (fromIntegral . subtract curP <$> curl)
                putParties (curP + 8) restl
            | otherwise = return ()
        putPartiesByte l = putWord8 (foldl setBit 0 l)

getUntaggedNominationSet :: NominationSetTag -> Get NominationSet
getUntaggedNominationSet tag = do
        nomMax <- getParty
        let
            getParties curP
                | curP <= nomMax = do
                    b <- getWord8
                    let bgn = (curP +) . fromIntegral <$> filter (testBit b) [0..7]
                    (bgn ++) <$> getParties (curP + 8)
                | otherwise = return []
        nomTop <- if tag == NSTop || tag == NSBoth then
                    Set.fromAscList <$> getParties minParty
                else
                    return Set.empty
        nomBot <- if tag == NSBot || tag == NSBoth then
                    Set.fromAscList <$> getParties minParty
                else
                    return Set.empty
        return $! (NominationSet{..})

emptyNominationSet :: NominationSet
emptyNominationSet = NominationSet minParty Set.empty Set.empty

addNomination :: Party -> Choice -> NominationSet -> NominationSet
addNomination p c ns =
        if c then
            ns {nomMax = nm, nomTop = Set.insert p (nomTop ns)}
        else
            ns {nomMax = nm, nomBot = Set.insert p (nomBot ns)}
    where
        nm = max p (nomMax ns)

subsumedBy :: NominationSet -> NominationSet -> Bool
subsumedBy s1 s2 = (nomTop s1 `Set.isSubsetOf` nomTop s2) && (nomBot s1 `Set.isSubsetOf` nomBot s2)

nominationSetToList :: NominationSet -> [(Party, Choice)]
nominationSetToList s = mkList True (nomTop s) ++ mkList False (nomBot s)
    where
        mkList b = fmap (, b) . Set.toList

nominations :: Party -> NominationSet -> Maybe Choices
nominations p s = case (p `Set.member` nomTop s, p `Set.member` nomBot s) of
    (True, True) -> Just Nothing
    (True, False) -> Just $ Just True
    (False, True) -> Just $ Just False
    (False, False) -> Nothing

singletonNominationSet :: Party -> Choice -> NominationSet
singletonNominationSet p True = NominationSet p (Set.singleton p) Set.empty
singletonNominationSet p False = NominationSet p Set.empty (Set.singleton p)

unionNominationSet :: NominationSet -> NominationSet -> NominationSet
unionNominationSet ns1 ns2 = NominationSet {
        nomMax = max (nomMax ns1) (nomMax ns2),
        nomTop = Set.union (nomTop ns1) (nomTop ns2),
        nomBot = Set.union (nomBot ns1) (nomBot ns2)
    }