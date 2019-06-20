{-# LANGUAGE RecordWildCards, TupleSections #-}
module Concordium.Afgjort.CSS.NominationSet where

import qualified Data.Set as Set
import Data.Set (Set)
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
    nomMax :: Party,
    nomTop :: Maybe (Set Party),
    nomBot :: Maybe (Set Party)
} deriving (Eq, Ord)

instance Show NominationSet where
    show NominationSet{..} = "{top: " ++ sh nomTop ++ ", bot: " ++ sh nomBot ++ "}"
        where
            sh Nothing = "None"
            sh (Just s) = show $ Set.toList s

data NominationSetTag
    = NSEmpty
    | NSTop
    | NSBot
    | NSBoth
    deriving (Eq)

nomTag :: NominationSet -> NominationSetTag
nomTag s = case (nomTop s, nomBot s) of
            (Nothing, Nothing) -> NSEmpty
            (Just _, Nothing) -> NSTop
            (Nothing, Just _) -> NSBot
            (Just _, Just _) -> NSBoth

putUntaggedNominationSet :: Putter NominationSet
putUntaggedNominationSet NominationSet{..} = do
        putParty nomMax
        forM_ nomTop $ putParties minParty . Set.toAscList
        forM_ nomBot $ putParties minParty . Set.toAscList
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
                    Just . Set.fromAscList <$> getParties minParty
                else
                    return Nothing
        nomBot <- if tag == NSBot || tag == NSBoth then
                    Just . Set.fromAscList <$> getParties minParty
                else
                    return Nothing
        return (NominationSet{..})

emptyNominationSet :: NominationSet
emptyNominationSet = NominationSet minParty Nothing Nothing

addNomination :: Party -> Choice -> NominationSet -> NominationSet
addNomination p c ns =
        if c then
            ns {nomMax = nm, nomTop = Just $! maybe (Set.singleton p) (Set.insert p) (nomTop ns)}
        else
            ns {nomMax = nm, nomBot = Just $! maybe (Set.singleton p) (Set.insert p) (nomBot ns)}
    where
        nm = max p (nomMax ns)

subsumedBy :: NominationSet -> NominationSet -> Bool
subsumedBy s1 s2 = (nomTop s1 `sby` nomTop s2) && (nomBot s1 `sby` nomBot s2)
    where
        Nothing `sby` _ = True
        _ `sby` Nothing = False
        (Just a) `sby` (Just b) = a `Set.isSubsetOf` b

nominationSetToList :: NominationSet -> [(Party, Choice)]
nominationSetToList s = mkList True (nomTop s) ++ mkList False (nomBot s)
    where
        mkList b = maybe [] (fmap (, b) . Set.toList)

nominations :: Party -> NominationSet -> Maybe Choices
nominations p s = case Set.member p <$> nomTop s of
                Just True -> Just $ case Set.member p <$> nomBot s of
                            Just True -> Nothing
                            _ -> Just True
                _ -> case Set.member p <$> nomBot s of
                            Just True -> Just $ Just False
                            _ -> Nothing
