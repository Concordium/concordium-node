{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.Afgjort.Types(
    module Concordium.Afgjort.Types,
    VoterPower(..),
    Signature
) where

import Data.Word
import Data.Serialize
import qualified Data.Map.Strict as Map
import Data.Bits
import Control.Monad

import Concordium.Types
import Concordium.Crypto.BlockSignature(Signature)

type Party = Word32
type Val = BlockHash

minParty :: Party
minParty = minBound :: Party

putParty :: Putter Party
putParty = putWord32be

putVal :: Putter Val
putVal = put

getParty :: Get Party
getParty = getWord32be

getVal :: Get Val
getVal = get

type Choice = Bool

type Choices = Maybe Choice

addChoice :: Choice -> Maybe Choices -> Maybe Choices
addChoice c Nothing = Just (Just c)
addChoice _ (Just Nothing) = Just Nothing
addChoice c cs@(Just (Just c')) = if c == c' then cs else Just Nothing

data CatchUpResult = CatchUpResult {
    -- |Indicates if we have relevant information that was not provided by the catch-up.
    curBehind :: !Bool,
    -- |Indicates if the catch-up refers to blocks we do not know about (and so requires Skov to catch up).
    curSkovCatchUp :: !Bool
}

instance Semigroup CatchUpResult where
    (CatchUpResult b1 c1) <> (CatchUpResult b2 c2) = CatchUpResult (b1 || b2) (c1 || c2)

instance Monoid CatchUpResult where
    mempty = CatchUpResult False False

-- |Serialize a map from parties to values.
-- The map is serialized as a (little-endian) bitmap indicating
-- which keys are present, followed by the values for these keys
-- in ascending order by key.
putPartyMap :: (Serialize v) =>
    Party   -- ^ The maximum possible index of a party
    -> Map.Map Party v  -- ^ Map of parties to values
    -> Put
putPartyMap maxParty m = putParties minParty (Map.keys m) >> putValues
    where
        putParties curP l
            | curP <= maxParty = do
                let (curl, restl) = span (< curP + 8) l
                putPartiesByte (fromIntegral . subtract curP <$> curl)
                putParties (curP + 8) restl
            | otherwise = return ()
        putPartiesByte l = putWord8 (foldl setBit 0 l)
        putValues = forM_ (Map.toAscList m) $ put . snd

-- |Deserialize a map from parties to values, as serialized by
-- 'putPartyMap'.
getPartyMap :: (Serialize v) =>
    Party   -- ^ The maximum possible index of a party
    -> Get (Map.Map Party v)
getPartyMap maxParty = do
        parties <- getParties minParty
        foldM buildMap Map.empty parties
    where
        getParties curP
            | curP <= maxParty = do
                b <- getWord8
                let bgn = (curP +) . fromIntegral <$> filter (testBit b) [0..7]
                (bgn ++) <$> getParties (curP + 8)
            | otherwise = return []
        buildMap m p = do
            v <- get
            return $ Map.insert p v m