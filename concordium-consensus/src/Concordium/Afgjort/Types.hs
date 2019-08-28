{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.Afgjort.Types(
    module Concordium.Afgjort.Types,
    VoterPower,
    Signature
) where

import Data.Word
import Data.Serialize

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
    -- |Indicates if catch-up resulted in obtaining new (relevant) information.
    curAhead :: !Bool,
    -- |Indicates if we have relevant information that was not provided by the catch-up.
    curBehind :: !Bool,
    -- |Indicates if the catch-up refers to blocks we do not know about (and so requires Skov to catch up).
    curSkovCatchUp :: !Bool
}

instance Semigroup CatchUpResult where
    (CatchUpResult a1 b1 c1) <> (CatchUpResult a2 b2 c2) = CatchUpResult (a1 || a2) (b1 || b2) (c1 || c2)

instance Monoid CatchUpResult where
    mempty = CatchUpResult False False False
