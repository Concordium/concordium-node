module Concordium.Afgjort.Types where

import Data.Word
import Data.Serialize

import Concordium.Types

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
