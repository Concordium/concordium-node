module Concordium.Afgjort.Types where

import Data.Word
import Data.Serialize

import Concordium.Types

type Party = Word32
type Val = BlockHash

putParty :: Putter Party
putParty = putWord32be

putVal :: Putter Val
putVal = put

getParty :: Get Party
getParty = getWord32be

getVal :: Get Val
getVal = get
