module Concordium.KonsensusV1.TreeState.Types where

import Data.Time
import Data.Word

import Concordium.Types

-- |Metadata about a block that has been executed.
data BlockMetadata = BlockMetadata
    { bmHeight :: !BlockHeight,
      bmHash :: !BlockHash,
      bmReceiveTime :: !UTCTime,
      bmArriveTime :: !UTCTime,
      bmTransactionsCount :: !Word64,
      bmTransactionsEnergyCost :: !Energy,
      bmTransactionsSize :: !Word64
    }
