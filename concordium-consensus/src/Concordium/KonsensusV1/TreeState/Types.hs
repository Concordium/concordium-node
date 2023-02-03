{-# LANGUAGE NumericUnderscores #-}

module Concordium.KonsensusV1.TreeState.Types where

import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
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

instance Serialize BlockMetadata where
    put BlockMetadata{..} = do
        put bmHeight
        put bmHash
        putUTCPOSIXMicros bmReceiveTime
        putUTCPOSIXMicros bmArriveTime
        putWord64be bmTransactionsCount
        put bmTransactionsEnergyCost
        putWord64be bmTransactionsSize
      where
        putUTCPOSIXMicros = putWord64be . floor . (1_000_000 *) . utcTimeToPOSIXSeconds
    get = do
        bmHeight <- get
        bmHash <- get
        bmReceiveTime <- getUTCPOSIXMicros
        bmArriveTime <- getUTCPOSIXMicros
        bmTransactionsCount <- getWord64be
        bmTransactionsEnergyCost <- get
        bmTransactionsSize <- getWord64be
        return BlockMetadata{..}
      where
        getUTCPOSIXMicros = posixSecondsToUTCTime . (/ 1_000_000) . realToFrac <$> getWord64be
