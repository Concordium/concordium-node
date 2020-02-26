module Concordium.Kontrol(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol
) where

import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.TimeMonad

currentTimestamp :: (TimeMonad m) => m Timestamp
currentTimestamp = truncate . (*1000) . utcTimeToPOSIXSeconds <$> currentTime

timeUntilNextSlot :: (TimeMonad m, SkovQueryMonad m) => m NominalDiffTime
timeUntilNextSlot = do
    gen <- getGenesisData
    now <- utcTimeToPOSIXSeconds <$> currentTime
    return $ (0.001 * fromIntegral (genesisTime gen) - now) `mod'` (0.001 * fromIntegral (genesisSlotDuration gen))

getCurrentSlot :: (TimeMonad m, SkovQueryMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- getGenesisData
        ct <- currentTimestamp
        return $ Slot $ if ct <= genesisTime then 0 else fromIntegral ((ct - genesisTime) `div` fromIntegral genesisSlotDuration)

-- |Get the timestamp at the beginning of the given slot.
getSlotTimestamp :: (SkovQueryMonad m) => Slot -> m Timestamp
getSlotTimestamp slot = do
  GenesisData{..} <- getGenesisData
  -- We should be safe with respect to any overflow issues here since Timestamp is Word64
  return (fromIntegral genesisSlotDuration * fromIntegral slot + genesisTime)
