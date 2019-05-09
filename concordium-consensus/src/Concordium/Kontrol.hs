{-# LANGUAGE DefaultSignatures, RecordWildCards #-}
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
currentTimestamp = truncate . utcTimeToPOSIXSeconds <$> currentTime

timeUntilNextSlot :: (TimeMonad m, SkovMonad m) => m NominalDiffTime
timeUntilNextSlot = do
    gen <- getGenesisData
    now <- utcTimeToPOSIXSeconds <$> currentTime
    return $ (now - (fromInteger $ toInteger (genesisTime gen))) `mod'` (fromInteger $ toInteger $ genesisSlotDuration gen)

getCurrentSlot :: (TimeMonad m, SkovMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- getGenesisData
        ct <- currentTimestamp
        return $ Slot ((ct - genesisTime) `div` genesisSlotDuration)
