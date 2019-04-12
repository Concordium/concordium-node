{-# LANGUAGE DefaultSignatures, RecordWildCards #-}
module Concordium.Kontrol.Monad(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol.Monad
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.GlobalState.Types
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.TimeMonad

class SkovMonad m => KontrolMonad m where
    currentTimestamp :: m Timestamp
    currentTimestamp = truncate . utcTimeToPOSIXSeconds <$> currentTime
    timeUntilNextSlot :: m NominalDiffTime
    timeUntilNextSlot = do
        gen <- getGenesisData
        now <- utcTimeToPOSIXSeconds <$> currentTime
        return $ (now - (fromInteger $ toInteger (genesisTime gen))) `mod'` (fromInteger $ toInteger $ genesisSlotDuration gen)

instance KontrolMonad m => KontrolMonad (MaybeT m) where
    currentTimestamp = lift currentTimestamp
    timeUntilNextSlot = lift timeUntilNextSlot

getCurrentSlot :: (KontrolMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- getGenesisData
        ct <- currentTimestamp
        return $ Slot ((ct - genesisTime) `div` genesisSlotDuration)
