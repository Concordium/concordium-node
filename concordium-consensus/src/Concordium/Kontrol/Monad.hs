{-# LANGUAGE DefaultSignatures, RecordWildCards #-}
module Concordium.Kontrol.Monad(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol.Monad
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.Types
import Concordium.Skov.Monad

class SkovMonad m => KontrolMonad m where
    currentTimestamp :: m Timestamp
    default currentTimestamp :: MonadIO m => m Timestamp
    currentTimestamp = truncate <$> liftIO getPOSIXTime
    timeUntilNextSlot :: m NominalDiffTime
    default timeUntilNextSlot :: MonadIO m => m NominalDiffTime
    timeUntilNextSlot = do
        gen <- genesisData
        now <- liftIO $ getPOSIXTime
        return $ (now - (fromInteger $ toInteger (genesisTime gen))) `mod'` (fromInteger $ toInteger $ genesisSlotDuration gen)

instance KontrolMonad m => KontrolMonad (MaybeT m) where
    currentTimestamp = lift currentTimestamp
    timeUntilNextSlot = lift timeUntilNextSlot

getCurrentSlot :: (KontrolMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- genesisData
        ct <- currentTimestamp
        return $ Slot ((ct - genesisTime) `div` genesisSlotDuration)
