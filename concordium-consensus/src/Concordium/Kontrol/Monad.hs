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

import Concordium.Types
import Concordium.Skov.Monad

class SkovMonad m => KontrolMonad m where
    currentTimestamp :: m Timestamp
    default currentTimestamp :: MonadIO m => m Timestamp
    currentTimestamp = truncate <$> liftIO getPOSIXTime

instance KontrolMonad m => KontrolMonad (MaybeT m) where
    currentTimestamp = lift currentTimestamp

getBirkParameters :: (KontrolMonad m) => BlockHash -> m BirkParameters
getBirkParameters _ = genesisBirkParameters <$> genesisData

getGenesisTime :: (SkovMonad m) => m Timestamp
getGenesisTime = genesisTime <$> genesisData

getCurrentSlot :: (KontrolMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- genesisData
        ct <- currentTimestamp
        return ((ct - genesisTime) `div` genesisSlotDuration)

getFinalizationParameters :: (KontrolMonad m) => Slot -> m FinalizationParameters
getFinalizationParameters _ = genesisFinalizationParameters <$> genesisData