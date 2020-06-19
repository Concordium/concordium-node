module Concordium.Kontrol(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol
) where

import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Types
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.TimeMonad
import Concordium.Afgjort.Finalize.Types

currentTimestamp :: (TimeMonad m) => m Timestamp
currentTimestamp = utcTimeToTimestamp <$> currentTime

timeUntilNextSlot :: (TimeMonad m, SkovQueryMonad m) => m NominalDiffTime
timeUntilNextSlot = do
    gen <- getGenesisData
    now <- utcTimeToPOSIXSeconds <$> currentTime
    return $ (0.001 * fromIntegral (tsMillis (genesisTime gen)) - now) `mod'` (durationToNominalDiffTime (genesisSlotDuration gen))

getCurrentSlot :: (TimeMonad m, SkovQueryMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- getGenesisData
        ct <- currentTimestamp
        return $ Slot $ if ct <= genesisTime then 0 else fromIntegral ((tsMillis $ ct - genesisTime) `div` durationMillis genesisSlotDuration)

-- |Get the timestamp at the beginning of the given slot.
getSlotTimestamp :: (SkovQueryMonad m) => Slot -> m Timestamp
getSlotTimestamp slot = do
  GenesisData{..} <- getGenesisData
  -- We should be safe with respect to any overflow issues here since Timestamp is Word64
  return (addDuration genesisTime (genesisSlotDuration * fromIntegral slot))

-- |Select the finalization committee based on bakers from the given block.
getFinalizationCommittee :: (SkovQueryMonad m, BakerOperations m) => BlockPointerType m -> m FinalizationCommittee
getFinalizationCommittee bp = do
       finParams <- getFinalizationParameters
       blockState <- queryBlockState bp
       gtu <- _totalGTU <$> getRewardStatus blockState
       bps <- getBirkParameters (blockSlot bp) bp
       bakers <- getCurrentBakers bps
       makeFinalizationCommittee finParams gtu <$> getFullBakerInfos bakers 
