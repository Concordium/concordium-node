module Concordium.Kontrol(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol
) where

import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.Types
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Types
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.TimeMonad
import Concordium.Afgjort.Finalize.Types

currentTimestamp :: (TimeMonad m) => m Timestamp
currentTimestamp = utcTimeToTimestamp <$> currentTime

timeUntilNextSlot :: (TimeMonad m, SkovQueryMonad pv m) => m NominalDiffTime
timeUntilNextSlot = do
    gen <- getGenesisData
    now <- utcTimeToPOSIXSeconds <$> currentTime
    return $ (0.001 * fromIntegral (tsMillis (gdGenesisTime gen)) - now) `mod'` (durationToNominalDiffTime (gdSlotDuration gen))

getCurrentSlot :: (TimeMonad m, SkovQueryMonad pv m) => m Slot
getCurrentSlot = do
        gen <- getGenesisData
        ct <- currentTimestamp
        return $ Slot $ if ct <= gdGenesisTime gen
                          then 0
                          else fromIntegral ((tsMillis $ ct - gdGenesisTime gen) `div` durationMillis (gdSlotDuration gen))

-- |Get the timestamp at the beginning of the given slot.
getSlotTimestamp :: (SkovQueryMonad pv m) => Slot -> m Timestamp
getSlotTimestamp slot = do
  gen <- getGenesisData
  -- We should be safe with respect to any overflow issues here since Timestamp is Word64
  return (addDuration (gdGenesisTime gen) (gdSlotDuration gen * fromIntegral slot))

-- |Select the finalization committee based on bakers from the given block.
getFinalizationCommittee :: (SkovQueryMonad pv m) => BlockPointerType m -> m FinalizationCommittee
getFinalizationCommittee bp = do
       finParams <- getFinalizationParameters
       blockState <- queryBlockState bp
       gtu <- _totalGTU <$> getRewardStatus blockState
       makeFinalizationCommittee finParams gtu <$> getCurrentEpochBakers blockState
