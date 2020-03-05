module Concordium.Kontrol(
    module Concordium.Skov.Monad,
    module Concordium.Kontrol
) where

import Data.Time.Clock.POSIX
import Data.Time
import Data.Fixed

import Concordium.Types
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import Concordium.Skov.Monad
import Concordium.TimeMonad
import Concordium.Afgjort.Finalize.Types

currentTimestamp :: (TimeMonad m) => m Timestamp
currentTimestamp = truncate . utcTimeToPOSIXSeconds <$> currentTime

timeUntilNextSlot :: (TimeMonad m, SkovQueryMonad m) => m NominalDiffTime
timeUntilNextSlot = do
    gen <- getGenesisData
    now <- utcTimeToPOSIXSeconds <$> currentTime
    return $ (fromIntegral (genesisTime gen) - now) `mod'` fromIntegral (genesisSlotDuration gen)

getCurrentSlot :: (TimeMonad m, SkovQueryMonad m) => m Slot
getCurrentSlot = do
        GenesisData{..} <- getGenesisData
        ct <- currentTimestamp
        return $ Slot $ if ct <= genesisTime then 0 else (ct - genesisTime) `div` genesisSlotDuration

-- |Get the timestamp at the beginning of the given slot.
getSlotTimestamp :: (SkovQueryMonad m) => Slot -> m Timestamp
getSlotTimestamp slot = do
  GenesisData{..} <- getGenesisData
  -- We should be safe with respect to any overflow issues here since Timestamp is Word64
  return (genesisSlotDuration * fromIntegral slot + genesisTime)

-- |Determine the finalization session ID and finalization committee used for finalizing
-- at the given index.
-- TODO (MR) update comment above
getFinalizationContext :: (SkovQueryMonad m) => FinalizationRecord -> m (Maybe (FinalizationSessionId, FinalizationCommittee))
getFinalizationContext FinalizationRecord{..} = do
        genHash <- bpHash <$> genesisBlock
        let finSessId = FinalizationSessionId genHash 0 -- FIXME: Don't hard-code this!
        -- we need to determine the block that was finalized at index `finalizationIndex - 1` 
        resolveBlock finalizationBlockPointer >>= \case
          Just bp -> Just . (finSessId,) <$> getFinalizationCommittee (bpLastFinalized bp) -- TODO (MR) ensure that bplastfinalized really does point to index - 1
          Nothing -> return Nothing

-- |Select the finalization committee based on bakers from the given block
-- TODO (MR) clarify that it will not be based on the last finalized block
getFinalizationCommittee :: SkovQueryMonad m => BlockPointer m -> m FinalizationCommittee
getFinalizationCommittee bp = do
       finParams <- getFinalizationParameters
       makeFinalizationCommittee finParams . _birkCurrentBakers <$> getBirkParameters (blockSlot bp) bp