{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module implements the P1.Reboot protocol update.
-- This protocol update is valid at protocol version P1, and updates
-- to protocol version P1.
-- The block state is preserved across the update with limited changes
-- that are specified by the auxiliary data.
--
-- This produces a new 'GenesisDataP1' using the 'GDP1Regenesis' constructor,
-- as follows:
--
-- * 'genesisCore':
--
--     * 'genesisTime' is the timestamp of the last finalized block of the previous chain.
--     * 'genesisSlotDuration' is 'updateSlotDuration'.
--     * 'genesisEpochLength' is 'updateEpochLength'.
--     * 'genesisMaxBlockEnergy' is 'updateMaxBlockEnergy'.
--     * 'genesisFinalizationParameters' is 'updateFinalizationParameters'.
--
-- * 'genesisFirstGenesis' is either:
--
--     * the hash of the genesis block of the previous chain, if it is a 'GDP1Initial'; or
--     * the 'genesisFirstGenesis' value of the genesis block of the previous chain, if it
--       is a 'GDP1Regenesis'.
--
-- * 'genesisPreviousGenesis' is the hash of the previous genesis block.
--
-- * 'genesisTerminalBlock' is the hash of the last finalized block of the previous chain.
--
-- * 'genesisStateHash' and 'genesisNewState' are the hash and (V0) serialized state of the
--   new genesis block, which are derived from the block state of the last finalized block of
--   the previous chain by applying the following changes:
--
--     * The 'SeedState' is updated with:
--
--         * 'epochLength' is 'updateEpochLength';
--         * 'epoch' is @0@;
--         * 'currentLeadershipElectionNonce' is the SHA256 hash of (@"Regenesis" <> encode (updatedNonce oldSeedState)@); and
--         * 'updatedNonce' is the same as 'currentLeadershipElectionNonce'.
--
--     * The 'Updates' are updated with:
--
--         * the election difficulty chain parameter is set to 'updateElectionDifficulty';
--         * the election difficulty update queue is emptied;
--         * the current protocol update is set to 'Nothing'; and
--         * the protocol update queue is emptied.
--
-- Note that, while the seed state is revised, the initial epoch of the new chain is not considered
-- a new epoch for the purposes of block rewards and baker/finalization committee determination.
-- This means that block rewards at the end of this epoch are paid for all blocks baked in this epoch
-- and in the final epoch of the previous chain.
-- Furthermore, the bakers from the final epoch of the previous chain are also the bakers for the
-- initial epoch of the new chain.
module Concordium.ProtocolUpdate.P1.Reboot where

import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.P1 as P1
import Concordium.Types
import Concordium.Types.Parameters
import Concordium.Types.SeedState

import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState
import Concordium.Kontrol

-- |The data required to perform a P1.Reboot update.
data UpdateData = UpdateData
    { updateSlotDuration :: !Duration,
      updateElectionDifficulty :: !ElectionDifficulty,
      updateEpochLength :: !EpochLength,
      updateMaxBlockEnergy :: !Energy,
      updateFinalizationParameters :: !FinalizationParameters
    }
    deriving (Show)

instance Serialize UpdateData where
    put UpdateData{..} = do
        put updateSlotDuration
        put updateElectionDifficulty
        put updateEpochLength
        put updateMaxBlockEnergy
        putFinalizationParametersGD3 updateFinalizationParameters
    get = do
        updateSlotDuration <- get
        updateElectionDifficulty <- get
        updateEpochLength <- get
        updateMaxBlockEnergy <- get
        updateFinalizationParameters <- getFinalizationParametersGD3
        return UpdateData{..}

-- |The hash that identifies a P1.Reboot update.
--
-- FIXME: This should be the hash of a specification document that
-- properly describes the update.
updateHash :: SHA256.Hash
updateHash = SHA256.hash "P1.Reboot"

-- |Construct the genesis data for a P1.Reboot update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    (BlockPointerMonad m, BlockStateStorage m, SkovQueryMonad m) =>
    UpdateData ->
    m PVGenesisData
updateRegenesis UpdateData{..} = do
    lfb <- lastFinalizedBlock
    -- Genesis time is the timestamp of the terminal block
    regenesisTime <- getSlotTimestamp (blockSlot lfb)
    -- Core parameters are derived from the UpdateData
    let genesisCore =
            GenesisData.CoreGenesisParameters
                { genesisTime = regenesisTime,
                  genesisSlotDuration = updateSlotDuration,
                  genesisEpochLength = updateEpochLength,
                  genesisMaxBlockEnergy = updateMaxBlockEnergy,
                  genesisFinalizationParameters = updateFinalizationParameters
                }
    gd <- getGenesisData
    -- genesisFirstGenesis is the block hash of the previous genesis, if it is initial,
    -- or the genesisFirstGenesis of the previous genesis otherwise.
    let genesisFirstGenesis = _gcFirstGenesis gd
    let genesisPreviousGenesis = _gcCurrentHash gd
    let genesisTerminalBlock = bpHash lfb
    -- Determine the new state by updating the terminal state.
    s0 <- thawBlockState =<< blockState lfb
    -- Update the seed state
    oldSeedState <- bsoGetSeedState s0
    s1 <-
        bsoSetSeedState s0 $
            initialSeedState
                (SHA256.hash $ "Regenesis" <> encode (updatedNonce oldSeedState))
                updateEpochLength
    -- Overwrite the election difficulty.
    s2 <- bsoOverwriteElectionDifficulty s1 updateElectionDifficulty
    -- Clear the protocol update.
    s3 <- bsoClearProtocolUpdate s2
    regenesisState <- freezeBlockState s3
    genesisStateHash <- getStateHash regenesisState
    genesisNewState <- serializeBlockState regenesisState
    let genesisRegenesis = GenesisData.RegenesisData{..}
    return $ PVGenesisData $ GDP1 P1.GDP1Regenesis{..}
