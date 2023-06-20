{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- FIXME: This is currently a dummy update, and the details, including the update hash, need to be resolved before release.
-- https://github.com/Concordium/concordium-update-proposals/issues/47

-- |This module implements the P5.ProtocolP6 protocol update.
-- This protocol update is valid at protocol version P6, and updates
-- to protocol version P6.
-- The block state is changed during the update.
--
-- In particular the following things are updated as part of the migration
-- from protocol P5 to protocol P6.
--
-- * The seed state is being updated as part of the migration and hence the
--   'P6.StateMigrationData' keeps the time of the trigger block so it can be used
--   to construct the new 'SeedStateV1' via that 'Timestamp' and the 'LeadershipElectionNonce'
--   that was recorded in the last finalized block of the P5 protocol.
--
-- * The protocol update queue is emptied during the migration.
--
-- This produces a new 'RegenesisDataP6' using the 'GDP6Regenesis' constructor,
-- as follows:
--
-- * 'genesisCore':
--
--     * 'genesisTime' is the timestamp of the last finalized block of the previous chain.
--     * 'genesisEpochDuration' is calculated from the previous epoch duration (in slots) times
--       the slot duration.
--     * 'genesisSignatureThreshold' is 2/3.
--
-- * 'genesisFirstGenesis' is either:
--
--     * the hash of the genesis block of the previous chain, if it is a 'GDP5Initial'; or
--     * the 'genesisFirstGenesis' value of the genesis block of the previous chain, if it
--       is a 'GDP5Regenesis'.
--
-- * 'genesisPreviousGenesis' is the hash of the previous genesis block.
--
-- * 'genesisTerminalBlock' is the hash of the last finalized block of the previous chain.
--
-- * 'genesisStateHash' is the state hash of the last finalized block of the previous chain.
--
-- Note that, the initial epoch of the new chain is not considered
-- a new epoch for the purposes of block rewards and baker/finalization committee determination.
-- This means that block rewards at the end of this epoch are paid for all blocks baked in this epoch
-- and in the final epoch of the previous chain.
-- Furthermore, the bakers from the final epoch of the previous chain are also the bakers for the
-- initial epoch of the new chain.
module Concordium.ProtocolUpdate.P5.ProtocolP6 where

import Data.Ratio

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.Genesis.Data.P6 as P6

import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Types
import Concordium.Kontrol
import Concordium.Types.ProtocolVersion

-- |The hash that identifies a update from P5 to P6 protocol.
-- This is the hash of the published specification document.
-- FIXME: Update the hash https://github.com/Concordium/concordium-update-proposals/issues/47
updateHash :: SHA256.Hash
updateHash = read "0000000000000000000000000000000000000000000000000000000000000000"

-- |Construct the genesis data for a P5.ProtocolP6 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    (MPV m ~ 'P5, BlockStateStorage m, SkovMonad m) =>
    P6.ProtocolUpdateData ->
    m (PVInit m)
updateRegenesis protocolUpdateData = do
    lfb <- lastFinalizedBlock
    -- Genesis time is the timestamp of the terminal block
    regenesisTime <- getSlotTimestamp (blockSlot lfb)
    -- Core parameters are derived from the old genesis, apart from genesis time which is set for
    -- the time of the last finalized block.
    gd <- getGenesisData
    -- Epoch duration is moved over from old protocol.
    -- Signature threshold is 2/3.
    let core =
            BaseV1.CoreGenesisParametersV1
                { BaseV1.genesisTime = regenesisTime,
                  BaseV1.genesisEpochDuration = GenesisData.gdSlotDuration gd * fromIntegral (GenesisData.gdEpochLength gd),
                  BaseV1.genesisSignatureThreshold = 2 % 3
                }
    -- genesisFirstGenesis is the block hash of the previous genesis, if it is initial,
    -- or the genesisFirstGenesis of the previous genesis otherwise.
    let genesisFirstGenesis = GenesisData._gcFirstGenesis gd
        genesisPreviousGenesis = GenesisData._gcCurrentHash gd
        genesisTerminalBlock = bpHash lfb
    regenesisBlockState <- blockState lfb
    genesisStateHash <- getStateHash regenesisBlockState
    let genesisMigration =
            P6.StateMigrationData
                { migrationProtocolUpdateData = protocolUpdateData,
                  migrationTriggerBlockTime = regenesisTime
                }
    let newGenesis = GenesisData.RGDP6 $ P6.GDP6RegenesisFromP5{genesisRegenesis = BaseV1.RegenesisDataV1{genesisCore = core, ..}, ..}
    return (PVInit newGenesis (GenesisData.StateMigrationParametersP5ToP6 genesisMigration) (bpHeight lfb))
