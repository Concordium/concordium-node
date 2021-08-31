{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |This module implements the P1.Memo protocol update.
-- This protocol update is valid at protocol version P1, and updates
-- to protocol version P2.
-- The block state is preserved across the update.
--
-- This produces a new 'GenesisDataP2' using the 'GenesisDataP2' constructor,
-- as follows:
--
-- * 'genesisCore':
--
--     * 'genesisTime' is the timestamp of the last finalized block of the previous chain.
--     * 'genesisSlotDuration' is unchanged.
--     * 'genesisEpochLength' is unchanged.
--     * 'genesisMaxBlockEnergy' is unchanged.
--     * 'genesisFinalizationParameters' is unchanged.
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
--         * 'epochLength' is unchanged;
--         * 'epoch' is @0@;
--         * 'currentLeadershipElectionNonce' is the SHA256 hash of (@"Regenesis" <> encode (updatedNonce oldSeedState)@); and
--         * 'updatedNonce' is the same as 'currentLeadershipElectionNonce'.
--
--     * The 'Updates' are updated with:
--
--         * the current protocol update is set to 'Nothing'; and
--         * the protocol update queue is emptied.
--
-- Note that, while the seed state is revised, the initial epoch of the new chain is not considered
-- a new epoch for the purposes of block rewards and baker/finalization committee determination.
-- This means that block rewards at the end of this epoch are paid for all blocks baked in this epoch
-- and in the final epoch of the previous chain.
-- Furthermore, the bakers from the final epoch of the previous chain are also the bakers for the
-- initial epoch of the new chain.
module Concordium.ProtocolUpdate.P1.Memo where

import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.P1 as P1
import qualified Concordium.Genesis.Data.P2 as P2
import Concordium.Types
import Concordium.Types.SeedState

import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState
import Concordium.Kontrol

-- |The hash that identifies a update from P1 to P2 protocol.
-- This is the hash of the published specification document.
updateHash :: SHA256.Hash
updateHash = read "7b47a079445f9c2a6028fcc169bb9c812c19630da150f0859e2ebed02bcd6207"

-- |Construct the genesis data for a P1.Reboot update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis :: (BlockPointerMonad m, BlockStateStorage m, SkovQueryMonad 'P1 m) => m PVGenesisData
updateRegenesis = do
    lfb <- lastFinalizedBlock
    -- Genesis time is the timestamp of the terminal block
    regenesisTime <- getSlotTimestamp (blockSlot lfb)
    -- Core parameters are derived from the old genesis, apart from genesis time which is set for
    -- the time of the last finalized block.
    gd <- getGenesisData
    let core = (P1.genesisCore $ unGDP1 gd) { GenesisData.genesisTime = regenesisTime }
    -- genesisFirstGenesis is the block hash of the previous genesis, if it is initial,
    -- or the genesisFirstGenesis of the previous genesis otherwise.
    let genesisFirstGenesis = case gd of
            GDP1 P1.GDP1Initial{} -> genesisBlockHash gd
            GDP1 P1.GDP1Regenesis{genesisRegenesis=GenesisData.RegenesisData{genesisFirstGenesis = firstGen}} -> firstGen
    let genesisPreviousGenesis = genesisBlockHash gd
    let genesisTerminalBlock = bpHash lfb
    -- Determine the new state by updating the terminal state.
    s0 <- thawBlockState =<< blockState lfb
    -- Update the seed state
    oldSeedState <- bsoGetSeedState s0
    s1 <-
        bsoSetSeedState s0 $
            initialSeedState
                (SHA256.hash $ "Regenesis" <> encode (updatedNonce oldSeedState))
                $ gdEpochLength gd
    -- Clear the protocol update.
    s3 <- bsoClearProtocolUpdate s1
    regenesisState <- freezeBlockState s3
    genesisStateHash <- getStateHash regenesisState
    genesisNewState <- serializeBlockState regenesisState
    return . PVGenesisData . GDP2 $ P2.GDP2Regenesis{genesisRegenesis = GenesisData.RegenesisData{genesisCore=core,..}}
