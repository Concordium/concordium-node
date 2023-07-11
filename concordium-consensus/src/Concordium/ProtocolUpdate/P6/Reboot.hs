{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module implements the P6.Reboot protocol update.
-- This protocol update is valid at protocol version P6, and updates
-- to protocol version P6.
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
-- The block state is taken from the last finalized block of the previous chain. It is updated
-- as part of the state migration, which makes the following changes:
--
-- * The seed state is migrated as follows:
--
--     * The current epoch is reset to zero.
--     * The current and updated leadership election nonce are set to the hash of
--       @"Regenesis" <> encode oldUpdatedNonce@.
--     * The trigger block time is kept the same, meaning that the epoch will transition as soon
--       as possible.
--     * The epoch transition triggered flag is set.
--     * The shutdown triggered flag is cleared.
--
-- * The old current epoch is subtracted from the next payday epoch.
--
-- * The protocol update queue is emptied during the migration.
--
-- Note that, the initial epoch of the new chain is not considered
-- a new epoch for the purposes of block rewards and baker/finalization committee determination.
-- In particular, the timing of the next payday will be the same as if the protocol update
-- had not happened. (For instance, if it would have happened at the start of the next epoch
-- prior to the protocol update, after the update it will happen at the start of epoch 1.
-- The trigger block time in epoch 0 of the new consensus is the same as the trigger block
-- time in the final epoch of the old consensus.)
-- Furthermore, the bakers from the final epoch of the previous chain are also the bakers for the
-- initial epoch of the new chain.
module Concordium.ProtocolUpdate.P6.Reboot where

import Control.Monad.State
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo (getHash)
import Concordium.Types.ProtocolVersion

-- |The hash that identifies the P6.Reboot update:
-- 8a984071ee285404c6148581369cf46ed325d1405d85e79cb6dfd5a8f5a70553
updateHash :: SHA256.Hash
updateHash = SHA256.hash "P6.Reboot"

-- |Construct the genesis data for a P5.ProtocolP6 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    ( MPV m ~ 'P6,
      BlockStateStorage m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    m (PVInit m)
updateRegenesis = do
    lfb <- use lastFinalized
    -- Genesis time is the timestamp of the terminal block
    let regenesisTime = blockTimestamp lfb
    -- Core parameters are derived from the old genesis, apart from genesis time which is set for
    -- the time of the last finalized block.
    gm <- use genesisMetadata
    BaseV1.CoreGenesisParametersV1{..} <- gmParameters <$> use genesisMetadata
    let core =
            BaseV1.CoreGenesisParametersV1
                { BaseV1.genesisTime = regenesisTime,
                  ..
                }
    -- genesisFirstGenesis is the block hash of the previous genesis, if it is initial,
    -- or the genesisFirstGenesis of the previous genesis otherwise.
    let genesisFirstGenesis = gmFirstGenesisHash gm
        genesisPreviousGenesis = gmCurrentGenesisHash gm
        genesisTerminalBlock = getHash lfb
    let regenesisBlockState = bpState lfb
    genesisStateHash <- getStateHash regenesisBlockState
    let newGenesis = GenesisData.RGDP6 $ P6.GDP6Regenesis{genesisRegenesis = BaseV1.RegenesisDataV1{genesisCore = core, ..}}
    return (PVInit newGenesis GenesisData.StateMigrationParametersTrivial (bmHeight $ bpInfo lfb))
