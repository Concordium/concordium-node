{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module implements the P8.ProtocolP9 protocol update.
--  This protocol update is valid at protocol version P8, and updates
--  to protocol version P9.
--
--  This produces a new 'RegenesisDataP9' using the 'GDP9RegenesisFromP8' constructor,
--  as follows:
--
--  * 'genesisCore':
--
--      * 'genesisTime' is the timestamp of the last finalized block of the previous chain.
--      * 'genesisEpochDuration' is taken from the previous genesis.
--      * 'genesisSignatureThreshold' is taken from the previous genesis.
--
--  * 'genesisFirstGenesis' is either:
--
--      * the hash of the genesis block of the previous chain, if it is a 'GDP8Initial'; or
--      * the 'genesisFirstGenesis' value of the genesis block of the previous chain, if it
--        is a 'GDP8Regenesis'.
--
--  * 'genesisPreviousGenesis' is the hash of the previous genesis block.
--
--  * 'genesisTerminalBlock' is the hash of the last finalized block of the previous chain.
--
--  * 'genesisStateHash' is the state hash of the last finalized block of the previous chain.
--
--  * 'genesisMigration' is derived from the data provided to the protocol update, which is the
--     CreatePLT access structure
--
--  The block state is taken from the last finalized block of the previous chain. It is updated
--  as part of the state migration, which makes the following changes:
--
--  * The seed state is migrated as follows:
--
--      * The current epoch is reset to zero.
--      * The current and updated leadership election nonce are set to the hash of
--        @"Regenesis" <> encode oldUpdatedNonce@.
--      * The trigger block time is kept the same, meaning that the epoch will transition as soon
--        as possible.
--      * The epoch transition triggered flag is set.
--      * The shutdown triggered flag is cleared.
--
--  * The old current epoch is subtracted from the next payday epoch.
--
--  * The protocol update queue is emptied during the migration.
--
--  Note that, the initial epoch of the new chain is not considered
--  a new epoch for the purposes of block rewards and baker/finalization committee determination.
--  In particular, the timing of the next payday will be the same as if the protocol update
--  had not happened. (For instance, if it would have happened at the start of the next epoch
--  prior to the protocol update, after the update it will happen at the start of epoch 1.
--  The trigger block time in epoch 0 of the new consensus is the same as the trigger block
--  time in the final epoch of the old consensus.)
--  Furthermore, the bakers from the final epoch of the previous chain are also the bakers for the
--  initial epoch of the new chain.
module Concordium.ProtocolUpdate.P8.ProtocolP9 where

import Control.Monad.State
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.Genesis.Data.P9 as P9
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.KonsensusV1.TreeState.Implementation as TreeState
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo (getHash)
import Concordium.Types.ProtocolVersion

-- | The hash that identifies a update from P8 to P9 protocol.
--  This is the hash of the published specification document.
updateHash :: SHA256.Hash
updateHash = read "38c093fafce85684da59135441f37ebf4ece195be15e23ab9072d3d12d6dc3a7"

-- | Construct the genesis data for a P8.ProtocolP9 update.
--  This takes the terminal block of the old chain which is used as the basis for constructing
--  the new genesis block.
updateRegenesis ::
    ( MPV m ~ 'P8,
      BlockStateStorage m,
      MonadState (TreeState.SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    P9.ProtocolUpdateData ->
    -- | The terminal block of the old chain.
    BlockPointer 'P8 ->
    m (PVInit m)
updateRegenesis protocolUpdateData terminalBlock = do
    -- Genesis time is the timestamp of the terminal block
    let regenesisTime = blockTimestamp terminalBlock
    -- Core parameters are derived from the old genesis, apart from genesis time which is set for
    -- the time of the terminal block.
    gMetadata <- use TreeState.genesisMetadata
    BaseV1.CoreGenesisParametersV1{..} <- gmParameters <$> use TreeState.genesisMetadata
    let core =
            BaseV1.CoreGenesisParametersV1
                { BaseV1.genesisTime = regenesisTime,
                  ..
                }
    -- genesisFirstGenesis is the block hash of the previous genesis, if it is initial,
    -- or the genesisFirstGenesis of the previous genesis otherwise.
    let genesisFirstGenesis = gmFirstGenesisHash gMetadata
        genesisPreviousGenesis = gmCurrentGenesisHash gMetadata
        genesisTerminalBlock = getHash terminalBlock
    let regenesisBlockState = bpState terminalBlock
    genesisStateHash <- getStateHash regenesisBlockState
    let genesisMigration =
            P9.StateMigrationData
                { migrationProtocolUpdateData = protocolUpdateData
                }
    let newGenesis = GenesisData.RGDP9 $ P9.GDP9RegenesisFromP8{genesisRegenesis = BaseV1.RegenesisDataV1{genesisCore = core, ..}, ..}
    return (PVInit newGenesis (GenesisData.StateMigrationParametersP8ToP9 genesisMigration) (bmHeight $ bpInfo terminalBlock))
