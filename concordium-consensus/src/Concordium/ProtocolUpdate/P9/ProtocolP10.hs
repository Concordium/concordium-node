{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module implements the P9.ProtocolP10 protocol update.
--  This protocol update is valid at protocol version P9, and updates
--  to protocol version P10.
--
--  This produces a new 'RegenesisDataP10' using the 'GDP10RegenesisFromP9' constructor,
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
--      * the hash of the genesis block of the previous chain, if it is a 'GDP9Initial'; or
--      * the 'genesisFirstGenesis' value of the genesis block of the previous chain, if it
--        is a 'GDP9Regenesis'.
--
--  * 'genesisPreviousGenesis' is the hash of the previous genesis block.
--
--  * 'genesisTerminalBlock' is the hash of the last finalized block of the previous chain.
--
--  * 'genesisStateHash' is the state hash of the last finalized block of the previous chain.
--
--  * 'genesisMigration' is empty as there is no state migration between P9 and P10.
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
module Concordium.ProtocolUpdate.P9.ProtocolP10 where

import Control.Monad.State
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.Genesis.Data.P10 as P10
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.KonsensusV1.TreeState.Implementation as TreeState
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo (getHash)
import Concordium.Types.ProtocolVersion

-- | The hash that identifies a update from P9 to P10 protocol.
updateHash :: SHA256.Hash
-- FIXME: this MUST match the hash of the specification document when available at
-- https://proposals.concordium.com/updates/P10.html
updateHash = read "0000000000000000000000000000000000000000000000000000000000000010"

-- | Construct the genesis data for a P9.ProtocolP10 update.
--  This takes the terminal block of the old chain which is used as the basis for constructing
--  the new genesis block.
updateRegenesis ::
    ( MPV m ~ 'P9,
      BlockStateStorage m,
      MonadState (TreeState.SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    BlockPointer 'P9 ->
    m (PVInit m)
updateRegenesis terminalBlock = do
    let regenesisTime = blockTimestamp terminalBlock
    gMetadata <- use TreeState.genesisMetadata
    BaseV1.CoreGenesisParametersV1{..} <- gmParameters <$> use TreeState.genesisMetadata
    let core =
            BaseV1.CoreGenesisParametersV1
                { BaseV1.genesisTime = regenesisTime,
                  ..
                }
    let genesisFirstGenesis = gmFirstGenesisHash gMetadata
        genesisPreviousGenesis = gmCurrentGenesisHash gMetadata
        genesisTerminalBlock = getHash terminalBlock
    let regenesisBlockState = bpState terminalBlock
    genesisStateHash <- getStateHash regenesisBlockState
    let genesisMigration = P10.StateMigrationData
    let newGenesis = GenesisData.RGDP10 $ P10.GDP10RegenesisFromP9{genesisRegenesis = BaseV1.RegenesisDataV1{genesisCore = core, ..}, ..}
    return (PVInit newGenesis (GenesisData.StateMigrationParametersP9ToP10 genesisMigration) (bmHeight $ bpInfo terminalBlock))
