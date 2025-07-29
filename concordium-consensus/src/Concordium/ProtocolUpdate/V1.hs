{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Protocol updates supported from consensus version 1.
module Concordium.ProtocolUpdate.V1 where

import Control.Monad.State

import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState (BlockStateStorage)
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types (PVInit)
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import qualified Concordium.ProtocolUpdate.P6 as P6
import qualified Concordium.ProtocolUpdate.P7 as P7
import qualified Concordium.ProtocolUpdate.P8 as P8
import qualified Concordium.ProtocolUpdate.P9 as P9

-- | Type representing currently supported protocol update types.
data Update (pv :: ProtocolVersion) where
    UpdateP6 :: P6.Update -> Update 'P6
    UpdateP7 :: P7.Update -> Update 'P7
    UpdateP8 :: P8.Update -> Update 'P8
    UpdateP9 :: P9.Update -> Update 'P9

instance Show (Update pv) where
    show (UpdateP6 u) = "P6." ++ show u
    show (UpdateP7 u) = "P7." ++ show u
    show (UpdateP8 u) = "P8." ++ show u
    show (UpdateP9 u) = "P8." ++ show u

-- | Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: forall pv. (IsProtocolVersion pv) => ProtocolUpdate -> Either String (Update pv)
checkUpdate = case protocolVersion @pv of
    -- These ones are only supported in V0.
    SP1 -> const $ Left "Update to P1 unsupported in V1."
    SP2 -> const $ Left "Update to P2 unsupported in V1."
    SP3 -> const $ Left "Update to P3 unsupported in V1."
    SP4 -> const $ Left "Update to P4 unsupported in V1."
    SP5 -> const $ Left "Update to P5 unsupported in V1."
    -- These ones are supported in V1.
    SP6 -> fmap UpdateP6 . P6.checkUpdate
    SP7 -> fmap UpdateP7 . P7.checkUpdate
    SP8 -> fmap UpdateP8 . P8.checkUpdate
    SP9 -> fmap UpdateP9 . P9.checkUpdate

-- | Construct the genesis data for a P1 update.
updateRegenesis ::
    ( BlockStateStorage m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- | The update to take effect.
    Update (MPV m) ->
    -- | The terminal block of the old chain.
    BlockPointer (MPV m) ->
    m (PVInit m)
updateRegenesis (UpdateP6 u) = P6.updateRegenesis u
updateRegenesis (UpdateP7 u) = P7.updateRegenesis u
updateRegenesis (UpdateP8 u) = P8.updateRegenesis u
updateRegenesis (UpdateP9 u) = P9.updateRegenesis u

-- | Determine the next protocol version for the given update. Although the same
--  information can be retrieved from 'updateRegenesis', this is more efficient
--  than 'updateRegenesis' if only the next protocol version is needed.
updateNextProtocolVersion ::
    Update pv ->
    SomeProtocolVersion
updateNextProtocolVersion (UpdateP6 u) = P6.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP7 u) = P7.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP8 u) = P8.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP9 u) = P9.updateNextProtocolVersion u
