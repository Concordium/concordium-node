{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |Protocol updates supported from consensus version 1.
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

-- |Type representing currently supported protocol update types.
data Update (pv :: ProtocolVersion) where
    UpdateP6 :: P6.Update -> Update 'P6

instance Show (Update pv) where
    show (UpdateP6 u) = "P6." ++ show u

-- |Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: forall pv. (IsProtocolVersion pv) => ProtocolUpdate -> Either String (Update pv)
checkUpdate = case protocolVersion @pv of
    SP6 -> fmap UpdateP6 . P6.checkUpdate
    _ -> const $ Left "Unsupported update."

-- |Construct the genesis data for a P1 update.
updateRegenesis ::
    ( BlockStateStorage m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |The update to take effect.
    Update (MPV m) ->
    -- |The terminal block of the old chain.
    BlockPointer (MPV m) ->
    m (PVInit m)
updateRegenesis (UpdateP6 u) = P6.updateRegenesis u

-- |Determine the next protocol version for the given update. Although the same
-- information can be retrieved from 'updateRegenesis', this is more efficient
-- than 'updateRegenesis' if only the next protocol version is needed.
updateNextProtocolVersion ::
    Update pv ->
    SomeProtocolVersion
updateNextProtocolVersion (UpdateP6 u) = P6.updateNextProtocolVersion u
