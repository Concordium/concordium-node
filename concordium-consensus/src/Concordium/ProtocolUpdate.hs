{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.ProtocolUpdate where

import Concordium.Types
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus (..))
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState (BlockStateStorage)
import Concordium.GlobalState.Types (PVInit, MPV)
import qualified Concordium.ProtocolUpdate.P1 as P1
import qualified Concordium.ProtocolUpdate.P2 as P2
import qualified Concordium.ProtocolUpdate.P3 as P3
import qualified Concordium.ProtocolUpdate.P4 as P4
import Concordium.Skov

-- |Type representing currently supported protocol update types.
data Update (pv :: ProtocolVersion) where
    UpdateP1 :: P1.Update -> Update 'P1
    UpdateP2 :: P2.Update -> Update 'P2
    UpdateP3 :: P3.Update -> Update 'P3
    UpdateP4 :: P4.Update -> Update 'P4

instance Show (Update pv) where
    show (UpdateP1 u) = "P1." ++ show u
    show (UpdateP2 u) = "P2." ++ show u
    show (UpdateP3 u) = "P3." ++ show u
    show (UpdateP4 u) = "P4." ++ show u

-- |Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: forall pv. (IsProtocolVersion pv) => ProtocolUpdate -> Either String (Update pv)
checkUpdate = case protocolVersion @pv of
    SP1 -> fmap UpdateP1 . P1.checkUpdate
    SP2 -> fmap UpdateP2 . P2.checkUpdate
    SP3 -> fmap UpdateP3 . P3.checkUpdate
    SP4 -> fmap UpdateP4 . P4.checkUpdate
    _ -> const $ Left "Unsupported update."

-- |Construct the genesis data for a P1 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    (BlockStateStorage m, SkovMonad m) =>
    Update (MPV m) ->
    m (PVInit m)
updateRegenesis (UpdateP1 u) = P1.updateRegenesis u
updateRegenesis (UpdateP2 u) = P2.updateRegenesis u
updateRegenesis (UpdateP3 u) = P3.updateRegenesis u
updateRegenesis (UpdateP4 u) = P4.updateRegenesis u

-- |Determine the next protocol version for the given update. Although the same
-- information can be retrieved from 'updateRegenesis', this is more efficient
-- than 'updateRegenesis' if only the next protocol version is needed.
updateNextProtocolVersion ::
    Update pv ->
    SomeProtocolVersion
updateNextProtocolVersion (UpdateP1 u) = P1.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP2 u) = P2.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP3 u) = P3.updateNextProtocolVersion u
updateNextProtocolVersion (UpdateP4 u) = P4.updateNextProtocolVersion u

-- |If a protocol update has taken effect, return its protocol version.
-- Otherwise return 'Nothing'.
getNextProtocolVersion :: forall m. (SkovQueryMonad m) => m (Maybe SomeProtocolVersion)
getNextProtocolVersion =
    getProtocolUpdateStatus >>= \case
        ProtocolUpdated pu -> case checkUpdate @(MPV m) pu of
            Left _ -> return Nothing
            Right u -> return . Just . updateNextProtocolVersion $ u
        PendingProtocolUpdates _ -> return Nothing
