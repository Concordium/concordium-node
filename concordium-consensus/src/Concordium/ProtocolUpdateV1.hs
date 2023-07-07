{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.ProtocolUpdateV1 where

import Control.Monad.State

import Concordium.Types
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus (..))
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState (BlockStateStorage)
import Concordium.GlobalState.Types (PVInit)
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation

-- import qualified Concordium.ProtocolUpdate.P1 as P1
-- import qualified Concordium.ProtocolUpdate.P2 as P2
-- import qualified Concordium.ProtocolUpdate.P3 as P3
-- import qualified Concordium.ProtocolUpdate.P4 as P4
-- import qualified Concordium.ProtocolUpdate.P5 as P5

import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.ProtocolUpdate.P6 as P6
import Concordium.Skov

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
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    ( BlockStateStorage m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    Update (MPV m) ->
    m (PVInit m)
updateRegenesis (UpdateP6 u) = P6.updateRegenesis u

-- |Determine the next protocol version for the given update. Although the same
-- information can be retrieved from 'updateRegenesis', this is more efficient
-- than 'updateRegenesis' if only the next protocol version is needed.
updateNextProtocolVersion ::
    Update pv ->
    SomeProtocolVersion
updateNextProtocolVersion (UpdateP6 u) = P6.updateNextProtocolVersion u

-- |If a protocol update has taken effect, return its protocol version.
-- Otherwise return 'Nothing'.
getNextProtocolVersion :: forall m. (SkovQueryMonad m) => m (Maybe SomeProtocolVersion)
getNextProtocolVersion =
    getProtocolUpdateStatus >>= \case
        ProtocolUpdated pu -> case checkUpdate @(MPV m) pu of
            Left _ -> return Nothing
            Right u -> return . Just . updateNextProtocolVersion $ u
        PendingProtocolUpdates _ -> return Nothing
