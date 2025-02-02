{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.ProtocolUpdate.P7 (
    Update (..),
    checkUpdate,
    updateRegenesis,
    updateNextProtocolVersion,
) where

import Control.Monad.State hiding (get)
import qualified Data.HashMap.Strict as HM
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.Updates

import qualified Concordium.Genesis.Data.P8 as P8
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import qualified Concordium.ProtocolUpdate.P7.ProtocolP8 as ProtocolP8
import qualified Concordium.ProtocolUpdate.P7.Reboot as Reboot

-- | Updates that are supported from protocol version P7.
data Update
    = Reboot
    | ProtocolP8 P8.ProtocolUpdateData
    deriving (Show)

-- | Hash map for resolving updates from their specification hash.
updates :: HM.HashMap SHA256.Hash (Get Update)
updates = HM.fromList [(Reboot.updateHash, return Reboot), (ProtocolP8.updateHash, ProtocolP8 <$> get)]

-- | Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: ProtocolUpdate -> Either String Update
checkUpdate ProtocolUpdate{..} = case HM.lookup puSpecificationHash updates of
    Nothing -> Left "Specification hash does not correspond to a known protocol update."
    Just updateGet -> case runGet updateGet puSpecificationAuxiliaryData of
        Left err -> Left $! "Could not deserialize auxiliary data: " ++ err
        Right update -> return update

-- | Construct the genesis data for a P7 update.
updateRegenesis ::
    ( MPV m ~ 'P7,
      BlockStateStorage m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- | The update taking effect.
    Update ->
    -- | The terminal block of the old chain.
    BlockPointer (MPV m) ->
    m (PVInit m)
updateRegenesis Reboot = Reboot.updateRegenesis
updateRegenesis (ProtocolP8 protocolUpdateData) = ProtocolP8.updateRegenesis protocolUpdateData

-- | Determine the protocol version the update will update to.
updateNextProtocolVersion ::
    Update ->
    SomeProtocolVersion
updateNextProtocolVersion Reboot{} = SomeProtocolVersion SP7
updateNextProtocolVersion ProtocolP8{} = SomeProtocolVersion SP8
