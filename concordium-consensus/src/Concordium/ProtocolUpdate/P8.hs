{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.ProtocolUpdate.P8 (
    Update (..),
    checkUpdate,
    updateRegenesis,
    updateNextProtocolVersion,
) where

import Control.Monad.State hiding (get)
import qualified Data.HashMap.Strict as HM
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Genesis.Data.P9 as P9
import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import qualified Concordium.ProtocolUpdate.P8.ProtocolP9 as ProtocolP9
import qualified Concordium.ProtocolUpdate.P8.Reboot as Reboot

-- | Updates that are supported from protocol version P8.
data Update
    = Reboot
    | ProtocolP9 P9.ProtocolUpdateData
    deriving (Show)

-- | Hash map for resolving updates from their specification hash.
updates :: HM.HashMap SHA256.Hash (Get Update)
updates = HM.fromList [(Reboot.updateHash, return Reboot), (ProtocolP9.updateHash, ProtocolP9 <$> get)]

-- | Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: ProtocolUpdate -> Either String Update
checkUpdate ProtocolUpdate{..} = case HM.lookup puSpecificationHash updates of
    Nothing -> Left "Specification hash does not correspond to a known protocol update."
    Just updateGet -> case runGet updateGet puSpecificationAuxiliaryData of
        Left err -> Left $! "Could not deserialize auxiliary data: " ++ err
        Right update -> return update

-- | Construct the genesis data for a P8 update.
updateRegenesis ::
    ( MPV m ~ 'P8,
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
updateRegenesis (ProtocolP9 protocolUpdateData) = ProtocolP9.updateRegenesis protocolUpdateData

-- | Determine the protocol version the update will update to.
updateNextProtocolVersion ::
    Update ->
    SomeProtocolVersion
updateNextProtocolVersion Reboot{} = SomeProtocolVersion SP8
updateNextProtocolVersion ProtocolP9{} = SomeProtocolVersion SP9
