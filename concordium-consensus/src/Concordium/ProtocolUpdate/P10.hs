{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.ProtocolUpdate.P10 (
    Update (..),
    checkUpdate,
    updateRegenesis,
    updateNextProtocolVersion,
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.Updates

import qualified Concordium.Genesis.Data.P11 as P11
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import qualified Concordium.ProtocolUpdate.P10.ProtocolP11 as ProtocolP11
import qualified Concordium.ProtocolUpdate.P10.Reboot as Reboot

-- | Updates that are supported from protocol version P10.
data Update
    = Reboot
    | ProtocolP11 P11.ProtocolUpdateData
    deriving (Show)

-- | Hash map for resolving updates from their specification hash.
updates :: HM.HashMap SHA256.Hash (S.Get Update)
updates =
    HM.fromList
        [ (Reboot.updateHash, return Reboot),
          (ProtocolP11.updateHash, ProtocolP11 <$> S.get)
        ]

-- | Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: ProtocolUpdate -> Either String Update
checkUpdate ProtocolUpdate{..} = case HM.lookup puSpecificationHash updates of
    Nothing -> Left "Specification hash does not correspond to a known protocol update."
    Just updateGet -> case S.runGet updateGet puSpecificationAuxiliaryData of
        Left err -> Left $! "Could not deserialize auxiliary data: " ++ err
        Right update -> return update

-- | Construct the genesis data for a P10 update.
updateRegenesis ::
    ( MPV m ~ 'P10,
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
updateRegenesis (ProtocolP11 protocolUpdateData) = ProtocolP11.updateRegenesis protocolUpdateData

-- | Determine the protocol version the update will update to.
updateNextProtocolVersion ::
    Update ->
    SomeProtocolVersion
updateNextProtocolVersion Reboot{} = SomeProtocolVersion SP10
updateNextProtocolVersion ProtocolP11{} = SomeProtocolVersion SP11
