{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.ProtocolUpdate.P1 (
    Update (..),
    checkUpdate,
    updateRegenesis,
    updateNextProtocolVersion,
) where

import qualified Data.HashMap.Strict as HM
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Genesis.Data
import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState
import Concordium.Kontrol
import qualified Concordium.ProtocolUpdate.P1.ProtocolP2 as ProtocolP2
import qualified Concordium.ProtocolUpdate.P1.Reboot as Reboot

-- |Updates that are supported from protocol version P1.
data Update = Reboot Reboot.UpdateData | ProtocolP2
    deriving (Show)

-- |Hash map for resolving updates from their specification hash.
updates :: HM.HashMap SHA256.Hash (Get Update)
updates = HM.fromList [(Reboot.updateHash, Reboot <$> get), (ProtocolP2.updateHash, return ProtocolP2)]

-- |Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: ProtocolUpdate -> Either String Update
checkUpdate ProtocolUpdate{..} = case HM.lookup puSpecificationHash updates of
    Nothing -> Left "Specification hash does not correspond to a known protocol update."
    Just g -> case runGet g puSpecificationAuxiliaryData of
        Left err -> Left $! "Could not deserialize auxiliary data: " ++ err
        Right r -> return r

-- |Construct the genesis data for a P1 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis :: (BlockStateStorage m, SkovQueryMonad m) => Update -> m PVGenesisData
updateRegenesis (Reboot upd) = Reboot.updateRegenesis upd
updateRegenesis ProtocolP2 = ProtocolP2.updateRegenesis

-- |Determine the protocol version the update will update to.
updateNextProtocolVersion ::
    Update ->
    SomeProtocolVersion
updateNextProtocolVersion (Reboot _) = SomeProtocolVersion SP1
updateNextProtocolVersion ProtocolP2 = SomeProtocolVersion SP2
