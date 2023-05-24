{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.ProtocolUpdate.P5 (
    Update (..),
    checkUpdate,
    updateRegenesis,
    updateNextProtocolVersion,
) where

import qualified Data.HashMap.Strict as HM
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Types
import Concordium.Kontrol
import qualified Concordium.ProtocolUpdate.P5.ProtocolP6 as ProtocolP6

-- |Updates that are supported from protocol version P5.
data Update = ProtocolP6
    deriving (Show)

-- |Hash map for resolving updates from their specification hash.
updates :: HM.HashMap SHA256.Hash (Get Update)
updates = HM.fromList [(ProtocolP6.updateHash, return ProtocolP6)]

-- |Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: ProtocolUpdate -> Either String Update
checkUpdate ProtocolUpdate{..} = case HM.lookup puSpecificationHash updates of
    Nothing -> Left "Specification hash does not correspond to a known protocol update."
    Just g -> case runGet g puSpecificationAuxiliaryData of
        Left err -> Left $! "Could not deserialize auxiliary data: " ++ err
        Right r -> return r

-- |Construct the genesis data for a P5 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis :: (MPV m ~ 'P5, BlockStateStorage m, SkovMonad m) => Update -> m (PVInit m)
updateRegenesis ProtocolP6 = ProtocolP6.updateRegenesis

-- |Determine the protocol version the update will update to.
updateNextProtocolVersion ::
    Update ->
    SomeProtocolVersion
updateNextProtocolVersion ProtocolP6 = SomeProtocolVersion SP6
