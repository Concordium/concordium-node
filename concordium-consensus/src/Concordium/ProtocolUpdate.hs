{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.ProtocolUpdate where

import Concordium.Genesis.Data
import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.BlockState (BlockStateStorage)
import qualified Concordium.ProtocolUpdate.P1 as P1
import Concordium.Skov

-- |Type representing currently supported protocol update types.
data Update (pv :: ProtocolVersion) where
    UpdateP1 :: P1.Update -> Update 'P1

instance Show (Update pv) where
    show (UpdateP1 u) = "P1." ++ show u

-- |Determine if a 'ProtocolUpdate' corresponds to a supported update type.
checkUpdate :: forall pv. (IsProtocolVersion pv) => ProtocolUpdate -> Either String (Update pv)
checkUpdate = case protocolVersion @pv of
    SP1 -> fmap UpdateP1 . P1.checkUpdate

-- |Construct the genesis data for a P1 update.
-- It is assumed that the last finalized block is the terminal block of the old chain:
-- i.e. it is the first (and only) explicitly-finalized block with timestamp after the
-- update takes effect.
updateRegenesis ::
    (BlockStateStorage m, SkovQueryMonad pv m) =>
    Update pv ->
    m PVGenesisData
updateRegenesis (UpdateP1 u) = P1.updateRegenesis u

-- |If a protocol update has taken effect, return the genesis data for the new chain.
getUpdateGenesisData ::
    (BlockStateStorage m, SkovQueryMonad pv m) =>
    m (Maybe PVGenesisData)
getUpdateGenesisData =
    getProtocolUpdateStatus >>= \case
        Left pu -> case checkUpdate pu of
            Left _ -> return Nothing
            Right u -> Just <$> updateRegenesis u
        Right _ -> return Nothing