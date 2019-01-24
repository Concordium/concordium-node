{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Concordium.Afgjort.Finalize where

import qualified Data.Vector as Vec

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Types
import Concordium.Kontrol.Monad

data FinalizeInstance m = FinalizeInstance {
    beginFinalize :: m ()
}

data Party = Party {
    partyIndex :: Int,
    partyWeight :: Int,
    partySignKey :: Sig.VerifyKey,
    partyVRFKey :: VRF.PublicKey
} deriving (Eq, Ord)

data FinalizationState = FinalizationState {
    
}

newFinalizeInstance :: forall m. (SkovMonad m) => FinalizationIndex -> BlockHeight -> Vec.Vector Party -> Int -> Sig.SignKey -> VRF.PrivateKey -> FinalizeInstance m
newFinalizeInstance index height parties me mySign myVRF = FinalizeInstance {..}
    where
        totalWeight = sum (partyWeight <$> parties)
        corruptWeight = (totalWeight - 1) `div` 3
        beginFinalize = undefined