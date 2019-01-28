{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards #-}
module Concordium.Afgjort.WMVBA where

import Lens.Micro.Platform
import Control.Monad.State.Class
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as BS

import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.ABBA

data WMVBAMessage val party
    = WMVBAFreezeMessage (FreezeMessage val)
    | WMVBAABBAMessage (ABBAMessage party)
    | WMVBAWitnessCreatorMessage val


data WMVBAState val party sig = WMVBAState {
    _freezeState :: FreezeState val party,
    _abbaState :: Either (Seq (ABBAMessage party)) (ABBAState party sig)
}
makeLenses ''WMVBAState

initialWMVBAState :: WMVBAState val party sig
initialWMVBAState = WMVBAState {
    _freezeState = initialFreezeState,
    _abbaState = Left (Seq.empty)
}


data WMVBAInstance val party sig m = WMVBAInstance {
    justifyWMVBAInput :: val -> m (),
    receiveWMVBAMessage :: party -> sig -> WMVBAMessage val party -> m ()
}

newWMVBAInstance :: forall val party sig m. (Ord party) => BS.ByteString -> Int -> Int -> (party -> Int) -> (party -> VRF.PublicKey) -> party -> VRF.PrivateKey -> WMVBAInstance val party sig m
newWMVBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey =  WMVBAInstance {..}
    where
        justifyWMVBAInput input = undefined