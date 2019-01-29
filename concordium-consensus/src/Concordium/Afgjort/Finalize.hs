{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Concordium.Afgjort.Finalize where

import qualified Data.Vector as Vec
import Data.Vector(Vector)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Word
import qualified Data.Serialize as S
import qualified Data.Serialize.Put as S
import Data.Serialize (Serialize)

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Types
import Concordium.Kontrol.Monad
import Concordium.Afgjort.WMVBA


data FinalizeInstance = FinalizeInstance {
    finMySignKey :: Sig.SignKey,
    finMyVerifyKey :: Sig.VerifyKey,
    finMyPrivateVRFKey :: VRF.PrivateKey,
    finMyPublicVRFKey :: VRF.PublicKey
}

data Party = Party {
    partyIndex :: Int,
    partyWeight :: Int,
    partySignKey :: Sig.VerifyKey,
    partyVRFKey :: VRF.PublicKey
} deriving (Eq, Ord)

data FinalizationCommittee = FinalizationCommittee {
    parties :: Vector Party,
    totalWeight :: Int,
    corruptWeight :: Int
}

makeFinalizationCommittee :: Vector Party -> FinalizationCommittee
makeFinalizationCommittee parties = FinalizationCommittee {..}
    where
        totalWeight = sum (partyWeight <$> parties)
        corruptWeight = (totalWeight - 1) `div` 3

data FinalizationRound = FinalizationRound {
    roundInput :: Maybe BlockHash,
    roundDelta :: BlockHeight,
    roundWMVBA :: WMVBAState BlockHash Party Sig.Signature
}

data FinalizationStage
    = StageNotParticipating
    | StageInProgress (WMVBAState BlockHash Party Sig.Signature)
    | StageComplete BlockHash

data FinalizationSessionId = FinalizationSessionId {
    fsidGenesis :: BlockHash,
    fsidIncarnation :: Word64
}

data FinalizationMessage = FinalizationMessage {
    msgSessionId :: FinalizationSessionId,
    msgFinalizationIndex :: FinalizationIndex,
    msgDelta :: BlockHeight,
    msgSender :: Int,
    msgData :: WMVBAMessage BlockHash Party,
    msgSignature :: Sig.Signature
}

{-
encodeFinalizationMessage :: FinalizationMessage -> BS.ByteString
encodeFinalizationMessage FinalizationMessage{..} = S.runPut $ do
    S.put (fsidGenesis msgSessionId)
    S.putWord64be (fsidIncarnation msgSessionId)
    S.put msgFinalizationIndex
    S.put msgDelta
    S.put msgSender
    S.put msgData
    S.put msgSignature
-}

data FinalizationState = FinalizationState {
    finSessionId :: FinalizationSessionId,
    finIndex :: FinalizationIndex,
    finHeight :: BlockHeight,
    finCommittee :: FinalizationCommittee,
    finPendingMessages :: Map FinalizationIndex (Map BlockHeight FinalizationMessage)
}


