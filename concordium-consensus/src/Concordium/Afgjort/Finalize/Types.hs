{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell, LambdaCase, FlexibleContexts, MultiParamTypeClasses, RankNTypes, DerivingStrategies, DerivingVia, StandaloneDeriving #-}
module Concordium.Afgjort.Finalize.Types where


import qualified Data.Vector as Vec
import Data.Vector(Vector)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Word
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Maybe
import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState(BlockPointerData(..))
import Concordium.Kontrol
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Freeze (FreezeMessage(..))
import Concordium.Kontrol.BestBlock
import Concordium.Logger

data FinalizationInstance = FinalizationInstance {
    finMySignKey :: !Sig.KeyPair,
    finMyVRFKey :: !VRF.KeyPair
}

data PartyInfo = PartyInfo {
    partyIndex :: !Party,
    partyWeight :: !VoterPower,
    partySignKey :: !Sig.VerifyKey,
    partyVRFKey :: !VRF.PublicKey
} deriving (Eq, Ord)

instance Show PartyInfo where
    show = show . partyIndex

data FinalizationCommittee = FinalizationCommittee {
    parties :: !(Vector PartyInfo),
    totalWeight :: !VoterPower,
    corruptWeight :: !VoterPower
}

makeFinalizationCommittee :: FinalizationParameters -> FinalizationCommittee
makeFinalizationCommittee (FinalizationParameters {..}) = FinalizationCommittee {..}
    where
        parties = Vec.fromList $ zipWith makeParty [0..] finalizationCommittee
        makeParty pix (VoterInfo psk pvk pow) = PartyInfo pix pow psk pvk
        totalWeight = sum (partyWeight <$> parties)
        corruptWeight = (totalWeight - 1) `div` 3

data FinalizationSessionId = FinalizationSessionId {
    fsidGenesis :: !BlockHash,
    fsidIncarnation :: !Word64
} deriving (Eq, Ord, Show)

instance S.Serialize FinalizationSessionId where
    put FinalizationSessionId{..} = S.put fsidGenesis >> putWord64be fsidIncarnation
    get = do
        fsidGenesis <- S.get
        fsidIncarnation <- getWord64be
        return FinalizationSessionId{..}

data FinalizationMessageHeader = FinalizationMessageHeader {
    msgSessionId :: !FinalizationSessionId,
    msgFinalizationIndex :: !FinalizationIndex,
    msgDelta :: !BlockHeight,
    msgSenderIndex :: !Party
} deriving (Eq, Ord)

instance S.Serialize FinalizationMessageHeader where
    put FinalizationMessageHeader{..} = do
        S.put msgSessionId
        S.put msgFinalizationIndex
        S.put msgDelta
        S.put msgSenderIndex
    get = do
        msgSessionId <- S.get
        msgFinalizationIndex <- S.get
        msgDelta <- S.get
        msgSenderIndex <- getWord32be
        return FinalizationMessageHeader{..}

data FinalizationMessage = FinalizationMessage {
    msgHeader :: !FinalizationMessageHeader,
    msgBody :: !WMVBAMessage,
    msgSignature :: !Sig.Signature
}

instance Show FinalizationMessage where
    show FinalizationMessage{msgHeader=FinalizationMessageHeader{..},..} = "[" ++ show msgFinalizationIndex ++ ":" ++ show msgDelta ++ "] " ++ show msgSenderIndex ++ "-> " ++ show msgBody

instance S.Serialize FinalizationMessage where
    put FinalizationMessage{..} = do
        S.put msgHeader
        S.put msgBody
        S.put msgSignature
    get = do
        msgHeader <- S.get
        msgBody <- S.get
        msgSignature <- S.get
        return FinalizationMessage{..}

signFinalizationMessage :: Sig.KeyPair -> FinalizationMessageHeader -> WMVBAMessage -> FinalizationMessage
signFinalizationMessage key msgHeader msgBody = FinalizationMessage {..}
    where
        msgSignature = Sig.sign key encMsg
        encMsg = runPut $ S.put msgHeader >> S.put msgBody

toPartyInfo :: FinalizationCommittee -> Word32 -> Maybe PartyInfo
toPartyInfo com p = parties com Vec.!? fromIntegral p

checkMessageSignature :: FinalizationCommittee -> FinalizationMessage -> Bool
checkMessageSignature com FinalizationMessage{..} = isJust $ do
        p <- toPartyInfo com (msgSenderIndex msgHeader)
        let encMsg = runPut $ S.put msgHeader >> S.put msgBody
        guard $ Sig.verify (partySignKey p) encMsg msgSignature

checkMessage :: FinalizationCommittee -> FinalizationMessage -> Bool
checkMessage com msg = all validParty (messageParties $ msgBody msg) && checkMessageSignature com msg
    where
        validParty = (< numParties)
        numParties = fromIntegral $ Vec.length $ parties com
