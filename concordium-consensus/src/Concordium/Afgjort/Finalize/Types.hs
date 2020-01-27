{-# LANGUAGE
    ScopedTypeVariables,
    RankNTypes,
    DerivingStrategies,
    DerivingVia #-}
module Concordium.Afgjort.Finalize.Types where


import qualified Data.Vector as Vec
import Data.Vector(Vector)
import Data.Word
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Maybe
import Control.Monad
import Data.ByteString(ByteString)
import Data.Map.Strict (Map)

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA

data FinalizationInstance = FinalizationInstance {
    finMySignKey :: !Sig.KeyPair,
    finMyVRFKey :: !VRF.KeyPair,
    finMyBlsKey :: !Bls.SecretKey
}

data PartyInfo = PartyInfo {
    partyIndex :: !Party,
    partyWeight :: !VoterPower,
    partySignKey :: !Sig.VerifyKey,
    partyVRFKey :: !VRF.PublicKey,
    partyBlsKey :: !Bls.PublicKey
} deriving (Eq, Ord)

instance Show PartyInfo where
    show = show . partyIndex

data FinalizationCommittee = FinalizationCommittee {
    parties :: !(Vector PartyInfo),
    totalWeight :: !VoterPower,
    corruptWeight :: !VoterPower
}

committeeMaxParty :: FinalizationCommittee -> Party
committeeMaxParty FinalizationCommittee{..} = fromIntegral (Vec.length parties)

makeFinalizationCommittee :: FinalizationParameters -> FinalizationCommittee
makeFinalizationCommittee FinalizationParameters {..} = FinalizationCommittee {..}
    where
        parties = Vec.fromList $ zipWith makeParty [0..] finalizationCommittee
        makeParty pix (VoterInfo psk pvk pow pbls) = PartyInfo pix pow psk pvk pbls
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
    show FinalizationMessage{msgHeader=FinalizationMessageHeader{..},..}
        = "[" ++ show msgFinalizationIndex ++ ":" ++ show msgDelta ++ "] " ++ show msgSenderIndex ++ "-> " ++ show msgBody

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

encodeForSign :: FinalizationMessageHeader -> WMVBAMessage -> ByteString
encodeForSign msgHeader msgBody = runPut $ S.put msgHeader >> putWMVBAMessageBody msgBody

signFinalizationMessage :: Sig.KeyPair -> FinalizationMessageHeader -> WMVBAMessage -> FinalizationMessage
signFinalizationMessage key msgHeader msgBody = FinalizationMessage {..}
    where
        msgSignature = Sig.sign key (encodeForSign msgHeader msgBody)

toPartyInfo :: FinalizationCommittee -> Word32 -> Maybe PartyInfo
toPartyInfo com p = parties com Vec.!? fromIntegral p

checkMessageSignature :: FinalizationCommittee -> FinalizationMessage -> Bool
checkMessageSignature com FinalizationMessage{..} = isJust $ do
        p <- toPartyInfo com (msgSenderIndex msgHeader)
        guard $ Sig.verify (partySignKey p) (encodeForSign msgHeader msgBody) msgSignature

checkMessage :: FinalizationCommittee -> FinalizationMessage -> Bool
checkMessage com msg = all validParty (messageParties $ msgBody msg) && checkMessageSignature com msg
    where
        validParty = (< numParties)
        numParties = fromIntegral $ Vec.length $ parties com

data FinalizationSummary = FinalizationSummary {
    -- |For each failed round (in order of increasing delta),
    -- a collection of signatures on 'WeAreDone False'.
    summaryFailedRounds :: [Map Party Sig.Signature],
    -- |Summary for the current round.
    summaryCurrentRound :: WMVBASummary Sig.Signature
}

putFinalizationSummary :: Party -> FinalizationSummary -> Put
putFinalizationSummary maxParty FinalizationSummary{..} = do
        putWord16be $ fromIntegral $ length summaryFailedRounds
        forM_ summaryFailedRounds (putPartyMap maxParty)
        putWMVBASummary maxParty summaryCurrentRound

getFinalizationSummary :: Party -> S.Get FinalizationSummary
getFinalizationSummary maxParty = do
        nFailedRounds <- S.getWord16be
        summaryFailedRounds <- forM [1..nFailedRounds] $ \_ -> getPartyMap maxParty
        summaryCurrentRound <- getWMVBASummary maxParty
        return FinalizationSummary{..}

data CatchUpMessage = CatchUpMessage {
    cuSessionId :: !FinalizationSessionId,
    cuFinalizationIndex :: !FinalizationIndex,
    cuSenderIndex :: !Party,
    cuMaxParty :: !Party,
    cuFinalizationSummary :: !FinalizationSummary,
    cuSignature :: !Sig.Signature
}

signCatchUpMessage :: Sig.KeyPair -> FinalizationSessionId -> FinalizationIndex -> Party -> Party -> FinalizationSummary -> CatchUpMessage
signCatchUpMessage keyPair cuSessionId cuFinalizationIndex cuSenderIndex cuMaxParty cuFinalizationSummary = CatchUpMessage{..}
    where
        cuSignature = Sig.sign keyPair encoded
        encoded = runPut $ do
            S.put FinalizationMessageHeader{
                msgSessionId = cuSessionId,
                msgFinalizationIndex = cuFinalizationIndex,
                msgDelta = 0,
                msgSenderIndex = cuSenderIndex
            }
            S.put cuMaxParty
            putFinalizationSummary cuMaxParty cuFinalizationSummary

checkCatchUpMessageSignature :: FinalizationCommittee -> CatchUpMessage -> Bool
checkCatchUpMessageSignature com CatchUpMessage{..} = isJust $ do
        p <- toPartyInfo com cuSenderIndex
        guard $ Sig.verify (partySignKey p) encoded cuSignature
    where
        encoded = runPut $ do
            S.put FinalizationMessageHeader{
                msgSessionId = cuSessionId,
                msgFinalizationIndex = cuFinalizationIndex,
                msgDelta = 0,
                msgSenderIndex = cuSenderIndex
            }
            S.put cuMaxParty
            putFinalizationSummary cuMaxParty cuFinalizationSummary

data FinalizationPseudoMessage
    = FPMMessage !FinalizationMessage
    | FPMCatchUp !CatchUpMessage

instance S.Serialize FinalizationPseudoMessage where
    put (FPMMessage msg) = S.put msg
    put (FPMCatchUp CatchUpMessage{..}) = do
        S.put FinalizationMessageHeader{
            msgSessionId = cuSessionId,
            msgFinalizationIndex = cuFinalizationIndex,
            msgDelta = 0,
            msgSenderIndex = cuSenderIndex
        }
        S.put cuMaxParty
        putFinalizationSummary cuMaxParty cuFinalizationSummary
        S.put cuSignature
    get = do
        msgHeader@FinalizationMessageHeader{..} <- S.get
        if msgDelta == 0 then do
            cuMaxParty <- S.get
            cuFinalizationSummary <- getFinalizationSummary cuMaxParty
            cuSignature <- S.get
            return $ FPMCatchUp CatchUpMessage{
                cuSessionId = msgSessionId,
                cuFinalizationIndex = msgFinalizationIndex,
                cuSenderIndex = msgSenderIndex,
                ..
            }
        else do
            msgBody <- S.get
            msgSignature <- S.get
            return $ FPMMessage FinalizationMessage{..}

instance Show FinalizationPseudoMessage where
    show (FPMMessage msg) = show msg
    show (FPMCatchUp _) = "[Finalization Catch-Up Message]"

fpmHeader :: FinalizationPseudoMessage -> FinalizationMessageHeader
fpmHeader (FPMMessage m) = msgHeader m
fpmHeader (FPMCatchUp CatchUpMessage{..}) = FinalizationMessageHeader {
            msgSessionId = cuSessionId,
            msgFinalizationIndex = cuFinalizationIndex,
            msgDelta = 0,
            msgSenderIndex = cuSenderIndex
        }
