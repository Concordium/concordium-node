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

import Concordium.Common.Version
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA

data FinalizationInstance = FinalizationInstance {
    finMySignKey :: !Sig.KeyPair,
    finMyVRFKey :: !VRF.KeyPair,
    finMyBlsKey :: !Bls.SecretKey
}

-- Show instance only exposing public data.
instance Show FinalizationInstance where
  show FinalizationInstance{..} =
    "{ Ed25519Key = " ++ show (Sig.verifyKey finMySignKey) ++ ", " ++
    "VRFKey = " ++ show (VRF.publicKey finMyVRFKey) ++ ", " ++
    "BLSKey = " ++ show (Bls.derivePublicKey finMyBlsKey) ++ "}"

class HasFinalizationInstance f where
    finalizationInstance :: f -> Maybe FinalizationInstance
instance HasFinalizationInstance FinalizationInstance where
    finalizationInstance = Just
    {-# INLINE finalizationInstance #-}
instance HasFinalizationInstance () where
    finalizationInstance _ = Nothing
    {-# INLINE finalizationInstance #-}

data PartyInfo = PartyInfo {
    partyIndex :: !Party,
    partyWeight :: !VoterPower,
    partySignKey :: !Sig.VerifyKey,
    partyVRFKey :: !VRF.PublicKey,
    partyBlsKey :: !Bls.PublicKey,
    partyBakerId :: !BakerId
} deriving (Eq, Ord)

instance Show PartyInfo where
    show = show . partyIndex

data FinalizationCommittee = FinalizationCommittee {
    -- |All eligible parties, in ascending order of baker ID
    parties :: !(Vector PartyInfo),
    totalWeight :: !VoterPower,
    corruptWeight :: !VoterPower
} deriving (Eq, Show)

committeeMaxParty :: FinalizationCommittee -> Party
committeeMaxParty FinalizationCommittee{..} = fromIntegral (Vec.length parties)

-- |Create a finalization committee by selecting only the bakers whose stake exceeds
-- a certain fraction of the total stake. The fraction is taken from the FinalizationParameters.
makeFinalizationCommittee :: FinalizationParameters -> Amount -> FullBakers -> FinalizationCommittee
makeFinalizationCommittee FinalizationParameters {..} totalGTU bakers = FinalizationCommittee {..}
    where
        parties = Vec.imap makeParty $ Vec.filter isInCommittee $ fullBakerInfos bakers
        minStake = totalGTU `div` fromIntegral finalizationCommitteeMaxSize
        isInCommittee FullBakerInfo{..} = _bakerStake >= minStake
        makeParty party FullBakerInfo {_bakerInfo = BakerInfo {..}, ..} = PartyInfo {
                    partyBakerId = _bakerIdentity,
                    partyWeight = VoterPower $ fromIntegral _bakerStake,
                    partySignKey = _bakerSignatureVerifyKey,
                    partyVRFKey = _bakerElectionVerifyKey,
                    partyBlsKey = _bakerAggregationVerifyKey,
                    partyIndex = fromIntegral party
                }
        totalWeight = sum (partyWeight <$> parties)
        corruptWeight = (totalWeight - 1) `div` 3

-- |A finalization committee with no parties. No finalization record is
-- ever valid with respect to such a committee, since at least one honest
-- party must sign a finalization record for it to be valid.
emptyFinalizationCommittee :: FinalizationCommittee
emptyFinalizationCommittee = FinalizationCommittee {
        parties = Vec.empty,
        totalWeight = 0,
        corruptWeight = 0
    }

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
} deriving (Eq, Ord, Show)

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
    summaryFailedRounds :: ![Map Party Sig.Signature],
    -- |Summary for the current round.
    summaryCurrentRound :: !(WMVBASummary Sig.Signature)
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

catchUpMessageDelta :: BlockHeight
catchUpMessageDelta = BlockHeight maxBound

signCatchUpMessage :: Sig.KeyPair -> FinalizationSessionId -> FinalizationIndex -> Party -> Party -> FinalizationSummary -> CatchUpMessage
signCatchUpMessage keyPair cuSessionId cuFinalizationIndex cuSenderIndex cuMaxParty cuFinalizationSummary = CatchUpMessage{..}
    where
        cuSignature = Sig.sign keyPair encoded
        encoded = runPut $ do
            S.put FinalizationMessageHeader{
                msgSessionId = cuSessionId,
                msgFinalizationIndex = cuFinalizationIndex,
                msgDelta = catchUpMessageDelta,
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
                msgDelta = catchUpMessageDelta,
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
            msgDelta = catchUpMessageDelta,
            msgSenderIndex = cuSenderIndex
        }
        S.put cuMaxParty
        putFinalizationSummary cuMaxParty cuFinalizationSummary
        S.put cuSignature
    get = do
        msgHeader@FinalizationMessageHeader{..} <- S.get
        if msgDelta == catchUpMessageDelta then do
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

-- |Read a finalization pseudo message according to the V0 format.
getFPMV0 :: Get FinalizationPseudoMessage
getFPMV0 = S.get

-- |Serialize a finalization pseudo message according to the V0 format.
putFPMV0 :: FinalizationPseudoMessage -> Put
putFPMV0 = S.put

-- |Deserialize a versioned finalization pseudo message.
-- Read the version and decide how to parse the remaining data based on the
-- version.
--
-- Currently only supports version 0
getExactVersionedFPM :: Get FinalizationPseudoMessage
getExactVersionedFPM =
  getVersion >>= \case
     0 -> getFPMV0
     n -> fail $ "Unsupported FinalizationPseudoMessage version: " ++ show n

-- |Serialize a Finalization Pseudo Message with a version according to the V0 format.
-- In contrast to 'putFPMV0' this function also prepends the version.
putVersionedFPMV0 :: FinalizationPseudoMessage -> Put
putVersionedFPMV0 fpm = S.put (0 :: Version) <> putFPMV0 fpm

instance Show FinalizationPseudoMessage where
    show (FPMMessage msg) = show msg
    show (FPMCatchUp _) = "[Finalization Catch-Up Message]"

fpmHeader :: FinalizationPseudoMessage -> FinalizationMessageHeader
fpmHeader (FPMMessage m) = msgHeader m
fpmHeader (FPMCatchUp CatchUpMessage{..}) = FinalizationMessageHeader {
            msgSessionId = cuSessionId,
            msgFinalizationIndex = cuFinalizationIndex,
            msgDelta = catchUpMessageDelta,
            msgSenderIndex = cuSenderIndex
        }

roundBaid :: FinalizationSessionId -> FinalizationIndex -> BlockHeight -> ByteString
roundBaid finSessId finIx finDelta = runPut $ do
        S.put finSessId
        S.put finIx
        S.put finDelta
