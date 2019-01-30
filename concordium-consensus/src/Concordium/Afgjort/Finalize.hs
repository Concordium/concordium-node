{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell, LambdaCase, FlexibleContexts #-}
module Concordium.Afgjort.Finalize where

import qualified Data.Vector as Vec
import Data.Vector(Vector)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Word
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize (Serialize)
import Data.Maybe
import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Types
import Concordium.Kontrol.Monad
import Concordium.Afgjort.WMVBA
import Concordium.Kontrol.BestBlock

data FinalizationInstance = FinalizationInstance {
    finMySignKey :: Sig.SignKey,
    finMyVerifyKey :: Sig.VerifyKey,
    finMyPrivateVRFKey :: VRF.PrivateKey,
    finMyPublicVRFKey :: VRF.PublicKey
}

data Party = Party {
    partyIndex :: Word32,
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
    roundMe :: Party,
    roundWMVBA :: WMVBAState BlockHash Party Sig.Signature
}

data FinalizationStage
    = StageNotParticipating
    | StageInProgress (WMVBAState BlockHash Party Sig.Signature)
    | StageComplete BlockHash

data FinalizationSessionId = FinalizationSessionId {
    fsidGenesis :: BlockHash,
    fsidIncarnation :: Word64
} deriving (Eq)

putFinalizationSessionId :: Putter FinalizationSessionId
putFinalizationSessionId FinalizationSessionId{..} = S.put fsidGenesis >> putWord64be fsidIncarnation

getFinalizationSessionId :: Get FinalizationSessionId
getFinalizationSessionId = do
    fsidGenesis <- S.get
    fsidIncarnation <- getWord64be
    return FinalizationSessionId{..}

data FinalizationMessageHeader = FinalizationMessageHeader {
    msgSessionId :: FinalizationSessionId,
    msgFinalizationIndex :: FinalizationIndex,
    msgDelta :: BlockHeight,
    msgSenderIndex :: Word32
}

putFinalizationMessageHeader :: Putter FinalizationMessageHeader
putFinalizationMessageHeader FinalizationMessageHeader{..} = do
    putFinalizationSessionId msgSessionId
    S.put msgFinalizationIndex
    S.put msgDelta
    putWord32be msgSenderIndex

getFinalizationMessageHeader :: Get FinalizationMessageHeader
getFinalizationMessageHeader = do
    msgSessionId <- getFinalizationSessionId
    msgFinalizationIndex <- S.get
    msgDelta <- S.get
    msgSenderIndex <- getWord32be
    return FinalizationMessageHeader{..}

data FinalizationMessage = FinalizationMessage {
    msgHeader :: FinalizationMessageHeader,
    msgBody :: WMVBAMessage BlockHash Party,
    msgSignature :: Sig.Signature
}

toParty :: FinalizationCommittee -> Word32 -> Maybe Party
toParty com p = parties com Vec.!? fromIntegral p

getParty :: FinalizationCommittee -> Get Party
getParty com = do
    p <- getWord32be
    case toParty com p of
        Nothing -> fail "Invalid party"
        Just party -> return party

putParty :: Putter Party
putParty = putWord32be . partyIndex

decodeFinalizationMessageTail :: FinalizationCommittee -> FinalizationMessageHeader -> BS.ByteString -> Maybe FinalizationMessage
decodeFinalizationMessageTail com hdr = either (const Nothing) Just . S.runGet myGet
    where
        myGet = do
            body <- getWMVBAMessage S.get (getParty com)
            sig <- S.get
            return $ FinalizationMessage hdr body sig

checkMessageSignature :: FinalizationCommittee -> FinalizationMessage -> Bool
checkMessageSignature com FinalizationMessage{..} = isJust $ do
        p <- toParty com (msgSenderIndex msgHeader)
        let encMsg = runPut $ putFinalizationMessageHeader msgHeader >> putWMVBAMessage S.put putParty msgBody
        guard $ Sig.verify (partySignKey p) encMsg msgSignature

decodeCheckMessage :: FinalizationCommittee -> FinalizationMessageHeader -> BS.ByteString -> Maybe FinalizationMessage
decodeCheckMessage com hdr bs = do
    msg <- decodeFinalizationMessageTail com hdr bs
    guard $ checkMessageSignature com msg
    return msg

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
    _finSessionId :: FinalizationSessionId,
    _finIndex :: FinalizationIndex,
    _finHeight :: BlockHeight,
    _finCommittee :: FinalizationCommittee,
    _finPendingMessages :: Map FinalizationIndex (Map BlockHeight [(Word32, BS.ByteString)]),
    _finCurrentRound :: Maybe FinalizationRound
}
makeLenses ''FinalizationState

class (MonadState FinalizationState m, MonadReader FinalizationInstance m, SkovMonad m) => FinalizationMonad m where
    broadcastFinalizationMessage :: BS.ByteString -> m ()
    broadcastFinalizationRecord :: FinalizationRecord -> m ()

tryNominateBlock :: FinalizationMonad m => m ()
tryNominateBlock = do
    currRound <- use finCurrentRound
    forM_ currRound $ \r@FinalizationRound{..} -> 
        when (isNothing roundInput) $ do
            h <- use finHeight
            lastFin <- lastFinalizedBlock
            bBlock <- bestBlock
            when (bpHeight bBlock >= h + roundDelta && bpLastFinalized bBlock == lastFin) $ do
                let findAtHeight bp
                        | bpHeight bp == h = bp
                        | otherwise = findAtHeight (bpParent bp)
                    nomBlock = bpHash $ findAtHeight bBlock
                finCurrentRound ?= r {roundInput = Just nomBlock}
                liftWMVBA $ startWMVBA nomBlock

newRound :: FinalizationMonad m => BlockHeight -> Party -> m ()
newRound newDelta me = do
    oldRound <- use finCurrentRound
    forM_ oldRound $ \r ->
        when (newDelta > roundDelta r) $ do
            finCurrentRound ?= FinalizationRound {
                roundInput = Nothing,
                roundDelta = newDelta,
                roundMe = me,
                roundWMVBA = initialWMVBAState
            }
            justifiedInputs <- getBlocksAtHeight =<< use finHeight
            finIx <- use finIndex
            pmsgs <- finPendingMessages . at finIx . non Map.empty . at newDelta . non [] <<.= []
            committee <- use finCommittee
            sessId <- use finSessionId
            let msgHdr src = FinalizationMessageHeader {
                msgSessionId = sessId,
                msgFinalizationIndex = finIx,
                msgDelta = newDelta,
                msgSenderIndex = src
            }
            liftWMVBA $ do
                -- Justify the blocks
                forM_ justifiedInputs $ justifyWMVBAInput . bpHash
                -- Receive the pending messages
                forM_ pmsgs $ \(src0, pmsg) -> 
                    forM_ (toParty committee src0) $ \src ->
                        forM_ (decodeCheckMessage committee (msgHdr src0) pmsg) $ \msg ->
                            receiveWMVBAMessage src (msgSignature msg) (msgBody msg)
            tryNominateBlock


handleWMVBAOutputEvents :: FinalizationMonad m => [WMVBAOutputEvent BlockHash Party Sig.Signature] -> m ()
handleWMVBAOutputEvents evs = do
        FinalizationState{..} <- get
        FinalizationInstance{..} <- ask
        forM_ _finCurrentRound $ \FinalizationRound{..} -> do
            let msgHdr = FinalizationMessageHeader{
                msgSessionId = _finSessionId,
                msgFinalizationIndex = _finIndex,
                msgDelta = roundDelta,
                msgSenderIndex = partyIndex roundMe
            }
            let
                handleEv (SendWMVBAMessage msg0) = do
                    let encMsg = runPut $ putFinalizationMessageHeader msgHdr >> putWMVBAMessage S.put putParty msg0
                    let sig = runPut $ S.put $ Sig.sign finMySignKey encMsg
                    let msg = encMsg <> sig
                    broadcastFinalizationMessage msg
                    -- We manually loop back messages here
                    receiveFinalizationMessage msg
                handleEv (WMVBAComplete Nothing) = do
                    -- Round failed, so start a new one
                    newRound (2 * roundDelta) roundMe
                handleEv (WMVBAComplete (Just (finBlock, sigs))) = do
                    let finRec = FinalizationRecord {
                        finalizationIndex = _finIndex,
                        finalizationBlockPointer = finBlock,
                        finalizationProof = FinalizationProof [(partyIndex p, sig) | (p, sig) <- sigs],
                        finalizationDelay = roundDelta
                    }
                    finalizeBlock finRec
                    broadcastFinalizationRecord finRec
            mapM_ handleEv evs

liftWMVBA :: FinalizationMonad m => WMVBA BlockHash Party Sig.Signature a -> m a
liftWMVBA a = do
    FinalizationState{..} <- get
    FinalizationInstance{..} <- ask
    case _finCurrentRound of
        Nothing -> error "No current finalization round"
        Just (fr@FinalizationRound{..}) -> do
            let
                baid = runPut $ putFinalizationSessionId _finSessionId >> S.put _finIndex >> S.put roundDelta
                inst = WMVBAInstance baid (totalWeight _finCommittee) (corruptWeight _finCommittee) partyWeight partyVRFKey roundMe finMyPrivateVRFKey
                (r, newState, evs) = runWMVBA a inst roundWMVBA
            finCurrentRound ?= fr {roundWMVBA = newState}
            handleWMVBAOutputEvents evs
            return r

-- |Called when a finalization message is received.
receiveFinalizationMessage :: (FinalizationMonad m) => BS.ByteString -> m ()
receiveFinalizationMessage msg0 = case runGetState getFinalizationMessageHeader msg0 0 of
        Left _ -> return () -- Message header could not be decoded
        Right (hdr@FinalizationMessageHeader{..}, bodyBS) -> do
            FinalizationState{..} <- get
            when (_finSessionId == msgSessionId) $ do
                case compare msgFinalizationIndex _finIndex of
                    LT -> return () -- Message is for an old round, so discard
                    GT -> -- Message is for a future round, so save
                        finPendingMessages . at msgFinalizationIndex . non Map.empty . at msgDelta . non [] %= ((msgSenderIndex, bodyBS) :)
                    EQ -> -- Message is for current round.  Discard if we're not participating, otherwise handle
                        forM_ _finCurrentRound $ \FinalizationRound{..} ->
                            case compare msgDelta roundDelta of
                                LT -> return ()
                                GT -> finPendingMessages . at msgFinalizationIndex . non Map.empty . at msgDelta . non [] %= ((msgSenderIndex, bodyBS) :)
                                EQ -> forM_ (decodeCheckMessage _finCommittee hdr bodyBS) $ \msg ->
                                    forM_ (toParty _finCommittee msgSenderIndex) $ \src ->
                                        liftWMVBA (receiveWMVBAMessage src (msgSignature msg) (msgBody msg))

-- |Called to notify the finalization routine when a new block arrives.
notifyBlockArrival :: (FinalizationMonad m) => BlockPointer -> m ()
notifyBlockArrival b = do
    FinalizationState{..} <- get
    forM_ _finCurrentRound $ \FinalizationRound{..} -> do
        when (bpHeight b == _finHeight) $
            liftWMVBA $ justifyWMVBAInput (bpHash b)
        tryNominateBlock


getMyParty :: (FinalizationMonad m) => m (Maybe Party)
getMyParty = do
        myVerifyKey <- asks finMyVerifyKey
        myPublicVRFKey <- asks finMyPublicVRFKey
        ps <- parties <$> use finCommittee
        case filter (\p -> partySignKey p == myVerifyKey && partyVRFKey p == myPublicVRFKey) (Vec.toList ps) of
            (p:_) -> return $ Just p
            [] -> return Nothing


-- |Called to notify the finalization routine when a new block is finalized.
notifyBlockFinalized :: (FinalizationMonad m) => FinalizationRecord -> BlockPointer -> m ()
notifyBlockFinalized FinalizationRecord{..} bp = do
        finIndex .= finalizationIndex + 1
        let newFinDelay = if finalizationDelay > 2 then finalizationDelay `div` 2 else 1
        -- Determine if we're in the committee
        finHeight .= bpHeight bp + finalizationDelay
        mMyParty <- getMyParty
        forM_ mMyParty $ \myParty -> do
            newRound newFinDelay myParty
            
        

-- |Check that a finalization record has a valid proof
verifyFinalProof :: FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> Bool
verifyFinalProof sid com@FinalizationCommittee{..} FinalizationRecord{..} = sum (sigWeight <$> sigs) > corruptWeight
    where
        FinalizationProof sigs = finalizationProof
        hdr si = FinalizationMessageHeader {
            msgSessionId = sid,
            msgFinalizationIndex = finalizationIndex,
            msgDelta = finalizationDelay,
            msgSenderIndex = si
        }
        sigWeight (pid, sig) = if checkMessageSignature com (FinalizationMessage (hdr pid) (WMVBAWitnessCreatorMessage finalizationBlockPointer) sig)
            then partyWeight (fromJust (toParty com pid))
            else 0
