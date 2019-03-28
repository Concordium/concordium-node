{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell, LambdaCase, FlexibleContexts, MultiParamTypeClasses #-}
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
import Data.Maybe
import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad

import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.Kontrol.Monad
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Freeze (FreezeMessage(..))
import Concordium.Kontrol.BestBlock
import Concordium.Logger

data FinalizationInstance = FinalizationInstance {
    finMySignKey :: Sig.KeyPair,
    finMyVRFKey :: VRF.KeyPair
}

data Party = Party {
    partyIndex :: Word32,
    partyWeight :: Int,
    partySignKey :: Sig.VerifyKey,
    partyVRFKey :: VRF.PublicKey
} deriving (Eq, Ord)

instance Show Party where
    show = show . partyIndex

data FinalizationCommittee = FinalizationCommittee {
    parties :: Vector Party,
    totalWeight :: Int,
    corruptWeight :: Int
}

makeFinalizationCommittee :: FinalizationParameters -> FinalizationCommittee
makeFinalizationCommittee (FinalizationParameters voters) = FinalizationCommittee {..}
    where
        parties = Vec.fromList $ zipWith makeParty [0..] voters
        makeParty pix (VoterInfo psk pvk pow) = Party pix pow psk pvk
        totalWeight = sum (partyWeight <$> parties)
        corruptWeight = (totalWeight - 1) `div` 3

data FinalizationRound = FinalizationRound {
    roundInput :: Maybe BlockHash,
    roundDelta :: BlockHeight,
    roundMe :: Party,
    roundWMVBA :: WMVBAState BlockHash Party Sig.Signature
}

instance Show FinalizationRound where
    show FinalizationRound{..} = "roundInput: " ++ take 11 (show roundInput) ++ " roundDelta: " ++ show roundDelta

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


ancestorAtHeight :: BlockHeight -> BlockPointer -> BlockPointer
ancestorAtHeight h bp
    | h == bpHeight bp = bp
    | h < bpHeight bp = ancestorAtHeight h (bpParent bp)
    | otherwise = error "ancestorAtHeight: block is below required height"

data FinalizationState = FinalizationState {
    _finsSessionId :: FinalizationSessionId,
    _finsIndex :: FinalizationIndex,
    _finsHeight :: BlockHeight,
    _finsCommittee :: FinalizationCommittee,
    _finsPendingMessages :: Map FinalizationIndex (Map BlockHeight [(Word32, BS.ByteString)]),
    _finsCurrentRound :: Maybe FinalizationRound
}
makeLenses ''FinalizationState

instance Show FinalizationState where
    show FinalizationState{..} = "finIndex: " ++ show _finsIndex ++ " finHeight: " ++ show _finsHeight ++ " " ++ show _finsCurrentRound

class FinalizationStateLenses s where
    finState :: Lens' s FinalizationState
    finSessionId :: Lens' s FinalizationSessionId
    finSessionId = finState . finsSessionId
    finIndex :: Lens' s FinalizationIndex
    finIndex = finState . finsIndex
    finHeight :: Lens' s BlockHeight
    finHeight = finState . finsHeight
    finCommittee :: Lens' s FinalizationCommittee
    finCommittee = finState . finsCommittee
    finPendingMessages :: Lens' s (Map FinalizationIndex (Map BlockHeight [(Word32, BS.ByteString)]))
    finPendingMessages = finState . finsPendingMessages
    finCurrentRound :: Lens' s (Maybe FinalizationRound)
    finCurrentRound = finState . finsCurrentRound

instance FinalizationStateLenses FinalizationState where
    finState = id

initialFinalizationState :: FinalizationInstance -> BlockHash -> FinalizationCommittee -> FinalizationState
initialFinalizationState FinalizationInstance{..} genHash com = FinalizationState {
    _finsSessionId = FinalizationSessionId genHash 0,
    _finsIndex = 1,
    _finsHeight = 1,
    _finsCommittee = com,
    _finsPendingMessages = Map.empty,
    _finsCurrentRound = case filter (\p -> partySignKey p == Sig.verifyKey finMySignKey && partyVRFKey p == VRF.publicKey finMyVRFKey) (Vec.toList (parties com)) of
        [] -> Nothing
        (p:_) -> Just FinalizationRound {
            roundInput = Nothing,
            roundDelta = 1,
            roundMe = p,
            roundWMVBA = initialWMVBAState
        }
}

data FinalizationOutputEvent
    = BroadcastFinalizationMessage BS.ByteString
    | BroadcastFinalizationRecord FinalizationRecord

class (SkovMonad m) => FinalizationMonad m where
    broadcastFinalizationMessage :: BS.ByteString -> m ()
    broadcastFinalizationRecord :: FinalizationRecord -> m ()

tryNominateBlock :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => m ()
tryNominateBlock = do
    currRound <- use finCurrentRound
    forM_ currRound $ \r@FinalizationRound{..} -> 
        when (isNothing roundInput) $ do
            h <- use finHeight
            bBlock <- bestBlock
            when (bpHeight bBlock >= h + roundDelta) $ do
                let nomBlock = bpHash $ ancestorAtHeight h bBlock
                finCurrentRound ?= r {roundInput = Just nomBlock}
                liftWMVBA $ startWMVBA nomBlock

nextRound :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => FinalizationIndex -> BlockHeight -> m ()
nextRound oldFinIndex oldDelta = do
    curFinIndex <- use finIndex
    when (curFinIndex == oldFinIndex) $ do
        oldRound <- use finCurrentRound
        forM_ oldRound $ \r ->
            when (roundDelta r == oldDelta) $ do
                newRound (2 * oldDelta) (roundMe r)


newRound :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => BlockHeight -> Party -> m ()
newRound newDelta me = do
        finCurrentRound ?= FinalizationRound {
            roundInput = Nothing,
            roundDelta = newDelta,
            roundMe = me,
            roundWMVBA = initialWMVBAState
        }
        h <- use finHeight
        logEvent Afgjort LLDebug $ "Starting finalization round: height=" ++ show (theBlockHeight h) ++ " delta=" ++ show (theBlockHeight newDelta)
        justifiedInputs <- fmap (ancestorAtHeight h) <$> getBlocksAtHeight (h + newDelta)
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


handleWMVBAOutputEvents :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => [WMVBAOutputEvent BlockHash Party Sig.Signature] -> m ()
handleWMVBAOutputEvents evs = do
        FinalizationState{..} <- use finState
        FinalizationInstance{..} <- ask
        forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
            let msgHdr = FinalizationMessageHeader{
                msgSessionId = _finsSessionId,
                msgFinalizationIndex = _finsIndex,
                msgDelta = roundDelta,
                msgSenderIndex = partyIndex roundMe
            }
            let
                handleEv (SendWMVBAMessage msg0) = do
                    case msg0 of
                        WMVBAFreezeMessage (Proposal v) -> logEvent Afgjort LLDebug $ "Nominating block " ++ show v
                        _ -> return ()
                    let encMsg = runPut $ putFinalizationMessageHeader msgHdr >> putWMVBAMessage S.put putParty msg0
                    let sig = runPut $ S.put $ Sig.sign finMySignKey encMsg
                    let msg = encMsg <> sig
                    broadcastFinalizationMessage msg
                    -- We manually loop back messages here
                    receiveFinalizationMessage msg
                handleEv (WMVBAComplete Nothing) =
                    -- Round failed, so start a new one
                    nextRound _finsIndex roundDelta
                handleEv (WMVBAComplete (Just (finBlock, sigs))) = do
                    let finRec = FinalizationRecord {
                        finalizationIndex = _finsIndex,
                        finalizationBlockPointer = finBlock,
                        finalizationProof = FinalizationProof [(partyIndex p, sig) | (p, sig) <- sigs],
                        finalizationDelay = roundDelta
                    }
                    finalizeBlock finRec
                    broadcastFinalizationRecord finRec
            mapM_ handleEv evs

liftWMVBA :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => WMVBA BlockHash Party Sig.Signature a -> m a
liftWMVBA a = do
    FinalizationState{..} <- use finState
    FinalizationInstance{..} <- ask
    case _finsCurrentRound of
        Nothing -> error "No current finalization round"
        Just (fr@FinalizationRound{..}) -> do
            let
                baid = runPut $ putFinalizationSessionId _finsSessionId >> S.put _finsIndex >> S.put roundDelta
                inst = WMVBAInstance baid (totalWeight _finsCommittee) (corruptWeight _finsCommittee) partyWeight partyVRFKey roundMe finMyVRFKey
                (r, newState, evs) = runWMVBA a inst roundWMVBA
            finCurrentRound ?= fr {roundWMVBA = newState}
            handleWMVBAOutputEvents evs
            return r

-- |Called when a finalization message is received.
receiveFinalizationMessage :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => BS.ByteString -> m ()
receiveFinalizationMessage msg0 = case runGetState getFinalizationMessageHeader msg0 0 of
        Left _ -> return () -- Message header could not be decoded
        Right (hdr@FinalizationMessageHeader{..}, bodyBS) -> do
            FinalizationState{..} <- use finState
            when (_finsSessionId == msgSessionId) $ do
                case compare msgFinalizationIndex _finsIndex of
                    LT -> return () -- Message is for an old round, so discard
                    GT -> -- Message is for a future round, so save
                        finPendingMessages . at msgFinalizationIndex . non Map.empty . at msgDelta . non [] %= ((msgSenderIndex, bodyBS) :)
                    EQ -> -- Message is for current round.  Discard if we're not participating, otherwise handle
                        forM_ _finsCurrentRound $ \FinalizationRound{..} ->
                            case compare msgDelta roundDelta of
                                LT -> return ()
                                GT -> finPendingMessages . at msgFinalizationIndex . non Map.empty . at msgDelta . non [] %= ((msgSenderIndex, bodyBS) :)
                                EQ -> forM_ (decodeCheckMessage _finsCommittee hdr bodyBS) $ \msg ->
                                    forM_ (toParty _finsCommittee msgSenderIndex) $ \src ->
                                        liftWMVBA (receiveWMVBAMessage src (msgSignature msg) (msgBody msg))

-- |Called to notify the finalization routine when a new block arrives.
notifyBlockArrival :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => BlockPointer -> m ()
notifyBlockArrival b = do
    FinalizationState{..} <- use finState
    forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
        when (bpHeight b == _finsHeight + roundDelta) $
            liftWMVBA $ justifyWMVBAInput (bpHash (ancestorAtHeight _finsHeight b))
        tryNominateBlock


getMyParty :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m) => m (Maybe Party)
getMyParty = do
        myVerifyKey <- asks (Sig.verifyKey . finMySignKey)
        myPublicVRFKey <- asks (VRF.publicKey . finMyVRFKey)
        ps <- parties <$> use finCommittee
        case filter (\p -> partySignKey p == myVerifyKey && partyVRFKey p == myPublicVRFKey) (Vec.toList ps) of
            (p:_) -> return $ Just p
            [] -> return Nothing


-- |Called to notify the finalization routine when a new block is finalized.
-- (NB: this should never be called with the genesis block.)
notifyBlockFinalized :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => FinalizationRecord -> BlockPointer -> m ()
notifyBlockFinalized FinalizationRecord{..} bp = do
        finIndex .= finalizationIndex + 1
        let newFinDelay = if finalizationDelay > 2 then finalizationDelay `div` 2 else 1
        -- TODO: The next finalization height is tweaked from the specification to give better
        -- finalization lag.  This needs to be brought in line eventually.
        finHeight .= bpHeight bp + max 1 ((bpHeight bp - bpHeight (bpLastFinalized bp)) `div` 2)
        -- Determine if we're in the committee
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
