{-# LANGUAGE
    RecordWildCards,
    ScopedTypeVariables,
    TemplateHaskell,
    LambdaCase,
    FlexibleContexts,
    MultiParamTypeClasses,
    FlexibleInstances,
    FunctionalDependencies,
    RankNTypes,
    DerivingStrategies,
    DerivingVia,
    StandaloneDeriving
    #-}
module Concordium.Afgjort.Finalize (
    FinalizationMonad(..),
    FinalizationStateLenses(..),
    FinalizationInstance(..),
    FinalizationState(..),
    FinalizationSessionId(..),
    FinalizationMessage(..),
    FinalizationPseudoMessage(..),
    FinalizationMessageHeader,
    initialFinalizationState,
    verifyFinalProof,
    makeFinalizationCommittee,
    notifyBlockArrival,
    notifyBlockFinalized,
    receiveFinalizationMessage,
    receiveFinalizationPseudoMessage,
    nextFinalizationJustifierHeight,
    finalizationCatchUpMessage,
    -- * For testing
    FinalizationRound(..)
) where

import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Maybe
import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.Bits
import Data.Time.Clock
import qualified Data.OrdPSQ as PSQ
import qualified Data.ByteString as BS

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState(BlockPointerData(..))
import Concordium.Kontrol
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Freeze (FreezeMessage(..))
import Concordium.Kontrol.BestBlock
import Concordium.Logger
import Concordium.Afgjort.Finalize.Types
import Concordium.TimeMonad
import Concordium.TimerMonad

atStrict :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
atStrict k f m = f mv <&> \case
        Nothing -> maybe m (const (Map.delete k m)) mv
        Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE atStrict #-}

data FinalizationRound = FinalizationRound {
    roundInput :: !(Maybe BlockHash),
    roundDelta :: !BlockHeight,
    roundMe :: !Party,
    roundWMVBA :: !(WMVBAState Sig.Signature)
}

instance Show FinalizationRound where
    show FinalizationRound{..} = "roundInput: " ++ take 11 (show roundInput) ++ " roundDelta: " ++ show roundDelta



ancestorAtHeight :: BlockPointerData bs bp => BlockHeight -> bp -> bp
ancestorAtHeight h bp
    | h == bpHeight bp = bp
    | h < bpHeight bp = ancestorAtHeight h (bpParent bp)
    | otherwise = error "ancestorAtHeight: block is below required height"

-- TODO: Only store pending messages for at most one round in the future.

data PendingMessage = PendingMessage !Party !WMVBAMessage !Sig.Signature
    deriving (Eq, Ord, Show)

type PendingMessageMap = Map FinalizationIndex (Map BlockHeight (Set PendingMessage))

data FinalizationState timer = FinalizationState {
    _finsSessionId :: !FinalizationSessionId,
    _finsIndex :: !FinalizationIndex,
    _finsHeight :: !BlockHeight,
    _finsIndexInitialDelta :: !BlockHeight,
    _finsCommittee :: !FinalizationCommittee,
    _finsMinSkip :: !BlockHeight,
    _finsPendingMessages :: !PendingMessageMap,
    _finsCurrentRound :: !(Maybe FinalizationRound),
    _finsFailedRounds :: [Map Party Sig.Signature],
    _finsCatchUpTimer :: !(Maybe timer),
    _finsCatchUpAttempts :: !Int,
    _finsCatchUpDeDup :: !(PSQ.OrdPSQ Sig.Signature UTCTime ())
}
makeLenses ''FinalizationState

instance Show (FinalizationState timer) where
    show FinalizationState{..} = "finIndex: " ++ show (theFinalizationIndex _finsIndex) ++ " finHeight: " ++ show (theBlockHeight _finsHeight) ++ " currentRound:" ++ show _finsCurrentRound
        ++ "\n pendingMessages:" ++ show (Map.toList $ fmap (Map.toList . fmap Set.size)  _finsPendingMessages)

class FinalizationStateLenses s timer | s -> timer where
    finState :: Lens' s (FinalizationState timer)
    finSessionId :: Lens' s FinalizationSessionId
    finSessionId = finState . finsSessionId
    finIndex :: Lens' s FinalizationIndex
    finIndex = finState . finsIndex
    finHeight :: Lens' s BlockHeight
    finHeight = finState . finsHeight
    -- |The round delta for the starting round at the current finalization index.
    finIndexInitialDelta :: Lens' s BlockHeight
    finIndexInitialDelta = finState . finsIndexInitialDelta
    finCommittee :: Lens' s FinalizationCommittee
    finCommittee = finState . finsCommittee
    -- |The minimum distance between finalized blocks will be @1 + finMinSkip@.
    finMinSkip :: Lens' s BlockHeight
    finMinSkip = finState . finsMinSkip
    -- |All received finalization messages for the current and future finalization indexes.
    -- (Previously, this was just future messages, but now we store all of them for catch-up purposes.)
    finPendingMessages :: Lens' s PendingMessageMap
    finPendingMessages = finState . finsPendingMessages
    finCurrentRound :: Lens' s (Maybe FinalizationRound)
    finCurrentRound = finState . finsCurrentRound
    -- |For each failed round (from most recent to oldest), signatures
    -- on @WeAreDone False@ proving failure.
    finFailedRounds :: Lens' s [Map Party Sig.Signature]
    finFailedRounds = finState . finsFailedRounds
    finCatchUpTimer :: Lens' s (Maybe timer)
    finCatchUpTimer = finState . finsCatchUpTimer
    finCatchUpAttempts :: Lens' s Int
    finCatchUpAttempts = finState . finsCatchUpAttempts
    finCatchUpDeDup :: Lens' s (PSQ.OrdPSQ Sig.Signature UTCTime ())
    finCatchUpDeDup = finState . finsCatchUpDeDup

instance FinalizationStateLenses (FinalizationState m) m where
    finState = id

initialFinalizationState :: FinalizationInstance -> BlockHash -> FinalizationParameters -> FinalizationState timer
initialFinalizationState FinalizationInstance{..} genHash finParams = FinalizationState {
    _finsSessionId = FinalizationSessionId genHash 0,
    _finsIndex = 1,
    _finsHeight = 1 + finalizationMinimumSkip finParams,
    _finsIndexInitialDelta = 1,
    _finsCommittee = com,
    _finsMinSkip = finalizationMinimumSkip finParams,
    _finsPendingMessages = Map.empty,
    _finsCurrentRound = case filter (\p -> partySignKey p == Sig.verifyKey finMySignKey && partyVRFKey p == VRF.publicKey finMyVRFKey) (Vec.toList (parties com)) of
        [] -> Nothing
        (p:_) -> Just FinalizationRound {
            roundInput = Nothing,
            roundDelta = 1,
            roundMe = partyIndex p,
            roundWMVBA = initialWMVBAState
        },
    _finsFailedRounds = [],
    _finsCatchUpTimer = Nothing,
    _finsCatchUpAttempts = 0,
    _finsCatchUpDeDup = PSQ.empty
    }
    where
        com = makeFinalizationCommittee finParams

class (SkovMonad m, MonadState s m, FinalizationStateLenses s (Timer m), MonadIO m, TimerMonad m) => FinalizationMonad s m where
    broadcastFinalizationMessage :: FinalizationMessage -> m ()
    broadcastFinalizationMessage = broadcastFinalizationPseudoMessage . FPMMessage
    {-# INLINE broadcastFinalizationMessage #-}
    broadcastFinalizationPseudoMessage :: FinalizationPseudoMessage -> m ()
    broadcastFinalizationRecord :: FinalizationRecord -> m ()
    getFinalizationInstance :: m FinalizationInstance
    -- resetCatchUpTimer :: Maybe NominalDiffTime -> m ()

doResetTimer :: (FinalizationMonad s m) => m ()
doResetTimer = do
        oldTimer <- finCatchUpTimer <<.= Nothing
        forM_ oldTimer cancelTimer
        curRound <- use finCurrentRound
        forM_ curRound $ \FinalizationRound{..} ->
            let spawnTimer = do
                    attempts <- use finCatchUpAttempts
                    timer <- onTimeout (DelayFor $ fromIntegral (attempts + 1) * (300 + 5 * fromIntegral roundMe)) $ do
                        finInst <- getFinalizationInstance
                        finSt <- get
                        mapM_ broadcastFinalizationPseudoMessage (finalizationCatchUpMessage finInst finSt)
                        finCatchUpAttempts %= (+1)
                        spawnTimer
                    finCatchUpTimer ?= timer
            in spawnTimer

tryNominateBlock :: (FinalizationMonad s m) => m ()
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

nextRound :: (FinalizationMonad s m) => FinalizationIndex -> BlockHeight -> m ()
nextRound oldFinIndex oldDelta = do
    curFinIndex <- use finIndex
    when (curFinIndex == oldFinIndex) $ do
        oldRound <- use finCurrentRound
        forM_ oldRound $ \r ->
            when (roundDelta r == oldDelta) $ do
                finFailedRounds %= (wmvbaWADBot (roundWMVBA r) :)
                newRound (2 * oldDelta) (roundMe r)


newRound :: (FinalizationMonad s m) => BlockHeight -> Party -> m ()
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
        committee <- use finCommittee
        sessId <- use finSessionId
        let
            msgHdr src = FinalizationMessageHeader {
                msgSessionId = sessId,
                msgFinalizationIndex = finIx,
                msgDelta = newDelta,
                msgSenderIndex = src
            }
            toFinMsg (PendingMessage src msg sig) = FinalizationMessage (msgHdr src) msg sig
        -- Filter the messages that have valid signatures and reference legitimate parties
        pmsgs <- finPendingMessages . atStrict finIx . non Map.empty . atStrict newDelta . non Set.empty <%= Set.filter (checkMessage committee . toFinMsg)
        -- Justify the blocks
        forM_ justifiedInputs $ \i -> do
            logEvent Afgjort LLTrace $ "Justified input at " ++ show finIx ++ ": " ++ show i
            liftWMVBA $ justifyWMVBAInput $ bpHash i
        -- Receive the pending messages
        forM_ pmsgs $ \smsg@(PendingMessage src msg sig) -> do
            logEvent Afgjort LLDebug $ "Handling message: " ++ show (toFinMsg smsg)
            liftWMVBA $ receiveWMVBAMessage src sig msg
        tryNominateBlock


handleWMVBAOutputEvents :: (FinalizationMonad s m) => [WMVBAOutputEvent Sig.Signature] -> m ()
handleWMVBAOutputEvents evs = do
        FinalizationState{..} <- use finState
        FinalizationInstance{..} <- getFinalizationInstance
        forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
            let msgHdr = FinalizationMessageHeader{
                msgSessionId = _finsSessionId,
                msgFinalizationIndex = _finsIndex,
                msgDelta = roundDelta,
                msgSenderIndex = roundMe
            }
            let
                handleEvs _ [] = return ()
                handleEvs b (SendWMVBAMessage msg0 : evs') = do
                    case msg0 of
                        WMVBAFreezeMessage (Proposal v) -> logEvent Afgjort LLDebug $ "Nominating block " ++ show v
                        _ -> return ()
                    let msg = signFinalizationMessage finMySignKey msgHdr msg0
                    broadcastFinalizationMessage msg
                    -- We manually loop back messages here
                    _ <- receiveFinalizationMessage msg
                    finCatchUpAttempts .= 0
                    doResetTimer
                    handleEvs b evs'
                handleEvs False (WMVBAComplete Nothing : evs') = do
                    -- Round failed, so start a new one
                    nextRound _finsIndex roundDelta
                    handleEvs True evs'
                handleEvs False (WMVBAComplete (Just (finBlock, (parties, sig))) : evs') = do
                    let finRec = FinalizationRecord {
                        finalizationIndex = _finsIndex,
                        finalizationBlockPointer = finBlock,
                        finalizationProof = FinalizationProof (parties, sig),
                        finalizationDelay = roundDelta
                    }
                    _ <- finalizeBlock finRec
                    broadcastFinalizationRecord finRec
                    handleEvs True evs'
                handleEvs True (WMVBAComplete _ : evs') = handleEvs True evs'
            handleEvs False evs

liftWMVBA :: (FinalizationMonad s m) => WMVBA Sig.Signature a -> m a
liftWMVBA a = do
    FinalizationState{..} <- use finState
    FinalizationInstance{..} <- getFinalizationInstance
    case _finsCurrentRound of
        Nothing -> error "No current finalization round"
        Just fr@FinalizationRound{..} -> do
            let
                baid = runPut $ S.put _finsSessionId >> S.put _finsIndex >> S.put roundDelta
                pWeight party = partyWeight (parties _finsCommittee Vec.! fromIntegral party)
                pVRFKey party = partyVRFKey (parties _finsCommittee Vec.! fromIntegral party)
                pBlsKey party = partyBlsKey (parties _finsCommittee Vec.! fromIntegral party)
                maxParty = fromIntegral $ Vec.length (parties _finsCommittee) - 1
                inst = WMVBAInstance baid (totalWeight _finsCommittee) (corruptWeight _finsCommittee) pWeight maxParty pVRFKey roundMe finMyVRFKey pBlsKey finMyBlsKey
            (r, newState, evs) <- liftIO $ runWMVBA a inst roundWMVBA
            finCurrentRound ?= fr {roundWMVBA = newState}
            -- logEvent Afgjort LLTrace $ "New WMVBA state: " ++ show newState
            handleWMVBAOutputEvents evs
            return r

-- |Determine if a message references blocks requiring Skov to catch up.
messageRequiresCatchUp :: (FinalizationMonad s m) => WMVBAMessage -> m Bool
messageRequiresCatchUp msg = rcu (messageValues msg)
    where
        rcu [] = return False
        rcu (b : bs) = resolveBlock b >>= \case
            Nothing -> return True
            Just _ -> do
                FinalizationState{..} <- use finState
                r <- forM _finsCurrentRound $ \_ -> liftWMVBA $ isJustifiedWMVBAInput b
                if fromMaybe True r then
                    rcu bs
                else
                    return True

savePendingMessage :: (FinalizationMonad s m) => FinalizationIndex -> BlockHeight -> PendingMessage -> m Bool
savePendingMessage finIx finDelta pmsg = do
    pmsgs <- use finPendingMessages
    case Map.lookup finIx pmsgs of
        Nothing -> do
            finPendingMessages .= Map.insert finIx (Map.singleton finDelta $ Set.singleton pmsg) pmsgs
            return False
        Just ipmsgs -> case Map.lookup finDelta ipmsgs of
            Nothing -> do
                finPendingMessages .= Map.insert finIx (Map.insert finDelta (Set.singleton pmsg) ipmsgs) pmsgs
                return False
            Just s -> if pmsg `Set.member` s then
                    return True
                else do
                    finPendingMessages .= Map.insert finIx (Map.insert finDelta (Set.insert pmsg s) ipmsgs) pmsgs
                    return False

-- |Called when a finalization message is received.
receiveFinalizationMessage :: (FinalizationMonad s m) => FinalizationMessage -> m UpdateResult
receiveFinalizationMessage msg@FinalizationMessage{msgHeader=FinalizationMessageHeader{..},..} = do
        FinalizationState{..} <- use finState
        -- Check this is the right session
        if _finsSessionId == msgSessionId then
            -- Check the finalization index is not out of date
            case compare msgFinalizationIndex _finsIndex of
                LT -> return ResultStale -- message is out of date
                GT -> -- Message is from the future; consider it invalid if it's not the index after the current one.
                    if msgFinalizationIndex - _finsIndex < 2 then do
                        -- Save the message for a later finalization index
                        isDuplicate <- savePendingMessage msgFinalizationIndex msgDelta (PendingMessage msgSenderIndex msgBody msgSignature)
                        if isDuplicate then
                            return ResultDuplicate
                        else do
                            -- Since we're behind, request the finalization record we're apparently missing
                            logEvent Afgjort LLDebug $ "Missing finalization at index " ++ show (msgFinalizationIndex - 1)
                            return ResultPendingFinalization
                    else
                        return ResultInvalid
                EQ -> -- handle the message now, since it's the current round
                    if checkMessage _finsCommittee msg then do
                        -- Save the message
                        isDuplicate <- savePendingMessage msgFinalizationIndex msgDelta (PendingMessage msgSenderIndex msgBody msgSignature)
                        if isDuplicate then
                            return ResultDuplicate
                        else do
                            -- Check if we're participating in finalization for this index
                            forM_ _finsCurrentRound $ \FinalizationRound{..} ->
                                -- And it's the current round
                                when (msgDelta == roundDelta) $ do
                                    logEvent Afgjort LLDebug $ "Handling message: " ++ show msg
                                    liftWMVBA (receiveWMVBAMessage msgSenderIndex msgSignature msgBody)
                            rcu <- messageRequiresCatchUp msgBody
                            if rcu then do
                                logEvent Afgjort LLDebug $ "Message refers to unjustified block; catch-up required."
                                return ResultPendingBlock
                            else
                                return ResultSuccess
                    else do
                        logEvent Afgjort LLWarning $ "Received bad finalization message"
                        return ResultInvalid
            else
                return ResultIncorrectFinalizationSession

-- |Called when a finalization pseudo-message is received.
receiveFinalizationPseudoMessage :: (FinalizationMonad s m) => FinalizationPseudoMessage -> m UpdateResult
receiveFinalizationPseudoMessage (FPMMessage msg) = receiveFinalizationMessage msg
receiveFinalizationPseudoMessage (FPMCatchUp cu@CatchUpMessage{..}) = do
        FinalizationState{..} <- use finState
        if _finsSessionId == cuSessionId then
            case compare cuFinalizationIndex _finsIndex of
                LT -> return ResultStale
                GT -> return ResultUnverifiable
                EQ -> if checkCatchUpMessageSignature _finsCommittee cu then do
                        now <- currentTime
                        oldDeDup <- use finCatchUpDeDup
                        let
                            (_, purgedDeDup) = PSQ.atMostView (addUTCTime (-60) now) oldDeDup
                            alterfun Nothing = (False, Just (now, ()))
                            alterfun (Just _) = (True, Just (now, ()))
                            (isDup, newDeDup) = PSQ.alter alterfun cuSignature purgedDeDup
                        finCatchUpDeDup .= newDeDup
                        if isDup then
                            return ResultDuplicate
                        else do
                            logEvent Afgjort LLTrace $ "Processing finalization summary"
                            CatchUpResult{..} <- processFinalizationSummary cuFinalizationSummary
                            unless curBehind doResetTimer
                            if curSkovCatchUp then
                                return ResultPendingBlock
                            else
                                return ResultSuccess
                    else
                        return ResultInvalid
        else
            return ResultIncorrectFinalizationSession


-- |Called to notify the finalization routine when a new block arrives.
notifyBlockArrival :: (FinalizationMonad s m, BlockPointerData bs bp) => bp -> m ()
notifyBlockArrival b = do
    FinalizationState{..} <- use finState
    forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
        when (bpHeight b == _finsHeight + roundDelta) $ do
            logEvent Afgjort LLTrace $ "Justified input at " ++ show _finsIndex ++ ": " ++ show (bpHash (ancestorAtHeight _finsHeight b))
            liftWMVBA $ justifyWMVBAInput (bpHash (ancestorAtHeight _finsHeight b))
        tryNominateBlock


getMyParty :: (FinalizationMonad s m) => m (Maybe Party)
getMyParty = do
        finInst <- getFinalizationInstance
        let
            myVerifyKey = (Sig.verifyKey . finMySignKey) finInst
            myPublicVRFKey = (VRF.publicKey . finMyVRFKey) finInst
        ps <- parties <$> use finCommittee
        case filter (\p -> partySignKey p == myVerifyKey && partyVRFKey p == myPublicVRFKey) (Vec.toList ps) of
            (p:_) -> return $ Just (partyIndex p)
            [] -> return Nothing


-- |Called to notify the finalization routine when a new block is finalized.
-- (NB: this should never be called with the genesis block.)
notifyBlockFinalized :: (FinalizationMonad s m, BlockPointerData bs bp) => FinalizationRecord -> bp -> m ()
notifyBlockFinalized fr@FinalizationRecord{..} bp = do
        -- Reset catch-up timer
        oldTimer <- finCatchUpTimer <<.= Nothing
        forM_ oldTimer cancelTimer
        finCatchUpAttempts .= 0
        -- Reset the deduplication buffer
        finCatchUpDeDup .= PSQ.empty
        -- Move to next index
        finIndex .= finalizationIndex + 1
        -- Discard finalization messages from old round
        finPendingMessages . atStrict finalizationIndex .= Nothing
        pms <- use finPendingMessages
        logEvent Afgjort LLTrace $ "Finalization complete. Pending messages: " ++ show pms
        let newFinDelay = nextFinalizationDelay fr
        fs <- use finMinSkip
        finHeight .= nextFinalizationHeight fs bp
        finIndexInitialDelta .= newFinDelay
        -- Determine if we're in the committee
        mMyParty <- getMyParty
        forM_ mMyParty $ \myParty -> do
            finFailedRounds .= []
            newRound newFinDelay myParty

nextFinalizationDelay :: FinalizationRecord -> BlockHeight
nextFinalizationDelay FinalizationRecord{..} = if finalizationDelay > 2 then finalizationDelay `div` 2 else 1

-- |Given the finalization minimum skip and an explicitly finalized block, compute
-- the height of the next finalized block.
nextFinalizationHeight :: (BlockPointerData bs bp)
    => BlockHeight -- ^Finalization minimum skip
    -> bp -- ^Last finalized block
    -> BlockHeight
nextFinalizationHeight fs bp = bpHeight bp + max (1 + fs) ((bpHeight bp - bpHeight (bpLastFinalized bp)) `div` 2)

-- |The height that a chain must be for a block to be eligible for finalization.
-- This is the next finalization height + the next finalization delay.
nextFinalizationJustifierHeight :: (BlockPointerData bs bp)
    => FinalizationParameters
    -> FinalizationRecord -- ^Last finalization record
    -> bp -- ^Last finalized block
    -> BlockHeight
nextFinalizationJustifierHeight fp fr bp = nextFinalizationHeight (finalizationMinimumSkip fp) bp + nextFinalizationDelay fr

getPartyWeight :: FinalizationCommittee -> Party -> VoterPower
getPartyWeight com pid = case parties com ^? ix (fromIntegral pid) of
        Nothing -> 0
        Just p -> partyWeight p

-- |Check that a finalization record has a valid proof
verifyFinalProof :: FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> Bool
verifyFinalProof sid com@FinalizationCommittee{..} FinalizationRecord{..} =
  if sigWeight finParties > corruptWeight then checkProofSignature sig else False
    where
        FinalizationProof (finParties, sig) = finalizationProof
        toSign = BS.append (runPut $ S.put sid >> S.put finalizationIndex >> S.put finalizationDelay) (runPut $ S.put finalizationBlockPointer)
        pks = foldl (\s pid -> case (toPartyInfo com pid) of Just info -> (partyBlsKey $ info) : s
                                                             Nothing -> s) [] finParties
        checkProofSignature s = Bls.verifyAggregate toSign pks s
        sigWeight ps = foldl (\s p -> s + (getPartyWeight com p)) 0 ps

-- |Produce a 'FinalizationSummary' based on the finalization state.
finalizationSummary :: (FinalizationStateLenses s m) => SimpleGetter s FinalizationSummary
finalizationSummary = to fs
    where
        fs s = FinalizationSummary{..}
            where
                summaryFailedRounds = reverse $ s ^. finFailedRounds
                summaryCurrentRound = case s ^. finCurrentRound of
                    Nothing -> WMVBASummary Nothing Nothing Nothing
                    Just FinalizationRound{..} -> roundWMVBA ^. wmvbaSummary

-- |Produce a 'FinalizationPseudoMessage' containing a catch up message based on the current finalization state.
finalizationCatchUpMessage :: (FinalizationStateLenses s m) => FinalizationInstance -> s -> Maybe FinalizationPseudoMessage
finalizationCatchUpMessage FinalizationInstance{..} s = _finsCurrentRound <&> \FinalizationRound{..} ->
        FPMCatchUp $! signCatchUpMessage finMySignKey _finsSessionId _finsIndex roundMe (committeeMaxParty _finsCommittee) summary
    where
        FinalizationState{..} = s ^. finState
        summary = s ^. finalizationSummary

-- |Process a 'FinalizationSummary', handling any new messages and returning a result indicating
-- whether the summary is behind, and whether we should initiate Skov catch-up.
processFinalizationSummary :: (FinalizationMonad s m) => FinalizationSummary -> m CatchUpResult
processFinalizationSummary FinalizationSummary{..} =
        use finCurrentRound >>= \case
            Nothing -> return mempty
            Just _ -> do
                FinalizationInstance{..} <- getFinalizationInstance
                committee@FinalizationCommittee{..} <- use finCommittee
                initDelta <- use finIndexInitialDelta
                msgSessionId <- use finSessionId
                msgFinalizationIndex <- use finIndex
                let
                    mkFinalizationMessage :: BlockHeight -> Party -> WMVBAMessage -> Sig.Signature -> FinalizationMessage
                    mkFinalizationMessage msgDelta msgSenderIndex = FinalizationMessage FinalizationMessageHeader{..}
                    checkSigDelta :: BlockHeight -> Party -> WMVBAMessage -> Sig.Signature -> Bool
                    checkSigDelta msgDelta msgSenderIndex msg sig = checkMessageSignature committee (mkFinalizationMessage msgDelta msgSenderIndex msg sig)
                roundsBehind <- forM (zip [0..] summaryFailedRounds) $
                    \(roundIndex, m) -> let delta = BlockHeight (shiftL (theBlockHeight initDelta) roundIndex) in use finCurrentRound >>= \case
                    -- Note, we need to get the current round each time, because processing might advance the round
                    Nothing -> return False
                    Just curRound -> case compare delta (roundDelta curRound) of
                            LT -> do
                                -- The round should already be failed for us
                                -- Just check the signatures to see if it is behind.
                                let
                                    -- TODO: Use existing signatures to short-cut signature checking
                                    checkSig party sig = checkSigDelta delta party wmvbaWADBotMessage sig
                                    cur' = Map.filterWithKey checkSig m
                                -- We consider it behind if it doesn't include (n-t) valid signatures
                                return $ sum (getPartyWeight committee <$> Map.keys cur') < totalWeight - corruptWeight
                            EQ -> -- This is our current round, so create a WMVBASummary and process that
                                curBehind <$> liftWMVBA (processWMVBASummary (wmvbaFailedSummary m) (checkSigDelta delta))
                            GT -> -- This case shouldn't happen unless the message is corrupt.
                                return False
                let delta = BlockHeight (shiftL (theBlockHeight initDelta) (length summaryFailedRounds))
                use finCurrentRound >>= \case
                    Nothing -> return mempty
                    Just curRound -> case compare delta (roundDelta curRound) of
                        LT -> return (CatchUpResult {curBehind = True, curSkovCatchUp = False})
                        EQ -> do
                            cur <- liftWMVBA $ processWMVBASummary summaryCurrentRound (checkSigDelta delta)
                            return (cur <> mempty {curBehind = or roundsBehind})
                        GT -> return (mempty {curBehind = or roundsBehind})
