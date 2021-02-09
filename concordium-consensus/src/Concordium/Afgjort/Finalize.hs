{-# LANGUAGE
    BangPatterns,
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
    StandaloneDeriving,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    UndecidableInstances,
    TypeFamilies
    #-}
module Concordium.Afgjort.Finalize (
    FinalizationStateMonad,
    FinalizationMonad(..),
    FinalizationStateLenses(..),
    FinalizationInstance(..),
    FinalizationState(..),
    FinalizationSessionId(..),
    FinalizationMessage(..),
    FinalizationPseudoMessage(..),
    FinalizationMessageHeader,
    FinalizationCurrentRound(..),
    recoverFinalizationState,
    initialFinalizationState,
    initialPassiveFinalizationState,
    verifyFinalProof,
    makeFinalizationCommittee,
    nextFinalizationRecord,
    ActiveFinalizationM(..),
    -- * For testing
    FinalizationRound(..),
    onActiveCurrentRound,
    RoundInput(..),
    nextFinalizationDelay
) where

import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe
import Lens.Micro.Platform
import Control.Applicative ((<|>))
import Control.Monad.State.Class
import Control.Monad.State.Strict (runState)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import qualified Data.Sequence as Seq
import Data.Bits
import Data.Time.Clock
import qualified Data.OrdPSQ as PSQ

import Concordium.Utils
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Block
import Concordium.Kontrol
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Freeze (FreezeMessage(..))
import Concordium.Afgjort.FinalizationQueue
import qualified Concordium.Afgjort.PartyMap as PM
import Concordium.Kontrol.BestBlock
import Concordium.Logger
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.Monad
import Concordium.TimeMonad
import Concordium.TimerMonad

-- |Input to the round, either a block hash or nothing.
-- NB: It is important that the block hash field here is strict.
data RoundInput = RoundInput !BlockHash | NoInput
  deriving(Eq, Show)

{-# INLINE checkNoInput #-}
-- |Check whether the current round has no input.
checkNoInput :: RoundInput -> Bool
checkNoInput NoInput = True
checkNoInput _ = False

data FinalizationRound = FinalizationRound {
    roundInput :: !RoundInput,
    roundDelta :: !BlockHeight,
    roundMe :: !Party,
    roundWMVBA :: !(WMVBAState Sig.Signature)
} deriving(Eq)

instance Show FinalizationRound where
    show FinalizationRound{..} = "roundInput: " ++ take 11 (show roundInput) ++ " roundDelta: " ++ show roundDelta

newtype PassiveFinalizationRound = PassiveFinalizationRound {
    passiveWitnesses :: (Map BlockHeight WMVBAPassiveState)
} deriving (Eq, Show)

initialPassiveFinalizationRound :: PassiveFinalizationRound
initialPassiveFinalizationRound = PassiveFinalizationRound Map.empty

ancestorAtHeight :: (GlobalStateTypes m, BlockPointerMonad m) => BlockHeight -> BlockPointerType m -> m (BlockPointerType m)
ancestorAtHeight h bp
    | h == bpHeight bp = return bp
    | h < bpHeight bp = do
        parent <- bpParent bp
        ancestorAtHeight h parent
    | otherwise = error "ancestorAtHeight: block is below required height"

-- TODO: Only store pending messages for at most one round in the future.
-- TODO: Revise what pending messages we store. Catch-up no longer is based
-- on pending messages.

data PendingMessage = PendingMessage !Party !WMVBAMessage !Sig.Signature
    deriving (Eq, Ord, Show)

type PendingMessageMap = Map FinalizationIndex (Map BlockHeight (Set PendingMessage))

-- |Information about the current round, whether we are actively participating
-- or not.
-- NB: It is very important that this has strict fields.
data FinalizationCurrentRound =
  PassiveCurrentRound !PassiveFinalizationRound
  | ActiveCurrentRound !FinalizationRound
  deriving(Eq, Show)

-- |Only perform an action on an active round.
onActiveCurrentRound :: Applicative f => FinalizationCurrentRound -> (FinalizationRound -> f ()) -> f ()
onActiveCurrentRound (ActiveCurrentRound cr) f = () <$ f cr
onActiveCurrentRound _ _ = pure ()

-- |State of the finalization. In the documentation of the fields "current" refers
-- to the round currently being executed, or if we are waiting for the tree to grow,
-- the upcoming finalization round.
data FinalizationState timer = FinalizationState {
    -- |Session identifier of the current session.
    _finsSessionId :: !FinalizationSessionId,
    -- |Index of the current finalization.
    _finsIndex :: !FinalizationIndex,
    -- |Height at which we are supposed to finalize in the current round.
    _finsHeight :: !BlockHeight,
    -- |How many descendants a block should have before it is considered
    -- for finalization at the beginning of the round.
    _finsIndexInitialDelta :: !BlockHeight,
    -- |Finalization committee for the current round.
    _finsCommittee :: !FinalizationCommittee,
    -- TODO: Remove this; replaced by direct access to FinalizationParameters
    _finsMinSkip :: !BlockHeight,
    -- |Gap (difference in height of blocks) after the last finalized block
    -- before finalization should start again.
    _finsGap :: !BlockHeight,
    _finsPendingMessages :: !PendingMessageMap,
    _finsCurrentRound :: !FinalizationCurrentRound,
    _finsFailedRounds :: ![Map Party Sig.Signature],
    _finsCatchUpTimer :: !(Maybe timer),
    _finsCatchUpAttempts :: !Int,
    _finsCatchUpDeDup :: !(PSQ.OrdPSQ Sig.Signature UTCTime ()),
    -- |Queue of finalization records that have not yet been included in blocks.
    _finsQueue :: !FinalizationQueue
} deriving(Eq)
makeLenses ''FinalizationState

-- |Recover finalization state from the tree state.
-- This method looks up the last finalized block in the tree state
-- and the surrounding context and constructs the finalization state the
-- finaler should use to continue from this point onward.
--
-- If a finalization instance is provided this function will try to construct an
-- active finalization round, provided the keys of the instance are in the
-- committee as well. If no finalization instance is given a passive
-- finalization round is constructed.
-- NB: This function should for now only be used in a tree state that has no branches,
-- and will raise an exception otherwise.
recoverFinalizationState :: (MonadIO m, SkovQueryMonad m, BlockPointerMonad m)
                         => Maybe FinalizationInstance
                         -> m (FinalizationState timer)
recoverFinalizationState mfinInstance = do
  (lastFinBlockPointer, lastFinRec) <- lastFinalizedBlockWithRecord
  currentHeight <- getCurrentHeight
  unless (currentHeight == bpHeight lastFinBlockPointer) $ do
    let msg = "Precondition violation: Trying to recover finalization state in a tree state with branches. This is currently unsupported."
    liftIO (throwIO (userError msg))

  finParams <- getFinalizationParameters
  let lastFinIndex = finalizationIndex lastFinRec

  -- Block finalized at the previous index (if applicable)
  prevFinalized <-
    if lastFinIndex == 0 then
      return lastFinBlockPointer
    else fromMaybe
         (error "Invariant violation: previous finalization index does not exist.")
         <$> (blockAtFinIndex (lastFinIndex - 1))

  -- Gap between the last two finalizations. This will be 0 if
  -- last finalized block is genesis, and
  -- the new gap will be 1 + finalizationMinimumSkip.
  let oldGap = bpHeight lastFinBlockPointer - bpHeight prevFinalized
  _finsGap <- nextFinalizationGap lastFinBlockPointer finParams oldGap
  _finsCommittee <- getFinalizationCommittee lastFinBlockPointer

  genHash <- bpHash <$> genesisBlock

  -- Find the finalization index of the first finalization record that is not
  -- yet included in a finalized block.
  let findFirstUnsettledIndex curPtr =
          case blockFinalizationData <$> blockFields curPtr of
            Nothing -> return 1 -- we've reached the genesis block
            Just NoFinalizationData -> findFirstUnsettledIndex =<< bpParent curPtr
            Just (BlockFinalizationData fRec) -> return (finalizationIndex fRec + 1)

  let sessId = FinalizationSessionId genHash 0 -- FIXME: This should not be hardcoded

  -- load all unsettled records and construct the initial
  -- finalization queue. Note that the queue might be empty.
  finalizationQueue <- do
    firstIdx <- findFirstUnsettledIndex lastFinBlockPointer
    proofs <-
      forM [firstIdx..finalizationIndex lastFinRec] $ \idx ->
        recordAtFinIndex idx >>= \case
          Nothing -> do
            error $ "Invariant violation. Lost block at finalization index " ++ show idx
          Just fRec -> do
            -- Finalization committee used to produce the record.
            -- This is, right now at least, the committee that finalized
            -- at the last finalization index.
            -- idx - 1 is fine here because findFirstUnsettledIndex is at least 1
            blockAtFinIndex (idx - 1) >>= \case
              Nothing -> error $ "Invariant violation: Missing block at known finalization index: " ++ show (idx - 1)
              Just bp ->
                do committee <- getFinalizationCommittee bp
                   return (newFinalizationProven sessId committee fRec)
    return $ FinalizationQueue firstIdx (Seq.fromList proofs)

  let _finsSessionId = sessId
      _finsIndexInitialDelta = nextFinalizationDelay finParams lastFinRec
      _finsHeight = bpHeight lastFinBlockPointer + _finsGap
      _finsMinSkip = finalizationMinimumSkip finParams
      _finsPendingMessages = Map.empty
      _finsFailedRounds = []
      _finsCatchUpTimer = Nothing
      _finsCatchUpAttempts = 0
      _finsCatchUpDeDup = PSQ.empty
      -- genesis block is finalized by definition and it does not have a valid
      -- finalization record, so it should not be included in subsequent blocks
      _finsQueue = finalizationQueue
      _finsIndex = finalizationIndex lastFinRec + 1
      _finsCurrentRound =
        case mfinInstance of
          Just finInstance
              | Just me <- findInstanceInCommittee finInstance _finsCommittee ->
                  ActiveCurrentRound (FinalizationRound {
                                         roundInput = NoInput,
                                         roundDelta = _finsIndexInitialDelta,
                                         roundMe = me,
                                         roundWMVBA = initialWMVBAState
                                         }
                                     )
          -- NB: The below should be correct since we don't have any pending messages stored.
          _ -> PassiveCurrentRound initialPassiveFinalizationRound
  return FinalizationState{..}

instance Show (FinalizationState timer) where
    show FinalizationState{..} = "finIndex: " ++ show (theFinalizationIndex _finsIndex) ++ " finHeight: " ++ show (theBlockHeight _finsHeight) ++ " currentRound:" ++ show _finsCurrentRound
        ++ "\n pendingMessages:" ++ show (Map.toList $ fmap (Map.toList . fmap Set.size)  _finsPendingMessages)
        ++ "\n finQueue: " ++ show _finsQueue

class FinalizationQueueLenses s => FinalizationStateLenses s timer | s -> timer where
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
    finGap :: Lens' s BlockHeight
    finGap = finState . finsGap
    -- |All received finalization messages for the current and future finalization indexes.
    -- (Previously, this was just future messages, but now we store all of them for catch-up purposes.)
    finPendingMessages :: Lens' s PendingMessageMap
    finPendingMessages = finState . finsPendingMessages
    finCurrentRound :: Lens' s FinalizationCurrentRound
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

instance FinalizationQueueLenses (FinalizationState m) where
    finQueue = finsQueue

instance FinalizationStateLenses (FinalizationState m) m where
    finState = id

initialPassiveFinalizationState :: BlockHash -> FinalizationParameters -> FullBakers -> Amount -> FinalizationState timer
initialPassiveFinalizationState genHash finParams genBakers totalGTU = FinalizationState {
    _finsSessionId = FinalizationSessionId genHash 0,
    _finsIndex = 1,
    _finsHeight = initialGap,
    _finsIndexInitialDelta = 1,
    _finsCommittee = makeFinalizationCommittee finParams totalGTU genBakers,
    _finsMinSkip = finalizationMinimumSkip finParams,
    _finsGap = initialGap,
    _finsPendingMessages = Map.empty,
    _finsCurrentRound = PassiveCurrentRound initialPassiveFinalizationRound,
    _finsFailedRounds = [],
    _finsCatchUpTimer = Nothing,
    _finsCatchUpAttempts = 0,
    _finsCatchUpDeDup = PSQ.empty,
    _finsQueue = initialFinalizationQueue
    }
    where
        initialGap = 1 + finalizationMinimumSkip finParams
{-# INLINE initialPassiveFinalizationState #-}

initialFinalizationState :: FinalizationInstance -> BlockHash -> FinalizationParameters -> FullBakers -> Amount -> FinalizationState timer
initialFinalizationState FinalizationInstance{..} genHash finParams genBakers totalGTU = (initialPassiveFinalizationState genHash finParams genBakers totalGTU) {
    _finsCurrentRound = case Vec.find (\p -> partySignKey p == Sig.verifyKey finMySignKey && partyVRFKey p == VRF.publicKey finMyVRFKey) (parties com) of
        Nothing -> PassiveCurrentRound initialPassiveFinalizationRound
        Just p -> ActiveCurrentRound FinalizationRound {
            roundInput = NoInput,
            roundDelta = 1,
            roundMe = partyIndex p,
            roundWMVBA = initialWMVBAState
        }
    }
    where
        com = makeFinalizationCommittee finParams totalGTU genBakers

getFinalizationInstance :: (MonadReader r m, HasFinalizationInstance r) => m (Maybe FinalizationInstance)
getFinalizationInstance = asks finalizationInstance

type FinalizationStateMonad r s m = (MonadState s m, FinalizationStateLenses s (Timer m), MonadReader r m, HasFinalizationInstance r)

type FinalizationBaseMonad r s m = (BlockPointerMonad m, SkovMonad m, FinalizationStateMonad r s m, MonadIO m, TimerMonad m, FinalizationOutputMonad m)

-- |This sets the base time for triggering finalization replay.
finalizationReplayBaseDelay :: NominalDiffTime
finalizationReplayBaseDelay = 300

-- |This sets the per-party additional delay for finalization replay.
--
finalizationReplayStaggerDelay :: NominalDiffTime
finalizationReplayStaggerDelay = 5

-- |Reset the finalization catch-up timer.  This is called when progress is
-- made in finalization (i.e. we produce a message).
doResetTimer :: (FinalizationBaseMonad r s m) => m ()
doResetTimer = do
        oldTimer <- finCatchUpTimer <<.= Nothing
        forM_ oldTimer cancelTimer
        curRound <- use finCurrentRound
        onActiveCurrentRound curRound $ \FinalizationRound{..} ->
            let spawnTimer = do
                    attempts <- use finCatchUpAttempts
                    logEvent Afgjort LLTrace $ "Setting replay timer (attempts: " ++ show attempts ++ ")"
                    timer <- onTimeout (DelayFor $ fromIntegral (attempts + 1) * (finalizationReplayBaseDelay + finalizationReplayStaggerDelay * fromIntegral roundMe)) $
                        getFinalizationInstance >>= mapM_ (\finInst -> do
                            finSt <- get
                            logEvent Afgjort LLTrace $ "Sending finalization summary (attempt " ++ show (attempts + 1) ++ ")"
                            mapM_ broadcastFinalizationPseudoMessage (finalizationCatchUpMessage finInst finSt)
                            finCatchUpAttempts %= (+1)
                            spawnTimer)
                    finCatchUpTimer ?= timer
            in spawnTimer

tryNominateBlock :: (FinalizationBaseMonad r s m, FinalizationMonad m) => m ()
tryNominateBlock = do
    curRound <- use finCurrentRound
    onActiveCurrentRound curRound $ \r@FinalizationRound{..} ->
        when (checkNoInput roundInput) $ do
            h <- use finHeight
            bBlock <- bestBlock
            when (bpHeight bBlock >= h + roundDelta) $ do
                ancestor <- ancestorAtHeight h bBlock
                let nomBlock = bpHash ancestor
                finCurrentRound .= ActiveCurrentRound (r {roundInput = RoundInput nomBlock})
                simpleWMVBA $ startWMVBA nomBlock

nextRound :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationIndex -> BlockHeight -> m ()
nextRound oldFinIndex oldDelta = do
    curFinIndex <- use finIndex
    when (curFinIndex == oldFinIndex) $ do
        oldRound <- use finCurrentRound
        onActiveCurrentRound oldRound $ \r ->
            when (roundDelta r == oldDelta) $ do
                finFailedRounds %= cons' (wmvbaWADBot (roundWMVBA r))
                FinalizationParameters{..} <- getFinalizationParameters
                let newDelta = max (1 + oldDelta) (ceiling $ finalizationDelayGrowFactor * fromIntegral oldDelta)
                newRound newDelta (roundMe r)

pendingToFinMsg :: FinalizationSessionId -> FinalizationIndex -> BlockHeight -> PendingMessage -> FinalizationMessage
pendingToFinMsg sessId finIx delta (PendingMessage src msg sig) =
     let msgHdr party = FinalizationMessageHeader {
             msgSessionId = sessId,
             msgFinalizationIndex = finIx,
             msgDelta = delta,
             msgSenderIndex = party
         }
     in FinalizationMessage (msgHdr src) msg sig

newRound :: (FinalizationBaseMonad r s m, FinalizationMonad m) => BlockHeight -> Party -> m ()
newRound newDelta me = do
        finCurrentRound .= ActiveCurrentRound FinalizationRound {
            roundInput = NoInput,
            roundDelta = newDelta,
            roundMe = me,
            roundWMVBA = initialWMVBAState
        }
        h <- use finHeight
        logEvent Afgjort LLDebug $ "Starting finalization round: height=" ++ show (theBlockHeight h) ++ " delta=" ++ show (theBlockHeight newDelta)
        blocksAtHeight <- getBlocksAtHeight (h + newDelta)
        justifiedInputs <- mapM (ancestorAtHeight h) blocksAtHeight
        finIx <- use finIndex
        committee <- use finCommittee
        sessId <- use finSessionId
        -- Filter the messages that have valid signatures and reference legitimate parties
        -- TODO: Drop pending messages for this round, because we've handled them
        let toFinMsg = pendingToFinMsg sessId finIx newDelta
        pmsgs <- finPendingMessages . at' finIx . non Map.empty . at' newDelta . non Set.empty <%= Set.filter (checkMessage committee . toFinMsg)
        -- Justify the blocks
        forM_ justifiedInputs $ \i -> do
            logEvent Afgjort LLTrace $ "Justified input at " ++ show finIx ++ ": " ++ show i
            simpleWMVBA $ justifyWMVBAInput $ bpHash i
        -- Receive the pending messages
        forM_ pmsgs $ \smsg@(PendingMessage src msg sig) -> do
            logEvent Afgjort LLDebug $ "Handling message: " ++ show (toFinMsg smsg)
            simpleWMVBA $ receiveWMVBAMessage src sig msg
        tryNominateBlock

-- TODO (MR) If this code is correct, consider reducing duplication with `receiveFinalizationMessage`
newPassiveRound :: (FinalizationBaseMonad r s m, FinalizationMonad m) => BlockHeight -> m ()
newPassiveRound newDelta = do
    fHeight       <- use finHeight
    finCom        <- use finCommittee
    finInd        <- use finIndex
    sessionId     <- use finSessionId
    logEvent Afgjort LLDebug $ "Starting passive finalization round: height=" ++ show (theBlockHeight fHeight) ++ " delta=" ++ show (theBlockHeight newDelta)
    maybeWitnessMsgs <- finPendingMessages . at' finInd . non Map.empty
                                           . at' newDelta . non Set.empty
                                           <%= Set.filter (checkMessage finCom . pendingToFinMsg sessionId finInd newDelta)
    let finParties = parties finCom
        partyInfo party = finParties Vec.! fromIntegral party
        pWeight = partyWeight . partyInfo
        pVRFKey = partyVRFKey . partyInfo
        pBlsKey = partyBlsKey . partyInfo
        baid = roundBaid sessionId finInd newDelta
        maxParty = fromIntegral $ Vec.length finParties - 1
        inst = WMVBAInstance baid (totalWeight finCom) (corruptWeight finCom) pWeight maxParty pVRFKey undefined undefined pBlsKey undefined
        -- Maps block hashes to `PartyMap`s
        blockToMsgs = foldr (\(PendingMessage src wm _) m ->
                        case wm of
                            WMVBAWitnessCreatorMessage (bh, sig) ->
                                let newPartyMap = PM.singleton src (pWeight src) sig
                                in Map.insertWith (PM.union pWeight) bh newPartyMap m
                            _ -> m
               ) Map.empty $ Set.toList maybeWitnessMsgs
        (mProof, passiveStates) = foldr (\(v, partyMap) (prevProofM, oldState) ->
                                            let (proofM, newState) = runState (passiveReceiveWMVBASignatures inst v partyMap pWeight) oldState
                                            in (proofM <|> prevProofM, newState))
                                        (Nothing, initialWMVBAPassiveState)
                                        $ Map.toList blockToMsgs
    finCurrentRound .= let passiveRound = (PassiveFinalizationRound $ passiveWitnesses initialPassiveFinalizationRound & at' newDelta ?~ passiveStates) in (PassiveCurrentRound passiveRound)
    forM_ mProof (handleFinalizationProof sessionId finInd newDelta finCom)

handleWMVBAOutputEvents :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationInstance -> [WMVBAOutputEvent Sig.Signature] -> m ()
handleWMVBAOutputEvents FinalizationInstance{..} evs = do
        FinalizationState{..} <- use finState
        onActiveCurrentRound _finsCurrentRound $ \FinalizationRound{..} -> do
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
                handleEvs False (WMVBAComplete (Just proof) : evs') = do
                    -- Round completed, so handle the proof.
                    handleFinalizationProof _finsSessionId _finsIndex roundDelta _finsCommittee proof
                    handleEvs True evs'
                handleEvs True (WMVBAComplete _ : evs') = handleEvs True evs'
                handleEvs b (WMVBADelay ticks action : evs') = do
                    FinalizationParameters{..} <- getFinalizationParameters
                    let delay = fromIntegral ticks * durationToNominalDiffTime finalizationWaitingTime
                    if delay == 0 then
                        triggerWMVBA _finsSessionId _finsIndex roundDelta action
                    else
                        () <$ onTimeout (DelayFor delay) (triggerWMVBA _finsSessionId _finsIndex roundDelta action)
                    handleEvs b evs'
            handleEvs False evs

-- |Handle when a finalization proof is generated:
--  * Notify Skov of finalization ('trustedFinalize').
--  * If the finalized block is known to Skov, handle this new finalization ('finalizationBlockFinal').
--  * If the block is not known, add the finalization to the queue ('addQueuedFinalization').
handleFinalizationProof :: (FinalizationMonad m, SkovMonad m, MonadState s m, FinalizationQueueLenses s) => FinalizationSessionId -> FinalizationIndex -> BlockHeight -> FinalizationCommittee -> (Val, ([Party], Bls.Signature)) -> m ()
handleFinalizationProof sessId fIndex delta committee (finB, (parties, sig)) = do
        let finRec = FinalizationRecord {
            finalizationIndex = fIndex,
            finalizationBlockPointer = finB,
            finalizationProof = FinalizationProof (parties, sig),
            finalizationDelay = delta
        }
        finRes <- trustedFinalize finRec
        case finRes of
            Left _ -> addQueuedFinalization sessId committee finRec
            Right finBlock -> finalizationBlockFinal finRec finBlock

triggerWMVBA :: (FinalizationBaseMonad r s m, FinalizationMonad m)
    => FinalizationSessionId
    -> FinalizationIndex
    -> BlockHeight
    -- ^Round delta
    -> DelayedABBAAction
    -> m ()
triggerWMVBA sessId finIx delta act = do
    FinalizationState{..} <- use finState
    when (_finsSessionId == sessId && _finsIndex == finIx) $
        use (finState . finCurrentRound) >>= \case
            ActiveCurrentRound finRound
                | roundDelta finRound == delta -> simpleWMVBA (triggerWMVBAAction act)
            _ -> return ()

liftWMVBA :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationInstance -> WMVBA Sig.Signature a -> m a
liftWMVBA fininst@FinalizationInstance{..} a = do
    FinalizationState{..} <- use finState
    case _finsCurrentRound of
        PassiveCurrentRound _ -> error "No current finalization round"
        ActiveCurrentRound fr@FinalizationRound{..} -> do
            let
                baid = roundBaid _finsSessionId _finsIndex roundDelta
                pWeight party = partyWeight (parties _finsCommittee Vec.! fromIntegral party)
                pVRFKey party = partyVRFKey (parties _finsCommittee Vec.! fromIntegral party)
                pBlsKey party = partyBlsKey (parties _finsCommittee Vec.! fromIntegral party)
                maxParty = fromIntegral $ Vec.length (parties _finsCommittee) - 1
                inst = WMVBAInstance baid (totalWeight _finsCommittee) (corruptWeight _finsCommittee) pWeight maxParty pVRFKey roundMe finMyVRFKey pBlsKey finMyBlsKey
            (r, newState, evs) <- liftIO $ runWMVBA a inst roundWMVBA
            finCurrentRound .= ActiveCurrentRound fr {roundWMVBA = newState}
            -- logEvent Afgjort LLTrace $ "New WMVBA state: " ++ show newState
            handleWMVBAOutputEvents fininst evs
            return r

simpleWMVBA :: (FinalizationBaseMonad r s m, FinalizationMonad m) => WMVBA Sig.Signature () -> m ()
simpleWMVBA a = getFinalizationInstance >>= \case
    Just inst -> liftWMVBA inst a
    Nothing -> logEvent Afgjort LLError $ "Finalization keys missing, but this node appears to be participating in finalization."

-- |Determine if a message references blocks requiring Skov to catch up.
messageRequiresCatchUp :: (FinalizationBaseMonad r s m, FinalizationMonad m) => WMVBAMessage -> m Bool
messageRequiresCatchUp msg = case messageValues msg of
        Nothing -> return False
        Just b -> resolveBlock b >>= \case
            Nothing -> return True -- Block not found
            Just _ -> do
                FinalizationState{..} <- use finState
                minst <- getFinalizationInstance
                case (_finsCurrentRound, minst) of
                    -- Check that the block is considered justified.
                    (ActiveCurrentRound _, Just finInst) -> liftWMVBA finInst $ not <$> isJustifiedWMVBAInput b
                    -- TODO: possibly we should also check if it is justified even when we are not active in finalization
                    _ -> return False

savePendingMessage :: (FinalizationBaseMonad r s m) => FinalizationIndex -> BlockHeight -> PendingMessage -> m Bool
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
receiveFinalizationMessage :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationMessage -> m UpdateResult
receiveFinalizationMessage msg@FinalizationMessage{msgHeader=FinalizationMessageHeader{..},..} = do
        FinalizationState{..} <- use finState
        -- Check this is the right session
        if _finsSessionId == msgSessionId then
            -- Check the finalization index is not out of date
            case compare msgFinalizationIndex _finsIndex of
                LT -> tryAddQueuedWitness msg
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
                        return ResultInvalid -- FIXME: possibly return ResultUnverifiable instead.
                EQ -> -- handle the message now, since it's the current round
                    if checkMessage _finsCommittee msg then do
                        -- Save the message
                        isDuplicate <- savePendingMessage msgFinalizationIndex msgDelta (PendingMessage msgSenderIndex msgBody msgSignature)
                        if isDuplicate then
                            return ResultDuplicate
                        else do
                            -- Check if we're participating in finalization for this index
                            case _finsCurrentRound of
                                ActiveCurrentRound (FinalizationRound{..}) ->
                                    -- And it's the current round
                                    when (msgDelta == roundDelta) $ do
                                        logEvent Afgjort LLDebug $ "Handling message: " ++ show msg
                                        simpleWMVBA (receiveWMVBAMessage msgSenderIndex msgSignature msgBody)
                                PassiveCurrentRound (PassiveFinalizationRound pw) -> do
                                    let
                                        baid = roundBaid _finsSessionId _finsIndex msgDelta
                                        pWeight party = partyWeight (parties _finsCommittee Vec.! fromIntegral party)
                                        pVRFKey party = partyVRFKey (parties _finsCommittee Vec.! fromIntegral party)
                                        pBlsKey party = partyBlsKey (parties _finsCommittee Vec.! fromIntegral party)
                                        maxParty = fromIntegral $ Vec.length (parties _finsCommittee) - 1
                                        inst = WMVBAInstance baid (totalWeight _finsCommittee) (corruptWeight _finsCommittee) pWeight maxParty pVRFKey undefined undefined pBlsKey undefined
                                        (mProof, ps') = runState (passiveReceiveWMVBAMessage inst msgSenderIndex msgBody) (pw ^. at' msgDelta . non initialWMVBAPassiveState)
                                    finCurrentRound .= PassiveCurrentRound (PassiveFinalizationRound (pw & at' msgDelta ?~ ps'))
                                    forM_ mProof (handleFinalizationProof _finsSessionId _finsIndex msgDelta _finsCommittee)
                            newFinIndex <- use finIndex
                            if newFinIndex == _finsIndex then do
                                rcu <- messageRequiresCatchUp msgBody
                                if rcu then do
                                    logEvent Afgjort LLDebug $ "Message refers to unjustified block; catch-up required."
                                    return ResultPendingBlock
                                else
                                    return ResultSuccess
                            else
                                return ResultSuccess
                    else do
                        logEvent Afgjort LLWarning $ "Received bad finalization message"
                        return ResultInvalid
            else
                return ResultIncorrectFinalizationSession

-- |Called when a finalization pseudo-message is received.
receiveFinalizationPseudoMessage :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationPseudoMessage -> m UpdateResult
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
                            logEvent Afgjort LLTrace $ "Processing finalization summary from " ++ show cuSenderIndex
                            CatchUpResult{..} <- processFinalizationSummary cuFinalizationSummary
                            logEvent Afgjort LLTrace $ "Finalization summary was " ++ (if curBehind then "behind" else "not behind")
                                        ++ " and " ++ (if curSkovCatchUp then "requires Skov catch-up." else "does not require Skov catch-up.")
                            unless curBehind doResetTimer
                            if curSkovCatchUp then
                                return ResultPendingBlock
                            else
                                return ResultSuccess
                    else
                        return ResultInvalid
        else
            return ResultIncorrectFinalizationSession

-- |Handle receipt of a finalization record.
--
-- If the record is for a finalization index that is settled (i.e. the finalization
-- record appears in a finalized block) then this returns 'ResultStale'.
--
-- If the record is for a finalization index where a valid finalization record is already
-- known, then one of the following applies:
--
--   * If the record is invalid, returns 'ResultInvalid'.
--   * If the record is valid and contains new signatures, stores the record and returns 'ResultSuccess'.
--   * If @validateDuplicate@ is not set or the record is valid, returns 'ResultDuplicate'.
--
-- When more than one case could apply, it is unspecified which is chosen. It is intended that
-- 'ResultSuccess' should be used wherever possible, but 'ResultDuplicate' can be returned in any
-- case.
--
-- If the record is for the next finalization index:
--
--   * If the record is valid and for a known block, that block is finalized and 'ResultSuccess' returned.
--   * If the record is invalid, 'ResultInvalid' is returned.
--   * If the block is unknown, then 'ResultUnverifiable' is returned.
--
-- If the record is for a future finalization index (that is not next), 'ResultUnverifiable' is returned
-- and the record is discarded.
receiveFinalizationRecord :: (SkovMonad m, MonadState s m, FinalizationQueueLenses s, FinalizationMonad m) => Bool -> FinalizationRecord -> m UpdateResult
receiveFinalizationRecord validateDuplicate finRec@FinalizationRecord{..} = do
        nextFinIx <- nextFinalizationIndex
        case compare finalizationIndex nextFinIx of
            LT -> do
                fi <- use (finQueue . fqFirstIndex)
                if finalizationIndex < fi then
                    return ResultStale
                else if validateDuplicate then
                    checkFinalizationProof finRec >>= \case
                        Nothing -> return ResultInvalid
                        Just (finSessId, finCom) -> do
                            addQueuedFinalization finSessId finCom finRec
                            return ResultDuplicate
                else
                    return ResultDuplicate
            EQ -> checkFinalizationProof finRec >>= \case
                Nothing -> return ResultInvalid
                Just _ -> trustedFinalize finRec >>= \case
                    -- In this case, we have received a valid finalization proof,
                    -- but it's not for a block that is known.  This shouldn't happen
                    -- often, and we are probably fine to throw it away.
                    Left res -> return res
                    Right newFinBlock -> do
                        -- finalizationBlockFinal adds the finalization to the queue
                        finalizationBlockFinal finRec newFinBlock
                        return ResultSuccess
            GT -> return ResultUnverifiable

-- |It is possible to have a validated finalization proof for a block that is
-- not currently known.  This function detects when such a block arrives and
-- triggers it to be finalized.
notifyBlockArrivalForPending :: (SkovMonad m, MonadState s m, FinalizationQueueLenses s, BlockPointerData bp, FinalizationMonad m) => bp -> m ()
notifyBlockArrivalForPending b = do
    nfi <- nextFinalizationIndex
    getQueuedFinalizationTrivial nfi >>= \case
        Just (_, _, finRec)
            | finalizationBlockPointer finRec == bpHash b ->
                trustedFinalize finRec >>= \case
                    Right newFinBlock ->
                      finalizationBlockFinal finRec newFinBlock
                    Left _ -> return ()
        _ -> return ()

-- |Called to notify the finalization routine when a new block arrives.
notifyBlockArrival :: (FinalizationBaseMonad r s m, FinalizationMonad m) => BlockPointerType m -> m ()
notifyBlockArrival b = do
    notifyBlockArrivalForPending b
    FinalizationState{..} <- use finState
    onActiveCurrentRound _finsCurrentRound $ \FinalizationRound{..} -> do
        when (bpHeight b == _finsHeight + roundDelta) $ do
            ancestor <- ancestorAtHeight _finsHeight b
            logEvent Afgjort LLTrace $ "Justified input at " ++ show _finsIndex ++ ": " ++ show (bpHash ancestor)
            simpleWMVBA $ justifyWMVBAInput (bpHash ancestor)
        tryNominateBlock

-- |Determine what index we have in the finalization committee.
-- This simply finds the first party in the committee whose
-- public keys match ours.
getMyParty :: (FinalizationBaseMonad r s m) => m (Maybe Party)
getMyParty = getFinalizationInstance >>= \case
    Nothing -> return Nothing
    Just finInst -> findInstanceInCommittee finInst <$> use finCommittee

-- |Find the party in the given finalization committee.
-- This simply finds the first party in the committee whose
-- public keys match the instance.
findInstanceInCommittee :: FinalizationInstance -> FinalizationCommittee -> Maybe Party
findInstanceInCommittee finInst committee =
  let myVerifyKey = (Sig.verifyKey . finMySignKey) finInst
      myPublicVRFKey = (VRF.publicKey . finMyVRFKey) finInst
      ps = parties committee
  in case Vec.find (\p -> partySignKey p == myVerifyKey && partyVRFKey p == myPublicVRFKey) ps of
       Just p -> Just (partyIndex p)
       Nothing -> Nothing

-- |Produce 'OutputWitnesses' based on the pending finalization messages.
-- This is used when we know finalization has occurred (by receiving a
-- valid finalization record) but we have not completed finalization, and
-- in particular, have not yet reached the round in which finalization
-- completed.  (In the case where we have reached that round,
-- 'getOutputWitnesses' should be called on the WMVBA instance for that round
-- instead.)
pendingToOutputWitnesses :: (FinalizationBaseMonad r s m)
    => FinalizationSessionId
    -> FinalizationIndex
    -> BlockHeight
    -> BlockHash
    -> m OutputWitnesses
pendingToOutputWitnesses sessId finIx delta finBlock = do
        -- Get the pending messages at the given finalization index and delta.
        pmsgs <- use $ finPendingMessages . at' finIx . non Map.empty . at' delta . non Set.empty
        committee <- use finCommittee
        -- Filter for only the witness creator messages that witness the correct
        -- block and are correctly signed.
        let
            f (PendingMessage src msg@(WMVBAWitnessCreatorMessage (b,blssig)) sig)
                | b == finBlock
                , checkMessage committee (FinalizationMessage (msgHdr src) msg sig)
                    = Just (src, blssig)
            f _ = Nothing
            msgHdr src = FinalizationMessageHeader {
                msgSessionId = sessId,
                msgFinalizationIndex = finIx,
                msgDelta = delta,
                msgSenderIndex = src
            }
            filtpmsgs = mapMaybe f (Set.toList pmsgs)
        -- The returned OutputWitnesses only consists of unchecked signatures,
        -- since we have made no effort to check the BLS signatures.
        return $ uncheckedOutputWitnesses (Map.fromList filtpmsgs)

-- |Called to notify the finalization routine when a new block is finalized.
-- (NB: this should never be called with the genesis block.)
notifyBlockFinalized :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationRecord -> BlockPointerType m -> m ()
notifyBlockFinalized fr@FinalizationRecord{..} bp = do
        -- Reset catch-up timer
        oldTimer <- finCatchUpTimer <<.= Nothing
        forM_ oldTimer cancelTimer
        finCatchUpAttempts .= 0
        -- Reset the deduplication buffer
        finCatchUpDeDup .= PSQ.empty
        -- Move to next index
        oldFinIndex <- finIndex <<.=! finalizationIndex + 1
        unless (finalizationIndex == oldFinIndex) $
            error $ "Non-sequential finalization, finalizationIndex = " ++ show finalizationIndex ++ ", oldIndex = " ++ show oldFinIndex
        -- Update the finalization queue index as necessary
        settleQueuedFinalizationByHash (bpLastFinalizedHash bp)
        -- Add all witnesses we have to the finalization queue
        sessId <- use finSessionId
        fc <- use finCommittee
        witnesses <- use finCurrentRound >>= \case
            -- If we aren't participating in this finalization round, we get the witnesses
            -- from the passive state.
            PassiveCurrentRound PassiveFinalizationRound{..} ->
                return $ passiveGetOutputWitnesses
                            finalizationBlockPointer
                            (passiveWitnesses ^. at' finalizationDelay . non initialWMVBAPassiveState)
            ActiveCurrentRound curRound
                | roundDelta curRound == finalizationDelay ->
                    -- If the WMVBA is on the same round as the finalization proof, get
                    -- the additional witnesses from there.
                    return $ getOutputWitnesses finalizationBlockPointer (roundWMVBA curRound)
                | otherwise ->
                    -- If not, get the witnesses from the pending queue.
                    pendingToOutputWitnesses sessId finalizationIndex finalizationDelay finalizationBlockPointer
        addNewQueuedFinalization sessId fc fr witnesses
        -- Discard finalization messages from old round
        finPendingMessages . at' finalizationIndex .= Nothing
        pms <- use finPendingMessages
        logEvent Afgjort LLTrace $ "Finalization complete. Pending messages: " ++ show pms
        -- Compute the next finalization height
        finParams <- getFinalizationParameters
        oldFinalizationGap <- use finGap
        newFinalizationGap <- nextFinalizationGap bp finParams oldFinalizationGap
        finGap .= newFinalizationGap
        finHeight += newFinalizationGap
        let newFinDelay = nextFinalizationDelay finParams fr
        finIndexInitialDelta .= newFinDelay
        finFailedRounds .= []
        -- Update finalization committee for the new round
        finCommittee <~ getFinalizationCommittee bp
        -- Determine if we're in the committee
        mMyParty <- getMyParty
        case mMyParty of
          Just myParty ->
            newRound newFinDelay myParty
          Nothing ->
            newPassiveRound newFinDelay

nextFinalizationDelay :: FinalizationParameters -> FinalizationRecord -> BlockHeight
nextFinalizationDelay FinalizationParameters{..} FinalizationRecord{..}
        = if finalizationAllowZeroDelay then shrunk else max 1 shrunk
    where
        shrunk = truncate (finalizationDelayShrinkFactor * fromIntegral finalizationDelay)

nextFinalizationGap :: (BlockPointerMonad m)
    => BlockPointerType m
    -- ^Last finalized block
    -> FinalizationParameters
    -- ^Finalization parameters
    -> BlockHeight
    -- ^Previous finalization gap
    -> m BlockHeight
nextFinalizationGap bp FinalizationParameters{..} oldGap = do
    lf <- bpLastFinalized bp
    let
        heightDiff = bpHeight bp - bpHeight lf
        newGap = if heightDiff == oldGap
                    then floor (finalizationSkipShrinkFactor * fromIntegral oldGap)
                    else ceiling (finalizationSkipGrowFactor * fromIntegral oldGap)
    return $ max (1 + finalizationMinimumSkip) newGap

getPartyWeight :: FinalizationCommittee -> Party -> VoterPower
getPartyWeight com pid = case parties com ^? ix (fromIntegral pid) of
        Nothing -> 0
        Just p -> partyWeight p

-- |Check that a finalization record has a valid proof
verifyFinalProof :: FinalizationSessionId -> FinalizationCommittee -> FinalizationRecord -> Bool
verifyFinalProof sid com@FinalizationCommittee{..} FinalizationRecord{..} =
        sigWeight finParties > corruptWeight && checkProofSignature
    where
        FinalizationProof (finParties, sig) = finalizationProof
        toSign = witnessMessage (roundBaid sid finalizationIndex finalizationDelay) finalizationBlockPointer
        mpks = sequence ((fmap partyBlsKey . toPartyInfo com) <$> finParties)
        checkProofSignature = case mpks of
            Nothing -> False -- If any parties are invalid, reject the proof
            Just pks -> Bls.verifyAggregate toSign pks sig
        sigWeight ps = sum (getPartyWeight com <$> ps)

-- |Determine the finalization session ID and finalization committee used for finalizing
-- at the given index i. Note that the finalization committee is determined based on the block state
-- at index i-1.
getFinalizationContext :: (SkovQueryMonad m) => FinalizationRecord -> m (Maybe (FinalizationSessionId, FinalizationCommittee))
getFinalizationContext FinalizationRecord{..} = do
        genHash <- bpHash <$> genesisBlock
        let finSessId = FinalizationSessionId genHash 0 -- FIXME: Don't hard-code this!
        blockAtFinIndex (finalizationIndex - 1) >>= \case
          Just bp -> Just . (finSessId,) <$> getFinalizationCommittee bp
          Nothing -> return Nothing

-- |Check a finalization proof, returning the session id and finalization committee if
-- successful.
checkFinalizationProof :: (MonadState s m, FinalizationQueueLenses s, SkovQueryMonad m) => FinalizationRecord -> m (Maybe (FinalizationSessionId, FinalizationCommittee))
checkFinalizationProof finRec =
    getQueuedFinalizationTrivial (finalizationIndex finRec) >>= \case
        Just (finSessId, finCom, altFinRec) -> return $ if finRec == altFinRec || verifyFinalProof finSessId finCom finRec then Just (finSessId, finCom) else Nothing
        Nothing -> getFinalizationContext finRec <&> \case
            Just (finSessId, finCom) -> if verifyFinalProof finSessId finCom finRec then Just (finSessId, finCom) else Nothing
            Nothing -> Nothing

-- |Produce a 'FinalizationSummary' based on the finalization state.
finalizationSummary :: (FinalizationStateLenses s m) => SimpleGetter s FinalizationSummary
finalizationSummary = to fs
    where
        fs s = FinalizationSummary{..}
            where
                summaryFailedRounds = reverse $ s ^. finFailedRounds
                summaryCurrentRound = case s ^. finCurrentRound of
                    PassiveCurrentRound _ -> WMVBASummary Nothing Nothing Nothing
                    ActiveCurrentRound FinalizationRound{..} -> roundWMVBA ^. wmvbaSummary

-- |Produce a 'FinalizationPseudoMessage' containing a catch up message based on the current finalization state.
finalizationCatchUpMessage :: (FinalizationStateLenses s m) => FinalizationInstance -> s -> Maybe FinalizationPseudoMessage
finalizationCatchUpMessage FinalizationInstance{..} s =
  case _finsCurrentRound of
    ActiveCurrentRound FinalizationRound{..} -> Just . FPMCatchUp $! signCatchUpMessage finMySignKey _finsSessionId _finsIndex roundMe (committeeMaxParty _finsCommittee) summary
    PassiveCurrentRound _ -> Nothing
    where
        FinalizationState{..} = s ^. finState
        summary = s ^. finalizationSummary

-- |Process a 'FinalizationSummary', handling any new messages and returning a result indicating
-- whether the summary is behind, and whether we should initiate Skov catch-up.
processFinalizationSummary :: (FinalizationBaseMonad r s m, FinalizationMonad m) => FinalizationSummary -> m CatchUpResult
processFinalizationSummary FinalizationSummary{..} =
        use finCurrentRound >>= \case
            PassiveCurrentRound _ -> return mempty -- TODO: actually do something with these
            ActiveCurrentRound _ -> getFinalizationInstance >>= \case
                Nothing -> return mempty -- This should not happen, since it means that we seem to be participating in finalization
                                            -- but do not have keys to do so
                Just finInst@(FinalizationInstance{..}) -> do
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
                        PassiveCurrentRound _ -> return False
                        ActiveCurrentRound curRound -> case compare delta (roundDelta curRound) of
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
                                    curBehind <$> liftWMVBA finInst (processWMVBASummary (wmvbaFailedSummary m) (checkSigDelta delta))
                                GT -> -- This case shouldn't happen unless the message is corrupt.
                                    return False
                    let delta = BlockHeight (shiftL (theBlockHeight initDelta) (length summaryFailedRounds))
                    use finCurrentRound >>= \case
                        PassiveCurrentRound _ -> return mempty
                        ActiveCurrentRound curRound -> case compare delta (roundDelta curRound) of
                            LT -> return (CatchUpResult {curBehind = True, curSkovCatchUp = False})
                            EQ -> do
                                cur <- liftWMVBA finInst $ processWMVBASummary summaryCurrentRound (checkSigDelta delta)
                                return (cur <> mempty {curBehind = or roundsBehind})
                            GT -> return (mempty {curBehind = or roundsBehind})


-- |Given an existing block, returns a 'FinalizationRecord' that can be included in
-- a child of that block, if available.
nextFinalizationRecord :: (FinalizationMonad m, SkovMonad m) => BlockPointerType m -> m (Maybe (FinalizationSessionId, FinalizationCommittee, FinalizationRecord))
nextFinalizationRecord parentBlock = do
    lfi <- blockLastFinalizedIndex parentBlock
    finalizationUnsettledRecordAt (lfi + 1)

-- |'ActiveFinalizationM' provides an implementation of 'FinalizationMonad' that
-- actively participates in finalization.
newtype ActiveFinalizationM r s m a = ActiveFinalizationM {runActiveFinalizationM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadReader r, TimerMonad, BlockStateTypes, BlockStateQuery, AccountOperations, BlockStateOperations, BlockStateStorage, BlockPointerMonad, PerAccountDBOperations, TreeStateMonad, SkovMonad, TimeMonad, MonadLogger, MonadIO, FinalizationOutputMonad, SkovQueryMonad)

deriving instance (BlockPointerData (BlockPointerType m)) => GlobalStateTypes (ActiveFinalizationM r s m)
deriving instance (CanExtend (ATIStorage m), CanRecordFootprint (Footprint (ATIStorage m))) => ATITypes (ActiveFinalizationM r s m)


instance (FinalizationBaseMonad r s m) => FinalizationMonad (ActiveFinalizationM r s m) where
    finalizationBlockArrival = notifyBlockArrival
    finalizationBlockFinal fr b = notifyBlockFinalized fr b
    finalizationReceiveMessage = receiveFinalizationPseudoMessage
    finalizationReceiveRecord b fr = receiveFinalizationRecord b fr
    finalizationUnsettledRecordAt = getQueuedFinalization
    finalizationUnsettledRecords = getQueuedFinalizationsBeyond
