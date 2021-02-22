{-# LANGUAGE
    TemplateHaskell,
    ScopedTypeVariables,
    BangPatterns,
    GeneralizedNewtypeDeriving,
    RankNTypes,
    OverloadedStrings #-}
{- |Another Binary Byzantine Agreement (ABBA) algorithm

For more information, check the konsensus paper, section 5.6.4.

= Definitions

* Phase-1 justified: a bit @b@ is @Jphase,1@-justified for us if it is @Jin@-justified.
* Phase-k justified: a bit @b@ is @Jphase,k@-justified for us if we have @t + 1@ signatures on @(baid, JUSTIFIED, b, k-1)@.
* Out justified: a bit @b@ is @Jout@-justified for us if we have @t + 1@ signatures on @(baid, WEAREDONE, b)@.

= Protocol

== Input

* @baid@: the identifier of the WMVBA instance.
* @Jin@: a justification.
* @delay@: delay for countering de-synchronization.

== Precondition

* We have an input @b@ which is @Jin@-justified for us.

== Execution

* Graded Agreement

  In each phase @k = 1,2...@ do

    1. The parties run @CSS(baid, Jphase,k, k * delay)@ with input @b@ to generate the @Core@.
    2. Compute my lottery ticket @ticket@ and broadcast signed @(baid,JUSTIFIED,b,k)@ with the ticket.
    3. Wait @k * delay@ and

        a) if all bits in @Core@ are @T@, let @b = T@ and @grade = 2@.

        b) else if @n-t@ bits in @Core@ are @T@, let @b = T@ and @grade = 1@.

        c) else if all bits in @Core@ are @Bottom@, let @b = Bottom@ and @grade = 2@.

        d) else if @n - t@ bits in @Core@ are @Bottom@, let @b = Bottom@ and @grade = 1@.

        e) else select a bit @b'@ which occurs @> t@ in @Core@. If it is not unique, verify all lottery tickets
           and select the bit @b'@ where @(b', P) in Core@ and @P@ has the highest valid lottery ticket in @Core@.
           let @b = b'@ and @grade = 0@.

* Closing Down

    1. When we achieve grade 2 for the first time, we send @(baid, WEAREDONE, b)@ to all parties.
    2. Once having received at least @t + 1@ signed @(baid, WEAREDONE, b')@, terminate outputting @b'@ which is then
       @Jout@-justified.
-}
module Concordium.Afgjort.ABBA(
    -- * Types
    Phase,
    Choice,
    -- * Messages
    ABBAMessage(..),
    -- * State
    ABBAState(..),
    initialABBAState,
    -- * Instance
    ABBAInstance(ABBAInstance),
    -- * Monad definition
    ABBAMonad(..),
    DelayedABBAAction,
    -- * Monad implementation
    ABBAOutputEvent(..),
    ABBA,
    runABBA,
    -- * Protocol
    beginABBA,
    justifyABBAChoice,
    receiveABBAMessage,
    triggerABBAAction,
    abbaOutcome,
    -- * Summary
    PhaseSummary(..),
    ABBASummary(..),
    abbaSummary,
    processABBASummary,
    getABBASummary,
    putABBASummary
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))
import Data.Maybe
import Data.Word
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Control.Arrow

import Concordium.Utils
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.PartyMap (PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM

--------------------------------------------------------------------------------
-- Types

-- |A phase in the ABBA protocol
type Phase = Word32

--------------------------------------------------------------------------------
-- Messages

-- |A message in the ABBA protocol
data ABBAMessage
    = Justified !Phase !Choice TicketProof
    -- ^Party's input for a given phase.
    -- NB: The TicketProof must be non-strict for now.
    | CSSSeen !Phase !NominationSet
    -- ^CSS seen message for a phase
    | CSSDoneReporting !Phase !NominationSet
    -- ^CSS done reporting message for a phase
    | WeAreDone !Choice
    -- ^Message that indicates consensus should be reached
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- State

-- |The state of a phase in the protocol.
--
-- This includes the lottery tickets submitted by parties, the CSS state for the current phase,
-- and the total weight and set of parties that have nominated each input.  Once the weight
-- exceeds the threshold, we record it as @Nothing@, forgetting the exact weight and parties.
--
-- INVARIANT: Each CSS 'Input' has a corresponding valid 'Ticket' and vice-versa.
data PhaseState sig = PhaseState {
    _lotteryTickets :: !(Map (Double, Party) Ticket),
    _phaseCSSState :: Either (Maybe Choices, Seq (Party, CSSMessage, sig)) (CSSState sig),
    _topInputWeight :: !(Maybe PartySet),
    _botInputWeight :: !(Maybe PartySet)
} deriving (Eq, Show)
makeLenses ''PhaseState

-- |The initial state of a phase
initialPhaseState :: PhaseState sig
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = Left (Nothing, Seq.empty),
    _topInputWeight = Just PS.empty,
    _botInputWeight = Just PS.empty
}

-- |The total weight and set of parties nominating a particular choice.
inputWeight :: Choice -> Lens' (PhaseState sig) (Maybe PartySet)
inputWeight True = topInputWeight
inputWeight False = botInputWeight

-- |The state of the ABBA protocol.
--
-- This includes the current phase, the state of all phases, the current grade,
-- and the set and weight of parties that have claimed we are done with each
-- possible output choice.
data ABBAState sig = ABBAState {
    _currentPhase :: !Phase,
    _phaseStates :: !(Map Phase (PhaseState sig)),
    _currentGrade :: !Word8,
    _topWeAreDone :: !(PartyMap sig),
    _botWeAreDone :: !(PartyMap sig),
    _completed :: !Bool
} deriving (Eq, Show)
makeLenses ''ABBAState

-- |The state of a particular phase
phaseState :: Phase -> Lens' (ABBAState sig) (PhaseState sig)
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . at' p ?~ t)

-- |The set of parties claiming we are done with a given choice
weAreDone :: Choice -> Lens' (ABBAState sig) (PartyMap sig)
weAreDone True = topWeAreDone
weAreDone False = botWeAreDone

-- |The initial state of the ABBA protocol.
initialABBAState :: ABBAState sig
initialABBAState = ABBAState {
    _currentPhase = 0,
    _phaseStates = Map.singleton 0 (initialPhaseState {_phaseCSSState = Right initialCSSState}),
    _currentGrade = 0,
    _topWeAreDone = PM.empty,
    _botWeAreDone = PM.empty,
    _completed = False
}

-- |Get the decision from an ABBAState.  Returns @Nothing@ if the protocol is not  yet completed.
{-# INLINE abbaOutcome #-}
abbaOutcome :: SimpleGetter (ABBAState sig) (Maybe Choice)
abbaOutcome = to aoc
    where
        aoc ABBAState{..} = if _completed then Just (PM.weight _topWeAreDone >= PM.weight _botWeAreDone) else Nothing

--------------------------------------------------------------------------------
-- Instance

-- |An @ABBAInstance@ consists of:
--
-- * The instance identifier for this instantiation of the protocol
-- * The total weight of all parties
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * The public key of each party
-- * My party
-- * My VRF key
data ABBAInstance = ABBAInstance {
    -- |The instance identifier for this instantiation of the protocol
    baid :: !BS.ByteString,
    -- |The total weight of all parties
    totalWeight :: !VoterPower,
    -- |The maximum weight of corrupt parties (must be less than @totalWeight/3@).
    corruptWeight :: !VoterPower,
    -- |The weight of each party
    partyWeight :: Party -> VoterPower,
    -- |The maximal party
    maxParty :: !Party,
    -- |The public VRF key of each party
    pubKeys :: Party -> VRF.PublicKey,
    -- |My party
    me :: !Party,
    -- |My VRF key
    privateKey :: !VRF.KeyPair
}

-- |Get the lottery identifier string for the given phase.
lotteryId :: Phase -> SimpleGetter ABBAInstance BS.ByteString
lotteryId phase = to $ \a ->
        Ser.runPut $ Ser.put (baid a) >> Ser.put phase

--------------------------------------------------------------------------------
-- Monad definition

-- |The @ABBAMonad@ class defines the events associated with the ABBA protocol.
class (MonadState (ABBAState sig) m, MonadReader ABBAInstance m, MonadIO m) => ABBAMonad sig m where
    -- |Sign and broadcast an ABBA message to all parties, __including__ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage -> m ()
    -- |Determine the result
    aBBAComplete :: Choice -> m ()
    -- |Wait for a specified number of deltaABBA intervals, then callback the given action.
    delayThen :: Word32 -> DelayedABBAAction -> m ()

data DelayedABBAAction
    = HandleCoreSet !Phase !CoreSet
    | CSSFinishReporting !Phase
    | CompletePhase !Phase !Bool !Word8
    deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Monad implementation

-- |A concrete implementation of the ABBA monad.
newtype ABBA sig a = ABBA {
    runABBA' :: RWST ABBAInstance (Endo [ABBAOutputEvent]) (ABBAState sig) IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState (ABBAState sig), MonadReader ABBAInstance)

instance ABBAMonad sig (ABBA sig) where
    sendABBAMessage !msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete !c = ABBA $ tell $ Endo (ABBAComplete c :)
    delayThen !ticks !action = ABBA $ tell $ Endo (ABBADelay ticks action :)

-- |Representation of (output) events associated with the ABBA protocol.
data ABBAOutputEvent
    = SendABBAMessage !ABBAMessage   -- ^Sign and broadcast a message
    | ABBAComplete !Choice                   -- ^Determine result
    | ABBADelay !Word32 !DelayedABBAAction

-- |Run part of the ABBA protocol, given an 'ABBAInstance' and 'ABBAState'.
-- The result includes the updated state and a list of 'ABBAOutputEvent's that occurred during the execution.
{-# INLINE runABBA #-}
runABBA :: ABBA sig a -> ABBAInstance -> ABBAState sig -> IO (a, ABBAState sig, [ABBAOutputEvent])
runABBA z i s = runRWST (runABBA' z) i s <&> _3 %~ (\(Endo f) -> f [])

--------------------------------------------------------------------------------
-- Lifting CSS

-- |Receive a CSS message for a given phase, handling the resulting events.
liftCSSReceiveMessage :: (ABBAMonad sig m) => Phase -> Party -> CSSMessage -> sig -> m ()
liftCSSReceiveMessage phase !src !msg !sig = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (justif, msgs |> (src, msg, sig))
            Right cssstate -> do
                let ((), !cssstate', !evs) = runCSS (receiveCSSMessage src msg sig) (CSSInstance totalWeight corruptWeight partyWeight maxParty) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

-- |Justify a CSS input for a given phase, handling the resulting events.
liftCSSJustifyChoice :: (ABBAMonad sig m) => Phase -> Choice -> m ()
liftCSSJustifyChoice phase c = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (addChoice c justif, msgs)
            Right cssstate -> do
                let (_, !cssstate', evs) = runCSS (justifyChoice c) (CSSInstance totalWeight corruptWeight partyWeight maxParty) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

liftCSSFinishReporting :: (ABBAMonad sig m) => Phase -> m ()
liftCSSFinishReporting phase = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left _ -> return () -- This should not typically happen; it would mean that we do not have a CSSState for the phase
            Right cssstate -> do
                let (_, cssstate', evs) = runCSS finishReporting (CSSInstance totalWeight corruptWeight partyWeight maxParty) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

handleCSSEvents :: (ABBAMonad sig m) => Phase -> [CSSOutputEvent] -> m ()
handleCSSEvents _ [] = return ()
handleCSSEvents phase (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleCSSEvents phase evs
    where
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen ns) = CSSSeen phase ns
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs
handleCSSEvents phase (SelectCoreSet cs : evs) = delayThen phase (HandleCoreSet phase cs) >> handleCSSEvents phase evs
handleCSSEvents phase (WaitThenFinishReporting : evs) = delayThen phase (CSSFinishReporting phase) >> handleCSSEvents phase evs

-- |Generate my lottery ticket for the given phase.
makeTicket :: (ABBAMonad sig m) => Phase -> m TicketProof
{-# INLINE makeTicket #-}
makeTicket phase = do
        a <- ask
        return $ makeTicketProof (a ^. lotteryId phase) (privateKey a)

{-# INLINE unlessCompleted #-}
unlessCompleted :: (ABBAMonad sig m) => m () -> m ()
unlessCompleted a = do
        c <- use completed
        unless c a

--------------------------------------------------------------------------------
-- Protocol

-- |Called to start the ABBA protocol
{-# SPECIALIZE beginABBA :: Choice -> ABBA sig () #-}
beginABBA :: (ABBAMonad sig m) => Choice -> m ()
beginABBA c = unlessCompleted $ do
    cp <- use currentPhase
    when (cp == 0) $ do
        tkt <- makeTicket 0
        sendABBAMessage (Justified 0 c $! tkt)

-- |Called to indicate that a given choice is justified.
{-# SPECIALIZE justifyABBAChoice :: Choice -> ABBA sig () #-}
justifyABBAChoice :: (ABBAMonad sig m) => Choice -> m ()
justifyABBAChoice c = unlessCompleted $ liftCSSJustifyChoice 0 c

-- |Called when an 'ABBAMessage' is received.
--
-- This will be a void action if ABBA is complete.
-- 'ABBAMessage's actually encapsulate 'CSSMessage's and those are forwarded to the 'CSS' running protocol.
-- Is the message belongs only to 'ABBA':
--
-- 1. a @Justified@ message is translated into an 'Input' message to CSS and if possible also to a
--    justification of the choice if we agree (__step 1 of Graded Agreement__)
--
-- 2. a @WEAREDONE@ message registers the information received and if we already have enough weigh
--    accumulated on one alternative, finish the protocol with the given choice (__step 2 of Closing Down__).
{-# SPECIALIZE receiveABBAMessage :: Party -> ABBAMessage -> sig -> ABBA sig () #-}
receiveABBAMessage :: (ABBAMonad sig m) => Party -> ABBAMessage -> sig -> m ()
receiveABBAMessage src (Justified phase c !ticketProof) sig = unlessCompleted $ do
    ABBAInstance{..} <- ask
    lid <- view $ lotteryId phase
    -- Make sure the ticket checks out before we proceed.
    case checkTicketProof lid (pubKeys src) ticketProof (partyWeight src) totalWeight of
        Nothing -> return ()
        Just ticket -> handleJustified src phase ticket c sig
receiveABBAMessage src (CSSSeen phase ns) sig =
    unlessCompleted $ liftCSSReceiveMessage phase src (Seen ns) sig
receiveABBAMessage src (CSSDoneReporting phase m) sig =
    unlessCompleted $ liftCSSReceiveMessage phase src (DoneReporting m) sig
receiveABBAMessage src (WeAreDone c) sig = unlessCompleted $ do
    ABBAInstance{..} <- ask
    oldWAD <- use $ weAreDone c
    unless (PM.member src oldWAD) $ do
        newWAD <- weAreDone c <%= PM.insert src (partyWeight src) sig
        when (PM.weight newWAD >= totalWeight - corruptWeight && PM.weight oldWAD < totalWeight - corruptWeight) $ do
            completed .= True
            aBBAComplete c

-- |Invoke a DelayedABBAAction after its delay has completed.
triggerABBAAction :: (ABBAMonad sig m) => DelayedABBAAction -> m ()
triggerABBAAction (HandleCoreSet phase cs) = handleCoreSet phase cs
triggerABBAAction (CSSFinishReporting phase) = liftCSSFinishReporting phase
triggerABBAAction (CompletePhase phase nextBit newGrade) = do
        oldGrade <- currentGrade <<.= newGrade
        when (newGrade == 2 && oldGrade /= 2) $
            sendABBAMessage (WeAreDone nextBit)
        currentPhase .= phase + 1
        beginPhase (phase + 1)

-- |When an input becomes justified, we send an `Input` message to 'CSS' and if
-- applicable, we justify the input in CSS (which will effectively start CSS if not started yet).
handleJustified :: (ABBAMonad sig m) => Party -> Phase -> Ticket -> Choice -> sig -> m ()
{-# INLINE handleJustified #-}
handleJustified src phase ticket c sig = do
        ABBAInstance{..} <- ask
        liftCSSReceiveMessage phase src (Input c) sig
        phaseState phase . lotteryTickets . at' (ticketValue ticket, src) ?= ticket
        inputw <- use $ phaseState phase . inputWeight c
        forM_ inputw $ \ps -> let (b, ps') = PS.insertLookup src (partyWeight src) ps in
            unless b $
                if PS.weight ps' > corruptWeight then do
                    phaseState phase . inputWeight c .= Nothing
                    liftCSSJustifyChoice (phase + 1) c
                else
                    phaseState phase . inputWeight c .= Just ps'

-- | Start CSS for the next phase and pass everything we had accumulated
beginPhase :: (ABBAMonad sig m) => Phase -> m ()
beginPhase phase = use (phaseState phase . phaseCSSState) >>= \case
        Left (justif, msgs) -> do
            phaseState phase . phaseCSSState .= Right initialCSSState
            case justif of
                Nothing -> return ()
                Just (Just c) -> liftCSSJustifyChoice phase c
                Just Nothing -> liftCSSJustifyChoice phase False >> liftCSSJustifyChoice phase True
            forM_ msgs $ \(party, msg, sig) -> liftCSSReceiveMessage phase party msg sig
        Right _ -> return ()
{-# INLINE beginPhase #-}

-- |Deal with a core set being generated by CSS.  The phase should always be the current phase.
--
-- This implements __step 3 of Graded Agreement__ and if possible, __step 1 of Closing Down__.
-- We also compute the ticket for the next phase and send it (__step 2 of the next iteration of Graded Agreement__).
{-# SPECIALIZE handleCoreSet :: Phase -> CoreSet -> ABBA sig () #-}
handleCoreSet :: (ABBAMonad sig m) => Phase -> CoreSet -> m ()
handleCoreSet phase cs = do
        ABBAInstance{..} <- ask
        cp <- use currentPhase
        if phase /= cp then
            error $ "handleCoreSet on phase " ++ show phase ++ " but current phase is " ++ show cp
        else do
            let
                csTop = nomTop cs
                csBot = nomBot cs
                csRes p
                    | p `BitSet.member` csTop = Just True
                    | p `BitSet.member` csBot = Just False
                    | otherwise = Nothing
                topWeight = sum $ partyWeight <$> BitSet.toList csTop
                botWeight = sum $ partyWeight <$> BitSet.toList csBot
            tkts <- Map.toDescList <$> use (phaseState phase . lotteryTickets)
            let (nextBit, newGrade)
                    | BitSet.null csBot
                        = (True, 2)
                    | BitSet.null csTop
                        = (False, 2)
                    | topWeight >= totalWeight - corruptWeight
                        = (True, 1)
                    | botWeight >= totalWeight - corruptWeight
                        = (False, 1)
                    | botWeight <= corruptWeight -- In this case, topWeight > corruptWeight
                        = (True, 0)
                    | topWeight <= corruptWeight -- In this case, botWeight > corruptWeight
                        = (False, 0)
                    | otherwise
                        = case catMaybes $ (\((_,party), _) -> csRes party) <$> tkts of
                            (res:_) -> (res, 0)
                            [] -> error "Finalization failure: no lottery ticket could be verified" -- This should not be possible under standard assumptions
            tkt <- makeTicket (phase + 1)
            sendABBAMessage (Justified (phase+1) nextBit $! tkt)
            delayThen phase (CompletePhase phase nextBit newGrade)

--------------------------------------------------------------------------------
-- Summary

-- |Summary of a particular phase of the ABBA protocol.
data PhaseSummary sig = PhaseSummary {
    summaryJustifiedTop :: Map Party (TicketProof, sig),
    summaryJustifiedBot :: Map Party (TicketProof, sig),
    summaryCSSSeen :: Map Party [(NominationSet, sig)],
    summaryCSSDoneReporting :: Map Party (DoneReportingDetails sig)
}

putPhase :: (Ser.Serialize sig) => Party -> PhaseSummary sig -> Ser.Put
putPhase maxParty PhaseSummary{..} = do
        putPartyMap maxParty summaryJustifiedTop
        putPartyMap maxParty summaryJustifiedBot
        putPartyMap maxParty summaryCSSSeen
        putPartyMap maxParty ((\(DoneReportingDetails ns sig) -> (ns, sig)) <$> summaryCSSDoneReporting)

getPhase :: (Ser.Serialize sig) => Party -> Ser.Get (PhaseSummary sig)
getPhase maxParty = do
        summaryJustifiedTop <- getPartyMap maxParty
        summaryJustifiedBot <- getPartyMap maxParty
        summaryCSSSeen <- getPartyMap maxParty
        summaryCSSDoneReporting <- fmap (uncurry DoneReportingDetails) <$> getPartyMap maxParty
        return PhaseSummary{..}

-- |Get a summary of a phase state.  If the phase has not begun,
-- this returns @Nothing@.
phaseSummary :: SimpleGetter (PhaseState sig) (Maybe (PhaseSummary sig))
phaseSummary = to phs
    where
        phs PhaseState{_phaseCSSState = Right css, ..} = Just PhaseSummary{..}
            where
                ticketProofs = Map.fromList $ (snd *** ticketProof) <$> Map.toList _lotteryTickets
                CSSSummary{..} = css ^. cssSummary
                summaryJustifiedTop = Map.intersectionWith (,) ticketProofs summaryInputsTop
                summaryJustifiedBot = Map.intersectionWith (,) ticketProofs summaryInputsBot
                summaryCSSSeen = summarySeen
                summaryCSSDoneReporting = summaryDoneReporting
        phs _ = Nothing

-- |Summary of the ABBA protocol.  If there are sufficient WeAreDone messages
-- then it is not necessary to include the phases.
data ABBASummary sig = ABBASummary {
    -- |Summary of the phases.
    summaryPhases :: [PhaseSummary sig],
    -- |WeAreDone (Top) messages.
    summaryWeAreDoneTop :: Map Party sig,
    -- |WeAreDone (Bottom) messages.
    summaryWeAreDoneBot :: Map Party sig
}

putABBASummary :: (Ser.Serialize sig) => Party -> ABBASummary sig -> Ser.Put
putABBASummary maxParty ABBASummary{..} = do
        Ser.putWord32be (fromIntegral $ length summaryPhases)
        forM_ summaryPhases (putPhase maxParty)
        putPartyMap maxParty summaryWeAreDoneTop
        putPartyMap maxParty summaryWeAreDoneBot

getABBASummary :: (Ser.Serialize sig) => Party -> Ser.Get (ABBASummary sig)
getABBASummary maxParty = do
        nPhases <- Ser.getWord32be
        summaryPhases <- forM [1..nPhases] (\_ -> getPhase maxParty)
        summaryWeAreDoneTop <- getPartyMap maxParty
        summaryWeAreDoneBot <- getPartyMap maxParty
        return ABBASummary{..}

-- |Derive an 'ABBASummary' from the given 'ABBAState'.
-- If the protocol is complete, then the WeAreDone weight for one of the outcomes
-- should be at least (totalWeight - corruptWeight).
abbaSummary :: SimpleGetter (ABBAState sig) (ABBASummary sig)
abbaSummary = to abbas
    where
        abbas ABBAState{_completed = True, ..} = ABBASummary {
                summaryPhases = [],
                summaryWeAreDoneTop = if PM.weight _topWeAreDone >= PM.weight _botWeAreDone then PM.partyMap _topWeAreDone else Map.empty,
                summaryWeAreDoneBot = if PM.weight _topWeAreDone >= PM.weight _botWeAreDone then Map.empty else PM.partyMap _botWeAreDone
            }
        abbas ABBAState{..} = ABBASummary{..}
            where
                summaryWeAreDoneTop = PM.partyMap _topWeAreDone
                summaryWeAreDoneBot = PM.partyMap _botWeAreDone
                summaryPhases = processPhaseStates 0 (Map.toAscList _phaseStates)
                processPhaseStates _ [] = []
                processPhaseStates expPhase ((phase,pst) : ps)
                    | expPhase == phase = case pst ^. phaseSummary of
                        Nothing -> []
                        Just p -> p : processPhaseStates (expPhase+1) ps
                    | otherwise = []

-- |Process the messages contained in an 'ABBASummary'.  The returned value indicates if the summary
-- is *behind* the current state: that is, if we produced a summary, then it would contain (useful)
-- messages that are not included in the given summary.
processABBASummary :: (ABBAMonad sig m, Eq sig) => ABBASummary sig -> (Party -> ABBAMessage -> sig -> Bool) -> m Bool
processABBASummary ABBASummary{..} checkSig = do
    ABBAInstance{..} <- ask
    st <- get
    let checkWAD b = \(party, sig) -> st ^? (if b then topWeAreDone else botWeAreDone) . ix party == Just sig || checkSig party (WeAreDone b) sig
        sWADTop = filter (checkWAD True) (Map.toList summaryWeAreDoneTop)
        sWADBot = filter (checkWAD False) (Map.toList summaryWeAreDoneBot)
        wadWeight m = sum (partyWeight . fst <$> m)
        checkWADWeight m = wadWeight m >= totalWeight - corruptWeight
        -- The summary is complete if it includes enough signed WeAreDone messages
        summaryComplete = checkWADWeight sWADTop || checkWADWeight sWADBot
    if st ^. completed then
        -- We have completed, so they are behind unless they have at least totalWeight - corruptWeight reporting done
        -- with a consistent outcome.
        return $! not summaryComplete
    else do
        -- First process the WeAreDone messages: if we have enough, there's no need to go further
        forM_ sWADTop $ \(party, sig) -> receiveABBAMessage party (WeAreDone True) sig
        forM_ sWADBot $ \(party, sig) -> receiveABBAMessage party (WeAreDone False) sig
        if summaryComplete then
            -- If the summary is complete, then we should be complete too now.
            return False
        else do
            st' <- get
            let sWADBehind = wadWeight sWADTop < st' ^. topWeAreDone . to PM.weight ||
                            wadWeight sWADBot < st' ^. botWeAreDone . to PM.weight
            cssBehind <- or <$> forM (zip [0..] summaryPhases) (\(phaseInd, PhaseSummary{..}) -> do
                ps <- fromMaybe initialPhaseState <$> use (phaseStates . at phaseInd)
                lid <- view $ lotteryId phaseInd
                let
                    myTickets :: Map Party Ticket
                    myTickets = ps ^. lotteryTickets . to (Map.fromList . fmap (first snd) . Map.toList)
                    myCheckTicketProof party (tp, _) = (ticketProof <$> myTickets ^? ix party) == Just tp || isJust (checkTicketProof lid (pubKeys party) tp (partyWeight party) totalWeight)
                    cssState = case ps ^. phaseCSSState of
                        -- This case shouldn't occur, but if it does, it doesn't matter
                        -- because the state is only used to avoid redundant signature checks
                        Left _ -> initialCSSState
                        Right s -> s
                    checkCSSSig' party (Input c) sig = checkSig party (Justified phaseInd c undefined) sig
                    checkCSSSig' party (Seen ns) sig = checkSig party (CSSSeen phaseInd ns) sig
                    checkCSSSig' party (DoneReporting ns) sig = checkSig party (CSSDoneReporting phaseInd ns) sig
                    checkCSSSig party msg sig = cssCheckExistingSig cssState party msg sig || checkCSSSig' party msg sig
                -- If we have completed this phase (CSS is complete), we just check if the summary is complete for this phase
                if st' ^. currentPhase > phaseInd then do
                    let
                        csss = CSSSummary {
                            summaryInputsTop = snd <$> Map.filterWithKey myCheckTicketProof summaryJustifiedTop,
                            summaryInputsBot = snd <$> Map.filterWithKey myCheckTicketProof summaryJustifiedBot,
                            summarySeen = summaryCSSSeen,
                            summaryDoneReporting = summaryCSSDoneReporting
                        }
                        cssInst = CSSInstance totalWeight corruptWeight partyWeight maxParty
                        cssComplete = cssSummaryCheckComplete csss cssInst checkCSSSig
                    return $! not cssComplete
                else do
                    -- If we haven't completed the phase
                    -- Check the signatures & tickets
                    let
                        checkTicketAndSig b party (tp, sig) = do
                            guard $ checkCSSSig party (Input b) sig
                            (, sig) <$> case myTickets ^? ix party of
                                Nothing -> checkTicketProof lid (pubKeys party) tp (partyWeight party) totalWeight
                                Just t -> if ticketProof t == tp then Just t else
                                        checkTicketProof lid (pubKeys party) tp (partyWeight party) totalWeight
                        sJTop = Map.mapMaybeWithKey (checkTicketAndSig True) summaryJustifiedTop
                        sJBot = Map.mapMaybeWithKey (checkTicketAndSig False) summaryJustifiedBot
                        sCS = Map.mapWithKey (\party -> filter (\(ns, sig) -> checkCSSSig party (Seen ns) sig)) summaryCSSSeen
                        sCDR = Map.filterWithKey (\party (DoneReportingDetails ns sig) -> checkCSSSig party (DoneReporting ns) sig) summaryCSSDoneReporting
                    -- Handle the messages
                    forM_ (Map.toList sJTop) $ \(party, (ticket, sig)) -> handleJustified party phaseInd ticket True sig
                    forM_ (Map.toList sJBot) $ \(party, (ticket, sig)) -> handleJustified party phaseInd ticket False sig
                    forM_ (Map.toList sCS) $ \(party, l) -> forM_ l $ \(ns,sig) -> liftCSSReceiveMessage phaseInd party (Seen ns) sig
                    forM_ (Map.toList sCDR) $ \(party, DoneReportingDetails ns sig) -> liftCSSReceiveMessage phaseInd party (DoneReporting ns) sig
                    -- Check if we have (or had) any messages that were not in the catch-up
                    return $! cssSummaryIsBehind cssState (CSSSummary (snd <$> sJTop) (snd <$> sJBot) sCS sCDR))
            return $! sWADBehind || cssBehind
