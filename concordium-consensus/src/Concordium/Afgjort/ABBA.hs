{-# LANGUAGE
    TemplateHaskell, 
    ScopedTypeVariables,
    GeneralizedNewtypeDeriving,
    RankNTypes,
    OverloadedStrings #-}
{- |Asynchronous Binary Byzantine Agreement algorithm -}
module Concordium.Afgjort.ABBA(
    Phase,
    ABBAMessage(..),
    ABBAInstance(ABBAInstance),
    ABBAState(..),
    initialABBAState,
    ABBAMonad(..),
    ABBAOutputEvent(..),
    ABBA,
    runABBA,
    beginABBA,
    justifyABBAChoice,
    receiveABBAMessage,
    Choice,
    PhaseSummary(..),
    ABBASummary(..),
    abbaSummary,
    processABBASummary,
    abbaOutcome,
    getABBASummary,
    putABBASummary
) where

import qualified Data.Map as Map
import Data.Map (Map)
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

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.PartySet (PartySet)
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.PartyMap (PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM

atStrict :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
atStrict k f m = f mv <&> \case
        Nothing -> maybe m (const (Map.delete k m)) mv
        Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE atStrict #-}

-- |A phase in the ABBA protocol
type Phase = Word32

-- |A message in the ABBA protocol
data ABBAMessage
    = Justified Phase Choice TicketProof            -- ^Party's input for a given phase
    | CSSSeen Phase NominationSet                   -- ^CSS seen message for a phase
    | CSSDoneReporting Phase NominationSet          -- ^CSS done reporting message for a phase
    | WeAreDone Choice                              -- ^Message that indicates consensus should be reached
    deriving (Eq, Ord, Show)

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
    baid :: BS.ByteString,
    -- |The total weight of all parties
    totalWeight :: VoterPower,
    -- |The maximum weight of corrupt parties (must be less than @totalWeight/3@).
    corruptWeight :: VoterPower,
    -- |The weight of each party
    partyWeight :: Party -> VoterPower,
    -- |The maximal party
    maxParty :: Party,
    -- |The public VRF key of each party
    pubKeys :: Party -> VRF.PublicKey,
    -- |My party
    me :: Party,
    -- |My VRF key
    privateKey :: VRF.KeyPair
}

-- |The state of a phase in the protocol.
--
-- This includes the lottery tickets submitted by parties, the CSS state for the current phase,
-- and the total weight and set of parties that have nominated each input.  Once the weight
-- exceeds the threshold, we record it as @Nothing@, forgetting the exact weight and parties.
--
-- INVARIANT: Each CSS 'Input' has a corresponding valid 'Ticket' and vice-versa.
data PhaseState sig = PhaseState {
    _lotteryTickets :: Map (Double, Party) Ticket,
    _phaseCSSState :: Either (Maybe Choices, Seq (Party, CSSMessage, sig)) (CSSState sig),
    _topInputWeight :: Maybe PartySet,
    _botInputWeight :: Maybe PartySet
} deriving (Show)
makeLenses ''PhaseState

-- |The total weight and set of parties nominating a particular choice.
inputWeight :: Choice -> Lens' (PhaseState sig) (Maybe PartySet)
inputWeight True = topInputWeight
inputWeight False = botInputWeight

-- |The initial state of a phase
initialPhaseState :: PhaseState sig
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = Left (Nothing, Seq.empty),
    _topInputWeight = Just PS.empty,
    _botInputWeight = Just PS.empty
}

-- |The state of the ABBA protocol.
--
-- This includes the current phase, the state of all phases, the current grade,
-- and the set and weight of parties that have claimed we are done with each
-- possible output choice.
data ABBAState sig = ABBAState {
    _currentPhase :: Phase,
    _phaseStates :: Map Phase (PhaseState sig),
    _currentGrade :: Word8,
    _topWeAreDone :: PartyMap sig,
    _botWeAreDone :: PartyMap sig,
    _completed :: Bool
} deriving (Show)
makeLenses ''ABBAState

-- |The state of a particular phase
phaseState :: Phase -> Lens' (ABBAState sig) (PhaseState sig)
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . atStrict p ?~ t)

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

-- |The @ABBAMonad@ class defines the events associated with the ABBA protocol.
class (MonadState (ABBAState sig) m, MonadReader ABBAInstance m, MonadIO m) => ABBAMonad sig m where
    -- |Sign and broadcast an ABBA message to all parties, __including__ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage -> m ()
    -- |Determine the result
    aBBAComplete :: Choice -> m ()

-- |Representation of (output) events associated with the ABBA protocol.
data ABBAOutputEvent
    = SendABBAMessage ABBAMessage   -- ^Sign and broadcast a message
    | ABBAComplete Choice                   -- ^Determine result

-- |A concrete implementation of the ABBA monad.
newtype ABBA sig a = ABBA {
    runABBA' :: RWST ABBAInstance (Endo [ABBAOutputEvent]) (ABBAState sig) IO a
} deriving (Functor, Applicative, Monad, MonadIO)

-- |Run part of the ABBA protocol, given an 'ABBAInstance' and 'ABBAState'.
-- The result includes the updated state and a list of 'ABBAOutputEvent's that occurred during the execution.
{-# INLINE runABBA #-}
runABBA :: ABBA sig a -> ABBAInstance -> ABBAState sig -> IO (a, ABBAState sig, [ABBAOutputEvent])
runABBA z i s = runRWST (runABBA' z) i s <&> _3 %~ (\(Endo f) -> f [])

instance MonadState (ABBAState sig) (ABBA sig) where
    get = ABBA get
    put = ABBA . put
    state = ABBA . state

instance MonadReader ABBAInstance (ABBA sig) where
    ask = ABBA ask
    reader = ABBA . reader
    local f = ABBA . local f . runABBA'

instance ABBAMonad sig (ABBA sig) where
    sendABBAMessage msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete c = ABBA $ tell $ Endo (ABBAComplete c :)

liftCSSReceiveMessage :: (ABBAMonad sig m) => Phase -> Party -> CSSMessage -> sig -> m ()
liftCSSReceiveMessage phase src msg sig = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (justif, msgs |> (src, msg, sig))
            Right cssstate -> do
                let (_, cssstate', evs) = runCSS (receiveCSSMessage src msg sig) (CSSInstance totalWeight corruptWeight partyWeight maxParty) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

liftCSSJustifyChoice :: (ABBAMonad sig m) => Phase -> Choice -> m ()
liftCSSJustifyChoice phase c = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (addChoice c justif, msgs)
            Right cssstate -> do
                let (_, cssstate', evs) = runCSS (justifyChoice c) (CSSInstance totalWeight corruptWeight partyWeight maxParty) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

handleCSSEvents :: (ABBAMonad sig m) => Phase -> [CSSOutputEvent] -> m ()
handleCSSEvents _ [] = return ()
handleCSSEvents phase (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleCSSEvents phase evs
    where
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen ns) = CSSSeen phase ns
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs
handleCSSEvents phase (SelectCoreSet cs : evs) = handleCoreSet phase cs >> handleCSSEvents phase evs

-- |Deal with a core set being generated by CSS.  The phase should always be the current phase.
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
            oldGrade <- currentGrade <<.= newGrade
            when (newGrade == 2 && oldGrade /= 2) $
                sendABBAMessage (WeAreDone nextBit)
            currentPhase .= phase + 1
            beginPhase (phase + 1)
            tkt <- makeTicket (phase + 1)
            sendABBAMessage (Justified (phase+1) nextBit tkt)

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

-- |Get the lottery identifier string for the given phase.
lotteryId :: Phase -> SimpleGetter ABBAInstance BS.ByteString
lotteryId phase = to $ \a ->
        Ser.runPut $ Ser.put (baid a) >> Ser.put phase

-- |Generate my lottery ticket for the given phase.
makeTicket :: (ABBAMonad sig m) => Phase -> m TicketProof
{-# INLINE makeTicket #-}
makeTicket phase = do
        a <- ask
        liftIO $ makeTicketProof (a ^. lotteryId phase) (privateKey a)

{-# INLINE unlessCompleted #-}
unlessCompleted :: (ABBAMonad sig m) => m () -> m ()
unlessCompleted a = do
        c <- use completed
        unless c a

-- |Called to indicate that a given choice is justified.
{-# SPECIALIZE justifyABBAChoice :: Choice -> ABBA sig () #-}
justifyABBAChoice :: (ABBAMonad sig m) => Choice -> m ()
justifyABBAChoice c = unlessCompleted $ liftCSSJustifyChoice 0 c

handleJustified :: (ABBAMonad sig m) => Party -> Phase -> Ticket -> Choice -> sig -> m ()
{-# INLINE handleJustified #-}
handleJustified src phase ticket c sig = do
        ABBAInstance{..} <- ask
        liftCSSReceiveMessage phase src (Input c) sig
        phaseState phase . lotteryTickets . atStrict (ticketValue ticket, src) ?= ticket
        inputw <- use $ phaseState phase . inputWeight c
        forM_ inputw $ \ps -> let (b, ps') = PS.insertLookup src (partyWeight src) ps in
            unless b $
                if PS.weight ps' > corruptWeight then do
                    phaseState phase . inputWeight c .= Nothing
                    liftCSSJustifyChoice (phase + 1) c
                else
                    phaseState phase . inputWeight c .= Just ps'

-- |Called when an 'ABBAMessage' is received.
{-# SPECIALIZE receiveABBAMessage :: Party -> ABBAMessage -> sig -> ABBA sig () #-}
receiveABBAMessage :: (ABBAMonad sig m) => Party -> ABBAMessage -> sig -> m ()
receiveABBAMessage src (Justified phase c ticketProof) sig = unlessCompleted $ do
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

-- |Called to start the ABBA protocol
{-# SPECIALIZE beginABBA :: Choice -> ABBA sig () #-}
beginABBA :: (ABBAMonad sig m) => Choice -> m ()
beginABBA c = unlessCompleted $ do
    cp <- use currentPhase
    when (cp == 0) $ do
        tkt <- makeTicket 0
        sendABBAMessage (Justified 0 c tkt)

-- |Get the decision from an ABBAState.  Returns @Nothing@ if the protocol is not  yet completed.
{-# INLINE abbaOutcome #-}
abbaOutcome :: SimpleGetter (ABBAState sig) (Maybe Choice)
abbaOutcome = to aoc
    where
        aoc ABBAState{..} = if _completed then Just (PM.weight _topWeAreDone >= PM.weight _botWeAreDone) else Nothing

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