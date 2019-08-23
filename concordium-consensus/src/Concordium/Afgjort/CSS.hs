{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, FlexibleInstances, BangPatterns #-}
-- |Core Set Selection algorithm
module Concordium.Afgjort.CSS(
    CSSMessage(..),
    CoreSet,
    CSSState(..),
    initialCSSState,
    CSSMonad(..),
    CSSOutputEvent(..),
    CSS,
    runCSS,
    CSSInstance(CSSInstance), -- Don't export projections or constructor
    justifyChoice,
    receiveCSSMessage,
    CSSSummary(..),
    cssSummary,
    processCSSSummary,
    DoneReportingDetails(..),
    cssSummaryCheckComplete,
    cssCheckExistingSig,
    -- * For internal use
    report,
    topJustified,
    botJustified,
    iSaw,
    inputTop,
    inputBot,
    input,
    justified,
    manySaw,
    unjustifiedDoneReporting,
    sawJustified,
    sawTop,
    sawBot,
    sawMessages,
    saw,
    core,
    justifiedDoneReporting,
    PartySet(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.Afgjort.Types
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.PartySet(PartySet)
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.PartyMap(PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM

atStrict :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
atStrict k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (Map.delete k m)) mv
        Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE atStrict #-}

nonEmpty :: (Monoid (f v), Foldable f) => Lens' (Maybe (f v)) (f v)
nonEmpty afb s = f <$> afb (fromMaybe mempty s)
    where
        f y = if null y then Nothing else Just y
{-# INLINE nonEmpty #-}

data CSSMessage
    = Input !Choice
    | Seen !NominationSet
    | DoneReporting !NominationSet
    deriving (Eq, Ord, Show)

data CSSInstance = CSSInstance {
    -- |The total weight of all parties
    totalWeight :: !VoterPower,
    -- |The (maximum) weight of all corrupt parties (should be less than @totalWeight/3@).
    corruptWeight :: !VoterPower,
    -- |The weight of each party
    partyWeight :: Party -> VoterPower,
    -- |The maximal party
    maxParty :: !Party
}

type CoreSet = NominationSet

data DoneReportingDetails sig = DoneReportingDetails !NominationSet sig
    deriving (Show, Eq)

-- | Invariant:
--
--  * If '_report' (we are in the reporting phase):
--      * if '_botJustified' then every party in '_inputBot' must have an entry in '_iSaw';
--      * if '_topJustified' then every party in '_inputTop' must have an entry in '_iSaw'.
--  * For every entry in the '_iSaw' nomination set:
--      * the nominated value is justified;
--      * the party nominated that value.
--  * '_iSaw' contains at most one nomination per party.
--  * @p@ is in '_manySaw' exactly when, for some @s@ with @PS.weight s >= totalWeight - corruptWeight@, either:
--      * '_topJustified' and @(p, s)@ is in '_sawTop'; or
--      * '_botJustified' and @(p, s)@ is in '_sawBot'.
--  * All keys of '_unjustifiedDoneReporting' should not have corresponding justified inputs.
data CSSState sig = CSSState {
    -- |Whether we are in the report stage (initially @True@)
    _report :: !Bool,
    -- |Whether *bottom* is considered justified
    _botJustified :: !Bool,
    -- |Whether *top* is considered justified
    _topJustified :: !Bool,
    -- |The parties that have nominated *top*
    _inputTop :: !(Map Party sig),
    -- |The parties that have nominated *bottom*
    _inputBot :: !(Map Party sig),
    -- |For each party, the total set of parties that report having seen a nomination of *top* by that party.
    _sawTop :: !(Map Party (PartySet)),
    -- |As above, for *bottom*
    _sawBot :: !(Map Party (PartySet)),
    -- |For each party, we record the Seen messages.
    -- We do not need to record every message; just enough to cover
    -- everything the party claims to have seen.  (When an honest
    -- party sends multiple Seen messages, one will subsume the other.)
    _sawMessages :: !(Map Party [(NominationSet, sig)]),
    -- |The set of nominations we saw.  That is, the first justified nomination we received from each party.
    _iSaw :: !NominationSet,
    -- |The total weight of parties in '_iSaw'.
    _iSawWeight :: !VoterPower,
    -- |The set of parties for which @(totalWeight-corruptWeight)@ parties have sent justified Seen messages for the same choice.
    _manySaw :: !PartySet,
    -- |For each pair @(seen,c)@ for which we have not received a justified input, this records
    -- parties that have sent DoneReporting messages that include this pair, where all previous
    -- pairs have been justified and all future pairs are held in the list.
    _unjustifiedDoneReporting :: !(Map (Party, Choice) (Map Party ([(Party, Choice)], DoneReportingDetails sig))),
    -- |The set of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReporting :: !(PartyMap (DoneReportingDetails sig)),
    -- |If @PM.weight _justifiedDoneReporting@ is at least @(totalWeight-corruptWeight)@, then the core set determined at that time.  Otherwise @Nothing@.
    _core :: Maybe CoreSet
} deriving (Show)
makeLenses ''CSSState

initialCSSState :: CSSState sig
initialCSSState = CSSState {
    _report = True,
    _botJustified = False,
    _topJustified = False,
    _inputTop = Map.empty,
    _inputBot = Map.empty,
    _sawTop = Map.empty,
    _sawBot = Map.empty,
    _sawMessages = Map.empty,
    _iSaw = emptyNominationSet,
    _iSawWeight = 0,
    _manySaw = PS.empty,
    _unjustifiedDoneReporting = Map.empty,
    _justifiedDoneReporting = PM.empty,
    _core = Nothing
}

justified :: Choice -> Lens' (CSSState sig) Bool
justified True = topJustified
justified False = botJustified

input :: Choice -> Lens' (CSSState sig) (Map Party sig)
input True = inputTop
input False = inputBot

saw :: Choice -> Lens' (CSSState sig) (Map Party (PartySet))
saw True = sawTop
saw False = sawBot

-- |Given a witnessing party (@seer@), a choice (@c@) and a nominating party (@seen@),
-- produces a 'SimpleGetter' for a 'CSSState' that returns @True@ exactly when we have
-- a justified message from @seer@ that @seen@ nominated @c@.
sawJustified ::
    Party       -- ^ @seer@
    -> Choice   -- ^ @c@
    -> Party    -- ^ @seen@
    -> SimpleGetter (CSSState sig) Bool
sawJustified seer c seen = to $ \s ->
    (s ^. justified c) && (seen `Map.member` (s ^. input c)) &&
        case s ^. saw c . at seen of
            Nothing -> False
            Just m -> seer `PS.member` m

class (MonadState (CSSState sig) m, MonadReader CSSInstance m) => CSSMonad sig m where
    -- |Sign and broadcast a CSS message to all parties, __including__ our own 'CSSInstance'.
    sendCSSMessage :: CSSMessage -> m ()
    -- |Determine the core set.
    selectCoreSet :: CoreSet -> m ()

data CSSOutputEvent
    = SendCSSMessage CSSMessage
    | SelectCoreSet CoreSet

newtype CSS sig a = CSS {
    runCSS' :: RWS CSSInstance (Endo [CSSOutputEvent]) (CSSState sig) a
} deriving (Functor, Applicative, Monad)

{-# INLINE runCSS #-}
runCSS :: CSS sig a -> CSSInstance -> CSSState sig -> (a, CSSState sig, [CSSOutputEvent])
runCSS z i s = runRWS (runCSS' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadState (CSSState sig) (CSS sig) where
    get = CSS get
    put = CSS . put
    state = CSS . state

instance MonadReader CSSInstance (CSS sig) where
    ask = CSS ask
    reader = CSS . reader
    local f = CSS . local f . runCSS'

instance CSSMonad sig (CSS sig) where
    sendCSSMessage msg = CSS $ tell $ Endo (SendCSSMessage msg :)
    selectCoreSet cs = CSS $ tell $ Endo (SelectCoreSet cs :)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM t a = t >>= \r -> when r a

-- |Called to notify when a choice becomes justified.
{-# SPECIALIZE justifyChoice :: Choice -> CSS sig () #-}
justifyChoice :: (CSSMonad sig m) => Choice -> m ()
justifyChoice c = do
    -- Record the choice as justified
    alreadyJustified <- justified c <<.= True
    -- If it wasn't already justified...
    unless alreadyJustified $ do
        inputs <- use (input c)
        forM_ (Map.keys inputs) $ \p -> justifyNomination p c

unlessComplete :: (CSSMonad sig m) => m () -> m ()
unlessComplete a = do
    complete <- isJust <$> use core
    unless complete a

-- |Handle an incoming CSSMessage from a party. 
{-# SPECIALIZE receiveCSSMessage :: Party -> CSSMessage -> sig -> CSS sig () #-}
receiveCSSMessage :: (CSSMonad sig m) => Party -> CSSMessage -> sig -> m ()
receiveCSSMessage src (Input c) sig = unlessComplete $ do
    -- Record that we've seen this nomination
    input c %= Map.insert src sig
    whenM (use (justified c)) $ justifyNomination src c
receiveCSSMessage src (Seen ns) sig = unlessComplete $ do
    CSSInstance{..} <- ask
    present <- forM (nominationSetToList ns) $ \(sp, c) -> do
        -- Update the set of parties that claim to have seen @sp@ make choice @c@
        let updateSaw Nothing = let w = partyWeight src in ((w, False), Just (PS.singleton src w))
            updateSaw (Just ps) = let (b, ps') = PS.insertLookup src (partyWeight src) ps in ((PS.weight ps', b), Just ps')
        (weight, alreadyPresent) <- state ((saw c . atStrict sp) updateSaw)
        unless alreadyPresent $
            -- Check if this seen message is justified
            whenM (use (justified c)) $ whenM (Map.member sp <$> use (input c)) $ do
                -- If the weight is high enough, record it in manySaw
                when (weight >= totalWeight - corruptWeight) $ addManySaw sp
                -- If there is a DoneReporting message awaiting this becoming justified, handle it
                hdr <- unjustifiedDoneReporting . atStrict (sp, c) . nonEmpty . atStrict src <<.= Nothing
                forM_ hdr $ handleDoneReporting src
        return alreadyPresent
    unless (all id present) $
        -- This message includes some new observations, so add it to sawMessages.
        -- We filter out any Seen messages that are subsumed by the new message.
        sawMessages . at src . nonEmpty %= \l -> (ns, sig) : filter (\(ns', _) -> not $ ns' `subsumedBy` ns) l
receiveCSSMessage src (DoneReporting sawSet) sig = unlessComplete $ handleDoneReporting src (nominationSetToList sawSet, DoneReportingDetails sawSet sig)


-- |Call when a nomination (@c@) by a party (@src@) becomes justified,
-- i.e. @input c@ contains @src@ and @justified c@ gives @True@.
{-# SPECIALIZE justifyNomination :: Party -> Choice -> CSS sig () #-}
justifyNomination :: (CSSMonad sig m) => Party -> Choice -> m ()
justifyNomination src c = do
    CSSInstance{..} <- ask
    -- In the report phase, add the nomination to @iSaw@ and send a seen message
    -- (unless we already did for this @src@).
    whenM (use report) $ do
        seen <- isJust . nominations src <$> use iSaw
        unless seen $ do
            newiSaw <- iSaw <%= addNomination src c
            newiSawWeight <- iSawWeight <%= (+ partyWeight src)
            -- Don't send a seen message until we have sufficient weight.
            when (newiSawWeight >= totalWeight - corruptWeight) $
                sendCSSMessage $ Seen newiSaw
    -- Update manySaw if the now-justified choice has been Seen sufficiently
    use (saw c . at src) >>= mapM_ (\parties ->
        when (PS.weight parties >= totalWeight - corruptWeight) $ addManySaw src)
    -- Consider any DoneReporting messages waiting on justified @Seen src c@ messages
    use (unjustifiedDoneReporting . at (src, c)) >>= mapM_ (\m ->
        -- Consider the @Seen@ messages (which now become justified)
        use (saw c . at src) >>= mapM_ (\jsaw -> do
            -- Divide the DoneReporting messages on whether we have got the
            -- corresponding (now justified) @Seen@ message
            let (js, ujs) = Map.partitionWithKey (\k _ -> k `PS.member` jsaw) m
            -- Put those messages back where we don't
            unjustifiedDoneReporting . atStrict (src, c) .= if Map.null ujs then Nothing else Just ujs
            -- And handle those where we do.
            forM_ (Map.toList js) $ uncurry handleDoneReporting))

{-# SPECIALIZE addManySaw :: Party -> CSS sig () #-}
addManySaw :: (CSSMonad sig m) => Party -> m ()
addManySaw party = do
    CSSInstance{..} <- ask
    msw <- PS.weight <$> (manySaw <%= PS.insert party (partyWeight party))
    when (msw >= totalWeight - corruptWeight) $ do
        oldRep <- report <<.= False
        when oldRep $
            sendCSSMessage . DoneReporting =<< use iSaw
{-# SPECIALIZE handleDoneReporting :: Party -> ([(Party, Choice)], DoneReportingDetails sig) -> CSS sig () #-}
handleDoneReporting :: (CSSMonad sig m) => Party -> ([(Party, Choice)], DoneReportingDetails sig) -> m ()
handleDoneReporting party ([], drd) = do
    CSSInstance{..} <- ask
    alreadyDone <- PM.member party <$> use justifiedDoneReporting
    unless alreadyDone $ do
        newJDR <- justifiedDoneReporting <%= PM.insert party (partyWeight party) drd
        -- newJDRW <- justifiedDoneReportingWeight <%= (+ partyWeight party)
        -- When we hit the mark, we can generate the core set.
        when (PM.weight newJDR >= totalWeight - corruptWeight) $ do
            css@CSSState{..} <- get
            when (isNothing _core) $ do
                let theCore = NominationSet {
                    nomMax = maxParty,
                    nomTop = if _topJustified then BitSet.fromList (Map.keys _inputTop) else BitSet.empty,
                    nomBot = if _botJustified then BitSet.fromList (Map.keys _inputBot) else BitSet.empty
                }
                put (css{_core = Just theCore})
                selectCoreSet theCore
handleDoneReporting party ((s, c) : remainder, drd) = do
    scJust <- use (sawJustified party c s)
    if scJust then
        handleDoneReporting party (remainder, drd)
    else
        unjustifiedDoneReporting . atStrict (s, c) %= \case
            Nothing -> Just (Map.singleton party (remainder, drd))
            Just l -> Just (Map.insert party (remainder, drd) l)

data CSSSummary sig = CSSSummary {
    summaryInputsTop :: Map Party sig,
    summaryInputsBot :: Map Party sig,
    summarySeen :: Map Party [(NominationSet, sig)],
    summaryDoneReporting :: Map Party (DoneReportingDetails sig)
}

cssSummary :: SimpleGetter (CSSState sig) (CSSSummary sig)
cssSummary = to csss
    where
        csss CSSState{..} = CSSSummary{..}
            where
                summaryInputsTop = _inputTop
                summaryInputsBot = _inputBot
                summarySeen = _sawMessages
                summaryDoneReporting = PM.partyMap _justifiedDoneReporting

-- |Process a 'CSSSummary', handling any new messages.  The result is a pair
-- @(ahead, behind)@, where @ahead@ is 'True' if we made progress in CSS as a
-- result, and @behind@ is true if the summary does not contain sufficient
-- information to complete CSS (given our justified inputs) and out state
-- has messages that contain new information.
processCSSSummary :: forall m sig. (CSSMonad sig m, Eq sig) => CSSSummary sig -> (Party -> CSSMessage -> sig -> Bool) -> m CatchUpResult
processCSSSummary CSSSummary{..} checkSig = do
        st <- get
        let
            myCheckSig party m@(Input c) sig = (st ^? input c . ix party) == Just sig || checkSig party m sig
            myCheckSig party m@(Seen ns) sig = (ns, sig) `elem` (st ^. sawMessages . at party . non []) || checkSig party m sig
            myCheckSig party m@(DoneReporting ns) sig = (st ^? justifiedDoneReporting . ix party) == Just (DoneReportingDetails ns sig) || checkSig party m sig
        done <- isJust <$> use core
        if done then checkComplete myCheckSig else do
            rInpTop <- checkInputs summaryInputsTop True
            rInpBot <- checkInputs summaryInputsBot False
            rSeen <- checkSeen myCheckSig
            rDR <- checkDR myCheckSig
            return (rInpTop <> rInpBot <> rSeen <> rDR)
    where
        checkComplete myCheckSig = do
            CSSInstance{..} <- ask
            -- For each done reporting:
            --   1. check the signature
            --   2. check that all reported nominations have justified inputs, with valid signatures
            --   3. check that all reported nominations have a corresponding seen message, with a valid signature
            -- If the total weight of done reporting messages that pass these tests is at least @totalWeight - corruptWeight@
            -- then the summary is complete.  (i.e. any honest party that receives this summary has sufficient input to
            -- complete the CSS round.)
            let
                checkDoneReps :: BitSet.BitSet -> BitSet.BitSet -> VoterPower -> [(Party, DoneReportingDetails sig)] -> m VoterPower
                checkDoneReps _ _ !drWeight [] = return drWeight
                checkDoneReps preCheckTop preCheckBot !drWeight ((party, DoneReportingDetails ns sig) : rest) =
                    if myCheckSig party (DoneReporting ns) sig then do
                        let
                            checkInps toCheck preCheck inps b = checkParties preCheck (BitSet.toList newCheck)
                                where
                                    newCheck = toCheck `BitSet.difference` preCheck
                                    checkParties newPreCheck [] = (newPreCheck, True)
                                    checkParties newPreCheck (p : ps) = case Map.lookup p inps of
                                        Nothing -> (newPreCheck, False)
                                        Just isig -> if myCheckSig party (Input b) isig then
                                                        checkParties (BitSet.insert party newPreCheck) ps
                                                    else
                                                        (newPreCheck, False)
                            (preCheckTop1, topChecked) = checkInps (nomTop ns) preCheckTop summaryInputsTop True
                            (preCheckBot1, botChecked) = checkInps (nomBot ns) preCheckBot summaryInputsBot False
                        if topChecked then
                            if botChecked then do
                                let partySeen = foldr unionNominationSet emptyNominationSet $
                                        fst <$> filter (\(sns, ssig) -> myCheckSig party (Seen sns) ssig) (summarySeen ^. at party . non [])
                                if ns `subsumedBy` partySeen then
                                    checkDoneReps preCheckTop1 preCheckBot1 (partyWeight party + drWeight) rest
                                else
                                    checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                            else
                                checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                        else
                            checkDoneReps preCheckTop1 preCheckBot drWeight rest
                    else
                        checkDoneReps preCheckTop preCheckBot drWeight rest
            drWeight <- checkDoneReps BitSet.empty BitSet.empty 0 (Map.toList summaryDoneReporting)
            if drWeight >= totalWeight - corruptWeight then
                return mempty
            else
                return $ mempty {curBehind = True}
        checkInputs summaryInputs c = do
            myInputs <- use (input c)
            let
                checkInSig party sig = checkSig party (Input c) sig
                -- Filter to only signed inputs that we don't already have
                newInputs = Map.filterWithKey checkInSig $ Map.difference summaryInputs myInputs
            forM_ (Map.toList newInputs) $ \(party, sig) -> receiveCSSMessage party (Input c) sig
            let isMissing (party, sig) = case Map.lookup party summaryInputs of
                    Nothing -> True
                    Just sig' -> sig /= sig' && not (checkInSig party sig')
            return $ mempty {
                    curAhead = not (null newInputs),
                    curBehind = any isMissing (Map.toList myInputs)
            }
        checkSeen myCheckSig = do
            let
                checkSeenParty (party, seens) = do
                    mySeens <- use (sawMessages . at party . nonEmpty)
                    let
                        mySeenNS = foldr unionNominationSet emptyNominationSet $ fst <$> mySeens
                        signedSeens = filter (\(ns, sig) -> myCheckSig party (Seen ns) sig) seens
                        theirSeenNS = foldr unionNominationSet emptyNominationSet $ fst <$> signedSeens
                        newSignedSeens = filter (\(ns, _) -> not (ns `subsumedBy` mySeenNS)) signedSeens
                    forM_ newSignedSeens $ \(ns, sig) -> receiveCSSMessage party (Seen ns) sig
                    tj <- use topJustified
                    bj <- use botJustified
                    return $ mempty {
                        curAhead = (tj && not (BitSet.null $ nomTop theirSeenNS `BitSet.difference` nomTop mySeenNS))
                                    || (bj && not (BitSet.null $ nomBot theirSeenNS `BitSet.difference` nomBot mySeenNS)),
                        curBehind = (tj && not (BitSet.null $ nomTop mySeenNS `BitSet.difference` nomTop theirSeenNS))
                                    || (bj && not (BitSet.null $ nomBot mySeenNS `BitSet.difference` nomBot theirSeenNS))
                    }
            mconcat <$> mapM checkSeenParty (Map.toList summarySeen)
        checkDR myCheckSig = do
            oldJustifiedDoneReporting <- use justifiedDoneReporting
            let filteredDR = Map.filterWithKey (\src (DoneReportingDetails ns sig) -> myCheckSig src (DoneReporting ns) sig) summaryDoneReporting
            forM_ (Map.toList filteredDR) $ \(src, DoneReportingDetails ns sig) -> receiveCSSMessage src (DoneReporting ns) sig
            newJustifiedDoneReporting <- use justifiedDoneReporting
            return $ mempty {
                curAhead = PM.weight newJustifiedDoneReporting > PM.weight oldJustifiedDoneReporting,
                curBehind = not $ null $ PM.partyMap oldJustifiedDoneReporting `Map.difference` filteredDR
            }

{-# INLINE cssCheckExistingSig #-}
cssCheckExistingSig :: (Eq sig) => CSSState sig -> Party -> CSSMessage -> sig -> Bool
cssCheckExistingSig st = chk
    where
        chk party (Input c) sig = (st ^? input c . ix party) == Just sig
        chk party (Seen ns) sig = (ns, sig) `elem` (st ^. sawMessages . at party . nonEmpty)
        chk party (DoneReporting ns) sig = (st ^? justifiedDoneReporting . ix party) == Just (DoneReportingDetails ns sig)


cssSummaryCheckComplete :: forall sig. (Eq sig) => CSSSummary sig -> CSSInstance -> (Party -> CSSMessage -> sig -> Bool) -> Bool
cssSummaryCheckComplete CSSSummary{..} CSSInstance{..} checkSig = doneRepWeight >= totalWeight - corruptWeight
    where
        doneRepWeight = checkDoneReps BitSet.empty BitSet.empty 0 (Map.toList summaryDoneReporting)
        -- For each done reporting:
        --   1. check the signature
        --   2. check that all reported nominations have justified inputs, with valid signatures
        --   3. check that all reported nominations have a corresponding seen message, with a valid signature
        -- If the total weight of done reporting messages that pass these tests is at least @totalWeight - corruptWeight@
        -- then the summary is complete.  (i.e. any honest party that receives this summary has sufficient input to
        -- complete the CSS round.)
        checkDoneReps :: BitSet.BitSet -> BitSet.BitSet -> VoterPower -> [(Party, DoneReportingDetails sig)] -> VoterPower
        checkDoneReps _ _ !drWeight [] = drWeight
        checkDoneReps preCheckTop preCheckBot !drWeight ((party, DoneReportingDetails ns sig) : rest) =
            if checkSig party (DoneReporting ns) sig then
                let
                    checkInps toCheck preCheck inps b = checkParties preCheck (BitSet.toList newCheck)
                        where
                            newCheck = toCheck `BitSet.difference` preCheck
                            checkParties newPreCheck [] = (newPreCheck, True)
                            checkParties newPreCheck (p : ps) = case Map.lookup p inps of
                                Nothing -> (newPreCheck, False)
                                Just isig -> if checkSig party (Input b) isig then
                                                checkParties (BitSet.insert party newPreCheck) ps
                                            else
                                                (newPreCheck, False)
                    (preCheckTop1, topChecked) = checkInps (nomTop ns) preCheckTop summaryInputsTop True
                    (preCheckBot1, botChecked) = checkInps (nomBot ns) preCheckBot summaryInputsBot False
                in if topChecked then
                    if botChecked then
                        let partySeen = foldr unionNominationSet emptyNominationSet $
                                fst <$> filter (\(sns, ssig) -> checkSig party (Seen sns) ssig) (summarySeen ^. at party . non [])
                        in if ns `subsumedBy` partySeen then
                            checkDoneReps preCheckTop1 preCheckBot1 (partyWeight party + drWeight) rest
                        else
                            checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                    else
                        checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                else
                    checkDoneReps preCheckTop1 preCheckBot drWeight rest
            else
                checkDoneReps preCheckTop preCheckBot drWeight rest

cssSummaryIsBehind :: CSSState sig -> CSSSummary sig -> Bool
cssSummaryIsBehind st CSSSummary{..} = inTopBehind || inBotBehind || seenBehind || drBehind
    where
        inTopBehind = any (`Map.notMember` summaryInputsTop) (Map.keys $ st ^. input True)
        inBotBehind = any (`Map.notMember` summaryInputsBot) (Map.keys $ st ^. input False)
        seenBehind = undefined
        drBehind = undefined