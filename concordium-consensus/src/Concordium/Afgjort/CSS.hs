{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Core Set Selection algorithm
--
-- For more information, check the konsensus paper, section 5.6.4.
--
-- The core set selection algorithm outputs a set @CoreSet@ that is later used to consider an input finalized.
--
-- = Definitions
--
-- One of the inputs to CSS is a justification, to which we will refer as Jcssin.
--
-- * TPL: A tuple @(P, b)@ is Jtpl-justified for us if it is signed by @P@ and @b@ is Jcssin-justified for us.
-- * Seen: A seen message @(SEEN, Pk, (Pi, bi))@ is Jseen-justified for us if it is signed by Pk and @(Pi, bi)@ is
--  Jtpl-justified for us.
-- * Done: A done-reporting message @(DONEREPORTING, Pk, iSaw_k)@ is Jdone-justified for us if it is signed by Pk and
--  each tuple @(Pi, bi)@ in @iSaw_k@ is Jseen-justified with a message @(SEEN, Pk, (Pi, bi))@.
--
-- = Protocol
--
-- == Input
-- * @baid@: the identifier of the WMVBA instance.
-- * @Jcssin@: a justification.
-- * @delay@: delay for countering de-synchronization.
--
-- == Precondition
-- * @b@ (either @T@ or @Bottom@) is @Jcssin@-justified for us.
--
-- == Execution
-- * Start:
--
--    1. Set the flag @report@ to @T@. Initialize @iSaw@ and @manySaw@ to the empty set.
--    2. Send @b@ signed to all parties and move to the next phase.
--
-- * Reporting Phase:
--
--     - Once I receive @bj@ from @Pj@ where @(Pj, bj)@ is @Jtpl@ justified for me, add @(Pj, bj)@ to @iSaw@
--       and send signed @(SEEN, Pi, (Pj, bj))@ to all parties.
--     - Once I receive a @Jseen@-justified @(SEEN, Pk, (Pj, bj))@ from @n - t@ parties, add @(Pj, bj)@ to @manySaw@.
--     - Once @manySaw@ has tuples @(Pj, .)@ for @n - t@ parties, wait for @delay@ and set @report@ to @Bottom@ and move
--       to the next phase.
--
-- * Closing Down:
--
--     - Send signed @(DONEREPORTING, Pi, iSaw)@ to all parties.
--     - Once I receive @Jdone@-justified @(DONEREPORTING, Pj, iSaw_j)@ from @n - t@ parties, set @Core@ to all currently
--       @Jtpl@-justified @(Pj, bj)@. Wait @delay@ and output @Core@.
module Concordium.Afgjort.CSS (
    -- * Types
    CoreSet,
    DoneReportingDetails (..),

    -- * Messages
    CSSMessage (..),

    -- * Instance
    CSSInstance (CSSInstance), -- Don't export projections or constructor

    -- * State
    CSSState (..),
    initialCSSState,
    cssCheckExistingSig,

    -- * Monad definition
    CSSMonad (..),

    -- * Monad implementation
    CSSOutputEvent (..),
    CSS,
    runCSS,

    -- * Protocol
    justifyChoice,
    receiveCSSMessage,
    finishReporting,

    -- * Summary
    CSSSummary (..),
    cssSummary,
    cssSummaryCheckComplete,
    cssSummaryIsBehind,

    -- * For testing
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
    PartySet (..),
) where

import Control.Monad.RWS.Strict
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Lens.Micro.Platform

import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.CSS.NominationSet
import Concordium.Afgjort.PartyMap (PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM
import Concordium.Afgjort.PartySet (PartySet)
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.Types
import Concordium.Utils

--------------------------------------------------------------------------------
-- Types

type CoreSet = NominationSet

data DoneReportingDetails sig = DoneReportingDetails !NominationSet !sig
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Messages

data CSSMessage
    = Input !Choice
    | Seen !NominationSet
    | DoneReporting !NominationSet
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Instance

data CSSInstance = CSSInstance
    { -- |The total weight of all parties
      totalWeight :: !VoterPower,
      -- |The (maximum) weight of all corrupt parties (should be less than @totalWeight/3@).
      corruptWeight :: !VoterPower,
      -- |The weight of each party
      partyWeight :: Party -> VoterPower,
      -- |The maximal party
      maxParty :: !Party
    }

--------------------------------------------------------------------------------
-- State

-- | Invariant:
--
--  * If '_report' (we are in the reporting phase):
--
--      - if '_botJustified' then every party in '_inputBot' must have an entry in '_iSaw';
--      - if '_topJustified' then every party in '_inputTop' must have an entry in '_iSaw'.
--
--  * For every entry in the '_iSaw' nomination set:
--
--      - the nominated value is justified;
--      - the party nominated that value.
--
--  * '_iSaw' contains at most one nomination per party.
--  * @p@ is in '_manySaw' exactly when, for some @s@ with @PS.weight s >= totalWeight - corruptWeight@, either:
--
--      - '_topJustified' and @(p, s)@ is in '_sawTop'; or
--      - '_botJustified' and @(p, s)@ is in '_sawBot'.
--
--  * All keys of '_unjustifiedDoneReporting' should not have corresponding justified inputs.
data CSSState sig = CSSState
    { -- |Whether we are in the report stage (initially @True@)
      _report :: !Bool,
      -- |Whether __bottom__ is considered justified
      _botJustified :: !Bool,
      -- |Whether __top__ is considered justified
      _topJustified :: !Bool,
      -- |The parties that have nominated __top__
      _inputTop :: !(Map Party sig),
      -- |The parties that have nominated __bottom__
      _inputBot :: !(Map Party sig),
      -- |For each party, the total set of parties that report having seen a nomination of __top__ by that party.
      _sawTop :: !(Map Party PartySet),
      -- |As above, for __bottom__
      _sawBot :: !(Map Party PartySet),
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
    }
    deriving (Eq, Show)

makeLenses ''CSSState

initialCSSState :: CSSState sig
initialCSSState =
    CSSState
        { _report = True,
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

saw :: Choice -> Lens' (CSSState sig) (Map Party PartySet)
saw True = sawTop
saw False = sawBot

-- |Given a witnessing party (@seer@), a choice (@c@) and a nominating party (@seen@),
-- produces a 'SimpleGetter' for a 'CSSState' that returns @True@ exactly when we have
-- a justified message from @seer@ that @seen@ nominated @c@.
sawJustified ::
    -- | @seer@
    Party ->
    -- | @c@
    Choice ->
    -- | @seen@
    Party ->
    SimpleGetter (CSSState sig) Bool
sawJustified seer c seen = to $ \s ->
    (s ^. justified c)
        && (seen `Map.member` (s ^. input c))
        && case s ^. saw c . at seen of
            Nothing -> False
            Just m -> seer `PS.member` m

{-# INLINE cssCheckExistingSig #-}
cssCheckExistingSig :: (Eq sig) => CSSState sig -> Party -> CSSMessage -> sig -> Bool
cssCheckExistingSig st = chk
  where
    chk party (Input c) sig = (st ^? input c . ix party) == Just sig
    chk party (Seen ns) sig = (ns, sig) `elem` (st ^. sawMessages . at party . nonEmpty)
    chk party (DoneReporting ns) sig = (st ^? justifiedDoneReporting . ix party) == Just (DoneReportingDetails ns sig)

--------------------------------------------------------------------------------
-- Monad definition

class (MonadState (CSSState sig) m, MonadReader CSSInstance m) => CSSMonad sig m where
    -- |Sign and broadcast a CSS message to all parties, __including__ our own 'CSSInstance'.
    sendCSSMessage :: CSSMessage -> m ()

    -- |Determine the core set.  This output should not be used until after waiting for deltaCSS.
    selectCoreSet :: CoreSet -> m ()

    -- |Wait for deltaCSS and then call back 'finishReporting'
    waitThenFinishReporting :: m ()

unlessComplete :: (CSSMonad sig m) => m () -> m ()
unlessComplete a = do
    complete <- isJust <$> use core
    unless complete a

{-# SPECIALIZE addManySaw :: Party -> CSS sig () #-}
addManySaw :: (CSSMonad sig m) => Party -> m ()
addManySaw party = do
    CSSInstance{..} <- ask
    oldMSW <- PS.weight <$> use manySaw
    msw <- PS.weight <$> (manySaw <%= PS.insert party (partyWeight party))
    when
        (msw >= totalWeight - corruptWeight && oldMSW < totalWeight - corruptWeight)
        waitThenFinishReporting

-- |Call after waiting deltaCSS since 'waitThenFinishReporting' was called.
{-# SPECIALIZE finishReporting :: CSS sig () #-}
finishReporting :: (CSSMonad sig m) => m ()
finishReporting = do
    oldRep <- report <<.= False
    when oldRep $
        sendCSSMessage . DoneReporting =<< use iSaw

--------------------------------------------------------------------------------
-- Monad implementation

data CSSOutputEvent
    = SendCSSMessage !CSSMessage
    | SelectCoreSet !CoreSet
    | WaitThenFinishReporting

newtype CSS sig a = CSS
    { runCSS' :: RWS CSSInstance (Endo [CSSOutputEvent]) (CSSState sig) a
    }
    deriving (Functor, Applicative, Monad, MonadReader CSSInstance, MonadState (CSSState sig))

{-# INLINE runCSS #-}
runCSS :: CSS sig a -> CSSInstance -> CSSState sig -> (a, CSSState sig, [CSSOutputEvent])
runCSS z i s = runRWS (runCSS' z) i s & _3 %~ (\(Endo f) -> f [])

instance CSSMonad sig (CSS sig) where
    sendCSSMessage !msg = CSS $ tell $ Endo (SendCSSMessage msg :)
    selectCoreSet !cs = CSS $ tell $ Endo (SelectCoreSet cs :)
    waitThenFinishReporting = CSS $ tell $ Endo (WaitThenFinishReporting :)

--------------------------------------------------------------------------------
-- Protocol

-- |Called to notify when a choice becomes justified.
--
-- This will trigger further `SEEN` messages in response to this justification.
-- We already stored the choices for other parties and now we can justify
-- the nominations as now the value is justified for us (__step 1 of Reporting Phase__).
{-# SPECIALIZE justifyChoice :: Choice -> CSS sig () #-}
justifyChoice :: (CSSMonad sig m) => Choice -> m ()
justifyChoice c = do
    -- Record the choice as justified
    alreadyJustified <- justified c <<.= True
    -- If it wasn't already justified...
    unless alreadyJustified $ do
        inputs <- use (input c)
        forM_ (Map.keys inputs) $ \p -> justifyNomination p c

-- |Handle an incoming CSSMessage from a party.
--
-- If the core is complete, this will be a no-op as CSS has ended.
--
-- Depending on the Message received, we will perform one of the following steps of the protocol
--
-- 1. If we receive an `Input` @c@ from party @src@ with signature @sig@, we record the input
--    in the appropiate set and if the input is justified, we justify the nomination. (__beginning of step 1 of Reporting Phase__)
-- 2. If we receive a `Seen` @ns@ message from party @src@ with signature @sig@, for each tuple received, we
--    add the tuple to the proper set and if it is a new entry and we can justify this choice and the sender
--    actually chose this option by a previously received `Input` @c@ message, we add it to @manySaw@ if the weight
--    is high enough (__step 2 of Reporting Phase__) and dispatch pending DoneReporting messages that were pending (__step
--    2 of Closing Down__).
-- 3. If we receive a `DoneReporting` @sawSet@ message, handle it.
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
        (weight, alreadyPresent) <- state ((saw c . at' sp) updateSaw)
        unless alreadyPresent $
            -- Check if this seen message is justified
            whenM (use (justified c)) $
                whenM (Map.member sp <$> use (input c)) $ do
                    -- If the weight is high enough, record it in manySaw
                    when (weight >= totalWeight - corruptWeight) $ addManySaw sp
                    -- If there is a DoneReporting message awaiting this becoming justified, handle it
                    hdr <- unjustifiedDoneReporting . at' (sp, c) . nonEmpty . at' src <<.= Nothing
                    forM_ hdr $ handleDoneReporting src
        return alreadyPresent
    unless (and present) $
        -- This message includes some new observations, so add it to sawMessages.
        -- We filter out any Seen messages that are subsumed by the new message.
        sawMessages . at src . nonEmpty %= \l -> (ns, sig) : filter (\(ns', _) -> not $ ns' `subsumedBy` ns) l
receiveCSSMessage src (DoneReporting sawSet) sig = unlessComplete $ handleDoneReporting src (nominationSetToList sawSet, DoneReportingDetails sawSet sig)

-- |When receiving a `DoneReporting`, we process the information we have.
--
-- For each tuple in the `DoneReporting` message, we check that we actually saw that input or else queue
-- the message until further justification. If we saw all the tuples, we can consider this message @Jdone@-justified.
-- If enough @Jdone@-justified messages have been received, we compute the core and finish
-- the execution (__the last part of Closing Down__). Note that the delay before outputting the core set
-- is not implemented here; the caller (namely ABBA) is responsible for observing the delay.
{-# SPECIALIZE handleDoneReporting :: Party -> ([(Party, Choice)], DoneReportingDetails sig) -> CSS sig () #-}
handleDoneReporting :: (CSSMonad sig m) => Party -> ([(Party, Choice)], DoneReportingDetails sig) -> m ()
handleDoneReporting party ([], drd) = do
    CSSInstance{..} <- ask
    alreadyDone <- PM.member party <$> use justifiedDoneReporting
    unless alreadyDone $ do
        newJDR <- justifiedDoneReporting <%= PM.insert party (partyWeight party) drd
        -- When we hit the mark, we can generate the core set.
        when (PM.weight newJDR >= totalWeight - corruptWeight) $ do
            css@CSSState{..} <- get
            when (isNothing _core) $ do
                let theCore =
                        NominationSet
                            { nomMax = maxParty,
                              nomTop = if _topJustified then BitSet.fromList (Map.keys _inputTop) else BitSet.empty,
                              nomBot = if _botJustified then BitSet.fromList (Map.keys _inputBot) else BitSet.empty
                            }
                put (css{_core = Just theCore})
                selectCoreSet theCore
handleDoneReporting party ((s, c) : remainder, drd) = do
    scJust <- use (sawJustified party c s)
    if scJust
        then handleDoneReporting party (remainder, drd)
        else
            unjustifiedDoneReporting . at' (s, c) %= \case
                Nothing -> Just (Map.singleton party (remainder, drd))
                Just l -> Just (Map.insert party (remainder, drd) l)

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
                sendCSSMessage $
                    Seen newiSaw
    -- Update manySaw if the now-justified choice has been Seen sufficiently
    use (saw c . at src)
        >>= mapM_
            ( \parties ->
                when (PS.weight parties >= totalWeight - corruptWeight) $ addManySaw src
            )
    -- Consider any DoneReporting messages waiting on justified @Seen src c@ messages
    use (unjustifiedDoneReporting . at (src, c))
        >>= mapM_
            ( \m ->
                -- Consider the @Seen@ messages (which now become justified)
                use (saw c . at src)
                    >>= mapM_
                        ( \jsaw -> do
                            -- Divide the DoneReporting messages on whether we have got the
                            -- corresponding (now justified) @Seen@ message
                            let (js, ujs) = Map.partitionWithKey (\k _ -> k `PS.member` jsaw) m
                            -- Put those messages back where we don't
                            unjustifiedDoneReporting . at' (src, c) .= if Map.null ujs then Nothing else Just ujs
                            -- And handle those where we do.
                            forM_ (Map.toList js) $ uncurry handleDoneReporting
                        )
            )

--------------------------------------------------------------------------------
-- Summary

data CSSSummary sig = CSSSummary
    { summaryInputsTop :: !(Map Party sig),
      summaryInputsBot :: !(Map Party sig),
      summarySeen :: !(Map Party [(NominationSet, sig)]),
      summaryDoneReporting :: !(Map Party (DoneReportingDetails sig))
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
        if checkSig party (DoneReporting ns) sig
            then
                let
                    checkInps toCheck preCheck inps b = checkParties preCheck (BitSet.toList newCheck)
                      where
                        newCheck = toCheck `BitSet.difference` preCheck
                        checkParties newPreCheck [] = (newPreCheck, True)
                        checkParties newPreCheck (p : ps) = case Map.lookup p inps of
                            Nothing -> (newPreCheck, False)
                            Just isig ->
                                if checkSig party (Input b) isig
                                    then checkParties (BitSet.insert party newPreCheck) ps
                                    else (newPreCheck, False)
                    (preCheckTop1, topChecked) = checkInps (nomTop ns) preCheckTop summaryInputsTop True
                    (preCheckBot1, botChecked) = checkInps (nomBot ns) preCheckBot summaryInputsBot False
                in
                    if topChecked
                        then
                            if botChecked
                                then
                                    let partySeen =
                                            foldl' unionNominationSet emptyNominationSet $
                                                fst <$> filter (\(sns, ssig) -> checkSig party (Seen sns) ssig) (summarySeen ^. at party . non [])
                                    in  if ns `subsumedBy` partySeen
                                            then checkDoneReps preCheckTop1 preCheckBot1 (partyWeight party + drWeight) rest
                                            else checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                                else checkDoneReps preCheckTop1 preCheckBot1 drWeight rest
                        else checkDoneReps preCheckTop1 preCheckBot drWeight rest
            else checkDoneReps preCheckTop preCheckBot drWeight rest

-- |Returns 'True' exactly when we can provide (potentially) useful information
-- for other parties to progress with CSS beyond what is provided by the given
-- 'CSSSummary'.  This is the case:
--
-- * if we have any inputs that are not included in the summary, or
-- * if we have any seen messages that are not subsumed by the summary, or
-- * if we have any justified done reporting messages that are not in the summary.
--
-- Note: we do not consider whether the input or seen messages are justified.  This is
-- since we do not consider that when we produce a 'CSSSummary' either.
cssSummaryIsBehind :: (Eq sig) => CSSState sig -> CSSSummary sig -> Bool
cssSummaryIsBehind st CSSSummary{..} = inTopBehind || inBotBehind || seenBehind || drBehind
  where
    inTopBehind = any (`Map.notMember` summaryInputsTop) (Map.keys $ st ^. input True)
    inBotBehind = any (`Map.notMember` summaryInputsBot) (Map.keys $ st ^. input False)
    totalSaw l = foldl' unionNominationSet emptyNominationSet (fst <$> l)
    notCovered (party, l) = not (totalSaw l `subsumedBy` totalSaw (Map.findWithDefault [] party summarySeen))
    seenBehind = any notCovered (Map.toList $ st ^. sawMessages)
    drMissing (party, drd) = Map.lookup party summaryDoneReporting /= Just drd
    drBehind = any drMissing (PM.toList $ st ^. justifiedDoneReporting)
