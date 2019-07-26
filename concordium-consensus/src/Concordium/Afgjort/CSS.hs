{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, FlexibleInstances #-}
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
    -- * For internal use
    manySawWeight,
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
    saw,
    core,
    justifiedDoneReporting,
    justifiedDoneReportingWeight,
    PartySet(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.Afgjort.Types
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet


atStrict :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
atStrict k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (Map.delete k m)) mv
        Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE atStrict #-}

data CSSMessage
    = Input !Choice
    | Seen !NominationSet
    | DoneReporting !NominationSet
    deriving (Eq, Ord, Show)

data CSSInstance = CSSInstance {
    -- |The total weight of all parties
    totalWeight :: !Int,
    -- |The (maximum) weight of all corrupt parties (should be less than @totalWeight/3@).
    corruptWeight :: !Int,
    -- |The weight of each party
    partyWeight :: Party -> Int,
    -- |The maximal party
    maxParty :: !Party
}

type CoreSet = NominationSet

data PartySet = PartySet {
    setWeight :: !Int,
    parties :: !BitSet.BitSet
} deriving (Show)

-- | Invariant:
--
--   * '_manySawWeight' should be the sum of the party weights that have entries in '_manySaw'
--
--   * '_inputTop' and '_inputBot' should have disjoint domains if both '_botJustified' and '_topJustified' hold
--
--   * if '_inputTop' at @p@ holds value @s@, then @s@ must be a valid signature by @p@ of the message @Input True@
--
--   * if '_inputBot' at @p@ holds value @s@, then @s@ must be a valid signature by @p@ of the message @Input False@

data CSSState = CSSState {
    -- |Whether we are in the report stage (initially @True@)
    _report :: !Bool,
    -- |Whether *bottom* is considered justified
    _botJustified :: !Bool,
    -- |Whether *top* is considered justified
    _topJustified :: !Bool,
    -- |The parties that have nominated *top*, with the signatures for their nominations
    _inputTop :: !(Set Party),
    -- |The parties that have nominated *bottom*, with the signatures for their nominations
    _inputBot :: !(Set Party),
    -- |For each party, the total weight and set of parties that report having seen a nomination of *top* by that party.
    _sawTop :: !(Map Party PartySet),
    -- |As above, for *bottom*
    _sawBot :: !(Map Party PartySet),
    -- |The set of nominations we saw.  That is, the first justified nomination we received from each party.
    _iSaw :: !NominationSet,
    -- |The total weight of parties in '_iSaw'.
    _iSawWeight :: !Int,
    -- |The set of parties for which (n-t) parties have sent justified Seen messages.
    _manySaw :: !(Map Party Choices),
    -- |The total weight of parties in '_manySaw'.
    _manySawWeight :: !Int,
    -- |For each pair @(seen,c)@ for which we have not received a justified input, this records
    -- parties that have sent DoneReporting messages that include this pair, where all previous
    -- pairs have been justified and all future pairs are held in the list.
    _unjustifiedDoneReporting :: !(Map (Party, Choice) (Map Party [(Party, Choice)])),
    -- |The set of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReporting :: !(Set Party),
    -- |The total weight of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReportingWeight :: !Int,
    -- |If '_justifiedDoneReportingWeight' is at least (n-t), then the core set determined at that time.  Otherwise @Nothing@.
    _core :: Maybe CoreSet
} deriving (Show)
makeLenses ''CSSState

initialCSSState :: CSSState
initialCSSState = CSSState {
    _report = True,
    _botJustified = False,
    _topJustified = False,
    _inputTop = Set.empty,
    _inputBot = Set.empty,
    _sawTop = Map.empty,
    _sawBot = Map.empty,
    _iSaw = emptyNominationSet,
    _iSawWeight = 0,
    _manySaw = Map.empty,
    _manySawWeight = 0,
    _unjustifiedDoneReporting = Map.empty,
    _justifiedDoneReporting = Set.empty,
    _justifiedDoneReportingWeight = 0,
    _core = Nothing
}

justified :: Choice -> Lens' CSSState Bool
justified True = topJustified
justified False = botJustified

input :: Choice -> Lens' CSSState (Set Party)
input True = inputTop
input False = inputBot

saw :: Choice -> Lens' CSSState (Map Party PartySet)
saw True = sawTop
saw False = sawBot

-- |Given a witnessing party (@seer@), a choice (@c@) and a nominating party (@seen@),
-- produces a 'SimpleGetter' for a 'CSSState' that returns @True@ exactly when we have
-- a justified message from @seer@ that @seen@ nominated @c@.
sawJustified ::
    Party       -- ^ @seer@
    -> Choice   -- ^ @c@
    -> Party    -- ^ @seen@
    -> SimpleGetter CSSState Bool
sawJustified seer c seen = to $ \s ->
    (s ^. justified c) && (seen `Set.member` (s ^. input c)) &&
        case s ^. saw c . at seen of
            Nothing -> False
            Just (PartySet _ m) -> seer `BitSet.member` m

class (MonadState CSSState m, MonadReader CSSInstance m) => CSSMonad m where
    -- |Sign and broadcast a CSS message to all parties, __including__ our own 'CSSInstance'.
    sendCSSMessage :: CSSMessage -> m ()
    -- |Determine the core set.
    selectCoreSet :: CoreSet -> m ()

data CSSOutputEvent
    = SendCSSMessage CSSMessage
    | SelectCoreSet CoreSet

newtype CSS a = CSS {
    runCSS' :: RWS CSSInstance (Endo [CSSOutputEvent]) CSSState a
} deriving (Functor, Applicative, Monad)

{-# INLINE runCSS #-}
runCSS :: CSS a -> CSSInstance -> CSSState -> (a, CSSState, [CSSOutputEvent])
runCSS z i s = runRWS (runCSS' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadState CSSState CSS where
    get = CSS get
    put = CSS . put
    state = CSS . state

instance MonadReader CSSInstance CSS where
    ask = CSS ask
    reader = CSS . reader
    local f = CSS . local f . runCSS'

instance CSSMonad CSS where
    sendCSSMessage msg = CSS $ tell $ Endo (SendCSSMessage msg :)
    selectCoreSet cs = CSS $ tell $ Endo (SelectCoreSet cs :)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM t a = t >>= \r -> when r a

-- |Called to notify when a choice becomes justified.
{-# SPECIALIZE justifyChoice :: Choice -> CSS () #-}
justifyChoice :: (CSSMonad m) => Choice -> m ()
justifyChoice c = do
    -- Record the choice as justified
    alreadyJustified <- justified c <<.= True
    -- If it wasn't already justified...
    unless alreadyJustified $ do
        inputs <- use (input c)
        forM_ (Set.toList inputs) $ \p -> justifyNomination p c

-- |Handle an incoming CSSMessage from a party. 
{-# SPECIALIZE receiveCSSMessage :: Party -> CSSMessage -> CSS () #-}
receiveCSSMessage :: (CSSMonad m) => Party -> CSSMessage -> m ()
receiveCSSMessage src (Input c) = do
    -- Record that we've seen this nomination
    input c %= Set.insert src
    whenM (use (justified c)) $ justifyNomination src c
receiveCSSMessage src (Seen ns) = do
    CSSInstance{..} <- ask
    forM_ (nominationSetToList ns) $ \(sp, c) -> do
        -- Update the set of parties that claim to have seen @sp@ make choice @c@
        let updateSaw Nothing = let w = partyWeight src in (w, Just (PartySet w (BitSet.singleton src)))
            updateSaw o@(Just (PartySet oldWeight oldMap))
                | src `BitSet.member` oldMap = (oldWeight, o)
                | otherwise = let w = oldWeight + partyWeight src in (w, Just (PartySet w $ BitSet.insert src oldMap))
        weight <- state ((saw c . atStrict sp) updateSaw)
        -- Just (weight, _) <- saw c . at sp <%= updateSaw
        -- Check if this seen message is justified
        whenM (use (justified c)) $ whenM (Set.member sp <$> use (input c)) $ do
            -- If the weight is high enough, record it in manySaw
            when (weight >= totalWeight - corruptWeight) $ addManySaw sp c
            -- If there is a DoneReporting message awaiting this becoming justified, handle it
            hdr <- unjustifiedDoneReporting . atStrict (sp, c) . non Map.empty . atStrict src <<.= Nothing
            forM_ hdr $ handleDoneReporting src
receiveCSSMessage src (DoneReporting sawSet) = handleDoneReporting src (nominationSetToList sawSet)


-- |Call when a nomination (@c@) by a party (@src@) becomes justified,
-- i.e. @input c@ contains @src@ and @justified c@ gives @True@.
{-# SPECIALIZE justifyNomination :: Party -> Choice -> CSS () #-}
justifyNomination :: (CSSMonad m) => Party -> Choice -> m ()
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
    use (saw c . at src) >>= mapM_ (\(PartySet w _) ->
        when (w >= totalWeight - corruptWeight) $ addManySaw src c)
    -- Consider any DoneReporting messages waiting on justified @Seen src c@ messages
    use (unjustifiedDoneReporting . at (src, c)) >>= mapM_ (\m ->
        -- Consider the @Seen@ messages (which now become justified)
        use (saw c . at src) >>= mapM_ (\(PartySet _ jsaw) -> do
            -- Divide the DoneReporting messages on whether we have got the
            -- corresponding (now justified) @Seen@ message
            let (js, ujs) = Map.partitionWithKey (\k _ -> k `BitSet.member` jsaw) m
            -- Put those messages back where we don't
            unjustifiedDoneReporting . atStrict (src, c) .= if Map.null ujs then Nothing else Just ujs
            -- And handle those where we do.
            forM_ (Map.toList js) $ uncurry handleDoneReporting))

{-# SPECIALIZE addManySaw :: Party -> Choice -> CSS () #-}
addManySaw :: (CSSMonad m) => Party -> Choice -> m ()
addManySaw party c = do
    CSSInstance{..} <- ask
    oldMS <- manySaw . atStrict party <<%= addChoice c
    msw <- if isNothing oldMS then manySawWeight <%= (+ partyWeight party) else use manySawWeight
    when (msw >= totalWeight - corruptWeight) $ do
        oldRep <- report <<.= False
        when oldRep $
            sendCSSMessage . DoneReporting =<< use iSaw
{-# SPECIALIZE handleDoneReporting :: Party -> [(Party, Choice)] -> CSS () #-}
handleDoneReporting :: (CSSMonad m) => Party -> [(Party, Choice)] -> m ()
handleDoneReporting party [] = do
    CSSInstance{..} <- ask
    alreadyDone <- Set.member party <$> use justifiedDoneReporting
    unless alreadyDone $ do
        justifiedDoneReporting %= Set.insert party
        newJDRW <- justifiedDoneReportingWeight <%= (+ partyWeight party)
        -- When we hit the mark, we can generate the core set.
        when (newJDRW >= totalWeight - corruptWeight) $ do
            css@CSSState{..} <- get
            when (isNothing _core) $ do
                let theCore = NominationSet {
                    nomMax = maxParty,
                    nomTop = if _topJustified then _inputTop else Set.empty,
                    nomBot = if _botJustified then _inputBot else Set.empty
                }
                put (css{_core = Just theCore})
                selectCoreSet theCore
handleDoneReporting party ((s, c) : remainder) = do
    scJust <- use (sawJustified party c s)
    if scJust then
        handleDoneReporting party remainder
    else
        unjustifiedDoneReporting . atStrict (s, c) %= \case
            Nothing -> Just (Map.singleton party remainder)
            Just l -> Just (Map.insert party remainder l)

