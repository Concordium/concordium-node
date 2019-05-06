{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- |Core Set Selection algorithm
module Concordium.Afgjort.CSS(
    Choice,
    CSSMessage(..),
    CoreSet(..),
    CSSState(..),
    initialCSSState,
    CSSMonad(..),
    CSSOutputEvent(..),
    CSS,
    runCSS,
    CSSInstance(CSSInstance), -- Don't export projections or constructor
    justifyChoice,
    receiveCSSMessage,
    Choices,
    addChoice,
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
    justifiedDoneReportingWeight
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.Afgjort.Types

type Choice = Bool

type Choices = Maybe Choice

addChoice :: Choice -> Maybe Choices -> Maybe Choices
addChoice c Nothing = Just (Just c)
addChoice _ (Just Nothing) = Just Nothing
addChoice c cs@(Just (Just c')) = if c == c' then cs else Just Nothing

data CSSMessage
    = Input Choice
    | Seen Party Choice
    | DoneReporting (Map Party Choice) -- Possbly use list instead
    deriving (Eq, Ord, Show)

data CSSInstance = CSSInstance {
    -- |The total weight of all parties
    totalWeight :: Int,
    -- |The (maximum) weight of all corrupt parties (should be less than @totalWeight/3@).
    corruptWeight :: Int,
    -- |The weight of each party
    partyWeight :: Party -> Int
}


data CoreSet = CoreSet {
    coreTop :: Maybe (Set Party),
    coreBot :: Maybe (Set Party)
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
    _report :: Bool,
    -- |Whether *bottom* is considered justified
    _botJustified :: Bool,
    -- |Whether *top* is considered justified
    _topJustified :: Bool,
    -- |The parties that have nominated *top*, with the signatures for their nominations
    _inputTop :: Set Party,
    -- |The parties that have nominated *bottom*, with the signatures for their nominations
    _inputBot :: Set Party,
    -- |For each party, the total weight and set of parties that report having seen a nomination of *top* by that party.
    _sawTop :: Map Party (Int, Set Party),
    -- |As above, for *bottom*
    _sawBot :: Map Party (Int, Set Party),
    -- |The set of nominations we saw.  That is, the first justified nomination we received from each party.
    _iSaw :: Map Party Choice,
    -- |The set of parties for which (n-t) parties have sent justified Seen messages.
    _manySaw :: Map Party Choices,
    -- |The total weight of parties in '_manySaw'.
    _manySawWeight :: Int,
    -- |For each pair @(seen,c)@ for which we have not received a justified input, this records
    -- parties that have sent DoneReporting messages that include this pair, where all previous
    -- pairs have been justified and all future pairs are held in the list.
    _unjustifiedDoneReporting :: Map (Party, Choice) (Map Party [(Party, Choice)]),
    -- |The set of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReporting :: Set Party,
    -- |The total weight of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReportingWeight :: Int,
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
    _iSaw = Map.empty,
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

saw :: Choice -> Lens' CSSState (Map Party (Int, Set Party))
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
            Just (_, m) -> seer `Set.member` m

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
receiveCSSMessage src (Seen sp c) = do
    CSSInstance{..} <- ask
    -- Update the set of parties that claim to have seen @sp@ make choice @c@
    let updateSaw Nothing = let w = partyWeight src in (w, Just (w, Set.singleton src))
        updateSaw o@(Just (oldWeight, oldMap))
            | src `Set.member` oldMap = (oldWeight, o)
            | otherwise = let w = oldWeight + partyWeight src in (w, Just (w, Set.insert src oldMap))
    weight <- state ((saw c . at sp) updateSaw)
    -- Just (weight, _) <- saw c . at sp <%= updateSaw
    -- Check if this seen message is justified
    whenM (use (justified c)) $ whenM (Set.member sp <$> use (input c)) $ do
        -- If the weight is high enough, record it in manySaw
        when (weight >= totalWeight - corruptWeight) $ addManySaw sp c
        -- If there is a DoneReporting message awaiting this becoming justified, handle it
        hdr <- unjustifiedDoneReporting . at (sp, c) . non Map.empty . at src <<.= Nothing
        forM_ hdr $ handleDoneReporting src
receiveCSSMessage src (DoneReporting sawSet) = handleDoneReporting src (Map.toList sawSet)


-- |Call when a nomination (@c@) by a party (@src@) becomes justified,
-- i.e. @input c@ contains @src@ and @justified c@ gives @True@.
{-# SPECIALIZE justifyNomination :: Party -> Choice -> CSS () #-}
justifyNomination :: (CSSMonad m) => Party -> Choice -> m ()
justifyNomination src c = do
    CSSInstance{..} <- ask
    -- In the report phase, add the nomination to @iSaw@ and send a seen message
    -- (unless we already did for this @src@).
    whenM (use report) $ do
        seen <- isJust <$> use (iSaw . at src)
        unless seen $ do
            iSaw . at src ?= c
            sendCSSMessage $ Seen src c
    -- Update manySaw if the now-justified choice has been Seen sufficiently
    use (saw c . at src) >>= mapM_ (\(w, _) ->
        when (w >= totalWeight - corruptWeight) $ addManySaw src c)
    -- Consider any DoneReporting messages waiting on justified @Seen src c@ messages
    use (unjustifiedDoneReporting . at (src, c)) >>= mapM_ (\m ->
        -- Consider the @Seen src c@ messages (which now become justified)
        use (saw c . at src) >>= mapM_ (\(_, jsaw) -> do
            -- Divide the DoneReporting messages on whether we have got the
            -- corresponding (now justified) @Seen@ message
            let (js, ujs) = Map.partitionWithKey (\k _ -> k `Set.member` jsaw) m
            -- Put those messages back where we don't
            unjustifiedDoneReporting . at (src, c) .= if Map.null ujs then Nothing else Just ujs
            -- And handle those where we do.
            forM_ (Map.toList js) $ uncurry handleDoneReporting))

{-# SPECIALIZE addManySaw :: Party -> Choice -> CSS () #-}
addManySaw :: (CSSMonad m) => Party -> Choice -> m ()
addManySaw party c = do
    CSSInstance{..} <- ask
    oldMS <- manySaw . at party <<%= addChoice c
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
                let theCore = CoreSet {
                    coreTop = if _topJustified then Just _inputTop else Nothing,
                    coreBot = if _botJustified then Just _inputBot else Nothing
                }
                put (css{_core = Just theCore})
                selectCoreSet theCore
handleDoneReporting party ((s, c) : remainder) = do
    scJust <- use (sawJustified party c s)
    if scJust then
        handleDoneReporting party remainder
    else
        unjustifiedDoneReporting . at (s, c) %= \case
            Nothing -> Just (Map.singleton party remainder)
            Just l -> Just (Map.insert party remainder l)

