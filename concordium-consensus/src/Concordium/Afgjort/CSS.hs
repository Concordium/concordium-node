{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- |Core Set Selection algorithm
module Concordium.Afgjort.CSS where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS
import Lens.Micro.Platform

type Choice = Bool

type Choices = Maybe Choice

addChoice :: Choice -> Maybe Choices -> Maybe Choices
addChoice c Nothing = Just (Just c)
addChoice _ (Just Nothing) = Just Nothing
addChoice c cs@(Just (Just c')) = if c == c' then cs else Just Nothing

data CSSMessage party
    = Input Choice
    | Seen party Choice
    | DoneReporting (Map party Choice) -- Possbly use list instead
    deriving (Eq, Ord, Show)


data CoreSet party sig = CoreSet {
    coreTop :: Maybe (Map party sig),
    coreBot :: Maybe (Map party sig)
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

data CSSState party sig = CSSState {
    -- |Whether we are in the report stage (initially @True@)
    _report :: Bool,
    -- |Whether *bottom* is considered justified
    _botJustified :: Bool,
    -- |Whether *top* is considered justified
    _topJustified :: Bool,
    -- |The parties that have nominated *top*, with the signatures for their nominations
    _inputTop :: Map party sig,
    -- |The parties that have nominated *bottom*, with the signatures for their nominations
    _inputBot :: Map party sig,
    -- |For each party, the total weight and set of parties that report having seen a nomination of *top* by that party,
    -- together with the signatures for the seen messages
    _sawTop :: Map party (Int, Map party sig),
    -- |As above, for *bottom*
    _sawBot :: Map party (Int, Map party sig),
    -- |The set of nominations we saw.  That is, the first justified nomination we received from each party.
    _iSaw :: Map party Choice,
    -- |The set of parties for which (n-t) parties have sent justified Seen messages.
    _manySaw :: Map party Choices,
    -- |The total weight of parties in '_manySaw'.
    _manySawWeight :: Int,
    -- |For each pair @(seen,c)@ for which we have not received a justified input, this records
    -- parties that have sent DoneReporting messages that include this pair, where all previous
    -- pairs have been justified and all future pairs are held in the list.
    _unjustifiedDoneReporting :: Map (party, Choice) (Map party [(party, Choice)]),
    -- |The set of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReporting :: Set party,
    -- |The total weight of parties for which we have received fully justified DoneReporting messages.
    _justifiedDoneReportingWeight :: Int,
    -- |If '_justifiedDoneReportingWeight' is at least (n-t), then the core set determined at that time.  Otherwise @Nothing@.
    _core :: Maybe (CoreSet party sig)
} deriving (Show)
makeLenses ''CSSState

initialCSSState :: CSSState party sig
initialCSSState = CSSState {
    _report = True,
    _botJustified = False,
    _topJustified = False,
    _inputTop = Map.empty,
    _inputBot = Map.empty,
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

justified :: Choice -> Lens' (CSSState party sig) Bool
justified True = topJustified
justified False = botJustified

input :: Choice -> Lens' (CSSState party sig) (Map party sig)
input True = inputTop
input False = inputBot

saw :: Choice -> Lens' (CSSState party sig) (Map party (Int, Map party sig))
saw True = sawTop
saw False = sawBot

-- |Given a witnessing party (@seer@), a choice (@c@) and a nominating party (@seen@),
-- produces a 'SimpleGetter' for a 'CSSState' that returns @True@ exactly when we have
-- a justified message from @seer@ that @seen@ nominated @c@.
sawJustified :: (Ord party) =>
    party       -- ^ @seer@
    -> Choice   -- ^ @c@
    -> party    -- ^ @seen@
    -> SimpleGetter (CSSState party sig) Bool
sawJustified seer c seen = to $ \s ->
    (s ^. justified c) && (isJust $ s ^. input c . at seen) &&
        case s ^. saw c . at seen of
            Nothing -> False
            Just (_, m) -> isJust $ m ^. at seer

class (MonadState (CSSState party sig) m) => CSSMonad party sig m where
    -- |Sign and broadcast a CSS message to all parties, _including_ our own 'CSSInstance'.
    sendCSSMessage :: CSSMessage party -> m ()
    -- |Determine the core set.
    selectCoreSet :: CoreSet party sig -> m ()

data CSSOutputEvent party sig
    = SendCSSMessage (CSSMessage party)
    | SelectCoreSet (CoreSet party sig)

newtype CSS party sig a = CSS {
    runCSS' :: RWS () (Endo [CSSOutputEvent party sig]) (CSSState party sig) a
} deriving (Functor, Applicative, Monad)

runCSS :: CSS party sig a -> CSSState party sig -> (a, CSSState party sig, [CSSOutputEvent party sig])
runCSS z s = runRWS (runCSS' z) () s & _3 %~ (\(Endo f) -> f [])

instance MonadState (CSSState party sig) (CSS party sig) where
    get = CSS get
    put = CSS . put
    state = CSS . state

instance CSSMonad party sig (CSS party sig) where
    sendCSSMessage msg = CSS $ tell $ Endo (SendCSSMessage msg :)
    selectCoreSet cs = CSS $ tell $ Endo (SelectCoreSet cs :)

data CSSInstance party sig m = CSSInstance {
    -- |Called to notify when a choice becomes justified.
    justifyChoice :: Choice -> m (),
    -- |Handle an incoming CSSMessage from a party.  The second argument is the signature,
    -- which must be valid for the party and message.
    receiveCSSMessage :: party -> sig -> CSSMessage party -> m ()
}

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM t a = t >>= \r -> when r a

{-# SPECIALIZE newCSSInstance :: forall party sig. Ord party => Int -> Int -> (party -> Int) -> CSSInstance party sig (CSS party sig) #-}
newCSSInstance :: forall party sig m. (CSSMonad party sig m, Ord party) => Int -> Int -> (party -> Int) -> CSSInstance party sig m
newCSSInstance totalWeight corruptWeight partyWeight = CSSInstance {..}
    where
        justifyChoice :: Choice -> m ()
        justifyChoice c = do
            -- Record the choice as justified
            alreadyJustified <- justified c <<.= True
            -- If it wasn't already justified...
            unless alreadyJustified $ do
                inputs <- use (input c)
                forM_ (Map.toList inputs) $ \(p, _) -> justifyNomination p c
        -- Call when a nomination (@c@) by a party (@src@) becomes justified,
        -- i.e. @input c . at src@ gives @Just sig@ and @justified c@ gives @True@.
        justifyNomination :: party -> Choice -> m ()
        justifyNomination src c = do
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
                use (saw c . at src) >>= mapM_ (\(_, jsMap) -> do
                    -- Divide the DoneReporting messages on whether we have got the
                    -- corresponding (now justified) @Seen@ message
                    let (js, ujs) = Map.partitionWithKey (\k _ -> isJust (jsMap ^. at k)) m
                    -- Put those messages back where we don't
                    unjustifiedDoneReporting . at (src, c) .= if Map.null ujs then Nothing else Just ujs
                    -- And handle those where we do.
                    forM_ (Map.toList js) $ uncurry handleDoneReporting))
        receiveCSSMessage :: party -> sig -> CSSMessage party -> m ()
        receiveCSSMessage src sig (Input c) = do
            -- Record that we've seen this nomination
            input c . at src ?= sig
            whenM (use (justified c)) $ justifyNomination src c
        receiveCSSMessage src sig (Seen sp c) = do
            -- Update the set of parties that claim to have seen @sp@ make choice @c@
            let updateSaw Nothing = Just (partyWeight src, Map.singleton src sig)
                updateSaw o@(Just (oldWeight, oldMap))
                    | isNothing (Map.lookup src oldMap) = Just (oldWeight + partyWeight src, Map.insert src sig oldMap)
                    | otherwise = o
            Just (weight, _) <- saw c . at sp <%= updateSaw
            -- Check if this seen message is justified
            whenM (use (justified c)) $ whenM (isJust <$> use (input c . at sp)) $ do
                -- If the weight is high enough, record it in manySaw
                when (weight >= totalWeight - corruptWeight) $ addManySaw sp c
                -- If there is a DoneReporting message awaiting this becoming justified, handle it
                hdr <- unjustifiedDoneReporting . at (sp, c) . non Map.empty . at src <<.= Nothing
                forM_ hdr $ handleDoneReporting src
        receiveCSSMessage src sig (DoneReporting sawSet) = handleDoneReporting src (Map.toList sawSet)
        addManySaw :: party -> Choice -> m ()
        addManySaw party c = do
            oldMS <- manySaw . at party <<%= addChoice c
            msw <- if isNothing oldMS then manySawWeight <%= (+ partyWeight party) else use manySawWeight
            when (msw >= totalWeight - corruptWeight) $ do
                oldRep <- report <<.= False
                when oldRep $
                    sendCSSMessage . DoneReporting =<< use iSaw
        handleDoneReporting :: party -> [(party, Choice)] -> m ()
        handleDoneReporting party [] = do
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

