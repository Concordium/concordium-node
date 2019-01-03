{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
-- |Core Set Selection algorithm
module Concordium.Afgjort.CSS where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad
import Lens.Micro.Platform

type Choice = Bool

type Choices = Maybe Choice

addChoice :: Choice -> Maybe Choices -> Maybe Choices
addChoice c Nothing = Just (Just c)
addChoice _ (Just Nothing) = Just Nothing
addChoice c cs@(Just (Just c')) = if c == c' then cs else Just Nothing

data CSSMessage party sig
    = Input Choice
    | Seen party Choice
    | DoneReporting (Map party Choice) -- Possbly use list instead

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

    _manySaw :: Map party Choices,
    -- |The total weight of parties in '_manySaw'.
    _manySawWeight :: Int,
    _unjustifiedDoneReporting :: Map (party, Choice) [(party, [(party, Choice)])],
    _justifiedDoneReporting :: Set party,
    _justifiedDoneReportingWeight :: Int
}
makeLenses ''CSSState

justified :: Choice -> Lens' (CSSState party sig) Bool
justified True = topJustified
justified False = botJustified

input :: Choice -> Lens' (CSSState party sig) (Map party sig)
input True = inputTop
input False = inputBot

saw :: Choice -> Lens' (CSSState party sig) (Map party (Int, Map party sig))
saw True = sawTop
saw False = sawBot

sawJustified :: (Ord party) => party -> Choice -> party -> SimpleGetter (CSSState party sig) Bool
sawJustified seer c seen = to $ \s ->
    (s ^. justified c) && (isJust $ s ^. input c . at seen) &&
        case s ^. saw c . at seen of
            Nothing -> False
            Just (_, m) -> isJust $ m ^. at seer

{-
iSaw :: (Ord party) => SimpleGetter (CSSState party sig) (Map party (Choice, sig))
iSaw = to g
    where
        g s = case (_botJustified s, _topJustified s) of
            (False, False) -> Map.empty
            (True, False) -> (False, ) <$> _inputBot s
            (False, True) -> (True, ) <$> _inputTop s
            (True, True) -> ((False, ) <$> _inputBot s) `Map.union` ((True, ) <$> _inputTop s)
-}
class (MonadState (CSSState party sig) m) => CSSMonad party sig m where
    -- |Sign and broadcast a CSS message to all parties, _including_ our own 'CSSInstance'.
    sendCSSMessage :: CSSMessage party sig -> m ()

data CSSInstance party sig m = CSSInstance {
    -- |Called to notify when a choice becomes justified.
    justifyChoice :: Choice -> m (),
    -- |Handle an incoming CSSMessage from a party.  The second argument is the signature,
    -- which must be valid for the party and message.
    receiveCSSMessage :: party -> sig -> CSSMessage party sig -> m ()
}

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM t a = t >>= \r -> when r a

newCSSInstance :: forall party sig m. (CSSMonad party sig m, Ord party) => Int -> Int -> (party -> Int) -> party -> (party -> sig -> CSSMessage party sig -> Bool) -> CSSInstance party sig m
newCSSInstance totalWeight corruptWeight partyWeight me checkSig = CSSInstance {..}
    where
        justifyChoice :: Choice -> m ()
        justifyChoice c = do
            alreadyJustified <- justified c <<.= True
            unless alreadyJustified $ whenM (use report) $ do
                inputs <- use (input c)
                forM_ (Map.toList inputs) $ \(p, sig) -> do
                    seen <- isJust <$> use (iSaw . at p)
                    unless seen $ do
                        iSaw . at p ?= c
                        sendCSSMessage $ Seen p c
        receiveCSSMessage :: party -> sig -> CSSMessage party sig -> m ()
        receiveCSSMessage src sig (Input c) = addInput src sig c
        receiveCSSMessage src sig (Seen sp c) = addSeen src sig sp c
        receiveCSSMessage src sig (DoneReporting sawSet) = addDoneReporting src sig sawSet
        addInput :: party -> sig -> Choice -> m ()
        addInput src sig c = do
            input c . at src ?= sig
            whenM (use report) $ do -- Only collect inputs while in report phase
                seen <- isJust <$> use (iSaw . at src)                
                unless seen $ do
                    -- Check if the choice is justified already
                    whenM (use (justified c)) $ do
                        -- If so, this value is going into iSaw, so send a Seen message.
                        iSaw . at src ?= c
                        sendCSSMessage $ Seen src c
        addSeen :: party -> sig -> party -> Choice -> m ()
        addSeen src sig sp c = do
            let updateSaw Nothing = Just (partyWeight src, Map.singleton src sig)
                updateSaw o@(Just (oldWeight, oldMap))
                    | isNothing (Map.lookup src oldMap) = Just (oldWeight + partyWeight src, Map.insert src sig oldMap)
                    | otherwise = o
            Just (weight, _) <- saw c . at sp <%= updateSaw
            when (weight >= totalWeight - corruptWeight) $ 
                whenM (use (justified c)) $
                    whenM (isJust <$> use (input c . at sp)) $
                        addManySaw sp c
        addManySaw :: party -> Choice -> m ()
        addManySaw party c = do
            oldMS <- manySaw . at party <<%= addChoice c
            msw <- if (isNothing oldMS) then manySawWeight <%= (+ partyWeight party) else use manySawWeight
            when (msw >= totalWeight - corruptWeight) $ do
                oldRep <- report <<.= False
                when oldRep $ do
                    sendCSSMessage . DoneReporting =<< use iSaw
        addDoneReporting :: party -> sig -> Map party choice -> m ()
        addDoneReporting src sig sawSet = undefined