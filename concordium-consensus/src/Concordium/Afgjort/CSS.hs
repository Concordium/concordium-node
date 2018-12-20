{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
-- |Core Set Selection algorithm
module Concordium.Afgjort.CSS where

import Data.Map (Map)
import qualified Data.Map as Map
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
    | DoneReporting (Map party (Choice, sig)) -- Possbly use list instead

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
    _report :: Bool,
    _botJustified :: Bool,
    _topJustified :: Bool,
    _inputTop :: Map party sig,
    _inputBot :: Map party sig,
    _sawTop :: Map party (Int, Map party sig),
    _sawBot :: Map party (Int, Map party sig),
    _manySaw :: Map party Choices,
    _manySawWeight :: Int
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

iSaw :: (Ord party) => SimpleGetter (CSSState party sig) (Map party (Choice, sig))
iSaw = to g
    where
        g s = case (_botJustified s, _topJustified s) of
            (False, False) -> Map.empty
            (True, False) -> (False, ) <$> _inputBot s
            (False, True) -> (True, ) <$> _inputTop s
            (True, True) -> ((False, ) <$> _inputBot s) `Map.union` ((True, ) <$> _inputTop s)

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
        justifyChoice c = undefined
        receiveCSSMessage :: party -> sig -> CSSMessage party sig -> m ()
        receiveCSSMessage src sig (Input c) = addInput src sig c
        receiveCSSMessage src sig (Seen sp c) = addSeen src sig sp c
        receiveCSSMessage src sig (DoneReporting sawSet) = undefined
        addInput :: party -> sig -> Choice -> m ()
        addInput src sig c = whenM (use report) $ do -- Only collect inputs while in report phase
            seen <- isJust <$> use (iSaw . at src)
            unless seen $ do
                input c . at src ?= sig
                -- Check if the choice is justified already
                whenM (use (justified c)) $ do
                    -- If so, this value is going into iSaw, so send a Seen message.
                    sendCSSMessage $ Seen src c
        addSeen :: party -> sig -> party -> Choice -> m ()
        addSeen src sig sp c = do
            let updateSaw Nothing = Just (partyWeight src, Map.singleton src sig)
                updateSaw o@(Just (oldWeight, oldMap))
                    | isNothing (Map.lookup src oldMap) = Just (oldWeight + partyWeight src, Map.insert src sig oldMap)
                    | otherwise = o
            Just (weight, _) <- saw c . at sp <%= updateSaw
            when (weight >= totalWeight - corruptWeight) $ addManySaw sp c
        addManySaw :: party -> Choice -> m ()
        addManySaw party c = do
            oldMS <- manySaw . at party <<%= addChoice c
            msw <- if (isNothing oldMS) then manySawWeight <%= (+ partyWeight party) else use manySawWeight
            when (msw >= totalWeight - corruptWeight) $ do
                oldRep <- report <<.= False
                when oldRep $ do
                    sendCSSMessage . DoneReporting =<< use iSaw
                