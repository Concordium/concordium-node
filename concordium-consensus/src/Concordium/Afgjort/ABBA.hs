{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}
{- |Asynchronous Binary Byzantine Agreement algorithm -}
module Concordium.Afgjort.ABBA where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Word
import Control.Monad.State.Class
import Control.Monad.RWS
import Lens.Micro.Platform

import qualified Concordium.Crypto.DummyVRF as VRF
import Concordium.Afgjort.CSS

data ABBAMessage party
    = Justified Word Bool VRF.Proof
    | CSSSeen Word party Choice
    | CSSDoneReporting Word (Map party Choice)
    | WeAreDone Choice
    deriving (Eq, Show)

data PhaseState party sig = PhaseState {
    _lotteryProofs :: Map party VRF.Proof,
    _phaseCSSState :: CSSState party sig
}
makeLenses ''PhaseState

initialPhaseState :: PhaseState party sig
initialPhaseState = PhaseState {
    _lotteryProofs = Map.empty,
    _phaseCSSState = initialCSSState
}

data ABBAState party sig = ABBAState {
    _currentPhase :: Word,
    _phaseStates :: Map Word (PhaseState party sig),
    _currentChoice :: Choice,
    _currentGrade :: Word8,
    _weAreDoneParties :: Map party (Choice, sig),
    _weAreDoneWeight :: Int
}
makeLenses ''ABBAState

phaseState :: Word -> Lens' (ABBAState party sig) (PhaseState party sig)
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . at p ?~ t)

initialABBAState :: Choice -> ABBAState party sig
initialABBAState b = ABBAState {
    _currentPhase = 0,
    _phaseStates = Map.empty,
    _currentChoice = b,
    _currentGrade = 0,
    _weAreDoneParties = Map.empty,
    _weAreDoneWeight = 0
}

class (MonadState (ABBAState party sig) m) => ABBAMonad party sig m where
    -- |Sign and broadcast an ABBA message to all parties, _including_ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage party -> m ()
    -- |Determine the core set.
    aBBAComplete :: Map party (Choice, sig) -> m ()

data ABBAOutputEvent party sig
    = SendABBAMessage (ABBAMessage party)
    | ABBAComplete (Map party (Choice, sig))

newtype ABBA party sig a = ABBA {
    runABBA' :: RWS () (Endo [ABBAOutputEvent party sig]) (ABBAState party sig) a
} deriving (Functor, Applicative, Monad)

runABBA :: ABBA party sig a -> ABBAState party sig -> (a, ABBAState party sig, [ABBAOutputEvent party sig])
runABBA z s = runRWS (runABBA' z) () s & _3 %~ (\(Endo f) -> f [])

instance MonadState (ABBAState party sig) (ABBA party sig) where
    get = ABBA get
    put = ABBA . put
    state = ABBA . state

instance ABBAMonad party sig (ABBA party sig) where
    sendABBAMessage msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete cs = ABBA $ tell $ Endo (ABBAComplete  cs :)

data ABBAInstance party sig m = ABBAInstance {
    justifyABBAChoice :: Choice -> m (),
    receiveABBAMessage :: party -> sig -> ABBAMessage party -> m (),
    beginABBA :: Choice -> m ()
}

liftCSS :: (ABBAMonad party sig m, Ord party) => Word -> CSS party sig a -> m (a, Maybe (CoreSet party sig))
liftCSS phase a = do
        cssstate <- use (phaseState phase . phaseCSSState)
        let (r, cssstate', evs) = runCSS a cssstate
        phaseState phase . phaseCSSState .= cssstate'
        cs <- handleEvents evs
        return (r, cs)
    where
        handleEvents [] = return Nothing
        handleEvents (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleEvents evs
        handleEvents (SelectCoreSet cs : evs) = handleEvents evs >> return (Just cs)
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen p c) = CSSSeen phase p c
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs

newABBAInstance :: forall party sig m. (ABBAMonad party sig m, Ord party) => Int -> Int -> (party -> Int) -> ABBAInstance party sig m
newABBAInstance totalWeight corruptWeight partyWeight = ABBAInstance {..}
    where
        CSSInstance{..} = newCSSInstance totalWeight corruptWeight partyWeight
        justifyABBAChoice :: Choice -> m ()
        justifyABBAChoice c = myLiftCSS 0 (justifyChoice c)
        handleCoreSet :: CoreSet party sig -> m ()
        handleCoreSet cs = undefined
        myLiftCSS :: Word -> CSS party sig a -> m a
        myLiftCSS p a = do
            (r, cs) <- liftCSS p a
            forM_ cs handleCoreSet
            return r
        receiveABBAMessage :: party -> sig -> ABBAMessage party -> m ()
        receiveABBAMessage src sig (Justified phase c pf) = do
            -- TODO: Check the proof!
            myLiftCSS phase (receiveCSSMessage src sig (Input c))
            phaseState phase . lotteryProofs . at src ?= pf
        receiveABBAMessage src sig (CSSSeen phase p c) =
            myLiftCSS phase (receiveCSSMessage src sig (Seen p c))
        receiveABBAMessage src sig (CSSDoneReporting phase m) =
            myLiftCSS phase (receiveCSSMessage src sig (DoneReporting m))
        receiveABBAMessage src sig (WeAreDone c) = do
            alreadyDone <- weAreDoneParties . at src <<.= Just (c, sig)
            when (isNothing alreadyDone) $ weAreDoneWeight += partyWeight src
            -- TODO: Check when threshold is met, etc.
        beginABBA :: Choice -> m ()
        beginABBA c = undefined
