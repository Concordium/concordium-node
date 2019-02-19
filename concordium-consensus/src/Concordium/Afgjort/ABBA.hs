{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes, OverloadedStrings, DeriveGeneric #-}
{- |Asynchronous Binary Byzantine Agreement algorithm -}
module Concordium.Afgjort.ABBA where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Word
import Control.Monad.State.Class
import Control.Monad.RWS
import Lens.Micro.Platform
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import GHC.Generics (Generic)

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS


type Phase = Word32

data ABBAMessage party
    = Justified Phase Choice TicketProof
    | CSSSeen Phase party Choice
    | CSSDoneReporting Phase (Map party Choice)
    | WeAreDone Choice
    deriving (Eq, Show, Generic)

data PhaseState party = PhaseState {
    _lotteryTickets :: Map (Double, party) Ticket,
    _phaseCSSState :: CSSState party,
    _topInputWeight :: Maybe (Int, Set party),
    _botInputWeight :: Maybe (Int, Set party)
}
makeLenses ''PhaseState

inputWeight :: Choice -> Lens' (PhaseState party) (Maybe (Int, Set party))
inputWeight True = topInputWeight
inputWeight False = botInputWeight

initialPhaseState :: PhaseState party
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = initialCSSState,
    _topInputWeight = Just (0, Set.empty),
    _botInputWeight = Just (0, Set.empty)
}

data ABBAState party = ABBAState {
    _currentPhase :: Phase,
    _phaseStates :: Map Phase (PhaseState party),
    _currentGrade :: Word8,
    _topWeAreDone :: Set party,
    _topWeAreDoneWeight :: Int,
    _botWeAreDone :: Set party,
    _botWeAreDoneWeight :: Int
}
makeLenses ''ABBAState

phaseState :: Phase -> Lens' (ABBAState party) (PhaseState party)
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . at p ?~ t)

weAreDone :: Choice -> Lens' (ABBAState party) (Set party)
weAreDone True = topWeAreDone
weAreDone False = botWeAreDone

weAreDoneWeight :: Choice -> Lens' (ABBAState party) Int
weAreDoneWeight True = topWeAreDoneWeight
weAreDoneWeight False = botWeAreDoneWeight

initialABBAState :: ABBAState party
initialABBAState = ABBAState {
    _currentPhase = 0,
    _phaseStates = Map.empty,
    _currentGrade = 0,
    _topWeAreDone = Set.empty,
    _topWeAreDoneWeight = 0,
    _botWeAreDone = Set.empty,
    _botWeAreDoneWeight = 0
}

class (MonadState (ABBAState party) m) => ABBAMonad party m where
    -- |Sign and broadcast an ABBA message to all parties, _including_ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage party -> m ()
    -- |Determine the result
    aBBAComplete :: Choice -> m ()

data ABBAOutputEvent party
    = SendABBAMessage (ABBAMessage party)
    | ABBAComplete Choice

newtype ABBA party a = ABBA {
    runABBA' :: RWS () (Endo [ABBAOutputEvent party]) (ABBAState party) a
} deriving (Functor, Applicative, Monad)

runABBA :: ABBA party a -> ABBAState party -> (a, ABBAState party, [ABBAOutputEvent party])
runABBA z s = runRWS (runABBA' z) () s & _3 %~ (\(Endo f) -> f [])

instance MonadState (ABBAState party) (ABBA party) where
    get = ABBA get
    put = ABBA . put
    state = ABBA . state

instance ABBAMonad party (ABBA party) where
    sendABBAMessage msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete c = ABBA $ tell $ Endo (ABBAComplete c :)

data ABBAInstance party m = ABBAInstance {
    justifyABBAChoice :: Choice -> m (),
    receiveABBAMessage :: party -> ABBAMessage party -> m (),
    beginABBA :: Choice -> m ()
}

liftCSS :: (ABBAMonad party m, Ord party) => Phase -> CSS party a -> m (a, Maybe (CoreSet party))
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

newABBAInstance :: forall party m. (ABBAMonad party m, Ord party) => BS.ByteString -> Int -> Int -> (party -> Int) -> (party -> VRF.PublicKey) -> party -> VRF.KeyPair -> ABBAInstance party m
newABBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey = ABBAInstance {..}
    where
        CSSInstance{..} = newCSSInstance totalWeight corruptWeight partyWeight
        myTicket :: Phase -> TicketProof
        myTicket phase = makeTicketProof (Ser.runPut $ Ser.put baid >> Ser.put phase) privateKey
        justifyABBAChoice :: Choice -> m ()
        justifyABBAChoice c = myLiftCSS 0 (justifyChoice c)
        handleCoreSet :: Phase -> CoreSet party -> m ()
        handleCoreSet phase cs = do
                tkts <- filter (\((_,party),tkt) -> checkTicket baid (pubKeys party) tkt) . Map.toDescList <$> use (phaseState phase . lotteryTickets)
                let (nextBit, newGrade) =
                        if Set.null csBot then
                            (True, 2)
                        else if Set.null csTop then
                            (False, 2)
                        else if topWeight >= totalWeight - corruptWeight then
                            (True, 1)
                        else if botWeight >= totalWeight - corruptWeight then
                            (False, 1)
                        else if botWeight <= corruptWeight then
                            (True, 0)
                        else if topWeight <= corruptWeight then
                            (False, 0)
                        else
                            (head $ catMaybes $ (\((_,party), _) -> csRes party) <$> tkts, 0)
                oldGrade <- currentGrade <<.= newGrade
                when (newGrade == 2 && oldGrade /= 2) $
                    sendABBAMessage (WeAreDone nextBit)
                -- currentChoice .= nextBit
                currentPhase .= phase + 1
                sendABBAMessage (Justified (phase+1) nextBit (myTicket (phase+1)))
            where
                csTop = fromMaybe Set.empty (coreTop cs)
                csBot = fromMaybe Set.empty (coreBot cs)
                csRes p = if p `Set.member` csTop then Just True else
                            if p `Set.member` csBot then Just False else Nothing
                topWeight = sum $ partyWeight <$> Set.toList csTop
                botWeight = sum $ partyWeight <$> Set.toList csBot
        myLiftCSS :: Phase -> CSS party a -> m a
        myLiftCSS p a = do
            (r, cs) <- liftCSS p a
            forM_ cs $ \cs' -> do
                cp <- use currentPhase
                when (p == cp) $ handleCoreSet p cs'
            return r
        receiveABBAMessage :: party -> ABBAMessage party -> m ()
        receiveABBAMessage src (Justified phase c ticketProof) = do
            myLiftCSS phase (receiveCSSMessage src (Input c))
            let ticket = proofToTicket ticketProof (partyWeight src) totalWeight
            phaseState phase . lotteryTickets . at (ticketValue ticket, src) ?= ticket
            inputw <- use $ phaseState phase . inputWeight c
            forM_ inputw $ \(w, ps) -> unless (src `Set.member` ps) $
                if w + partyWeight src > corruptWeight then do
                    phaseState phase . inputWeight c .= Nothing
                    myLiftCSS (phase + 1) (justifyChoice c)
                else
                    phaseState phase . inputWeight c .= Just (w + partyWeight src, Set.insert src ps)
        receiveABBAMessage src (CSSSeen phase p c) =
            myLiftCSS phase (receiveCSSMessage src (Seen p c))
        receiveABBAMessage src (CSSDoneReporting phase m) =
            myLiftCSS phase (receiveCSSMessage src (DoneReporting m))
        receiveABBAMessage src (WeAreDone c) = do
            alreadyDone <- weAreDone c <<%= Set.insert src
            unless (src `Set.member` alreadyDone) $ do
                owadw <- weAreDoneWeight c <<%= (+ partyWeight src)
                when (owadw + partyWeight src >= totalWeight - corruptWeight && owadw < totalWeight - corruptWeight) $
                    aBBAComplete c
            -- TODO: Check when threshold is met, etc.
        beginABBA :: Choice -> m ()
        beginABBA c = do
            cp <- use currentPhase
            when (cp == 0) $ sendABBAMessage (Justified 0 c (myTicket 0))
