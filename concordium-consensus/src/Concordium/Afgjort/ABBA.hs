{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes, OverloadedStrings #-}
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
import System.Random
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Data.Semigroup

import qualified Concordium.Crypto.DummyVRF as VRF
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Afgjort.CSS

data Ticket = Ticket {
    ticketValue :: H.Hash,
    ticketIndex :: Word32,
    ticketProof :: VRF.Proof
} deriving (Eq, Show)

calculateTicketValue :: Word32 -> VRF.Proof -> H.Hash
calculateTicketValue ind pf =  H.hashLazy $ Ser.runPutLazy $ Ser.put (VRF.proofToHash pf) >> Ser.put ind

-- |Generate a ticket for a lottery
makeTicket :: BS.ByteString -- ^Lottery identifier
            -> Int          -- ^Party weight, must be strictly positive
            -> VRF.PrivateKey   -- ^Private VRF key
            -> Ticket
makeTicket lotteryid weight privKey = Ticket val idx pf
    where
        pf = VRF.prove privKey ("AL" <> lotteryid)
        Just (Max (val, idx)) = mconcat [Just (Max (calculateTicketValue ind pf, ind)) | ind <- [0..fromIntegral weight-1]]

checkTicket :: BS.ByteString -> Int -> VRF.PublicKey -> Ticket -> Bool
checkTicket lotteryid weight key Ticket{..} =
        ticketIndex < fromIntegral weight &&
        VRF.verifyKey key && -- TODO: possibly this is not necessary
        VRF.verify key ("AL" <> lotteryid) ticketProof &&
        ticketValue == calculateTicketValue ticketIndex ticketProof

data ABBAMessage party
    = Justified Word Bool Ticket
    | CSSSeen Word party Choice
    | CSSDoneReporting Word (Map party Choice)
    | WeAreDone Choice
    deriving (Eq, Show)

data PhaseState party sig = PhaseState {
    _lotteryTickets :: Map (H.Hash, party) Ticket,
    _phaseCSSState :: CSSState party sig,
    _topInputWeight :: Maybe (Int, Set party),
    _botInputWeight :: Maybe (Int, Set party)
}
makeLenses ''PhaseState

inputWeight :: Choice -> Lens' (PhaseState party sig) (Maybe (Int, Set party))
inputWeight True = topInputWeight
inputWeight False = botInputWeight

initialPhaseState :: PhaseState party sig
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = initialCSSState,
    _topInputWeight = Just (0, Set.empty),
    _botInputWeight = Just (0, Set.empty)
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

newABBAInstance :: forall party sig m. (ABBAMonad party sig m, Ord party) => BS.ByteString -> Int -> Int -> (party -> Int) -> (party -> VRF.PublicKey) -> party -> VRF.PrivateKey -> ABBAInstance party sig m
newABBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey = ABBAInstance {..}
    where
        CSSInstance{..} = newCSSInstance totalWeight corruptWeight partyWeight
        myTicket :: Word -> Ticket
        myTicket phase = makeTicket (Ser.runPut $ Ser.put baid >> Ser.put phase) (partyWeight me) privateKey
        justifyABBAChoice :: Choice -> m ()
        justifyABBAChoice c = myLiftCSS 0 (justifyChoice c)
        handleCoreSet :: Word -> CoreSet party sig -> m ()
        handleCoreSet phase cs = do
                tkts <- filter (\((_,party),tkt) -> checkTicket baid (partyWeight party) (pubKeys party) tkt) . Map.toDescList <$> use (phaseState phase . lotteryTickets)
                let (nextBit, newGrade) =
                        if Map.null csBot then
                            (True, 2)
                        else if Map.null csTop then
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
                            (head $ catMaybes $ (\((_,party), _) -> Map.lookup party csAll) <$> tkts, 0)
                oldGrade <- currentGrade <<.= newGrade
                when (newGrade == 2 && oldGrade /= 2) $
                    sendABBAMessage (WeAreDone nextBit)
                currentChoice .= nextBit
                currentPhase .= phase + 1
                sendABBAMessage (Justified (phase+1) nextBit (myTicket (phase+1)))
            where
                csTop = fromMaybe Map.empty (coreTop cs)
                csBot = fromMaybe Map.empty (coreBot cs)
                csAll = Map.union (const True <$> csTop) (const False <$> csBot)
                topWeight = sum $ partyWeight <$> Map.keys csTop
                botWeight = sum $ partyWeight <$> Map.keys csBot
        myLiftCSS :: Word -> CSS party sig a -> m a
        myLiftCSS p a = do
            (r, cs) <- liftCSS p a
            forM_ cs $ \cs' -> do
                cp <- use currentPhase
                when (p == cp) $ handleCoreSet p cs'
            return r
        receiveABBAMessage :: party -> sig -> ABBAMessage party -> m ()
        receiveABBAMessage src sig (Justified phase c ticket) = do
            myLiftCSS phase (receiveCSSMessage src sig (Input c))
            phaseState phase . lotteryTickets . at (ticketValue ticket, src) ?= ticket
            inputw <- use $ phaseState phase . inputWeight c
            forM_ inputw $ \(w, ps) -> unless (src `Set.member` ps) $
                if w + partyWeight src > corruptWeight then do
                    phaseState phase . inputWeight c .= Nothing
                    myLiftCSS (phase + 1) (justifyChoice c)
                else
                    phaseState phase . inputWeight c .= Just (w + partyWeight src, Set.insert src ps)
        receiveABBAMessage src sig (CSSSeen phase p c) =
            myLiftCSS phase (receiveCSSMessage src sig (Seen p c))
        receiveABBAMessage src sig (CSSDoneReporting phase m) =
            myLiftCSS phase (receiveCSSMessage src sig (DoneReporting m))
        receiveABBAMessage src sig (WeAreDone c) = do
            alreadyDone <- weAreDoneParties . at src <<.= Just (c, sig)
            when (isNothing alreadyDone) $ weAreDoneWeight += partyWeight src
            -- TODO: Check when threshold is met, etc.
        beginABBA :: Choice -> m ()
        beginABBA c = do
            cp <- use currentPhase
            when (cp == 0) $ sendABBAMessage (Justified 0 c (myTicket 0))
