{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes, OverloadedStrings, DeriveGeneric #-}
{- |Asynchronous Binary Byzantine Agreement algorithm -}
module Concordium.Afgjort.ABBA(
    Phase,
    ABBAMessage(..),
    ABBAInstance(ABBAInstance),
    ABBAState(..),
    initialABBAState,
    ABBAMonad(..),
    ABBAOutputEvent(..),
    ABBA,
    runABBA,
    beginABBA,
    justifyABBAChoice,
    receiveABBAMessage,
    Choice
) where

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
import Control.Exception.Base(assert)

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS

-- |A phase in the ABBA protocol
type Phase = Word32

-- |A message in the ABBA protocol
data ABBAMessage party
    = Justified Phase Choice TicketProof            -- ^Party's input for a given phase
    | CSSSeen Phase party Choice                    -- ^CSS seen message for a phase
    | CSSDoneReporting Phase (Map party Choice)     -- ^CSS done reporting message for a phase
    | WeAreDone Choice                              -- ^Message that indicates consensus should be reached
    deriving (Eq, Show)

-- |An @ABBAInstance@ consists of:
--
-- * The instance identifier for this instantiation of the protocol
-- * The total weight of all parties
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * The public key of each party
-- * My party
-- * My VRF key
data ABBAInstance party = ABBAInstance {
    -- |The instance identifier for this instantiation of the protocol
    baid :: BS.ByteString,
    -- |The total weight of all parties
    totalWeight :: Int,
    -- |The maximum weight of corrupt parties (must be less than @totalWeight/3@).
    corruptWeight :: Int,
    -- |The weight of each party
    partyWeight :: party -> Int,
    -- |The public key of each party
    pubKeys :: party -> VRF.PublicKey,
    -- |My party
    me :: party,
    -- |My VRF key
    privateKey :: VRF.KeyPair
}

-- |The state of a phase in the protocol.
--
-- This includes the lottery tickets submitted by parties, the CSS state for the current phase,
-- and the total weight and set of parties that have nominated each input.  Once the weight
-- exceeds the threshold, we record it as @Nothing@, forgetting the exact weigth and parties.
data PhaseState party = PhaseState {
    _lotteryTickets :: Map (Double, party) Ticket,
    _phaseCSSState :: CSSState party,
    _topInputWeight :: Maybe (Int, Set party),
    _botInputWeight :: Maybe (Int, Set party)
} deriving (Show)
makeLenses ''PhaseState

-- |The total weight and set of parties nominating a particular choice.
inputWeight :: Choice -> Lens' (PhaseState party) (Maybe (Int, Set party))
inputWeight True = topInputWeight
inputWeight False = botInputWeight

-- |
initialPhaseState :: PhaseState party
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = initialCSSState,
    _topInputWeight = Just (0, Set.empty),
    _botInputWeight = Just (0, Set.empty)
}

-- |The state of the ABBA protocol.
--
-- This includes the current phase, the state of all phases, the current grade,
-- and the set and weight of parties that have claimed we are done with each
-- possible output choice.
data ABBAState party = ABBAState {
    _currentPhase :: Phase,
    _phaseStates :: Map Phase (PhaseState party),
    _currentGrade :: Word8,
    _topWeAreDone :: Set party,
    _topWeAreDoneWeight :: Int,
    _botWeAreDone :: Set party,
    _botWeAreDoneWeight :: Int,
    _completed :: Bool
} deriving (Show)
makeLenses ''ABBAState

-- |The state of a particular phase
phaseState :: Phase -> Lens' (ABBAState party) (PhaseState party)
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . at p ?~ t)

-- |The set of parties claiming we are done with a given choice
weAreDone :: Choice -> Lens' (ABBAState party) (Set party)
weAreDone True = topWeAreDone
weAreDone False = botWeAreDone

-- |The weight of parties claiming we are done with a given choice
weAreDoneWeight :: Choice -> Lens' (ABBAState party) Int
weAreDoneWeight True = topWeAreDoneWeight
weAreDoneWeight False = botWeAreDoneWeight

-- |The initial state of the ABBA protocol.
initialABBAState :: ABBAState party
initialABBAState = ABBAState {
    _currentPhase = 0,
    _phaseStates = Map.empty,
    _currentGrade = 0,
    _topWeAreDone = Set.empty,
    _topWeAreDoneWeight = 0,
    _botWeAreDone = Set.empty,
    _botWeAreDoneWeight = 0,
    _completed = False
}

-- |The @ABBAMonad@ class defines the events associated with the ABBA protocol.
class (MonadState (ABBAState party) m, MonadReader (ABBAInstance party) m) => ABBAMonad party m where
    -- |Sign and broadcast an ABBA message to all parties, __including__ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage party -> m ()
    -- |Determine the result
    aBBAComplete :: Choice -> m ()

-- |Representation of (output) events associated with the ABBA protocol.
data ABBAOutputEvent party
    = SendABBAMessage (ABBAMessage party)   -- ^Sign and broadcast a message
    | ABBAComplete Choice                   -- ^Determine result

-- |A concrete implementation of the ABBA monad.
newtype ABBA party a = ABBA {
    runABBA' :: RWS (ABBAInstance party) (Endo [ABBAOutputEvent party]) (ABBAState party) a
} deriving (Functor, Applicative, Monad)

-- |Run part of the ABBA protocol, given an 'ABBAInstance' and 'ABBAState'.
-- The result includes the updated state and a list of 'ABBAOutputEvent's that occurred during the execution.
{-# INLINE runABBA #-}
runABBA :: ABBA party a -> ABBAInstance party -> ABBAState party -> (a, ABBAState party, [ABBAOutputEvent party])
runABBA z i s = runRWS (runABBA' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadState (ABBAState party) (ABBA party) where
    get = ABBA get
    put = ABBA . put
    state = ABBA . state

instance MonadReader (ABBAInstance party) (ABBA party) where
    ask = ABBA ask
    reader = ABBA . reader
    local f = ABBA . local f . runABBA'

instance ABBAMonad party (ABBA party) where
    sendABBAMessage msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete c = ABBA $ tell $ Endo (ABBAComplete c :)

-- |Lift a CSS operation to the ABBA monad in a given phase.
{-# SPECIALIZE liftCSS :: Ord party => Phase -> CSS party a -> ABBA party a #-}
liftCSS :: (ABBAMonad party m, Ord party) => Phase -> CSS party a -> m a
liftCSS phase a = do
        ABBAInstance{..} <- ask
        cssstate <- use (phaseState phase . phaseCSSState)
        let (r, cssstate', evs) = runCSS a (CSSInstance totalWeight corruptWeight partyWeight) cssstate
        phaseState phase . phaseCSSState .= cssstate'
        cs <- handleEvents evs
        forM_ cs $ \cs' -> do
            cp <- use currentPhase
            assert (phase == cp) $ handleCoreSet phase cs'
        return r
    where
        handleEvents [] = return Nothing
        handleEvents (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleEvents evs
        handleEvents (SelectCoreSet cs : evs) = handleEvents evs >> return (Just cs)
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen p c) = CSSSeen phase p c
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs

-- |Deal with a core set being generated by CSS.  The phase should always be the current phase.
{-# SPECIALIZE handleCoreSet :: Ord party => Phase -> CoreSet party -> ABBA party () #-}
handleCoreSet :: (ABBAMonad party m, Ord party) => Phase -> CoreSet party -> m ()
handleCoreSet phase cs = do
        ABBAInstance{..} <- ask
        let
            csTop = fromMaybe Set.empty (coreTop cs)
            csBot = fromMaybe Set.empty (coreBot cs)
            csRes p = if p `Set.member` csTop then Just True else
                        if p `Set.member` csBot then Just False else Nothing
            topWeight = sum $ partyWeight <$> Set.toList csTop
            botWeight = sum $ partyWeight <$> Set.toList csBot
        lid <- view $ lotteryId phase
        tkts <- filter (\((_,party),tkt) -> checkTicket lid (pubKeys party) tkt) . Map.toDescList <$> use (phaseState phase . lotteryTickets)
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
        currentPhase .= phase + 1
        tkt <- view $ myTicket (phase + 1)
        sendABBAMessage (Justified (phase+1) nextBit tkt)

-- |Get the lottery identifier string for the given phase.
lotteryId :: Phase -> SimpleGetter (ABBAInstance party) BS.ByteString
lotteryId phase = to $ \a ->
        Ser.runPut $ Ser.put (baid a) >> Ser.put phase

-- |Get my lottery ticket for the given phase.
myTicket :: Phase -> SimpleGetter (ABBAInstance party) TicketProof
myTicket phase = to $ \a ->
        makeTicketProof (a ^. lotteryId phase) (privateKey a)

{-# INLINE unlessCompleted #-}
unlessCompleted :: (ABBAMonad party m) => m () -> m ()
unlessCompleted a = do
        c <- use completed
        unless c a

-- |Called to indicate that a given choice is justified.
{-# SPECIALIZE justifyABBAChoice :: Ord party => Choice -> ABBA party () #-}
justifyABBAChoice :: (ABBAMonad party m, Ord party) => Choice -> m ()
justifyABBAChoice c = unlessCompleted $ liftCSS 0 (justifyChoice c)

-- |Called when an 'ABBAMessage' is received.
{-# SPECIALIZE receiveABBAMessage :: Ord party => party -> ABBAMessage party -> ABBA party () #-}
receiveABBAMessage :: (ABBAMonad party m, Ord party) => party -> ABBAMessage party -> m ()
receiveABBAMessage src (Justified phase c ticketProof) = unlessCompleted $ do
    ABBAInstance{..} <- ask
    liftCSS phase (receiveCSSMessage src (Input c))
    let ticket = proofToTicket ticketProof (partyWeight src) totalWeight
    phaseState phase . lotteryTickets . at (ticketValue ticket, src) ?= ticket
    inputw <- use $ phaseState phase . inputWeight c
    forM_ inputw $ \(w, ps) -> unless (src `Set.member` ps) $
        if w + partyWeight src > corruptWeight then do
            phaseState phase . inputWeight c .= Nothing
            liftCSS (phase + 1) (justifyChoice c)
        else
            phaseState phase . inputWeight c .= Just (w + partyWeight src, Set.insert src ps)
receiveABBAMessage src (CSSSeen phase p c) =
    unlessCompleted $ liftCSS phase (receiveCSSMessage src (Seen p c))
receiveABBAMessage src (CSSDoneReporting phase m) =
    unlessCompleted $ liftCSS phase (receiveCSSMessage src (DoneReporting m))
receiveABBAMessage src (WeAreDone c) = unlessCompleted $ do
    ABBAInstance{..} <- ask
    alreadyDone <- weAreDone c <<%= Set.insert src
    unless (src `Set.member` alreadyDone) $ do
        owadw <- weAreDoneWeight c <<%= (+ partyWeight src)
        when (owadw + partyWeight src >= totalWeight - corruptWeight && owadw < totalWeight - corruptWeight) $ do
            completed .= True
            aBBAComplete c

-- |Called to start the ABBA protocol
{-# SPECIALIZE beginABBA :: Choice -> ABBA party () #-}
beginABBA :: (ABBAMonad party m) => Choice -> m ()
beginABBA c = unlessCompleted $ do
    cp <- use currentPhase
    when (cp == 0) $ do
        tkt <- view $ myTicket 0
        sendABBAMessage (Justified 0 c tkt)

