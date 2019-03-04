{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, RecordWildCards, LambdaCase, FlexibleContexts, RankNTypes, ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Concordium.Afgjort.Freeze(
    FreezeMessage(..),
    FreezeState(..),
    initialFreezeState,
    FreezeInstance(FreezeInstance),
    FreezeMonad(..),
    FreezeOutputEvent(..),
    Freeze,
    runFreeze,
    propose,
    justifyCandidate,
    receiveFreezeMessage
) where

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Control.Monad.State.Class
import Control.Monad.RWS
import Lens.Micro.Platform

-- |Freeze protocol messages
data FreezeMessage val
    = Proposal val      -- ^Propose a value
    | Vote (Maybe val)  -- ^Vote for a value (or nothing)
    deriving (Eq, Ord, Show)

-- |The state of the Freeze protocol.
data FreezeState val party = FreezeState {
    -- |The proposals.  Each proposal is associated with a triple consisting of:
    -- a @Bool@ indicating whether the proposed candidate is justified;
    -- the total weight of parties proposing the value; and
    -- the set of parties that have made the proposal.
    -- Note that each party should be allowed only one proposal.
    -- In particular, if a party has made a proposal, no additional
    -- proposals should be considered from that party.
    _proposals :: Map val (Bool, Int, Set party),
    -- |The set of parties that have submitted proposals.  This includes proposals that are not yet justified,
    -- in order to avoid allowing a party to make more than one proposal.
    _proposers :: Set party,
    -- |The weighted total of the parties who have made justified proposals.
    _totalProposals :: Int,
    -- |The number of distinct justified proposals.  This is used to determine
    -- when a vote for no value is justified.
    _distinctJustifiedProposals :: Int,
    -- |The votes.  Each vote is associated with a triple consisting of:
    -- a @Bool@ indicating whether the vote is justified;
    -- the total weight of parties voting for the value; and
    -- the set of parties that have voted for the value.
    -- Like proposals, we only consider the first vote for a given party.
    _votes :: Map (Maybe val) (Bool, Int, Set party),
    -- |The set of parties from which votes have been received.
    -- This includes votes that are not yet justified.
    _voters :: Set party,
    -- |The weighted total of the parties who have made justified votes.
    _totalVotes :: Int,
    -- |The set of decisions that have become justified.
    _justifiedDecisions :: Set (Maybe val),
    -- |Whether we have output a justifed decision already.
    _completed :: Bool
} deriving (Eq, Ord, Show)
makeLenses ''FreezeState

-- |The initial state of the Freeze protocol.
initialFreezeState :: FreezeState val party
initialFreezeState = FreezeState Map.empty Set.empty 0 0 Map.empty Set.empty 0 Set.empty False

-- |The context for running the Freeze protocol, which includes
--
-- * The total weight of all parties participating in the protocol
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * My party
data FreezeInstance party = FreezeInstance {
    totalWeight :: Int,
    corruptWeight :: Int,
    partyWeight :: party -> Int,
    me :: party
}

-- | 'FreezeMonad' is implemented by a client that wishes to use the Freeze protocol
class (MonadReader (FreezeInstance party) m, MonadState (FreezeState val party) m) => FreezeMonad val party m where
    -- |Broadcast a 'FreezeMessage' to all parties.  It is not necessary for the
    -- messages to be sent back to the 'FreezeInstance', but should not be harmful
    -- either.
    sendFreezeMessage :: FreezeMessage val -> m ()
    -- |Output the decision from running the freeze protocol.
    frozen :: Maybe val -> m ()
    -- |Notify that a decision from the freeze protocol has become justified.
    decisionJustified :: Maybe val -> m ()


data FreezeOutputEvent val
    = SendFreezeMessage (FreezeMessage val)
    | Frozen (Maybe val)
    | DecisionJustified (Maybe val)

newtype Freeze val party a = Freeze {
    runFreeze' :: RWS (FreezeInstance party) (Endo [FreezeOutputEvent val]) (FreezeState val party) a
} deriving (Functor, Applicative, Monad)

runFreeze :: Freeze val party a -> FreezeInstance party -> FreezeState val party -> (a, FreezeState val party, [FreezeOutputEvent val])
runFreeze z i s = runRWS (runFreeze' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadReader (FreezeInstance party) (Freeze val party) where
    ask = Freeze ask
    reader = Freeze . reader
    local f = Freeze . local f . runFreeze'

instance MonadState (FreezeState val party) (Freeze val party) where
    get = Freeze get
    put = Freeze . put
    state = Freeze . state

instance FreezeMonad val party (Freeze val party) where
    sendFreezeMessage = Freeze . tell . Endo . (:) . SendFreezeMessage
    frozen = Freeze . tell . Endo . (:) . Frozen
    decisionJustified = Freeze . tell . Endo . (:) . DecisionJustified

whenAddToSet :: (Ord v, MonadState s m) => v -> Lens' s (Set v) -> m () -> m ()
whenAddToSet val setLens act = do
    theSet <- use setLens
    unless (val `Set.member` theSet) $ do
        setLens .= Set.insert val theSet
        act

addProposal :: (Ord val, Ord party, FreezeMonad val party m) => party -> val -> m ()
addProposal party value = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $ whenAddToSet party proposers $
        use (proposals . at value) >>= \case
            Nothing -> proposals . at value ?= (False, partyWeight party, Set.singleton party)
            Just (isCandidate, oldWeight, parties) -> do
                let newWeight = partyWeight party + oldWeight
                proposals . at value ?= (isCandidate, newWeight, Set.insert party parties)
                when isCandidate $ do
                    newTotalProposals <- totalProposals <%= (partyWeight party +)
                    when (oldWeight == 0) $ do
                        currJProps <- distinctJustifiedProposals <%= (1+)
                        when (currJProps == 2) $ justifyVote Nothing
                    when (newWeight >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                    when (newTotalProposals >= totalWeight - corruptWeight) doVote

justifyVote :: (Ord val, Ord party, FreezeMonad val party m) => Maybe val -> m ()
justifyVote vote = do
    FreezeInstance{..} <- ask
    use (votes . at vote) >>= \case
        Nothing -> votes . at vote ?= (True, 0, Set.empty)
        Just (False, weight, parties) -> do
            votes . at vote ?= (True, weight, parties)
            newTotalVotes <- totalVotes <%= (weight +)
            -- Record when a decision becomes justified
            when (weight > corruptWeight) $ justifyDecision vote
            when (newTotalVotes >= totalWeight - corruptWeight) doFinish
        Just (True, _, _) -> return () 

addVote :: (Ord val, Ord party, FreezeMonad val party m) => party -> Maybe val -> m ()
addVote party vote = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $ whenAddToSet party voters $
        use (votes . at vote) >>= \case
            Nothing -> 
                votes . at vote ?= (False, partyWeight party, Set.singleton party)
            Just (False, weight, parties) ->
                votes . at vote ?= (False, partyWeight party + weight, Set.insert party parties)
            Just (True, weight, parties) -> do
                votes . at vote ?= (True, partyWeight party + weight, Set.insert party parties)
                newTotalVotes <- totalVotes <%= (partyWeight party +)
                -- Record when a decision becomes justified
                when (partyWeight party + weight > corruptWeight) $ justifyDecision vote
                when (newTotalVotes >= totalWeight - corruptWeight) doFinish

justifyDecision :: (Ord val, Ord party, FreezeMonad val party m) => Maybe val -> m ()
justifyDecision decision = whenAddToSet decision justifiedDecisions $ decisionJustified decision


doVote :: forall val party m. (Ord val, Ord party, FreezeMonad val party m) => m ()
doVote = do
        FreezeInstance{..} <- ask
        vtrs <- use voters
        unless (me `Set.member` vtrs) $ do
            let
                determineVote [] = Nothing
                determineVote ((_, (False, _, _)) : rs) = determineVote rs
                determineVote ((val, (True, weight, _)) : rs) = if weight >= totalWeight - corruptWeight then Just val else determineVote rs
            vote <- determineVote . Map.toList <$> use proposals
            addVote me vote
            sendFreezeMessage (Vote vote)

doFinish :: (Ord val, Ord party, FreezeMonad val party m) => m ()
doFinish = do
    alreadyCompleted <- completed <<.= True
    unless alreadyCompleted $
        (Set.toDescList <$> use justifiedDecisions) >>= \case
            [] -> completed .= False -- "This should not happen."
            (res : _) -> frozen res -- Take the first justified decision. Using toDescList ensures that Nothing is the least preferred decision.
                                
-- |Propose a candidate value.  The value must already be a candidate (i.e. 
-- 'justifyCandidate' should have been called for this value).
-- This should only be called once per instance.  If it is called more than once,
-- or if it is called after receiving a proposal from our own party, only the first
-- proposal is considered, and subsequent proposals are not broadcast.
propose :: (Ord val, Ord party, FreezeMonad val party m) => val -> m ()
propose candidate = do
        FreezeInstance{..} <- ask
        pps <- use proposers
        unless (me `Set.member` pps) $ do
            addProposal me candidate
            sendFreezeMessage (Proposal candidate)

-- |Justify a value as a candidate.
justifyCandidate :: (Ord val, Ord party, FreezeMonad val party m) => val -> m ()
justifyCandidate value = do
    FreezeInstance{..} <- ask
    use (proposals . at value) >>= \case
        Nothing -> proposals . at value ?= (True, 0, Set.empty)
        Just (False, weight, parties) -> do
            proposals . at value ?= (True, weight, parties)
            when (weight > 0) $ do
                newTotalProposals <- totalProposals <%= (weight +)
                currJProps <- distinctJustifiedProposals <%= (1+)
                when (weight >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                when (currJProps == 2) $ justifyVote Nothing
                when (newTotalProposals >= totalWeight - corruptWeight) doVote
        Just (True, _, _) -> return ()

-- |Handle a Freeze message from the network
receiveFreezeMessage :: (Ord val, Ord party, FreezeMonad val party m) => party -> FreezeMessage val -> m ()
receiveFreezeMessage party (Proposal val) = addProposal party val
receiveFreezeMessage party (Vote vote) = addVote party vote
