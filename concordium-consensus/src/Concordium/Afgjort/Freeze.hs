{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, RecordWildCards, LambdaCase, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Concordium.Afgjort.Freeze where

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Control.Monad.State.Class
import Control.Monad
import Lens.Micro.Platform

data FreezeMessage val
    = Proposal val
    | Vote (Maybe val)
    deriving (Eq, Ord, Show)

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


initialFreezeState :: FreezeState val party
initialFreezeState = FreezeState Map.empty Set.empty 0 0 Map.empty Set.empty 0 Set.empty False

-- | 'FreezeMonad' is implemented by a client that wishes to use the Freeze protocol
class (MonadState (FreezeState val party) m) => FreezeMonad val party m where
    -- |Broadcast a 'FreezeMessage' to all parties.  It is not necessary for the
    -- messages to be sent back to the 'FreezeInstance', but should not be harmful
    -- either.
    broadcastFreezeMessage :: FreezeMessage val -> m ()
    -- |Output the decision from running the freeze protocol.
    frozen :: Maybe val -> m ()
    -- |Notify that a decision from the freeze protocol has become justified.
    decisionJustified :: Maybe val -> m ()


data FreezeInstance val party m = FreezeInstance {
    -- |Propose a candidate value.  The value must already be a candidate (i.e. 
    -- 'justifyCandidate' should have been called for this value).
    -- This should only be called once per instance.  If it is called more than once,
    -- or if it is called after receiving a proposal from our own party, only the first
    -- proposal is considered, and subsequent proposals are not broadcast.
    propose :: val -> m (),
    justifyCandidate :: val -> m (),
    freezeMessage :: party -> FreezeMessage val -> m ()
}

whenAddToSet :: (Ord v, MonadState s m) => v -> Lens' s (Set v) -> m () -> m ()
whenAddToSet val setLens act = do
    theSet <- use setLens
    unless (val `Set.member` theSet) $ do
        setLens .= Set.insert val theSet
        act

-- |Create a new instance of the freeze protocol.  The implementation assumes synchronous
-- access to the FreezeState.  That is, the state cannot be updated concurrently during calls
-- to 'propose' or 'freezeMessage'.  The implementation is not re-entrant: 'awaitCandidate',
-- 'broadcastFreezeMessage', 'frozen' and 'decisionJustified' should _not_ call back
-- 'propose' or 'freezeMessage'.
newFreezeInstance :: forall val party m. (Ord val, Ord party, FreezeMonad val party m) => Int -> Int -> (party -> Int) -> party -> FreezeInstance val party m
newFreezeInstance totalWeight corruptWeight partyWeight me = FreezeInstance {..}
    where
        -- Add a proposal.
        addProposal party value = when (partyWeight party > 0) $ whenAddToSet party proposers $
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
        justifyCandidate value = use (proposals . at value) >>= \case
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
        justifyVote vote =
            use (votes . at vote) >>= \case
                Nothing -> votes . at vote ?= (True, 0, Set.empty)
                Just (False, weight, parties) -> do
                    votes . at vote ?= (True, weight, parties)
                    newTotalVotes <- totalVotes <%= (weight +)
                    -- Record when a decision becomes justified
                    when (weight > corruptWeight) $ justifyDecision vote
                    when (newTotalVotes >= totalWeight - corruptWeight) doFinish
                Just (True, _, _) -> return () 
        addVote party vote = when (partyWeight party > 0) $ whenAddToSet party voters $
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
        justifyDecision decision = whenAddToSet decision justifiedDecisions $ decisionJustified decision
        determineVote [] = Nothing
        determineVote ((_, (False, _, _)) : rs) = determineVote rs
        determineVote ((val, (True, weight, _)) : rs) = if weight >= totalWeight - corruptWeight then Just val else determineVote rs
        doVote = do
            vtrs <- use voters
            unless (me `Set.member` vtrs) $ do
                vote <- determineVote . Map.toList <$> use proposals
                addVote me vote
                broadcastFreezeMessage (Vote vote)
        doFinish = do
            alreadyCompleted <- completed <<.= True
            unless alreadyCompleted $
                (Set.toDescList <$> use justifiedDecisions) >>= \case
                    [] -> completed .= False -- "This should not happen."
                    (res : _) -> frozen res -- Take the first justified decision. Using toDescList ensures that Nothing is the least preferred decision.
        propose :: val -> m ()
        propose candidate = do
            pps <- use proposers
            unless (me `Set.member` pps) $ do
                addProposal me candidate
                broadcastFreezeMessage (Proposal candidate)
        freezeMessage :: party -> FreezeMessage val -> m ()
        freezeMessage party (Proposal val) = addProposal party val
        freezeMessage party (Vote vote) = addVote party vote
