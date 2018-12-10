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
    _totalVotes :: Int
}
makeLenses ''FreezeState


initialFreezeState :: FreezeState val party
initialFreezeState = FreezeState Map.empty Set.empty 0 0 Map.empty Set.empty 0

-- | 'FreezeMonad' is implemented by a client that wishes to use the Freeze protocol
class (MonadState (FreezeState val party) m) => FreezeMonad val party m where
    -- |Perform an action when a candidate becomes justified.
    -- If the candidate is already justified, then the action is perfomed
    -- immediately.
    awaitCandidate :: val -> m () -> m ()
    broadcastFreezeMessage :: FreezeMessage val -> m ()
    frozen :: Maybe val -> m ()


data FreezeInstance val party m = FreezeInstance {
    -- |Propose a candidate value.  The value must already be a candidate.
    -- This should only be called once per instance.  If it is called more than once,
    -- or if it is called after receiving a proposal from our own party, only the first
    -- proposal is considered, and subsequent proposals are not broadcast.
    propose :: val -> m (),
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
-- to 'propose' or 'freezeMessage'.  The implementation should be re-entrant, in the sense
-- that 'awaitCandidate', 'broadcastFreezeMessage' and 'frozen' could call back 'propose'
-- or 'freezeMessage'.  However, it's generally considered that this should not happen.
newFreezeInstance :: forall val party m. (Ord val, Ord party, FreezeMonad val party m) => Int -> Int -> (party -> Int) -> party -> FreezeInstance val party m
newFreezeInstance totalWeight corruptWeight partyWeight me = FreezeInstance {..}
    where
        -- Add a proposal.
        addProposal party value = whenAddToSet party proposers $ do
            use (proposals . at value) >>= \case
                Nothing -> do
                    proposals . at value ?= (False, partyWeight party, Set.singleton party)
                    awaitCandidate value $ do
                        Just (False, weight, parties) <- use (proposals . at value)
                        proposals . at value ?= (True, weight, parties)
                        newTotalProposals <- totalProposals <%= (weight +)
                        when (newTotalProposals >= totalWeight - corruptWeight) doVote
                        when (weight >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                        currJProps <- distinctJustifiedProposals <%= (1+)
                        when (currJProps == 2) $ justifyVote Nothing
                Just (isCandidate, oldWeight, parties) -> do
                    let newWeight = partyWeight party + oldWeight
                    proposals . at value ?= (isCandidate, newWeight, Set.insert party parties)
                    when isCandidate $ do
                        newTotalProposals <- totalProposals <%= (partyWeight party +)
                        when (newTotalProposals > totalWeight - corruptWeight) doVote
                        when (newWeight >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
        justifyVote vote =
            use (votes . at vote) >>= \case
                Nothing -> votes . at vote ?= (True, 0, Set.empty)
                Just (False, weight, parties) -> do
                    votes . at vote ?= (True, weight, parties)
                    newTotalVotes <- totalVotes <%= (weight +)
                    when (newTotalVotes >= totalWeight - corruptWeight) doFinish
                Just (True, _, _) -> return () 
        addVote party vote = whenAddToSet party voters $ do
            use (votes . at vote) >>= \case
                Nothing -> 
                    votes . at vote ?= (False, partyWeight party, Set.singleton party)
                Just (False, weight, parties) ->
                    votes . at vote ?= (False, partyWeight party + weight, Set.insert party parties)
                Just (True, weight, parties) -> do
                    votes . at vote ?= (True, partyWeight party + weight, Set.insert party parties)
                    newTotalVotes <- totalVotes <%= (partyWeight party +)
                    when (newTotalVotes >= totalWeight - corruptWeight) doFinish
        determineVote [] = Nothing
        determineVote ((_, (False, _, _)) : rs) = determineVote rs
        determineVote ((val, (True, weight, _)) : rs) = if weight >= totalWeight - corruptWeight then Just val else determineVote rs
        doVote = do
            vtrs <- use voters
            unless (me `Set.member` vtrs) $ do
                vote <- determineVote . Map.toList <$> use proposals
                addVote me vote
                broadcastFreezeMessage (Vote vote)
        determineResult [] = Nothing
        determineResult ((_, (False, _, _)) : rs) = determineResult rs
        determineResult ((vote, (True, weight, _)) : rs) = if weight > corruptWeight then Just vote else determineResult rs
        doFinish = do
            -- Use Map.toDescList to ensure that Nothing is the least preferred outcome
            (determineResult . Map.toDescList <$> use votes) >>= \case
                Nothing -> return () -- "This should not happen."
                Just res -> frozen res -- We have determined an outcome.
        propose :: val -> m ()
        propose candidate = do
            pps <- use proposers
            unless (me `Set.member` pps) $ do
                addProposal me candidate
                broadcastFreezeMessage (Proposal candidate)
        freezeMessage :: party -> FreezeMessage val -> m ()
        freezeMessage party (Proposal val) = addProposal party val
        freezeMessage party (Vote vote) = addVote party vote
