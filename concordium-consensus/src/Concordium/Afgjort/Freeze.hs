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
    isProposalJustified,
    receiveFreezeMessage,
    FreezeSummary,
    freezeSummary
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.Afgjort.Types
import Concordium.Afgjort.PartySet(PartySet)
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.PartyMap(PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM


-- |Freeze protocol messages
data FreezeMessage
    = Proposal Val      -- ^Propose a value
    | Vote (Maybe Val)  -- ^Vote for a value (or nothing)
    deriving (Eq, Ord, Show)

-- |The state of the Freeze protocol.
data FreezeState = FreezeState {
    -- |The proposals.  Each proposal is associated with a pair consisting of:
    -- a @Bool@ indicating whether the proposed candidate is justified; and
    -- the parties that have made the proposal with their signatures.
    -- Note that each party can have more than one justified proposal.
    _proposals :: Map Val (Bool, PartyMap Signature),
    -- |The set of parties that have submitted justified proposals.
    _justifiedProposers :: PartySet,
    -- |The number of distinct justified proposals.  This is used to determine
    -- when a vote for no value is justified.
    _distinctJustifiedProposals :: Int,
    -- |The votes.  Each vote is associated with a pair consisting of:
    -- a @Bool@ indicating whether the vote is justified; and
    -- the parties that have voted for the value with their signatures.
    -- Like proposals, we only consider the first vote for a given party.
    _votes :: Map (Maybe Val) (Bool, PartyMap Signature),
    -- |The set of parties from which justified votes have been received.
    _justifiedVoters :: PartySet,
    -- |The set of decisions that have become justified.
    _justifiedDecisions :: Set (Maybe Val),
    -- |Whether we have output a justifed decision already.
    _completed :: Bool
} deriving (Eq, Ord, Show)
makeLenses ''FreezeState

-- |The initial state of the Freeze protocol.
initialFreezeState :: FreezeState
initialFreezeState = FreezeState Map.empty PS.empty 0 Map.empty PS.empty Set.empty False

-- |The context for running the Freeze protocol, which includes
--
-- * The total weight of all parties participating in the protocol
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * My party
data FreezeInstance = FreezeInstance {
    totalWeight :: VoterPower,
    corruptWeight :: VoterPower,
    partyWeight :: Party -> VoterPower,
    me :: Party
}

-- | 'FreezeMonad' is implemented by a client that wishes to use the Freeze protocol
class (MonadReader FreezeInstance m, MonadState FreezeState m) => FreezeMonad m where
    -- |Broadcast a 'FreezeMessage' to all parties.  It is not necessary for the
    -- messages to be sent back to the 'FreezeInstance', but should not be harmful
    -- either.
    sendFreezeMessage :: FreezeMessage -> m ()
    -- |Output the decision from running the freeze protocol.
    frozen :: Maybe Val -> m ()
    -- |Notify that a decision from the freeze protocol has become justified.
    decisionJustified :: Maybe Val -> m ()


data FreezeOutputEvent
    = SendFreezeMessage FreezeMessage
    | Frozen (Maybe Val)
    | DecisionJustified (Maybe Val)

newtype Freeze a = Freeze {
    runFreeze' :: RWS FreezeInstance (Endo [FreezeOutputEvent]) FreezeState a
} deriving (Functor, Applicative, Monad)

runFreeze :: Freeze a -> FreezeInstance -> FreezeState -> (a, FreezeState, [FreezeOutputEvent])
runFreeze z i s = runRWS (runFreeze' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadReader FreezeInstance Freeze where
    ask = Freeze ask
    reader = Freeze . reader
    local f = Freeze . local f . runFreeze'

instance MonadState FreezeState Freeze where
    get = Freeze get
    put = Freeze . put
    state = Freeze . state

instance FreezeMonad Freeze where
    sendFreezeMessage = Freeze . tell . Endo . (:) . SendFreezeMessage
    frozen = Freeze . tell . Endo . (:) . Frozen
    decisionJustified = Freeze . tell . Endo . (:) . DecisionJustified

whenAddToSet :: (Ord v, MonadState s m) => v -> Lens' s (Set v) -> m () -> m ()
whenAddToSet val setLens act = do
    theSet <- use setLens
    unless (val `Set.member` theSet) $ do
        setLens .= Set.insert val theSet
        act

addProposal :: (FreezeMonad m) => Party -> Val -> m ()
addProposal party value = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $
        use (proposals . at value) >>= \case
            Nothing -> proposals . at value ?= (False, PM.singleton party (partyWeight party) undefined)
            Just (isCandidate, parties) -> unless (party `PM.member` parties) $ do
                let newParties = PM.insert party (partyWeight party) undefined parties
                proposals . at value ?= (isCandidate, newParties)
                when isCandidate $ do
                    newJustifiedProposers <- justifiedProposers <%= PS.insert party (partyWeight party)
                    currJProps <- if PM.weight parties == 0 then distinctJustifiedProposals <%= (1+) else use distinctJustifiedProposals
                    when (currJProps >= 2 && PS.size newJustifiedProposers >= 2) $ justifyVote Nothing
                    when (PM.weight newParties >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                    when (PS.weight newJustifiedProposers >= totalWeight - corruptWeight) doVote

justifyVote :: (FreezeMonad m) => Maybe Val -> m ()
justifyVote vote = do
    FreezeInstance{..} <- ask
    use (votes . at vote) >>= \case
        Nothing -> votes . at vote ?= (True, PM.empty) -- Record that the vote is justified
        Just (False, parties) -> do
            -- Record that the vote is justified
            votes . at vote ?= (True, parties)
            -- Add newly justified voters to the set of justified voters
            newJustifiedVoters <- justifiedVoters <%= PS.union partyWeight (PM.keysSet parties)
            -- If the weight of the parties for this vote exceeds the corrupt weight,
            -- then it is justified as a decision.
            when (PM.weight parties > corruptWeight) $ justifyDecision vote
            -- If the total weight of voters is sufficient, then we can output our decision
            when (PS.weight newJustifiedVoters >= totalWeight - corruptWeight) doFinish
        Just (True, _) -> return () -- Vote already justified, so nothing to do

addVote :: (FreezeMonad m) => Party -> Maybe Val -> m ()
addVote party vote = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $
        use (votes . at vote) >>= \case
            Nothing -> 
                votes . at vote ?= (False, PM.singleton party (partyWeight party) undefined)
            Just (False, parties) ->
                votes . at vote ?= (False, PM.insert party (partyWeight party) undefined parties)
            Just (True, parties) -> do
                let newParties = PM.insert party (partyWeight party) undefined parties
                votes . at vote ?= (True, newParties)
                newJustifiedVoters <- justifiedVoters <%= PS.insert party (partyWeight party)
                -- Record when a decision becomes justified
                when (PM.weight newParties > corruptWeight) $ justifyDecision vote
                when (PS.weight newJustifiedVoters >= totalWeight - corruptWeight) doFinish

justifyDecision :: (FreezeMonad m) => Maybe Val -> m ()
justifyDecision decision = whenAddToSet decision justifiedDecisions $ decisionJustified decision

doVote :: forall m. (FreezeMonad m) => m ()
doVote = do
        FreezeInstance{..} <- ask
        vtrs <- use justifiedVoters
        unless (me `PS.member` vtrs) $ do
            let
                determineVote [] = Nothing
                determineVote ((_, (False, _)) : rs) = determineVote rs
                determineVote ((val, (True, parties)) : rs) = if PM.weight parties >= totalWeight - corruptWeight then Just val else determineVote rs
            vote <- determineVote . Map.toList <$> use proposals
            addVote me vote
            sendFreezeMessage (Vote vote)

doFinish :: (FreezeMonad m) => m ()
doFinish = do
    alreadyCompleted <- completed <<.= True
    unless alreadyCompleted $
        (Set.toDescList <$> use justifiedDecisions) >>= \case
            [] -> completed .= False -- If this occurs, there's too much corruption.
            (res : _) -> frozen res -- Take the first justified decision. Using toDescList ensures that Nothing is the least preferred decision.
                                
-- |Propose a candidate value.  The value must already be a candidate (i.e. 
-- 'justifyCandidate' should have been called for this value).
-- This should only be called once per instance.  If it is called more than once,
-- or if it is called after receiving a proposal from our own party, only the first
-- proposal is considered, and subsequent proposals are not broadcast.
propose :: (FreezeMonad m) => Val -> m ()
{-# SPECIALIZE propose :: Val -> Freeze () #-}
propose candidate = do
        FreezeInstance{..} <- ask
        pps <- use justifiedProposers
        unless (me `PS.member` pps) $ do
            addProposal me candidate
            sendFreezeMessage (Proposal candidate)

-- |Justify a value as a candidate.
justifyCandidate :: (FreezeMonad m) => Val -> m ()
{-# SPECIALIZE justifyCandidate :: Val -> Freeze () #-}
justifyCandidate value = do
    FreezeInstance{..} <- ask
    use (proposals . at value) >>= \case
        Nothing -> proposals . at value ?= (True, PM.empty)
        Just (False, parties) -> do
            proposals . at value ?= (True, parties)
            when (PM.weight parties > 0) $ do
                newJustifiedProposers <- justifiedProposers <%= PS.union partyWeight (PM.keysSet parties)
                currJProps <- distinctJustifiedProposals <%= (1+)
                when (PM.weight parties >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                when (currJProps >= 2 && PS.size newJustifiedProposers >= 2) $ justifyVote Nothing
                when (PS.weight newJustifiedProposers >= totalWeight - corruptWeight) doVote
        Just (True, _) -> return ()

-- |Check if a given value is a justified candidate.
isProposalJustified :: (FreezeMonad m) => Val -> m Bool
{-# SPECIALIZE isProposalJustified :: Val -> Freeze Bool #-}
isProposalJustified value = use (proposals . at value) >>= \case
        Just (True, _) -> return True
        _ -> return False

-- |Handle a Freeze message from the network
receiveFreezeMessage :: (FreezeMonad m) => Party -> FreezeMessage -> m ()
{-# SPECIALIZE receiveFreezeMessage :: Party -> FreezeMessage -> Freeze () #-}
receiveFreezeMessage party (Proposal val) = addProposal party val
receiveFreezeMessage party (Vote vote) = addVote party vote

data FreezeSummary = FreezeSummary {
    summaryProposalsVotes :: Map Val (PartyMap Signature, PartyMap Signature),
    summaryBotVotes :: PartyMap Signature
}

-- |Generate a summary of the justified freeze messages.
freezeSummary :: SimpleGetter FreezeState FreezeSummary
freezeSummary = to fs
    where
        fs FreezeState{..} = FreezeSummary{..}
            where
                summaryProposalsVotes = Map.mapMaybeWithKey makePV _proposals
                makePV _ (False, _) = Nothing
                makePV k (True, ps) = Just (ps, vs)
                    where
                        vs = case Map.lookup (Just k) _votes of
                            Just (True, pm) -> pm
                            _ -> PM.empty
                summaryBotVotes = case Map.lookup Nothing _votes of
                            Just (True, pm) -> pm
                            _ -> PM.empty