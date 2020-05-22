{-# LANGUAGE
    TemplateHaskell, 
    RankNTypes,
    BangPatterns,
    ScopedTypeVariables,
    GeneralizedNewtypeDeriving,
    ViewPatterns #-}
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
    freezeSummary,
    processFreezeSummary,
    freezeCompleted,
    putFreezeSummary,
    getFreezeSummary
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform
import qualified Data.Serialize as S
import Data.Word
import Control.Exception (assert)

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
data FreezeState sig = FreezeState {
    -- |The proposals.  Each proposal is associated with a pair consisting of:
    -- a @Bool@ indicating whether the proposed candidate is justified; and
    -- the parties that have made the proposal with their signatures.
    -- Note that each party can have more than one justified proposal.
    _proposals :: !(Map Val (Bool, PartyMap sig)),
    -- |The set of parties that have submitted justified proposals.
    _justifiedProposers :: !PartySet,
    -- |The number of distinct justified proposals.  This is used to determine
    -- when a vote for no value is justified.
    _distinctJustifiedProposals :: !Int,
    -- |The votes.  Each vote is associated with a pair consisting of:
    -- a @Bool@ indicating whether the vote is justified; and
    -- the parties that have voted for the value with their signatures.
    -- Like proposals, we only consider the first vote for a given party.
    _votes :: !(Map (Maybe Val) (Bool, PartyMap sig)),
    -- |The set of parties from which justified votes have been received.
    _justifiedVoters :: !PartySet,
    -- |The set of decisions that have become justified.
    _justifiedDecisions :: !(Set (Maybe Val)),
    -- |Whether we have output a justifed decision already.
    _completed :: !Bool
} deriving (Eq, Ord, Show)
makeLenses ''FreezeState

-- |The initial state of the Freeze protocol.
initialFreezeState :: FreezeState sig
initialFreezeState = FreezeState Map.empty PS.empty 0 Map.empty PS.empty Set.empty False

-- |Whether the Freeze protocol has produced a justified decision.
freezeCompleted :: SimpleGetter (FreezeState sig) Bool
freezeCompleted = to _completed

-- |The context for running the Freeze protocol, which includes
--
-- * The total weight of all parties participating in the protocol
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * My party
data FreezeInstance = FreezeInstance {
    totalWeight :: !VoterPower,
    corruptWeight :: !VoterPower,
    partyWeight :: Party -> VoterPower,
    me :: !Party
}

-- | 'FreezeMonad' is implemented by a client that wishes to use the Freeze protocol
class (MonadReader FreezeInstance m, MonadState (FreezeState sig) m) => FreezeMonad sig m where
    -- |Broadcast a 'FreezeMessage' to all parties.  It is assumed that the message
    -- will be looped back.
    sendFreezeMessage :: FreezeMessage -> m ()
    -- |Output the decision from running the freeze protocol.
    frozen :: Maybe Val -> m ()
    -- |Notify that a decision from the freeze protocol has become justified.
    decisionJustified :: Maybe Val -> m ()


data FreezeOutputEvent
    = SendFreezeMessage !FreezeMessage
    | Frozen !(Maybe Val)
    | DecisionJustified !(Maybe Val)

newtype Freeze sig a = Freeze {
    runFreeze' :: RWS FreezeInstance (Endo [FreezeOutputEvent]) (FreezeState sig) a
} deriving (Functor, Applicative, Monad, MonadReader FreezeInstance, MonadState (FreezeState sig))

runFreeze :: Freeze sig a -> FreezeInstance -> FreezeState sig -> (a, FreezeState sig, [FreezeOutputEvent])
runFreeze z i s = runRWS (runFreeze' z) i s & _3 %~ (\(Endo f) -> f [])

instance FreezeMonad sig (Freeze sig) where
    sendFreezeMessage !msg = Freeze . tell . Endo . (:) . SendFreezeMessage $ msg
    frozen !mv = Freeze . tell . Endo . (:) . Frozen $ mv
    decisionJustified !dec = Freeze . tell . Endo . (:) . DecisionJustified $ dec

addProposal :: (FreezeMonad sig m) => Party -> Val -> sig -> m ()
addProposal party value sig = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $
        use (proposals . at value) >>= \case
            Nothing -> proposals . at value ?= (False, PM.singleton party (partyWeight party) sig)
            Just (isCandidate, parties) -> unless (party `PM.member` parties) $ do
                let newParties = PM.insert party (partyWeight party) sig parties
                proposals . at value ?= (isCandidate, newParties)
                when isCandidate $ do
                    newJustifiedProposers <- justifiedProposers <%= PS.insert party (partyWeight party)
                    currJProps <- if PM.weight parties == 0 then distinctJustifiedProposals <%= (1+) else use distinctJustifiedProposals
                    when (currJProps >= 2 && PS.size newJustifiedProposers >= 2) $ justifyVote Nothing
                    when (PM.weight newParties >= totalWeight - 2 * corruptWeight) $ justifyVote (Just value)
                    when (PS.weight newJustifiedProposers >= totalWeight - corruptWeight) doVote

justifyVote :: (FreezeMonad sig m) => Maybe Val -> m ()
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

addVote :: (FreezeMonad sig m) => Party -> Maybe Val -> sig -> m ()
addVote party vote sig = do
    FreezeInstance{..} <- ask
    when (partyWeight party > 0) $
        use (votes . at vote) >>= \case
            Nothing -> 
                votes . at vote ?= (False, PM.singleton party (partyWeight party) sig)
            Just (False, parties) ->
                votes . at vote ?= (False, PM.insert party (partyWeight party) sig parties)
            Just (True, parties) -> do
                let newParties = PM.insert party (partyWeight party) sig parties
                votes . at vote ?= (True, newParties)
                newJustifiedVoters <- justifiedVoters <%= PS.insert party (partyWeight party)
                -- Record when a decision becomes justified
                when (PM.weight newParties > corruptWeight) $ justifyDecision vote
                when (PS.weight newJustifiedVoters >= totalWeight - corruptWeight) doFinish

whenAddToSet :: (Ord v, MonadState s m) => v -> Lens' s (Set v) -> m () -> m ()
whenAddToSet val setLens act = do
    theSet <- use setLens
    unless (val `Set.member` theSet) $ do
        setLens .= Set.insert val theSet
        act

justifyDecision :: (FreezeMonad sig m) => Maybe Val -> m ()
justifyDecision decision = whenAddToSet decision justifiedDecisions $ decisionJustified decision

doVote :: forall m sig. (FreezeMonad sig m) => m ()
doVote = do
        FreezeInstance{..} <- ask
        vtrs <- use justifiedVoters
        unless (me `PS.member` vtrs) $ do
            let
                determineVote [] = Nothing
                determineVote ((_, (False, _)) : rs) = determineVote rs
                determineVote ((val, (True, parties)) : rs) = if PM.weight parties >= totalWeight - corruptWeight then Just val else determineVote rs
            vote <- determineVote . Map.toList <$> use proposals
            -- addVote me vote
            sendFreezeMessage (Vote vote)

doFinish :: (FreezeMonad sig m) => m ()
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
propose :: (FreezeMonad sig m) => Val -> m ()
{-# SPECIALIZE propose :: Val -> Freeze sig () #-}
propose candidate = do
        FreezeInstance{..} <- ask
        pps <- use justifiedProposers
        unless (me `PS.member` pps) $
            sendFreezeMessage (Proposal candidate)

-- |Justify a value as a candidate.
justifyCandidate :: (FreezeMonad sig m) => Val -> m ()
{-# SPECIALIZE justifyCandidate :: Val -> Freeze sig () #-}
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
isProposalJustified :: (FreezeMonad sig m) => Val -> m Bool
{-# SPECIALIZE isProposalJustified :: Val -> Freeze sig Bool #-}
isProposalJustified value = use (proposals . at value) >>= \case
        Just (True, _) -> return True
        _ -> return False

-- |Handle a Freeze message from the network
receiveFreezeMessage :: (FreezeMonad sig m) => Party -> FreezeMessage -> sig -> m ()
{-# SPECIALIZE receiveFreezeMessage :: Party -> FreezeMessage -> sig -> Freeze sig () #-}
{-# SPECIALIZE receiveFreezeMessage :: Party -> FreezeMessage -> Signature -> Freeze Signature () #-}
receiveFreezeMessage party (Proposal val) sig = addProposal party val sig
receiveFreezeMessage party (Vote vote) sig = addVote party vote sig

data FreezeSummary sig = FreezeSummary {
    summaryProposalsVotes :: Map Val (Map Party sig, Map Party sig),
    summaryBotVotes :: Map Party sig
}

-- |Generate a summary of the justified freeze messages.
freezeSummary :: SimpleGetter (FreezeState sig) (FreezeSummary sig)
freezeSummary = to fs
    where
        fs FreezeState{..} = FreezeSummary{..}
            where
                summaryProposalsVotes = Map.mapMaybeWithKey makePV _proposals
                makePV _ (False, _) = Nothing
                makePV k (True, ps) = if PM.weight ps == 0 then Nothing else Just (PM.partyMap ps, vs (Just k))
                summaryBotVotes = vs Nothing
                vs v = case Map.lookup v _votes of
                    Just (True, pm) -> PM.partyMap pm
                    _ -> Map.empty

-- |Process a freeze summary, handling the new messages, and determining when we have more to offer.
processFreezeSummary :: (FreezeMonad sig m, Eq sig) => FreezeSummary sig -> (Party -> FreezeMessage -> sig -> Bool) -> m CatchUpResult
processFreezeSummary FreezeSummary{..} checkSig = do
        r1 <- mconcat <$> forM (Map.toList summaryProposalsVotes) (\(val, (props, vts)) -> do
            let checkProposalSig party sig = checkSig party (Proposal val) sig
            r1props <- if null props then
                    return mempty
                else
                    use (proposals . at val) >>= \case
                        Just (b, PM.partyMap -> myProps) -> do
                            -- Take the proposals we haven't already seen and that have valid signatures
                            let newProposals = Map.filterWithKey checkProposalSig $ Map.difference props myProps
                            -- Add these
                            forM_ (Map.toList newProposals) $ \(party, sig) -> addProposal party val sig
                            return $! CatchUpResult {
                                    -- It's behind if it is already justified and we have some valid signatures that it is missing
                                    curBehind = b && not (null (Map.differenceWithKey (\party x y -> if x == y || checkProposalSig party y then Nothing else Just x) myProps props)),
                                    -- We need to catch up if the choice is not already justified for us
                                    curSkovCatchUp = not b
                                    }
                        Nothing -> do
                            -- Filter to those proposals with valid signatures
                            let newProposals = Map.filterWithKey checkProposalSig props
                            -- Add these
                            forM_ (Map.toList newProposals) $ \(party, sig) -> addProposal party val sig
                            return $! CatchUpResult {curBehind = False, curSkovCatchUp = not (null newProposals)}
            r1votes <- doVotes (Just val) vts
            return (r1props <> r1votes))
        r2 <- doVotes Nothing summaryBotVotes
        return (r1 <> r2)
    where
        doVotes jval vts = if null vts then return mempty else
            use (votes . at jval) >>= \case
                Just (b, PM.partyMap -> myVotes) -> do
                    let newVotes = Map.filterWithKey checkVoteSig $ Map.difference vts myVotes
                    forM_ (Map.toList newVotes) addVoteSig
                    return $! CatchUpResult {
                        curBehind = b && not (null (Map.differenceWithKey (\party x y -> if x == y || checkVoteSig party y then Nothing else Just x) myVotes vts)),
                        curSkovCatchUp = not b
                    }
                Nothing -> do
                    let newVotes = Map.filterWithKey checkVoteSig vts
                    forM_ (Map.toList newVotes) addVoteSig
                    return $! CatchUpResult {curBehind = False, curSkovCatchUp = not (null newVotes)}
            where
                checkVoteSig party sig = checkSig party (Vote jval) sig
                addVoteSig (party, sig) = addVote party jval sig

putFreezeSummary :: (S.Serialize sig) => Party -> FreezeSummary sig -> S.Put
putFreezeSummary maxParty FreezeSummary{..} = do
        assert (Map.size summaryProposalsVotes <= fromIntegral (maxBound :: Word16)) $
            S.putWord16be (fromIntegral $ Map.size summaryProposalsVotes)
        forM_ (Map.toList summaryProposalsVotes) $ \(v, (props, vts)) -> do
            S.put v
            putPartyMap maxParty props
            putPartyMap maxParty vts
        putPartyMap maxParty summaryBotVotes

getFreezeSummary :: (S.Serialize sig) => Party -> S.Get (FreezeSummary sig)
getFreezeSummary maxParty = do
        pvcount <- S.getWord16be
        summaryProposalsVotes <- Map.fromList <$> forM [1..pvcount] (\_ -> do
            v <- S.get
            props <- getPartyMap maxParty
            vts <- getPartyMap maxParty
            return (v, (props, vts)))
        summaryBotVotes <- getPartyMap maxParty
        return $! FreezeSummary{..}
