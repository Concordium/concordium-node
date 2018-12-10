{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module ConcordiumTests.Afgjort.Freeze where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Concordium.Afgjort.Freeze
import Control.Monad.RWS
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Lens.Micro.Platform

-- |An invariant predicate over 'FreezeState's.
invariantFreezeState :: (Ord val, Ord party) => Int -> Int -> (party -> Int) -> FreezeState val party -> Bool
invariantFreezeState totalWeight corruptWeight partyWeight fs =
    allProps <= totalWeight &&
    justProps == _totalProposals fs &&
    pers == _proposers fs &&
    propOK &&
    distJProps == _distinctJustifiedProposals fs &&
    allVotes <= totalWeight &&
    justVotes == _totalVotes fs &&
    vers == _voters fs &&
    voteOK
    where
        propAcc (aps, jps, prs, djps, po) v (vd, wt, pts) = (aps + wt, if vd then jps + wt else jps, prs `Set.union` pts, if vd then djps + 1 else djps,
                                                        wt == sum (partyWeight <$> Set.toList pts) && Set.null (prs Set.\\ pts) && po)
        (allProps, justProps, pers, distJProps, propOK) = Map.foldlWithKey propAcc (0, 0, Set.empty, 0, True) (_proposals fs)
        voteAcc (avs, jvs, vrs, vo) v (vd, wt, pts) = (avs + wt, if vd then jvs + wt else jvs, vrs `Set.union` pts,
                                                        wt == sum (partyWeight <$> Set.toList pts) && Set.null (vrs Set.\\ pts) 
                                                        && (vd == voteJustified v) && vo)
        voteJustified Nothing = distJProps >= 2
        voteJustified (Just v) = case Map.lookup v (_proposals fs) of
            Nothing -> False
            (Just (j, wt, _)) -> j && wt >= totalWeight - 2 * corruptWeight
        (allVotes, justVotes, vers, voteOK) = Map.foldlWithKey voteAcc (0, 0, Set.empty, True) (_votes fs)

data FreezeInput party val
    = FICandiate val
    | FIProposal party val
    | FIVote party (Maybe val)

data FreezeOutput val
    = FOMessage (FreezeMessage val)
    | FOComplete (Maybe val)

newtype FreezeT val party m a = FreezeT {
    runFreezeT :: RWST () [FreezeOutput val] (Map.Map val (Maybe (FreezeT val party m ())), FreezeState val party) m a
} deriving (Functor, Applicative, Monad)

instance (Monad m) => (MonadState (FreezeState val party) (FreezeT val party m)) where
    get = snd <$> FreezeT get
    put v = FreezeT (state (\(a, _) -> ((), (a,v))))
    state f = FreezeT (state (\(a, v) -> let (r, v') = f v in (r, (a, v'))))

instance (Monad m, Ord val) => FreezeMonad val party (FreezeT val party m) where
    awaitCandidate v x = FreezeT $ do
        candWait <- use (_1 . at v)
        case candWait of
            Nothing -> (_1 . at v) ?= Just x
            (Just Nothing) -> runFreezeT x
            (Just (Just a)) -> (_1 . at v) ?= Just (a >> x)
    broadcastFreezeMessage m = FreezeT (tell [FOMessage m])
    frozen v = FreezeT (tell [FOComplete v])