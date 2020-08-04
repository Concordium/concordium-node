{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module Concordium.GlobalState.Basic.BlockState.Updates where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.Parameters

data UpdateQueue e = UpdateQueue {
    -- |The next available sequence number for an update.
    _uqNextSequenceNumber :: !UpdateSequenceNumber,
    -- |Pending updates, in ascending order of timestamp.
    _uqQueue :: ![(Timestamp, e)]
} deriving (Show)
makeLenses ''UpdateQueue

emptyUpdateQueue :: UpdateQueue e
emptyUpdateQueue = UpdateQueue {
        _uqNextSequenceNumber = minUpdateSequenceNumber,
        _uqQueue = []
    }

-- |Try to add an update event to an update queue. Fails if the sequence number
-- is not correct.
checkEnqueue :: UpdateSequenceNumber -> Timestamp -> e -> UpdateQueue e -> Maybe (UpdateQueue e)
checkEnqueue sn !t !e UpdateQueue{..}
    | sn == _uqNextSequenceNumber = Just (UpdateQueue {
            _uqNextSequenceNumber = sn + 1,
            _uqQueue = let !r = takeWhile ((< t) . fst) _uqQueue in r ++ [(t, e)]
        })
    | otherwise = Nothing

data PendingUpdates = PendingUpdates {
    _pAuthorizationQueue :: !(UpdateQueue Authorizations),
    _pProtocolQueue :: !(UpdateQueue ProtocolUpdate),
    _pParameterQueue :: !(UpdateQueue ParameterUpdate)
} deriving (Show)
makeLenses ''PendingUpdates

emptyPendingUpdates :: PendingUpdates
emptyPendingUpdates = PendingUpdates emptyUpdateQueue emptyUpdateQueue emptyUpdateQueue

data Updates = Updates {
    _currentAuthorizations :: !Authorizations,
    _currentProtocolUpdate :: !(Maybe ProtocolUpdate),
    _currentParameters :: !ChainParameters,
    _pendingUpdates :: !PendingUpdates
} deriving (Show)
makeClassy ''Updates

initialUpdates :: Authorizations -> ChainParameters -> Updates
initialUpdates _currentAuthorizations _currentParameters = Updates {
        _currentProtocolUpdate = Nothing,
        _pendingUpdates = emptyPendingUpdates,
        ..
    }

processAuthorizationUpdates :: Timestamp -> Authorizations -> UpdateQueue Authorizations -> (Authorizations, UpdateQueue Authorizations)
processAuthorizationUpdates t a0 uq = (getLast (sconcat (Last <$> a0 :| (snd <$> ql))), uq {_uqQueue = qr})
    where
        (ql, qr) = span ((<= t) . fst) (uq ^. uqQueue)

processProtocolUpdates :: Timestamp -> Maybe ProtocolUpdate -> UpdateQueue ProtocolUpdate -> (Maybe ProtocolUpdate, UpdateQueue ProtocolUpdate)
processProtocolUpdates t pu0 uq = (pu', uq {_uqQueue = qr})
    where
        (ql, qr) = span ((<= t) . fst) (uq ^. uqQueue)
        pu' = getFirst <$> mconcat ((First <$> pu0) : (Just . First . snd <$> ql))

processParameterUpdates :: Timestamp -> ChainParameters -> UpdateQueue ParameterUpdate -> (ChainParameters, UpdateQueue ParameterUpdate)
processParameterUpdates t cp0 uq = (cp', uq {_uqQueue = qr})
    where
        (ql, qr) = span ((<= t) . fst) (uq ^. uqQueue)
        cp' = foldl' applyPU cp0 (snd <$> ql)
        applyPU ChainParameters{..} ParameterUpdate{..} = makeChainParameters
                (fromMaybe _cpElectionDifficulty puElectionDifficulty)
                (fromMaybe _cpEuroPerEnergy puEuroPerEnergy)
                (fromMaybe _cpMicroGTUPerEuro puMicroGTUPerEuro)

-- |Process the update queues to determine the current state of
-- updates.
processUpdateQueues
    :: Timestamp
    -- ^Current timestamp
    -> Updates
    -> Updates
processUpdateQueues t Updates{_pendingUpdates = PendingUpdates{..},..} = Updates {
            _currentAuthorizations = newAuthorizations,
            _currentProtocolUpdate = newProtocolUpdate,
            _currentParameters = newParameters,
            _pendingUpdates = PendingUpdates {
                    _pAuthorizationQueue = newAuthorizationQueue,
                    _pProtocolQueue = newProtocolQueue,
                    _pParameterQueue = newParameterQueue
                }
        }
    where
        (newAuthorizations, newAuthorizationQueue) = processAuthorizationUpdates t _currentAuthorizations _pAuthorizationQueue
        (newProtocolUpdate, newProtocolQueue) = processProtocolUpdates t _currentProtocolUpdate _pProtocolQueue
        (newParameters, newParameterQueue) = processParameterUpdates t _currentParameters _pParameterQueue

futureElectionDifficulty :: Updates -> Timestamp -> ElectionDifficulty
futureElectionDifficulty Updates{_pendingUpdates = PendingUpdates{..},..} = fed (_cpElectionDifficulty _currentParameters) eds
    where
        eds = [(ts, ed) | (ts, ParameterUpdate{puElectionDifficulty = Just ed}) <- _uqQueue _pParameterQueue]
        fed ced [] _ = ced
        fed ced ((ts', ed) : r) ts
            | ts' <= ts = fed ed r ts
            | otherwise = ced