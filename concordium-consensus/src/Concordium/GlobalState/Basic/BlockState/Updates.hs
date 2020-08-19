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
    _pElectionDifficultyQueue :: !(UpdateQueue ElectionDifficulty),
    _pEuroPerEnergyQueue :: !(UpdateQueue ExchangeRate),
    _pMicroGTUPerEuroQueue :: !(UpdateQueue ExchangeRate)
} deriving (Show)
makeLenses ''PendingUpdates

emptyPendingUpdates :: PendingUpdates
emptyPendingUpdates = PendingUpdates emptyUpdateQueue emptyUpdateQueue emptyUpdateQueue emptyUpdateQueue emptyUpdateQueue

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

-- |Process the update queue to determine the new value of a parameter (or the authorizations).
-- This splits the queue at the given timestamp. The last value up to and including the timestamp
-- is the new value, if any -- otherwise the current value is retained. The queue is updated to
-- be the updates with later timestamps.
processValueUpdates ::
    Timestamp
    -- ^Current timestamp
    -> v
    -- ^Current value
    -> UpdateQueue v
    -- ^Current queue
    -> (v, UpdateQueue v)
processValueUpdates t a0 uq = (getLast (sconcat (Last <$> a0 :| (snd <$> ql))), uq {_uqQueue = qr})
    where
        (ql, qr) = span ((<= t) . fst) (uq ^. uqQueue)

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
-- FIXME: We may just want to keep unused protocol updates in the queue, even if their timestamps have
-- elapsed.
processProtocolUpdates :: Timestamp -> Maybe ProtocolUpdate -> UpdateQueue ProtocolUpdate -> (Maybe ProtocolUpdate, UpdateQueue ProtocolUpdate)
processProtocolUpdates t pu0 uq = (pu', uq {_uqQueue = qr})
    where
        (ql, qr) = span ((<= t) . fst) (uq ^. uqQueue)
        pu' = getFirst <$> mconcat ((First <$> pu0) : (Just . First . snd <$> ql))


-- |Process the update queues to determine the current state of
-- updates.
processUpdateQueues
    :: Timestamp
    -- ^Current timestamp
    -> Updates
    -> Updates
processUpdateQueues t Updates{_pendingUpdates = PendingUpdates{..},_currentParameters = ChainParameters{..}, ..} = Updates {
            _currentAuthorizations = newAuthorizations,
            _currentProtocolUpdate = newProtocolUpdate,
            _currentParameters = makeChainParameters newElectionDifficulty newEuroPerEnergy newMicroGTUPerEuro,
            _pendingUpdates = PendingUpdates {
                    _pAuthorizationQueue = newAuthorizationQueue,
                    _pProtocolQueue = newProtocolQueue,
                    _pElectionDifficultyQueue = newElectionDifficultyQueue,
                    _pEuroPerEnergyQueue = newEuroPerEnergyQueue,
                    _pMicroGTUPerEuroQueue = newMicroGTUPerEuroQueue
                }
        }
    where
        (newAuthorizations, newAuthorizationQueue) = processValueUpdates t _currentAuthorizations _pAuthorizationQueue
        (newProtocolUpdate, newProtocolQueue) = processProtocolUpdates t _currentProtocolUpdate _pProtocolQueue
        (newElectionDifficulty, newElectionDifficultyQueue) = processValueUpdates t _cpElectionDifficulty _pElectionDifficultyQueue
        (newEuroPerEnergy, newEuroPerEnergyQueue) = processValueUpdates t _cpEuroPerEnergy _pEuroPerEnergyQueue
        (newMicroGTUPerEuro, newMicroGTUPerEuroQueue) = processValueUpdates t _cpMicroGTUPerEuro _pMicroGTUPerEuroQueue

futureElectionDifficulty :: Updates -> Timestamp -> ElectionDifficulty
futureElectionDifficulty Updates{_pendingUpdates = PendingUpdates{..},..} ts
        = fst (processValueUpdates ts (_cpElectionDifficulty _currentParameters) _pElectionDifficultyQueue)

lookupNextUpdateSequenceNumber :: Updates -> UpdateType -> UpdateSequenceNumber
lookupNextUpdateSequenceNumber u UpdateAuthorization = u ^. pendingUpdates . pAuthorizationQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateProtocol = u ^. pendingUpdates . pProtocolQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateParameters = u ^. pendingUpdates . pParameterQueue . uqNextSequenceNumber
