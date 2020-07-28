{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module Concordium.GlobalState.Basic.BlockState.Updates where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Updates

import Concordium.GlobalState.Updates

data UpdateQueue e = UpdateQueue {
    -- |The next available sequence number for an update.
    _uqNextSequenceNumber :: !UpdateSequenceNumber,
    -- |Pending updates, in ascending order of timestamp.
    _uqQueue :: ![(Timestamp, e)]
}
makeLenses ''UpdateQueue

-- |Try to add an update event to an update queue. Fails if the sequence number
-- is not correct.
checkEnqueue :: UpdateSequenceNumber -> Timestamp -> e -> UpdateQueue e -> Maybe (UpdateQueue e)
checkEnqueue sn !t !e UpdateQueue{..}
    | sn == _uqNextSequenceNumber = Just (UpdateQueue {
            _uqNextSequenceNumber = sn + 1,
            _uqQueue = let !r = dropWhile ((<= t) . fst) _uqQueue in (t, e) : r
        })
    | otherwise = Nothing

data PendingUpdates = PendingUpdates {
    _pAuthorizationQueue :: !(UpdateQueue Authorizations),
    _pProtocolQueue :: !(UpdateQueue ProtocolUpdate),
    _pParameterQueue :: !(UpdateQueue ParameterUpdate)
}
makeLenses ''PendingUpdates

data ChainParameters = ChainParameters {
    -- |Election difficulty parameter.
    _cpElectionDifficulty :: !ElectionDifficulty,
    -- |Euro:Energy rate.
    _cpEuroPerEnergy :: !ExchangeRate,
    -- |uGTU:Euro rate.
    _cpMicroGTUPerEuro :: !ExchangeRate,
    -- |uGTU:Energy rate.
    -- This is derived, but will be computed when the other
    -- rates are updated since it is more useful.
    _cpEnergyRate :: !EnergyRate
}

data Updates = Updates {
    _currentAuthorizations :: !Authorizations,
    _currentProtocolUpdate :: !(Maybe ProtocolUpdate),
    _currentParameters :: !ChainParameters,
    _pendingUpdates :: !PendingUpdates
}
makeClassy ''Updates

{-
getUpdateAuthorization :: (HasUpdates u) => UpdateType -> u -> AccessStructure
getUpdateAuthorization UpdateAuthorization u = u ^. currentAuthorizations . to asAuthorization
getUpdateAuthorization UpdateProtocol u = u ^. currentAuthorizations . to asProtocol
getUpdateAuthorization UpdateParameters u = u ^. currentAuthorizations . to asParameters
getUpdateAuthorization UpdateExchangeRate u = u ^. currentAuthorizations . to asExchangeRate
-}

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
        applyPU cp@ChainParameters{..} ParameterUpdate{..} = cp {
                _cpElectionDifficulty = fromMaybe _cpElectionDifficulty puElectionDifficulty,
                _cpEuroPerEnergy = fromMaybe _cpEuroPerEnergy puEuroPerEnergy,
                _cpEnergyRate = maybe _cpEnergyRate (computeEnergyRate _cpMicroGTUPerEuro) puEuroPerEnergy
            }

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

