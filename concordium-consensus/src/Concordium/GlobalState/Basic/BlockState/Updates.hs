{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveFunctor, OverloadedStrings #-}
-- |Implementation of the chain update mechanism: https://concordium.gitlab.io/whitepapers/update-mechanism/main.pdf
module Concordium.GlobalState.Basic.BlockState.Updates where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import Data.Semigroup
import Lens.Micro.Platform

import Concordium.Types
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.Types.IdentityProviders as IPS
import Concordium.Types.Updates

import Concordium.GlobalState.Parameters
import Concordium.Types.UpdateQueues

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
    -> (v, UpdateQueue v, Map.Map TransactionTime v)
processValueUpdates t a0 uq = (getLast (sconcat (Last <$> a0 :| (snd <$> ql))), uq {_uqQueue = qr}, Map.fromAscList ql)
    where
        (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
-- FIXME: We may just want to keep unused protocol updates in the queue, even if their timestamps have
-- elapsed.
processProtocolUpdates ::
  Timestamp ->
  Maybe ProtocolUpdate ->
  UpdateQueue ProtocolUpdate ->
  (Maybe ProtocolUpdate, UpdateQueue ProtocolUpdate, Map.Map TransactionTime ProtocolUpdate)
processProtocolUpdates t pu0 uq = (pu', uq {_uqQueue = qr}, Map.fromAscList ql)
    where
        (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)
        pu' = getFirst <$> mconcat ((First <$> pu0) : (Just . First . snd <$> ql))


-- |Process AR and IP update queues.
-- Ensuring that new IPs/ARs have unique ids is difficult when enqueueing.
-- Instead, it is handled here by ignoring updates with duplicate IPs/ARs.
processARsAndIPsUpdates :: Ord k
                        => Map.Map k v -- ^ The existing IPs / ARs.
                        -> (v -> k) -- ^ Getter for the key field.
                        -> Timestamp
                        -> UpdateQueue v
                        -> (Map.Map k v, UpdateQueue v, Map.Map TransactionTime v)
processARsAndIPsUpdates oldValMap getKey t uq = (updatedValMap, uq {_uqQueue = qr}, changes)
  where (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)
        (changes, updatedValMap) = foldl' go (Map.empty, oldValMap) ql

        -- Adds non-duplicate updates to the map with IPs/ARs and accumulates the actual changes that occured.
        go (changesMap, valMap) (tt, v) =
          if Map.member k valMap
            then (changesMap, valMap) -- Ignore invalid update
            else (changesMap', valMap')
          where k = getKey v
                changesMap' = Map.insert tt v changesMap
                valMap' = Map.insert k v valMap

type UpdatesWithARsAndIPs = (Updates, ARS.AnonymityRevokers, IPS.IdentityProviders)

-- |Process the update queues to determine the current state of
-- updates.
processUpdateQueues
    :: Timestamp
    -- ^Current timestamp
    -> UpdatesWithARsAndIPs
    -> (Map.Map TransactionTime UpdateValue, UpdatesWithARsAndIPs)
processUpdateQueues t (theUpdates, ars, ips) =
  (res
  , ( Updates {
            _currentKeyCollection = makeHashed $ UpdateKeysCollection newRootKeys newLevel1Keys newLevel2Keys,
            _currentProtocolUpdate = newProtocolUpdate,
            _currentParameters = makeChainParameters
                        newElectionDifficulty
                        newEuroPerEnergy
                        newMicroGTUPerEuro
                        _cpBakerExtraCooldownEpochs
                        _cpAccountCreationLimit
                        RewardParameters {
                            _rpMintDistribution = newMintDistribution,
                            _rpTransactionFeeDistribution = newTransactionFeeDistribution,
                            _rpGASRewards = newGASRewards
                        }
                        newFoundationAccount
                        newBakerStakeThreshold,
            _pendingUpdates = PendingUpdates {
                    _pRootKeysUpdateQueue = newRootKeysQueue,
                    _pLevel1KeysUpdateQueue = newLevel1KeysQueue,
                    _pLevel2KeysUpdateQueue = newLevel2KeysQueue,
                    _pProtocolQueue = newProtocolQueue,
                    _pElectionDifficultyQueue = newElectionDifficultyQueue,
                    _pEuroPerEnergyQueue = newEuroPerEnergyQueue,
                    _pMicroGTUPerEuroQueue = newMicroGTUPerEuroQueue,
                    _pFoundationAccountQueue = newFoundationAccountQueue,
                    _pMintDistributionQueue = newMintDistributionQueue,
                    _pTransactionFeeDistributionQueue = newTransactionFeeDistributionQueue,
                    _pGASRewardsQueue = newGASRewardsQueue,
                    _pBakerStakeThresholdQueue = newBakerStakeThresholdQueue,
                    _pAddAnonymityRevokerQueue = newAddAnonymityRevokerQueue,
                    _pAddIdentityProviderQueue = newAddIdentityProviderQueue
                }
        }
    , ARS.AnonymityRevokers updatedARs
    , IPS.IdentityProviders updatedIPs))
    where
        Updates{_pendingUpdates = PendingUpdates{..}, _currentParameters = ChainParameters{_cpRewardParameters=RewardParameters{..}, ..}, ..} = theUpdates

        (newRootKeys, newRootKeysQueue, resRootKeys) = processValueUpdates t (rootKeys $ _unhashed _currentKeyCollection) _pRootKeysUpdateQueue
        (newLevel1Keys, newLevel1KeysQueue, resLevel1Keys) = processValueUpdates t (level1Keys $ _unhashed _currentKeyCollection) _pLevel1KeysUpdateQueue
        (newLevel2Keys, newLevel2KeysQueue, resLevel2Keys) = processValueUpdates t (level2Keys $ _unhashed _currentKeyCollection) _pLevel2KeysUpdateQueue
        (newProtocolUpdate, newProtocolQueue, resProtocol) = processProtocolUpdates t _currentProtocolUpdate _pProtocolQueue
        (newElectionDifficulty, newElectionDifficultyQueue, resElectionDifficulty) = processValueUpdates t _cpElectionDifficulty _pElectionDifficultyQueue
        (newEuroPerEnergy, newEuroPerEnergyQueue, resEuroPerEnergy) = processValueUpdates t _cpEuroPerEnergy _pEuroPerEnergyQueue
        (newMicroGTUPerEuro, newMicroGTUPerEuroQueue, resMicroGTUPerEuro) = processValueUpdates t _cpMicroGTUPerEuro _pMicroGTUPerEuroQueue
        (newFoundationAccount, newFoundationAccountQueue, resFoundationAccount) = processValueUpdates t _cpFoundationAccount _pFoundationAccountQueue
        (newMintDistribution, newMintDistributionQueue, resMintDistribution) = processValueUpdates t _rpMintDistribution _pMintDistributionQueue
        (newTransactionFeeDistribution, newTransactionFeeDistributionQueue, resTransactionFeeDistribution) = processValueUpdates t _rpTransactionFeeDistribution _pTransactionFeeDistributionQueue
        (newGASRewards, newGASRewardsQueue, resGASRewards) = processValueUpdates t _rpGASRewards _pGASRewardsQueue
        (newBakerStakeThreshold, newBakerStakeThresholdQueue, resBakerStakeThreshold) = processValueUpdates t _cpBakerStakeThreshold _pBakerStakeThresholdQueue
        (updatedARs, newAddAnonymityRevokerQueue, resAddAnonymityRevoker) = processARsAndIPsUpdates (ARS.arRevokers ars) ARS.arIdentity t _pAddAnonymityRevokerQueue
        (updatedIPs, newAddIdentityProviderQueue, resAddIdentityProvider) = processARsAndIPsUpdates (IPS.idProviders ips) IPS.ipIdentity t _pAddIdentityProviderQueue
        res =
            (UVRootKeys <$> resRootKeys) <>
            (UVLevel1Keys <$> resLevel1Keys) <>
            (UVLevel2Keys <$> resLevel2Keys) <>
            (UVProtocol <$> resProtocol) <>
            (UVElectionDifficulty <$> resElectionDifficulty) <>
            (UVEuroPerEnergy <$> resEuroPerEnergy) <>
            (UVMicroGTUPerEuro <$> resMicroGTUPerEuro) <>
            (UVFoundationAccount <$> resFoundationAccount) <>
            (UVMintDistribution <$> resMintDistribution) <>
            (UVTransactionFeeDistribution <$> resTransactionFeeDistribution) <>
            (UVGASRewards <$> resGASRewards) <>
            (UVBakerStakeThreshold <$> resBakerStakeThreshold) <>
            (UVAddAnonymityRevoker <$> resAddAnonymityRevoker) <>
            (UVAddIdentityProvider <$> resAddIdentityProvider)

-- |Determine the future election difficulty (at a given time) based
-- on a current 'Updates'.
futureElectionDifficulty :: Updates -> Timestamp -> ElectionDifficulty
futureElectionDifficulty Updates{_pendingUpdates = PendingUpdates{..},..} ts
        = processValueUpdates ts (_cpElectionDifficulty _currentParameters) _pElectionDifficultyQueue ^. _1

-- |Get the protocol update status: either an effective protocol update or
-- a list of pending future protocol updates.
protocolUpdateStatus :: Updates -> Either ProtocolUpdate [(TransactionTime, ProtocolUpdate)]
protocolUpdateStatus Updates{_pendingUpdates = PendingUpdates{..},..}
        = case _currentProtocolUpdate of
            Nothing -> Right (_uqQueue _pProtocolQueue)
            Just pu -> Left pu

-- |Determine the next sequence number for a given update type.
lookupNextUpdateSequenceNumber :: Updates -> UpdateType -> UpdateSequenceNumber
lookupNextUpdateSequenceNumber u UpdateProtocol = u ^. pendingUpdates . pProtocolQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateElectionDifficulty = u ^. pendingUpdates . pElectionDifficultyQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateEuroPerEnergy = u ^. pendingUpdates . pEuroPerEnergyQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateMicroGTUPerEuro = u ^. pendingUpdates . pMicroGTUPerEuroQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateFoundationAccount = u ^. pendingUpdates . pFoundationAccountQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateMintDistribution = u ^. pendingUpdates . pMintDistributionQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateTransactionFeeDistribution = u ^. pendingUpdates . pTransactionFeeDistributionQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateGASRewards = u ^. pendingUpdates . pGASRewardsQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateBakerStakeThreshold = u ^. pendingUpdates . pBakerStakeThresholdQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateAddAnonymityRevoker = u ^. pendingUpdates . pAddAnonymityRevokerQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateAddIdentityProvider = u ^. pendingUpdates . pAddIdentityProviderQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateRootKeys = u ^. pendingUpdates . pRootKeysUpdateQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateLevel1Keys = u ^. pendingUpdates . pLevel1KeysUpdateQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateLevel2Keys = u ^. pendingUpdates . pLevel2KeysUpdateQueue . uqNextSequenceNumber

-- |Enqueue an update in the appropriate queue.
enqueueUpdate :: TransactionTime -> UpdateValue -> Updates -> Updates
enqueueUpdate effectiveTime (UVRootKeys rk) = pendingUpdates . pRootKeysUpdateQueue %~ enqueue effectiveTime rk
enqueueUpdate effectiveTime (UVLevel1Keys l1k) = pendingUpdates . pLevel1KeysUpdateQueue %~ enqueue effectiveTime l1k
enqueueUpdate effectiveTime (UVLevel2Keys l2k) = pendingUpdates . pLevel2KeysUpdateQueue %~ enqueue effectiveTime l2k
enqueueUpdate effectiveTime (UVProtocol protUp) = pendingUpdates . pProtocolQueue %~ enqueue effectiveTime protUp
enqueueUpdate effectiveTime (UVElectionDifficulty edUp) = pendingUpdates . pElectionDifficultyQueue %~ enqueue effectiveTime edUp
enqueueUpdate effectiveTime (UVEuroPerEnergy epeUp) = pendingUpdates . pEuroPerEnergyQueue %~ enqueue effectiveTime epeUp
enqueueUpdate effectiveTime (UVMicroGTUPerEuro mgtupeUp) = pendingUpdates . pMicroGTUPerEuroQueue %~ enqueue effectiveTime mgtupeUp
enqueueUpdate effectiveTime (UVFoundationAccount upd) = pendingUpdates . pFoundationAccountQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVMintDistribution upd) = pendingUpdates . pMintDistributionQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVTransactionFeeDistribution upd) = pendingUpdates . pTransactionFeeDistributionQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVGASRewards upd) = pendingUpdates . pGASRewardsQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVBakerStakeThreshold upd) = pendingUpdates . pBakerStakeThresholdQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVAddAnonymityRevoker upd) = pendingUpdates . pAddAnonymityRevokerQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVAddIdentityProvider upd) = pendingUpdates . pAddIdentityProviderQueue %~ enqueue effectiveTime upd
