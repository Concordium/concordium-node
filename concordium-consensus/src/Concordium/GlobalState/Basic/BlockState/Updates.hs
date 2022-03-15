{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |Implementation of the chain update mechanism: https://concordium.gitlab.io/whitepapers/update-mechanism/main.pdf
module Concordium.GlobalState.Basic.BlockState.Updates where

import Data.Foldable
import Data.Semigroup
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))

import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Updates
import Concordium.Types.UpdateQueues
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.Types.IdentityProviders as IPS

import Concordium.GlobalState.Parameters

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

processValueUpdatesForCPV1 ::
    Timestamp
    -- ^Current timestamp
    -> v
    -- ^Current value
    -> UpdateQueueForCPV1 'ChainParametersV1 v
    -- ^Current queue
    -> (v, UpdateQueueForCPV1 'ChainParametersV1 v, Map.Map TransactionTime v)
processValueUpdatesForCPV1 t a0 uq =
    let (v, uq1, m) = processValueUpdates t a0 (unJustForCPV1 uq)
    in (v, JustForCPV1 uq1, m)

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
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

        -- Adds non-duplicate updates to the map with IPs/ARs and accumulates the actual changes that occurred.
        go (changesMap, valMap) (tt, v) =
          if Map.member k valMap
            then (changesMap, valMap) -- Ignore invalid update
            else (changesMap', valMap')
          where k = getKey v
                changesMap' = Map.insert tt v changesMap
                valMap' = Map.insert k v valMap

type UpdatesWithARsAndIPs cpv = (Updates' cpv, ARS.AnonymityRevokers, IPS.IdentityProviders)

-- |Process the update queues to determine the current state of updates for chain parameters version 0.
processUpdateQueues
    :: forall cpv
     . IsChainParametersVersion cpv
    => Timestamp
    -- ^Current timestamp
    -> UpdatesWithARsAndIPs cpv
    -> (Map.Map TransactionTime (UpdateValue cpv), UpdatesWithARsAndIPs cpv)
processUpdateQueues t (theUpdates, ars, ips) =
  (res
  , ( Updates {
            _currentKeyCollection = makeHashed $ UpdateKeysCollection newRootKeys newLevel1Keys newLevel2Keys,
            _currentProtocolUpdate = newProtocolUpdate,
            _currentParameters = ChainParameters {
                _cpElectionDifficulty = newElectionDifficulty,
                _cpExchangeRates = makeExchangeRates newEuroPerEnergy newMicroGTUPerEuro,
                _cpTimeParameters = newTimeParameters,
                _cpCooldownParameters = newCooldownParameters,
                _cpAccountCreationLimit = _cpAccountCreationLimit,
                _cpRewardParameters = RewardParameters {
                    _rpMintDistribution = newMintDistribution,
                    _rpTransactionFeeDistribution = newTransactionFeeDistribution,
                    _rpGASRewards = newGASRewards
                },
                _cpFoundationAccount = newFoundationAccount,
                _cpPoolParameters = newPoolParameters
            },
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
                    _pPoolParametersQueue = newPoolParametersQueue,
                    _pAddAnonymityRevokerQueue = newAddAnonymityRevokerQueue,
                    _pAddIdentityProviderQueue = newAddIdentityProviderQueue,
                    _pCooldownParametersQueue = newCooldownParametersQueue,
                    _pTimeParametersQueue = newTimeParametersQueue
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
        (newEuroPerEnergy, newEuroPerEnergyQueue, resEuroPerEnergy) = processValueUpdates t (_cpExchangeRates ^. erEuroPerEnergy) _pEuroPerEnergyQueue
        (newMicroGTUPerEuro, newMicroGTUPerEuroQueue, resMicroGTUPerEuro) = processValueUpdates t (_cpExchangeRates ^. erMicroGTUPerEuro) _pMicroGTUPerEuroQueue
        (newFoundationAccount, newFoundationAccountQueue, resFoundationAccount) = processValueUpdates t _cpFoundationAccount _pFoundationAccountQueue
        (newMintDistribution, newMintDistributionQueue, resMintDistribution) = processValueUpdates t _rpMintDistribution _pMintDistributionQueue
        (newTransactionFeeDistribution, newTransactionFeeDistributionQueue, resTransactionFeeDistribution) = processValueUpdates t _rpTransactionFeeDistribution _pTransactionFeeDistributionQueue
        (newGASRewards, newGASRewardsQueue, resGASRewards) = processValueUpdates t _rpGASRewards _pGASRewardsQueue
        (newPoolParameters, newPoolParametersQueue, resPoolParameters) = processValueUpdates t _cpPoolParameters _pPoolParametersQueue
        (updatedARs, newAddAnonymityRevokerQueue, resAddAnonymityRevoker) = processARsAndIPsUpdates (ARS.arRevokers ars) ARS.arIdentity t _pAddAnonymityRevokerQueue
        (updatedIPs, newAddIdentityProviderQueue, resAddIdentityProvider) = processARsAndIPsUpdates (IPS.idProviders ips) IPS.ipIdentity t _pAddIdentityProviderQueue
        (newTimeParameters, newTimeParametersQueue, resTimeParameters) =
            case chainParametersVersion @cpv of
                SCPV0 -> (_cpTimeParameters, NothingForCPV1, Map.empty)
                SCPV1 -> processValueUpdatesForCPV1 t _cpTimeParameters _pTimeParametersQueue & _3 %~ fmap UVTimeParameters
        (newCooldownParameters, newCooldownParametersQueue, resCooldownParameters) =
            case chainParametersVersion @cpv of
                SCPV0 -> (_cpCooldownParameters, NothingForCPV1, Map.empty)
                SCPV1 -> processValueUpdatesForCPV1 t _cpCooldownParameters _pCooldownParametersQueue & _3 %~ fmap UVCooldownParameters
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
            (UVPoolParameters <$> resPoolParameters) <>
            (UVAddAnonymityRevoker <$> resAddAnonymityRevoker) <>
            (UVAddIdentityProvider <$> resAddIdentityProvider) <>
            resCooldownParameters <>
            resTimeParameters

-- |Determine the future election difficulty (at a given time) based
-- on a current 'Updates'.
futureElectionDifficulty :: Updates' cpv -> Timestamp -> ElectionDifficulty
futureElectionDifficulty Updates{_pendingUpdates = PendingUpdates{..},..} ts
        = processValueUpdates ts (_cpElectionDifficulty _currentParameters) _pElectionDifficultyQueue ^. _1

-- |Get the protocol update status: either an effective protocol update or
-- a list of pending future protocol updates.
protocolUpdateStatus :: Updates' cpv -> ProtocolUpdateStatus
protocolUpdateStatus Updates{_pendingUpdates = PendingUpdates{..},..}
        = case _currentProtocolUpdate of
            Nothing -> PendingProtocolUpdates (_uqQueue _pProtocolQueue)
            Just pu -> ProtocolUpdated pu

-- |Get next update sequence number from update queue if @cpv@ is 'ChainParametersV1',
-- and return 'minUpdateSequenceNumber' on 'ChainParametersV0'.
nextUpdateSequenceNumberForCPV1
    :: forall cpv v
     . IsChainParametersVersion cpv
    => UpdateQueueForCPV1 cpv v
    -> UpdateSequenceNumber
nextUpdateSequenceNumberForCPV1 uq =
    case chainParametersVersion @cpv of
        SCPV0 -> minUpdateSequenceNumber
        SCPV1 -> unJustForCPV1 uq ^. uqNextSequenceNumber

-- |Determine the next sequence number for a given update type.
lookupNextUpdateSequenceNumber :: forall cpv. IsChainParametersVersion cpv => Updates' cpv -> UpdateType -> UpdateSequenceNumber
lookupNextUpdateSequenceNumber u UpdateProtocol = u ^. pendingUpdates . pProtocolQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateElectionDifficulty = u ^. pendingUpdates . pElectionDifficultyQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateEuroPerEnergy = u ^. pendingUpdates . pEuroPerEnergyQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateMicroGTUPerEuro = u ^. pendingUpdates . pMicroGTUPerEuroQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateFoundationAccount = u ^. pendingUpdates . pFoundationAccountQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateMintDistribution = u ^. pendingUpdates . pMintDistributionQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateTransactionFeeDistribution = u ^. pendingUpdates . pTransactionFeeDistributionQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateGASRewards = u ^. pendingUpdates . pGASRewardsQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdatePoolParameters = u ^. pendingUpdates . pPoolParametersQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateAddAnonymityRevoker = u ^. pendingUpdates . pAddAnonymityRevokerQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateAddIdentityProvider = u ^. pendingUpdates . pAddIdentityProviderQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateRootKeys = u ^. pendingUpdates . pRootKeysUpdateQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateLevel1Keys = u ^. pendingUpdates . pLevel1KeysUpdateQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateLevel2Keys = u ^. pendingUpdates . pLevel2KeysUpdateQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateCooldownParameters =
    nextUpdateSequenceNumberForCPV1 (u ^. pendingUpdates . pCooldownParametersQueue)
lookupNextUpdateSequenceNumber u UpdateTimeParameters =
    nextUpdateSequenceNumberForCPV1 (u ^. pendingUpdates . pTimeParametersQueue)

-- |Enqueue an update in the appropriate queue.
enqueueUpdate :: TransactionTime -> UpdateValue cpv -> Updates' cpv -> Updates' cpv
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
enqueueUpdate effectiveTime (UVPoolParameters upd) = pendingUpdates . pPoolParametersQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVAddAnonymityRevoker upd) = pendingUpdates . pAddAnonymityRevokerQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVAddIdentityProvider upd) = pendingUpdates . pAddIdentityProviderQueue %~ enqueue effectiveTime upd
enqueueUpdate effectiveTime (UVCooldownParameters upd) =
    pendingUpdates . pCooldownParametersQueue %~ JustForCPV1 . enqueue effectiveTime upd . unJustForCPV1
enqueueUpdate effectiveTime (UVTimeParameters upd) =
    pendingUpdates . pTimeParametersQueue %~ JustForCPV1 . enqueue effectiveTime upd . unJustForCPV1

-- |Overwrite the election difficulty with the specified value and remove
-- any pending updates to the election difficulty from the queue.
overwriteElectionDifficulty :: ElectionDifficulty -> Updates' cpv -> Updates' cpv
overwriteElectionDifficulty newDifficulty =
    (currentParameters . cpElectionDifficulty .~ newDifficulty) .
    (pendingUpdates . pElectionDifficultyQueue . uqQueue .~ [])

-- |Clear the protocol update and remove any pending protocol updates from
-- the queue.
clearProtocolUpdate :: Updates' cpv -> Updates' cpv
clearProtocolUpdate =
    (currentProtocolUpdate .~ Nothing) .
    (pendingUpdates . pProtocolQueue . uqQueue .~ [])