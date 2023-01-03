{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |Implementation of the chain update mechanism: https://concordium.gitlab.io/whitepapers/update-mechanism/main.pdf
module Concordium.GlobalState.Basic.BlockState.Updates where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Semigroup

import Lens.Micro.Platform

import Concordium.Types
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.Types.IdentityProviders as IPS
import Concordium.Types.UpdateQueues
import Concordium.Types.Updates

import Concordium.GlobalState.Parameters

-- |Process the update queue to determine the new value of a parameter (or the authorizations).
-- This splits the queue at the given timestamp. The last value up to and including the timestamp
-- is the new value, if any -- otherwise the current value is retained. The queue is updated to
-- be the updates with later timestamps.
processValueUpdates ::
    -- |Current timestamp
    Timestamp ->
    -- |Current value
    v ->
    -- |Current queue
    UpdateQueue v ->
    (v, UpdateQueue v, Map.Map TransactionTime v)
processValueUpdates t a0 uq = (getLast (sconcat (Last <$> a0 :| (snd <$> ql))), uq{_uqQueue = qr}, Map.fromAscList ql)
  where
    (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)

-- |Process the update queue to determine the new value of a parameter for a parameter introduced
-- in 'ChainParametersV1'.  The return value is the current value of the parameter, the new queue,
-- and a map of updates to the parameter by time.
processValueUpdatesO ::
    (IsSupported pt cpv ~ 'True) =>
    -- |Current timestamp
    Timestamp ->
    -- |Current value
    v ->
    -- |Current queue
    OUpdateQueue pt cpv v ->
    (v, OUpdateQueue pt cpv v, Map.Map TransactionTime v)
processValueUpdatesO t a0 uq =
    let (v, uq1, m) = processValueUpdates t a0 (unOParam uq)
    in  (v, SomeParam uq1, m)

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
processProtocolUpdates ::
    Timestamp ->
    Maybe ProtocolUpdate ->
    UpdateQueue ProtocolUpdate ->
    (Maybe ProtocolUpdate, UpdateQueue ProtocolUpdate, Map.Map TransactionTime ProtocolUpdate)
processProtocolUpdates t pu0 uq = (pu', uq{_uqQueue = qr}, Map.fromAscList ql)
  where
    (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)
    pu' = getFirst <$> mconcat ((First <$> pu0) : (Just . First . snd <$> ql))

-- |Process AR and IP update queues.
-- Ensuring that new IPs/ARs have unique ids is difficult when enqueueing.
-- Instead, it is handled here by ignoring updates with duplicate IPs/ARs.
processARsAndIPsUpdates ::
    Ord k =>
    -- | The existing IPs / ARs.
    Map.Map k v ->
    -- | Getter for the key field.
    (v -> k) ->
    Timestamp ->
    UpdateQueue v ->
    (Map.Map k v, UpdateQueue v, Map.Map TransactionTime v)
processARsAndIPsUpdates oldValMap getKey t uq = (updatedValMap, uq{_uqQueue = qr}, changes)
  where
    (ql, qr) = span ((<= t) . transactionTimeToTimestamp . fst) (uq ^. uqQueue)
    (changes, updatedValMap) = foldl' go (Map.empty, oldValMap) ql

    -- Adds non-duplicate updates to the map with IPs/ARs and accumulates the actual changes that occurred.
    go (changesMap, valMap) (tt, v) =
        if Map.member k valMap
            then (changesMap, valMap) -- Ignore invalid update
            else (changesMap', valMap')
      where
        k = getKey v
        changesMap' = Map.insert tt v changesMap
        valMap' = Map.insert k v valMap

type UpdatesWithARsAndIPs (cpv :: ChainParametersVersion) =
    (Updates' cpv, ARS.AnonymityRevokers, IPS.IdentityProviders)

-- |Process the update queues to determine the current state of updates for chain parameters version 0.
processUpdateQueues ::
    forall cpv.
    IsChainParametersVersion cpv =>
    -- |Current timestamp
    Timestamp ->
    UpdatesWithARsAndIPs cpv ->
    (Map.Map TransactionTime (UpdateValue cpv), UpdatesWithARsAndIPs cpv)
processUpdateQueues t (theUpdates, ars, ips) =
    ( res,
        ( Updates
            { _currentKeyCollection = makeHashed $ UpdateKeysCollection newRootKeys newLevel1Keys newLevel2Keys,
              _currentProtocolUpdate = newProtocolUpdate,
              _currentParameters =
                ChainParameters
                    { _cpConsensusParameters = newConsensusParameters,
                      _cpExchangeRates = makeExchangeRates newEuroPerEnergy newMicroGTUPerEuro,
                      _cpTimeParameters = newTimeParameters,
                      _cpCooldownParameters = newCooldownParameters,
                      _cpAccountCreationLimit = _cpAccountCreationLimit,
                      _cpRewardParameters =
                        RewardParameters
                            { _rpMintDistribution = newMintDistribution,
                              _rpTransactionFeeDistribution = newTransactionFeeDistribution,
                              _rpGASRewards = newGASRewards
                            },
                      _cpFoundationAccount = newFoundationAccount,
                      _cpPoolParameters = newPoolParameters
                    },
              _pendingUpdates =
                PendingUpdates
                    { _pRootKeysUpdateQueue = newRootKeysQueue,
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
                      _pTimeParametersQueue = newTimeParametersQueue,
                      _pTimeoutParametersQueue = newTimeoutParametersQueue,
                      _pMinBlockTimeQueue = newMinBlockTimeQueue,
                      _pBlockEnergyLimitQueue = newBlockEnergyLimitQueue
                    }
            },
          ARS.AnonymityRevokers updatedARs,
          IPS.IdentityProviders updatedIPs
        )
    )
  where
    Updates{_pendingUpdates = PendingUpdates{..}, _currentParameters = ChainParameters{_cpRewardParameters = RewardParameters{..}, _cpConsensusParameters  = consensusParams, ..}, ..} = theUpdates


    (newRootKeys, newRootKeysQueue, resRootKeys) = processValueUpdates t (rootKeys $ _unhashed _currentKeyCollection) _pRootKeysUpdateQueue
    (newLevel1Keys, newLevel1KeysQueue, resLevel1Keys) = processValueUpdates t (level1Keys $ _unhashed _currentKeyCollection) _pLevel1KeysUpdateQueue
    (newLevel2Keys, newLevel2KeysQueue, resLevel2Keys) = processValueUpdates t (level2Keys $ _unhashed _currentKeyCollection) _pLevel2KeysUpdateQueue
    (newProtocolUpdate, newProtocolQueue, resProtocol) = processProtocolUpdates t _currentProtocolUpdate _pProtocolQueue
    (newEuroPerEnergy, newEuroPerEnergyQueue, resEuroPerEnergy) = processValueUpdates t (_cpExchangeRates ^. euroPerEnergy) _pEuroPerEnergyQueue
    (newMicroGTUPerEuro, newMicroGTUPerEuroQueue, resMicroGTUPerEuro) = processValueUpdates t (_cpExchangeRates ^. microGTUPerEuro) _pMicroGTUPerEuroQueue
    (newFoundationAccount, newFoundationAccountQueue, resFoundationAccount) = processValueUpdates t _cpFoundationAccount _pFoundationAccountQueue
    (newMintDistribution, newMintDistributionQueue, resMintDistribution) = processValueUpdates t _rpMintDistribution _pMintDistributionQueue
    (newTransactionFeeDistribution, newTransactionFeeDistributionQueue, resTransactionFeeDistribution) = processValueUpdates t _rpTransactionFeeDistribution _pTransactionFeeDistributionQueue
    (newGASRewards, newGASRewardsQueue, resGASRewards) = processValueUpdates t _rpGASRewards _pGASRewardsQueue
    (newPoolParameters, newPoolParametersQueue, resPoolParameters) = processValueUpdates t _cpPoolParameters _pPoolParametersQueue
    (updatedARs, newAddAnonymityRevokerQueue, resAddAnonymityRevoker) = processARsAndIPsUpdates (ARS.arRevokers ars) ARS.arIdentity t _pAddAnonymityRevokerQueue
    (updatedIPs, newAddIdentityProviderQueue, resAddIdentityProvider) = processARsAndIPsUpdates (IPS.idProviders ips) IPS.ipIdentity t _pAddIdentityProviderQueue
    (newTimeParameters, newTimeParametersQueue, resTimeParameters) =
        case chainParametersVersion @cpv of
            SChainParametersV0 -> (_cpTimeParameters, NoParam, Map.empty)
            SChainParametersV1 -> processValueUpdatesO t (unOParam _cpTimeParameters) _pTimeParametersQueue & _3 %~ fmap UVTimeParameters & _1 %~ SomeParam
            SChainParametersV2 -> processValueUpdatesO t (unOParam _cpTimeParameters) _pTimeParametersQueue & _3 %~ fmap UVTimeParameters & _1 %~ SomeParam

    (newCooldownParameters, newCooldownParametersQueue, resCooldownParameters) =
        case chainParametersVersion @cpv of
            SChainParametersV0 -> (_cpCooldownParameters, NoParam, Map.empty)
            SChainParametersV1 -> processValueUpdatesO t _cpCooldownParameters _pCooldownParametersQueue & _3 %~ fmap UVCooldownParameters
            SChainParametersV2 -> processValueUpdatesO t _cpCooldownParameters _pCooldownParametersQueue & _3 %~ fmap UVCooldownParameters
    (newConsensusParameters, newElectionDifficultyQueue, newTimeoutParametersQueue, newMinBlockTimeQueue, newBlockEnergyLimitQueue, resElectionDifficulty, resTimeoutParameters, resMinBlockTime, resBlockEnergyLimit) =
        case chainParametersVersion @cpv of
            SChainParametersV0 ->
                let electionDifficulty = _cpElectionDifficulty consensusParams
                    (newElectionDifficulty, newEDQueue, resElectionDifficulty') = processValueUpdatesO t electionDifficulty _pElectionDifficultyQueue & _3 %~ fmap UVElectionDifficulty
                in (ConsensusParametersV0 newElectionDifficulty, newEDQueue, NoParam, NoParam, NoParam, resElectionDifficulty', Map.empty, Map.empty, Map.empty)
            SChainParametersV1 ->
                let electionDifficulty = _cpElectionDifficulty consensusParams
                    (newElectionDifficulty, newEDQueue, resElectionDifficulty') = processValueUpdatesO t electionDifficulty _pElectionDifficultyQueue & _3 %~ fmap UVElectionDifficulty
                in (ConsensusParametersV0 newElectionDifficulty, newEDQueue, NoParam, NoParam, NoParam, resElectionDifficulty', Map.empty, Map.empty, Map.empty)
            SChainParametersV2 ->
                let (newTimeoutParameters, newTOQueue, resTimeoutParameters') = processValueUpdatesO t (_cpTimeoutParameters consensusParams) _pTimeoutParametersQueue & _3 %~ fmap UVTimeoutParameters
                    (newMinBlockTime, newMBTQueue, resMinBlockTime') = processValueUpdatesO t (_cpMinBlockTime consensusParams) _pMinBlockTimeQueue  & _3 %~ fmap UVMinBlockTime
                    (newBlockEnergyLimit, newBELQueue, resBlockEnergyLimit') = processValueUpdatesO t (_cpBlockEnergyLimit consensusParams) _pBlockEnergyLimitQueue  & _3 %~ fmap UVBlockEnergyLimit
                in (ConsensusParametersV1 newTimeoutParameters newMinBlockTime newBlockEnergyLimit,
                    NoParam,
                    newTOQueue,
                    newMBTQueue,
                    newBELQueue,
                    Map.empty,
                    resTimeoutParameters',
                    resMinBlockTime',
                    resBlockEnergyLimit')
    res =
        (UVRootKeys <$> resRootKeys)
            <> (UVLevel1Keys <$> resLevel1Keys)
            <> (UVLevel2Keys <$> resLevel2Keys)
            <> (UVProtocol <$> resProtocol)
            <> resElectionDifficulty
            <> (UVEuroPerEnergy <$> resEuroPerEnergy)
            <> (UVMicroGTUPerEuro <$> resMicroGTUPerEuro)
            <> (UVFoundationAccount <$> resFoundationAccount)
            <> (UVMintDistribution <$> resMintDistribution)
            <> (UVTransactionFeeDistribution <$> resTransactionFeeDistribution)
            <> (UVGASRewards <$> resGASRewards)
            <> (UVPoolParameters <$> resPoolParameters)
            <> (UVAddAnonymityRevoker <$> resAddAnonymityRevoker)
            <> (UVAddIdentityProvider <$> resAddIdentityProvider)
            <> resCooldownParameters
            <> resTimeParameters
            <> resTimeoutParameters
            <> resMinBlockTime
            <> resBlockEnergyLimit

-- |Determine the future election difficulty (at a given time) based
-- on a current 'Updates'.
futureElectionDifficulty :: IsSupported 'PTElectionDifficulty cpv ~ 'True => Updates' cpv -> Timestamp -> ElectionDifficulty
futureElectionDifficulty Updates{_pendingUpdates = PendingUpdates{..}, ..} ts =
    processValueUpdatesO ts (_cpElectionDifficulty $ _cpConsensusParameters _currentParameters) _pElectionDifficultyQueue ^. _1

-- |Get the protocol update status: either an effective protocol update or
-- a list of pending future protocol updates.
protocolUpdateStatus :: Updates' cpv -> ProtocolUpdateStatus
protocolUpdateStatus Updates{_pendingUpdates = PendingUpdates{..}, ..} =
    case _currentProtocolUpdate of
        Nothing -> PendingProtocolUpdates (_uqQueue _pProtocolQueue)
        Just pu -> ProtocolUpdated pu

-- |Get next update sequence number from an update queue that may not be present for all chain
-- parameter versions.  If queue is missing, this returns 'minUpdateSequenceNumber'.
nextUpdateSequenceNumberO ::
    forall pt cpv v.
    IsChainParametersVersion cpv =>
    OUpdateQueue pt cpv v ->
    UpdateSequenceNumber
nextUpdateSequenceNumberO uq = case uq of
    NoParam -> minUpdateSequenceNumber
    SomeParam q -> q ^. uqNextSequenceNumber

-- |Determine the next sequence number for a given update type.
lookupNextUpdateSequenceNumber :: forall cpv. IsChainParametersVersion cpv => Updates' cpv -> UpdateType -> UpdateSequenceNumber
lookupNextUpdateSequenceNumber u UpdateProtocol = u ^. pendingUpdates . pProtocolQueue . uqNextSequenceNumber
lookupNextUpdateSequenceNumber u UpdateElectionDifficulty =
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pElectionDifficultyQueue)
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
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pCooldownParametersQueue)
lookupNextUpdateSequenceNumber u UpdateTimeParameters =
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pTimeParametersQueue)
lookupNextUpdateSequenceNumber u UpdateTimeoutParameters =
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pTimeoutParametersQueue)
lookupNextUpdateSequenceNumber u UpdateMinBlockTime =
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pMinBlockTimeQueue)
lookupNextUpdateSequenceNumber u UpdateBlockEnergyLimit =
    nextUpdateSequenceNumberO (u ^. pendingUpdates . pBlockEnergyLimitQueue)

-- |Enqueue an update in the appropriate queue.
enqueueUpdate :: TransactionTime -> UpdateValue cpv -> Updates' cpv -> Updates' cpv
enqueueUpdate effectiveTime (UVRootKeys rk) = pendingUpdates . pRootKeysUpdateQueue %~ enqueue effectiveTime rk
enqueueUpdate effectiveTime (UVLevel1Keys l1k) = pendingUpdates . pLevel1KeysUpdateQueue %~ enqueue effectiveTime l1k
enqueueUpdate effectiveTime (UVLevel2Keys l2k) = pendingUpdates . pLevel2KeysUpdateQueue %~ enqueue effectiveTime l2k
enqueueUpdate effectiveTime (UVProtocol protUp) = pendingUpdates . pProtocolQueue %~ enqueue effectiveTime protUp
enqueueUpdate effectiveTime (UVElectionDifficulty edUp) = pendingUpdates . pElectionDifficultyQueue %~ fmap (enqueue effectiveTime edUp)
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
    pendingUpdates . pCooldownParametersQueue %~ fmap (enqueue effectiveTime upd)
enqueueUpdate effectiveTime (UVTimeParameters upd) =
    pendingUpdates . pTimeParametersQueue %~ fmap (enqueue effectiveTime upd)
enqueueUpdate effectiveTime (UVTimeoutParameters upd) =
    pendingUpdates . pTimeoutParametersQueue %~ fmap (enqueue effectiveTime upd)
enqueueUpdate effectiveTime (UVMinBlockTime upd) =
    pendingUpdates . pMinBlockTimeQueue %~ fmap (enqueue effectiveTime upd)
enqueueUpdate effectiveTime (UVBlockEnergyLimit upd) =
    pendingUpdates . pBlockEnergyLimitQueue %~ fmap (enqueue effectiveTime upd)

-- |Empty the queue of an OUpdateQueue.
emptyQueueO :: (IsSupported pt cpv ~ 'True) => OUpdateQueue pt cpv a -> OUpdateQueue pt cpv a
emptyQueueO (SomeParam q) = SomeParam (q{_uqQueue = []})

-- |Overwrite the election difficulty with the specified value and remove
-- any pending updates to the election difficulty from the queue.
overwriteElectionDifficulty :: (IsSupported 'PTElectionDifficulty cpv ~ 'True, ConsensusParametersVersionFor cpv ~ 'ConsensusParametersVersion0) => ElectionDifficulty -> Updates' cpv -> Updates' cpv
overwriteElectionDifficulty newDifficulty =
    (currentParameters . cpConsensusParameters . cpElectionDifficulty .~ newDifficulty)
        . (pendingUpdates . pElectionDifficultyQueue %~ emptyQueueO)

-- |Clear the protocol update and remove any pending protocol updates from
-- the queue.
clearProtocolUpdate :: Updates' cpv -> Updates' cpv
clearProtocolUpdate =
    (currentProtocolUpdate .~ Nothing)
        . (pendingUpdates . pProtocolQueue . uqQueue .~ [])
