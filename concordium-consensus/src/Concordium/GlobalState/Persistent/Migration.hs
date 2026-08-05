{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Persistent.Migration where

import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.P11 as P11
import qualified Concordium.Genesis.Data.P4 as P4
import qualified Concordium.Genesis.Data.P6 as P6
import qualified Concordium.Genesis.Data.P8 as P8
import qualified Concordium.Genesis.Data.P9 as P9
import Concordium.GlobalState.Persistent.BlobStore (SupportMigration)
import qualified Concordium.GlobalState.Persistent.BlockState.ExternalChainParameters as ECP
import Concordium.GlobalState.Persistent.BlockState.Parameters hiding (updateMaxLockDuration)
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Conditionally
import Concordium.Types.Parameters
import Concordium.Types.Updates

-- | A witness for the migration of 'ChainParametersVersion's between two protocol versions.
--  This is used to select the correct migration for types parametrised by 'ChainParametersVersion'
--  (or its derivatives), without having to case on the 'StateMigrationParameters' directly.
data ChainParametersMigration (cpvOld :: ChainParametersVersion) (cpvNew :: ChainParametersVersion) where
    ChainParametersMigrationTrivial :: ChainParametersMigration cpv cpv
    ChainParametersMigrationCPV0toCPV1 :: ChainParametersMigration 'ChainParametersV0 'ChainParametersV1
    ChainParametersMigrationCPV1toCPV2 :: ChainParametersMigration 'ChainParametersV1 'ChainParametersV2
    ChainParametersMigrationCPV2toCPV3 :: ChainParametersMigration 'ChainParametersV2 'ChainParametersV3

-- | Get a 'ChainParametersMigration' witness from 'StateMigrationParameters'.
chainParametersMigrationFor ::
    StateMigrationParameters oldpv pv ->
    ChainParametersMigration (ChainParametersVersionFor oldpv) (ChainParametersVersionFor pv)
chainParametersMigrationFor StateMigrationParametersTrivial = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP1P2{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP2P3{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP3ToP4{} = ChainParametersMigrationCPV0toCPV1
chainParametersMigrationFor StateMigrationParametersP4ToP5{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP5ToP6{} = ChainParametersMigrationCPV1toCPV2
chainParametersMigrationFor StateMigrationParametersP6ToP7{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP7ToP8{} = ChainParametersMigrationCPV2toCPV3
chainParametersMigrationFor StateMigrationParametersP8ToP9{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP9ToP10{} = ChainParametersMigrationTrivial
chainParametersMigrationFor StateMigrationParametersP10ToP11{} = ChainParametersMigrationTrivial

-- | A witness for the migration of 'AccountVersion's between two protocol versions.
--  This is used to select the correct migration for types parametrised by 'AccountVersion',
--  without having to case on the 'StateMigrationParameters' directly.
data AccountTypeMigration (avOld :: AccountVersion) (avNew :: AccountVersion) where
    AccountMigrationTrivial :: AccountTypeMigration av av
    AccountMigrationV0ToV1 :: AccountTypeMigration 'AccountV0 'AccountV1
    AccountMigrationV1ToV2 :: AccountTypeMigration 'AccountV1 'AccountV2
    AccountMigrationV2ToV3 :: AccountTypeMigration 'AccountV2 'AccountV3
    AccountMigrationV3ToV4 :: AccountTypeMigration 'AccountV3 'AccountV4
    AccountMigrationV4ToV5 :: AccountTypeMigration 'AccountV4 'AccountV5

-- | Get an 'AccountTypeMigration' witness from 'StateMigrationParameters'.
accountTypeMigrationFor ::
    StateMigrationParameters oldpv pv ->
    AccountTypeMigration (AccountVersionFor oldpv) (AccountVersionFor pv)
accountTypeMigrationFor StateMigrationParametersTrivial = AccountMigrationTrivial
accountTypeMigrationFor StateMigrationParametersP1P2{} = AccountMigrationTrivial
accountTypeMigrationFor StateMigrationParametersP2P3{} = AccountMigrationTrivial
accountTypeMigrationFor StateMigrationParametersP3ToP4{} = AccountMigrationV0ToV1
accountTypeMigrationFor StateMigrationParametersP4ToP5{} = AccountMigrationV1ToV2
accountTypeMigrationFor StateMigrationParametersP5ToP6{} = AccountMigrationTrivial
accountTypeMigrationFor StateMigrationParametersP6ToP7{} = AccountMigrationV2ToV3
accountTypeMigrationFor StateMigrationParametersP7ToP8{} = AccountMigrationV3ToV4
accountTypeMigrationFor StateMigrationParametersP8ToP9{} = AccountMigrationV4ToV5
accountTypeMigrationFor StateMigrationParametersP9ToP10{} = AccountMigrationTrivial
accountTypeMigrationFor StateMigrationParametersP10ToP11{} = AccountMigrationTrivial

-- | Apply a state migration to an 'Authorizations' structure.
--
--  [P3 to P4]: access structures for cooldown and time parameters are added.
migrateAuthorizations ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    Authorizations (AuthorizationsVersionFor oldpv) ->
    Authorizations (AuthorizationsVersionFor pv)
migrateAuthorizations StateMigrationParametersTrivial auths = auths
migrateAuthorizations StateMigrationParametersP1P2 auths = auths
migrateAuthorizations StateMigrationParametersP2P3 auths = auths
migrateAuthorizations (StateMigrationParametersP3ToP4 migration) Authorizations{..} =
    Authorizations
        { asCooldownParameters = CTrue updateCooldownParametersAccessStructure,
          asTimeParameters = CTrue updateTimeParametersAccessStructure,
          ..
        }
  where
    P4.ProtocolUpdateData{..} = P4.migrationProtocolUpdateData migration
migrateAuthorizations StateMigrationParametersP4ToP5 auths = auths
-- Note that the authorization for the consensus parameters v0
-- are carried over to consensus parameters v1.
migrateAuthorizations StateMigrationParametersP5ToP6{} auths = auths
migrateAuthorizations StateMigrationParametersP6ToP7{} auths = auths
migrateAuthorizations StateMigrationParametersP7ToP8{} auths = auths
migrateAuthorizations (StateMigrationParametersP8ToP9 migration) Authorizations{..} =
    Authorizations
        { asCreatePLT = CTrue updateCreatePLTAccessStructure,
          ..
        }
  where
    P9.ProtocolUpdateData{..} = P9.migrationProtocolUpdateData migration
migrateAuthorizations StateMigrationParametersP9ToP10{} auths = auths
migrateAuthorizations (StateMigrationParametersP10ToP11 migration) Authorizations{..} =
    Authorizations
        { asTokenParameters = CTrue updateTokenParametersAccessStructure,
          ..
        }
  where
    P11.ProtocolUpdateData{..} = P11.migrationProtocolUpdateData migration

-- | Apply a state migration to an 'UpdateKeysCollection' structure.
--
--  [P3 to P4]: access structures for cooldown and time parameters are added.
migrateUpdateKeysCollection ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    UpdateKeysCollection (AuthorizationsVersionFor oldpv) ->
    UpdateKeysCollection (AuthorizationsVersionFor pv)
migrateUpdateKeysCollection migration UpdateKeysCollection{..} =
    UpdateKeysCollection{level2Keys = migrateAuthorizations migration level2Keys, ..}

-- | Apply a state migration to a 'MintDistribution' structure.
--
--  [P3 to P4]: the mint-per-slot rate is removed.
migrateMintDistribution ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    MintDistribution (MintDistributionVersionFor (ChainParametersVersionFor oldpv)) ->
    MintDistribution (MintDistributionVersionFor (ChainParametersVersionFor pv))
migrateMintDistribution migration = case chainParametersMigrationFor migration of
    ChainParametersMigrationTrivial -> id
    ChainParametersMigrationCPV0toCPV1 -> \MintDistribution{..} ->
        MintDistribution{_mdMintPerSlot = CFalse, ..}
    ChainParametersMigrationCPV1toCPV2 -> id
    ChainParametersMigrationCPV2toCPV3 -> id

-- | Apply a state migration to a 'PoolParameters' structure.
--
--  [P3 to P4]: the new pool parameters are defined by the migration parameters.
migratePoolParameters ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    PoolParameters (ChainParametersVersionFor oldpv) ->
    PoolParameters (ChainParametersVersionFor pv)
migratePoolParameters migration = case chainParametersMigrationFor migration of
    ChainParametersMigrationTrivial -> id
    ChainParametersMigrationCPV0toCPV1 -> case migration of
        StateMigrationParametersP3ToP4 migrationData ->
            \_ -> P4.updatePoolParameters (P4.migrationProtocolUpdateData migrationData)
    ChainParametersMigrationCPV1toCPV2 -> id
    ChainParametersMigrationCPV2toCPV3 -> id

-- | Apply a state migration to a 'GASRewards' structure.
--
--  This does nothing except for the P5->P6 protocol update,
--  which removes the finalization proof reward.
migrateGASRewards ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    GASRewards (GasRewardsVersionFor (ChainParametersVersionFor oldpv)) ->
    GASRewards (GasRewardsVersionFor (ChainParametersVersionFor pv))
migrateGASRewards migration = case chainParametersMigrationFor migration of
    ChainParametersMigrationTrivial -> id
    ChainParametersMigrationCPV0toCPV1 -> id
    ChainParametersMigrationCPV1toCPV2 -> case migration of
        StateMigrationParametersP5ToP6{} ->
            \GASRewards{..} -> GASRewards{_gasFinalizationProof = CFalse, ..}
    ChainParametersMigrationCPV2toCPV3 -> id

-- | Apply a state migration to a 'ChainParameters' structure.
--
--  [P3 to P4]: the new cooldown, time and pool parameters are given by the migration parameters;
--    the mint-per-slot rate is removed from the reward parameters.
--
--  [P5 to P6]: the new consensus, finalization committee parameters are given by the migration parameters;
--    the GAS finalization proof reward is removed.
--
--  [P7 to P8]: the new validator score parameters are given by the migration parameters.
--
--  [P10 to P11]: the new max lock duration is given by the migration parameters
migrateChainParameters ::
    forall oldpv pv t m.
    (SupportMigration m t) =>
    StateMigrationParameters oldpv pv ->
    PersistentChainParameters oldpv ->
    t m (PersistentChainParameters pv)
migrateChainParameters migration = case chainParametersMigrationFor migration of
    ChainParametersMigrationTrivial -> migrateChainParametersVersionUnchanged migration
    ChainParametersMigrationCPV0toCPV1 -> \PersistentChainParameters{..} -> case migration of
        StateMigrationParametersP3ToP4 migrationData ->
            return $
                PersistentChainParameters
                    { pcpCooldownParameters = updateCooldownParameters,
                      pcpTimeParameters = SomeParam updateTimeParameters,
                      pcpRewardParameters =
                        RewardParameters
                            { _rpMintDistribution = migrateMintDistribution migration _rpMintDistribution,
                              _rpGASRewards = migrateGASRewards migration _rpGASRewards,
                              ..
                            },
                      pcpPoolParameters = migratePoolParameters migration pcpPoolParameters,
                      pcpFinalizationCommitteeParameters = NoParam,
                      pcpValidatorScoreParameters = NoParam,
                      pcpExternalChainParameters = CFalse,
                      ..
                    }
          where
            RewardParameters{..} = pcpRewardParameters
            P4.ProtocolUpdateData{..} = P4.migrationProtocolUpdateData migrationData
    ChainParametersMigrationCPV1toCPV2 -> \PersistentChainParameters{..} -> case migration of
        StateMigrationParametersP5ToP6 migrationData ->
            return $
                PersistentChainParameters
                    { pcpConsensusParameters = updateConsensusParameters,
                      pcpRewardParameters =
                        RewardParameters
                            { _rpMintDistribution = migrateMintDistribution migration _rpMintDistribution,
                              _rpGASRewards = migrateGASRewards migration _rpGASRewards,
                              ..
                            },
                      pcpPoolParameters = migratePoolParameters migration pcpPoolParameters,
                      pcpFinalizationCommitteeParameters = SomeParam updateFinalizationCommitteeParameters,
                      -- We unwrap and wrap here in order to associate the correct cpv
                      -- with the time parameters.
                      pcpTimeParameters = SomeParam $ unOParam pcpTimeParameters,
                      pcpValidatorScoreParameters = NoParam,
                      pcpExternalChainParameters = CFalse,
                      ..
                    }
          where
            P6.ProtocolUpdateData{..} = P6.migrationProtocolUpdateData migrationData
            RewardParameters{..} = pcpRewardParameters
    ChainParametersMigrationCPV2toCPV3 -> \PersistentChainParameters{..} -> case migration of
        StateMigrationParametersP7ToP8 migrationData ->
            return $
                PersistentChainParameters
                    { pcpValidatorScoreParameters = SomeParam updateValidatorScoreParameters,
                      pcpTimeParameters = SomeParam $ unOParam pcpTimeParameters,
                      pcpFinalizationCommitteeParameters = SomeParam $ unOParam pcpFinalizationCommitteeParameters,
                      pcpPoolParameters = migratePoolParameters migration pcpPoolParameters,
                      pcpRewardParameters =
                        RewardParameters
                            { _rpMintDistribution = migrateMintDistribution migration _rpMintDistribution,
                              _rpGASRewards = migrateGASRewards migration _rpGASRewards,
                              ..
                            },
                      pcpExternalChainParameters = CFalse,
                      ..
                    }
          where
            P8.ProtocolUpdateData{..} = P8.migrationProtocolUpdateData migrationData
            RewardParameters{..} = pcpRewardParameters

-- | Migrate persistent chain parameters when the chain parameter version is unchanged.
migrateChainParametersVersionUnchanged ::
    forall oldpv pv t m.
    (SupportMigration m t) =>
    StateMigrationParameters oldpv pv ->
    PersistentChainParameters oldpv ->
    t m (PersistentChainParameters pv)
migrateChainParametersVersionUnchanged StateMigrationParametersTrivial params = return params
migrateChainParametersVersionUnchanged StateMigrationParametersP1P2 PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged StateMigrationParametersP2P3 PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged StateMigrationParametersP4ToP5 PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged StateMigrationParametersP6ToP7 PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged StateMigrationParametersP8ToP9{} PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged StateMigrationParametersP9ToP10{} PersistentChainParameters{..} =
    return PersistentChainParameters{pcpExternalChainParameters = CFalse, ..}
migrateChainParametersVersionUnchanged (StateMigrationParametersP10ToP11 migration) PersistentChainParameters{..} = do
    newExternalChainParameters <- CTrue <$> ECP.p11NewExternalChainParameters updateMaxLockDuration
    return PersistentChainParameters{pcpExternalChainParameters = newExternalChainParameters, ..}
  where
    P11.ProtocolUpdateData{..} = P11.migrationProtocolUpdateData migration
migrateChainParametersVersionUnchanged _ _ = error "migrateChainParametersVersionUnchanged called for non-trivial chain parameter version migration"

-- | Migrate time of the effective change from V0 to V1 accounts. Currently this
--  translates times relative to genesis to times relative to the unix epoch.
migratePendingChangeEffective :: P4.StateMigrationData -> PendingChangeEffective 'AccountV0 -> PendingChangeEffective 'AccountV1
migratePendingChangeEffective P4.StateMigrationData{..} (PendingChangeEffectiveV0 eff) =
    PendingChangeEffectiveV1 $
        addDuration
            migrationPreviousGenesisTime
            (migrationPreviousEpochDuration * fromIntegral eff)

-- | Migrate the stake pending change from the representation used by protocol
--  version @oldpv@ to the representation used by the protocol version @pv@. The
--  migration parameters supply auxiliary data needed for the migration.
migrateStakePendingChange ::
    forall oldpv pv.
    StateMigrationParameters oldpv pv ->
    StakePendingChange (AccountVersionFor oldpv) ->
    StakePendingChange (AccountVersionFor pv)
migrateStakePendingChange migration = case accountTypeMigrationFor migration of
    AccountMigrationTrivial -> id
    AccountMigrationV0ToV1 -> case migration of
        StateMigrationParametersP3ToP4 migrationData -> \case
            NoChange -> NoChange
            ReduceStake amnt eff -> ReduceStake amnt (migratePendingChangeEffective migrationData eff)
            RemoveStake eff -> RemoveStake (migratePendingChangeEffective migrationData eff)
    AccountMigrationV1ToV2 -> fmap coercePendingChangeEffectiveV1
    AccountMigrationV2ToV3 -> const NoChange
    AccountMigrationV3ToV4 -> \NoChange -> NoChange
    AccountMigrationV4ToV5 -> \NoChange -> NoChange
