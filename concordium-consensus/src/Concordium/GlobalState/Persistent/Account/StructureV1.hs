{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module implements accounts for account versions 'AccountV2' (protocol 'P5', 'P6')
--  and 'AccountV3' (protocol 'P7').  It should not be necessary to use this module directly,
--  but instead through the interface provided by "Concordium.GlobalState.Persistent.Account".
module Concordium.GlobalState.Persistent.Account.StructureV1 where

import Control.Monad
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as State (StateT (..))
import Data.Bits
import Data.Bool.Singletons
import Data.Foldable
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Genesis.Data
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.ID.Types hiding (values)
import Concordium.Logger
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Accounts.Releases
import Concordium.Types.Conditionally
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Utils

import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount, replaceUpTo)
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as TARS
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import Concordium.GlobalState.BlockState (AccountAllowance (..))
import Concordium.GlobalState.CooldownQueue (Cooldowns (..))
import Concordium.GlobalState.Persistent.Account.CooldownQueue as CooldownQueue
import Concordium.GlobalState.Persistent.Account.EncryptedAmount
import Concordium.GlobalState.Persistent.Account.MigrationStateInterface
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens
import qualified Concordium.GlobalState.Persistent.Account.StructureV0 as V0
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as ARSV0
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseScheduleV1
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.ID.Parameters

-- * Terminology

-- $Terminology
--
-- The terms "persistent", "persisting" and "enduring" have separate meanings in our nomenclature:
--
-- * "Persistent" is used to make explicit that the datastructure is persisted in the blob store.
--
-- * "Persisting" is used specifically to refer to 'PersistingAccountData'.
--
-- * "Enduring" is used to refer to that data encapsulated by 'PersistentAccountEnduringData',
--   which consists of all account data except nonce, current balance, and staked balance.

-- * 'PersistentBakerInfoEx'

-- | A reference to a 'BakerInfoEx'. These references are shared between baker accounts
--  and the current/next epoch bakers. We use a 'LazyBufferedRef' so that the reference does not
--  need to be loaded whenever the account is loaded, but once it is loaded, copies of the reference
--  (e.g. in the account cache) will also be loaded.
type PersistentBakerInfoEx av = LazyBufferedRef (BakerInfoEx av)

-- ** Query

-- | Load 'BakerInfo' from a 'PersistentBakerInfoEx'.
loadBakerInfo :: (MonadBlobStore m, IsAccountVersion av) => PersistentBakerInfoEx av -> m BakerInfo
loadBakerInfo = fmap _bieBakerInfo . refLoad

-- | Load a 'BakerInfoEx' from a 'PersistentBakerInfoEx'.
loadPersistentBakerInfoEx ::
    (MonadBlobStore m, IsAccountVersion av) =>
    PersistentBakerInfoEx av ->
    m (BakerInfoEx av)
loadPersistentBakerInfoEx = refLoad

-- | Load the 'BakerId' from a 'PersistentBakerInfoEx'.
loadBakerId :: (MonadBlobStore m, IsAccountVersion av) => PersistentBakerInfoEx av -> m BakerId
loadBakerId = fmap (view bakerIdentity) . refLoad

-- ** Construction

-- | Construct a 'PersistentBakerInfoEx' from a 'BakerInfoEx'.
makePersistentBakerInfoEx :: (MonadBlobStore m, IsAccountVersion av) => BakerInfoEx av -> m (PersistentBakerInfoEx av)
makePersistentBakerInfoEx = refMake

-- ** Migration

-- | See documentation of @migratePersistentBlockState@.
migratePersistentBakerInfoEx ::
    ( IsProtocolVersion oldpv,
      AccountStructureVersionFor (AccountVersionFor oldpv) ~ 'AccountStructureV1,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoEx StateMigrationParametersTrivial = migrateReference return
migratePersistentBakerInfoEx StateMigrationParametersP5ToP6{} = migrateReference return
migratePersistentBakerInfoEx StateMigrationParametersP6ToP7{} = migrateReference migrateBakerInfoExV1
  where
    migrateBakerInfoExV1 ::
        (AVSupportsDelegation av1, AVSupportsDelegation av2, SupportsValidatorSuspension av2 ~ 'False, Monad m') =>
        BakerInfoEx av1 ->
        m' (BakerInfoEx av2)
    migrateBakerInfoExV1 BakerInfoExV1{..} = return BakerInfoExV1{_bieIsSuspended = CFalse, ..}
migratePersistentBakerInfoEx StateMigrationParametersP7ToP8{} = migrateReference migrateBakerInfoExV1
  where
    migrateBakerInfoExV1 ::
        (AVSupportsDelegation av1, AVSupportsDelegation av2, SupportsValidatorSuspension av2 ~ 'True, Monad m') =>
        BakerInfoEx av1 ->
        m' (BakerInfoEx av2)
    migrateBakerInfoExV1 BakerInfoExV1{..} = return BakerInfoExV1{_bieIsSuspended = CTrue False, ..}
migratePersistentBakerInfoEx StateMigrationParametersP8ToP9{} = migrateReference (return . coerceBakerInfoExV1)

-- | Migrate a 'V0.PersistentBakerInfoEx' to a 'PersistentBakerInfoEx'.
--  See documentation of @migratePersistentBlockState@.
migratePersistentBakerInfoExFromV0 ::
    ( AccountVersionFor oldpv ~ 'AccountV1,
      AccountVersionFor pv ~ 'AccountV2,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    V0.PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoExFromV0 StateMigrationParametersP4ToP5{} V0.PersistentBakerInfoEx{..} = do
    bkrInfoEx <- lift $ do
        bkrInfo <- refLoad bakerInfoRef
        bkrPoolInfo <- refLoad (V0._theExtraBakerInfo bakerInfoExtra)
        return $! BakerInfoExV1 bkrInfo bkrPoolInfo CFalse
    (ref, _) <- refFlush =<< refMake bkrInfoEx
    return $! ref

-- * Enduring account stake data

-- | This is the information about the stake associated with an account, excluding the staked
--  amount. The staked amount is excluded as it expected to change commonly for accounts that
--  restake their earnings.
--
--  Note, only the baker info is stored under a reference. The reference is a 'LazyBufferedRef'
--  rather than an 'EagerBufferedRef', as it will often be unnecessary to load the baker info.
data PersistentAccountStakeEnduring av where
    PersistentAccountStakeEnduringNone :: PersistentAccountStakeEnduring av
    PersistentAccountStakeEnduringBaker ::
        { paseBakerRestakeEarnings :: !Bool,
          paseBakerInfo :: !(LazyBufferedRef (BakerInfoEx av)),
          paseBakerPendingChange :: !(StakePendingChange av)
        } ->
        PersistentAccountStakeEnduring av
    PersistentAccountStakeEnduringDelegator ::
        { paseDelegatorId :: !DelegatorId,
          paseDelegatorRestakeEarnings :: !Bool,
          paseDelegatorTarget :: !DelegationTarget,
          paseDelegatorPendingChange :: !(StakePendingChange av)
        } ->
        PersistentAccountStakeEnduring av

-- | Convert a 'PersistentAccountStakeEnduring' to an 'AccountStake' given the amount of the stake.
--  This is used to implement 'getStake', and is also used in computing the stake hash.
persistentToAccountStake ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    PersistentAccountStakeEnduring av ->
    Amount ->
    m (AccountStake av)
persistentToAccountStake PersistentAccountStakeEnduringNone _ = return AccountStakeNone
persistentToAccountStake PersistentAccountStakeEnduringBaker{..} _stakedAmount = do
    _accountBakerInfo <- refLoad paseBakerInfo
    return $!
        AccountStakeBaker
            AccountBaker
                { _stakeEarnings = paseBakerRestakeEarnings,
                  _bakerPendingChange = paseBakerPendingChange,
                  ..
                }
persistentToAccountStake PersistentAccountStakeEnduringDelegator{..} _delegationStakedAmount = do
    return $!
        AccountStakeDelegate
            AccountDelegationV1
                { _delegationIdentity = paseDelegatorId,
                  _delegationStakeEarnings = paseDelegatorRestakeEarnings,
                  _delegationTarget = paseDelegatorTarget,
                  _delegationPendingChange = paseDelegatorPendingChange,
                  ..
                }

-- | Migrate a 'PersistentAccountStakeEnduring' from one blob store to another, where the account
--  version is unchanged.
migratePersistentAccountStakeEnduring ::
    (SupportMigration m t, IsAccountVersion av) =>
    PersistentAccountStakeEnduring av ->
    t m (PersistentAccountStakeEnduring av)
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringNone =
    return PersistentAccountStakeEnduringNone
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringBaker{..} = do
    newBakerInfo <- migrateReference return paseBakerInfo
    return $!
        PersistentAccountStakeEnduringBaker
            { paseBakerInfo = newBakerInfo,
              ..
            }
migratePersistentAccountStakeEnduring PersistentAccountStakeEnduringDelegator{..} =
    return $! PersistentAccountStakeEnduringDelegator{..}

-- | Migrate a 'PersistentAccountStakeEnduring' from one blob store to another, where the both
-- the old and new account version support delegation and validator suspension. This holds in account
-- version 4 onwards.
migratePersistentAccountStakeEnduringAV4 ::
    forall m t av1 av2.
    ( SupportMigration m t,
      IsAccountVersion av1,
      IsAccountVersion av2,
      AVSupportsDelegation av1,
      AVSupportsDelegation av2,
      AVSupportsValidatorSuspension av1,
      AVSupportsValidatorSuspension av2
    ) =>
    PersistentAccountStakeEnduring av1 ->
    t m (PersistentAccountStakeEnduring av2)
migratePersistentAccountStakeEnduringAV4 PersistentAccountStakeEnduringNone =
    return PersistentAccountStakeEnduringNone
migratePersistentAccountStakeEnduringAV4 PersistentAccountStakeEnduringBaker{..} = do
    newBakerInfo <- migrateReference (return . coerceBakerInfoExV1) paseBakerInfo
    return $!
        PersistentAccountStakeEnduringBaker
            { paseBakerInfo = newBakerInfo,
              paseBakerPendingChange = NoChange,
              ..
            }
migratePersistentAccountStakeEnduringAV4 PersistentAccountStakeEnduringDelegator{..} =
    return $!
        PersistentAccountStakeEnduringDelegator
            { paseDelegatorPendingChange = NoChange,
              ..
            }

-- | A monad transformer transformer that left-composes @StateT Amount@
--  with a given monad transformer @t@. The purpose of this is to add state functionality for
--  tracking the current active stake on an account, which is used when migrating an account
--  between certain protocol versions.
--
--  A monad transformer transformer is used so that the 'lift' operation removes both the
--  @StateT Amount@ and the underlying monad transformer @t@. This is important as the reference
--  migration functions depend on using 'lift' to access the source block state.
newtype StakedBalanceStateTT (t :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) (a :: Type) = StakedBalanceStateTT
    { runStakedBalanceStateTT' :: State.StateT Amount (t m) a
    }
    deriving newtype (Functor, Applicative, Monad, State.MonadState Amount, MonadIO, MonadLogger)

-- | Run an 'StakedBalanceStateTT' computation with the given initial staked balance state.
runStakedBalanceStateTT :: StakedBalanceStateTT t m a -> Amount -> t m (a, Amount)
runStakedBalanceStateTT = State.runStateT . runStakedBalanceStateTT'

instance (MonadTrans t) => MonadTrans (StakedBalanceStateTT t) where
    lift = StakedBalanceStateTT . lift . lift

deriving via
    forall (t :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type).
    State.StateT Amount (t m)
    instance
        (MonadBlobStore (t m)) =>
        MonadBlobStore (StakedBalanceStateTT t m)

-- | Lift a computation in the base monad to the transformed monad.
liftStakedBalanceStateTT ::
    (Monad (t m)) =>
    t m a ->
    StakedBalanceStateTT t m a
liftStakedBalanceStateTT = StakedBalanceStateTT . lift

-- | Migrate a 'PersistentAccountStakeEnduring' from 'AccountV2' to 'AccountV3'.  This runs in the
--   @StakedBalanceStateTT t m@ monad, where the state is the amount of active stake on the account.
--
--   * If there is a pending change on the account, then the pending change is removed and the
--     active stake is updated to apply the pending change. The change in the stake is moved to
--     pre-pre-cooldown in the returned 'CooldownQueue'. 'addAccountInPrePreCooldown' is called
--     to record that the account is in pre-pre-cooldown. If the pending change was a removal,
--     the baker or delegator record is removed from the account.

--   * If the account is (still) a delegator and the baker it was delegating to has been removed
--     (according to 'isBakerRemoved'), then the delegator is changed to delegate to passive
--     instead.
--
--   * If the account is (still) a delegator, then 'retainDelegator' is called to record the
--     delegator's (updated) stake and target.
migratePersistentAccountStakeEnduringV2toV3 ::
    (SupportMigration m t, AccountMigration 'AccountV3 (t m)) =>
    PersistentAccountStakeEnduring 'AccountV2 ->
    -- | Returns the new 'PersistentAccountStakeEnduring' and 'CooldownQueue'.
    StakedBalanceStateTT t m (PersistentAccountStakeEnduring 'AccountV3, CooldownQueue 'AccountV3)
migratePersistentAccountStakeEnduringV2toV3 PersistentAccountStakeEnduringNone =
    return (PersistentAccountStakeEnduringNone, emptyCooldownQueue)
migratePersistentAccountStakeEnduringV2toV3 PersistentAccountStakeEnduringBaker{..} =
    case paseBakerPendingChange of
        RemoveStake _ -> do
            -- The baker is being removed, so we don't migrate it.
            -- Get the old stake, updating it to 0.
            cooldownAmount <- State.get
            State.put 0
            cooldown <- initialPrePreCooldownQueue cooldownAmount
            liftStakedBalanceStateTT addAccountInPrePreCooldown
            return (PersistentAccountStakeEnduringNone, cooldown)
        ReduceStake newStake _ -> do
            oldStake <- State.get
            unless (newStake <= oldStake) $
                error $
                    "Stake on baker 'reduced' from "
                        ++ show oldStake
                        ++ " to "
                        ++ show newStake
            State.put newStake
            cooldown <- initialPrePreCooldownQueue (oldStake - newStake)
            liftStakedBalanceStateTT addAccountInPrePreCooldown
            newPASE <- keepBakerInfo
            return (newPASE, cooldown)
        NoChange -> (,emptyCooldownQueue) <$> keepBakerInfo
  where
    keepBakerInfo = do
        newBakerInfo <- migrateReference (return . coerceBakerInfoExV1) paseBakerInfo
        return
            PersistentAccountStakeEnduringBaker
                { paseBakerInfo = newBakerInfo,
                  paseBakerPendingChange = NoChange,
                  ..
                }
migratePersistentAccountStakeEnduringV2toV3 PersistentAccountStakeEnduringDelegator{..} =
    case paseDelegatorPendingChange of
        RemoveStake _ -> do
            -- Get the old stake, updating it to 0.
            cooldownAmount <- State.get
            State.put 0
            cooldown <- initialPrePreCooldownQueue cooldownAmount
            liftStakedBalanceStateTT addAccountInPrePreCooldown
            return (PersistentAccountStakeEnduringNone, cooldown)
        _ -> do
            newTarget <- case paseDelegatorTarget of
                DelegatePassive -> return DelegatePassive
                DelegateToBaker bid -> do
                    removed <- liftStakedBalanceStateTT $ isBakerRemoved bid
                    return $ if removed then DelegatePassive else paseDelegatorTarget
            let newDelegatorInfo =
                    PersistentAccountStakeEnduringDelegator
                        { paseDelegatorPendingChange = NoChange,
                          paseDelegatorTarget = newTarget,
                          ..
                        }
            oldStake <- State.get
            case paseDelegatorPendingChange of
                ReduceStake newStake _ -> do
                    unless (newStake <= oldStake) $
                        error $
                            "Stake on delegator "
                                ++ show paseDelegatorId
                                ++ " 'reduced' from "
                                ++ show oldStake
                                ++ " to "
                                ++ show newStake
                    State.put newStake
                    cooldown <- initialPrePreCooldownQueue (oldStake - newStake)
                    liftStakedBalanceStateTT $ do
                        addAccountInPrePreCooldown
                        retainDelegator paseDelegatorId newStake newTarget
                    return $!! (newDelegatorInfo, cooldown)
                NoChange -> do
                    liftStakedBalanceStateTT $ retainDelegator paseDelegatorId oldStake newTarget
                    return $!! (newDelegatorInfo, emptyCooldownQueue)

-- | Migrate PersistentAccountStakeEnduring from AccountV3 to AccountV4. There
--  are no pending changes to take care of in V3, only the BakerInfoExV1 data
--  type needs to be migrated by setting the new `_bieIsSuspended` flag to
--  false.
migratePersistentAccountStakeEnduringV3toV4 ::
    (SupportMigration m t, AccountMigration 'AccountV4 (t m)) =>
    PersistentAccountStakeEnduring 'AccountV3 ->
    -- | Returns the new 'PersistentAccountStakeEnduring' and 'CooldownQueue'.
    t m (PersistentAccountStakeEnduring 'AccountV4)
migratePersistentAccountStakeEnduringV3toV4 PersistentAccountStakeEnduringNone =
    return PersistentAccountStakeEnduringNone
migratePersistentAccountStakeEnduringV3toV4 PersistentAccountStakeEnduringBaker{..} = do
    newBakerInfo <- migrateReference (return . (\BakerInfoExV1{..} -> BakerInfoExV1{_bieIsSuspended = CTrue False, ..})) paseBakerInfo
    return
        PersistentAccountStakeEnduringBaker
            { paseBakerInfo = newBakerInfo,
              paseBakerPendingChange = NoChange,
              ..
            }
migratePersistentAccountStakeEnduringV3toV4 PersistentAccountStakeEnduringDelegator{..} =
    return $!
        PersistentAccountStakeEnduringDelegator
            { paseDelegatorPendingChange = NoChange,
              ..
            }

-- | This relies on the fact that the 'AccountV2' hashing of 'AccountStake' is independent of the
--  staked amount.
instance (MonadBlobStore m) => MHashableTo m (AccountStakeHash 'AccountV2) (PersistentAccountStakeEnduring 'AccountV2) where
    getHashM stake = getHash <$> persistentToAccountStake stake 0

instance (MonadBlobStore m) => MHashableTo m (AccountStakeHash 'AccountV3) (PersistentAccountStakeEnduring 'AccountV3) where
    getHashM stake = getHash <$> persistentToAccountStake stake 0

instance (MonadBlobStore m) => MHashableTo m (AccountStakeHash 'AccountV4) (PersistentAccountStakeEnduring 'AccountV4) where
    getHashM stake = getHash <$> persistentToAccountStake stake 0

instance (MonadBlobStore m) => MHashableTo m (AccountStakeHash 'AccountV5) (PersistentAccountStakeEnduring 'AccountV5) where
    getHashM stake = getHash <$> persistentToAccountStake stake 0

-- * Enduring account data

-- | Enduring data associated with an account. This is data that does not change very often.
--  The 'AccountMerkleHash' is computed and stored for this data so that we can avoid deserializing
--  elements of it unnecessarily.
--
--  The persisting account data is stored under an 'EagerBufferedRef' as this includes the account
--  keys, which are required for any transaction originating from the account, and so are loaded
--  eagerly.
--
--  The encrypted amount is 'Nullable' so that accounts with no encrypted balance can be represented
--  succinctly.  The encrypted balance is stored as a 'LazyBufferedRef', which is not automatically
--  cached with the account, since loading the encrypted balance is relatively expensive and likely
--  not necessary for many operations.
--
--  The release schedule is likewise 'Nullable' and stored under a 'LazyBufferedRef' for similar reasons.
--  However, as well as the 'LazyBufferedRef', we store the locked balance (if it is non-trivial).
--  This is because we typically require ready access to the locked balance so that we can compute
--  the available balance.
--
--  The stake is not stored under a reference (excepting the baker info, as per the definition of
--  'PersistentAccountStakeEnduring'). This is since the information is relatively succinct.
data PersistentAccountEnduringData (av :: AccountVersion) = PersistentAccountEnduringData
    { -- | The Merkle hash computed from the other fields.
      paedHash :: !(AccountMerkleHash av),
      -- | A reference to the persisting account data.
      paedPersistingData :: !(EagerBufferedRef PersistingAccountData),
      -- | The encrypted amount. Invariant: if this is present, it will not satisfy
      --  'isInitialPersistentAccountEncryptedAmount'.
      paedEncryptedAmount :: !(Nullable (LazyBufferedRef PersistentAccountEncryptedAmount)),
      -- | The release schedule and total locked amount. Invariant: if this is present,
      --  it does not satisfy 'isEmptyAccountReleaseSchedule', and the amount will be the total of them.
      paedReleaseSchedule :: !(Nullable (LazyBufferedRef AccountReleaseSchedule, Amount)),
      -- | The staking details associated with the account.
      paedStake :: !(PersistentAccountStakeEnduring av),
      -- | The inactive stake in cooldown.
      paedStakeCooldown :: !(CooldownQueue av)
    }

-- | Get the locked amount from a 'PersistingAccountEnduringData'.
paedLockedAmount :: PersistentAccountEnduringData av -> Amount
paedLockedAmount PersistentAccountEnduringData{..} = case paedReleaseSchedule of
    Some (_, amt) -> amt
    Null -> 0

instance HashableTo (AccountMerkleHash av) (PersistentAccountEnduringData av) where
    getHash = paedHash

-- | Construct a 'PersistentAccountEnduringData' from the components by computing the hash.
--  Used for 'AccountV2'.
--
--  Precondition: if the 'PersistentAccountEncryptedAmount' is present then it must not satisfy
--  'isInitialPersistentAccountEncryptedAmount'.
--
--  Precondition: if the 'AccountReleaseSchedule' is present, then it must have some releases
--  and the total amount of the releases must be the provided amount.
makeAccountEnduringDataAV2 ::
    ( MonadBlobStore m
    ) =>
    EagerBufferedRef PersistingAccountData ->
    Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) ->
    Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) ->
    PersistentAccountStakeEnduring 'AccountV2 ->
    m (PersistentAccountEnduringData 'AccountV2)
makeAccountEnduringDataAV2 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake = do
    amhi2PersistingAccountDataHash <- getHashM paedPersistingData
    (amhi2AccountStakeHash :: AccountStakeHash 'AccountV2) <- getHashM paedStake
    amhi2EncryptedAmountHash <- case paedEncryptedAmount of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi2AccountReleaseScheduleHash <- case paedReleaseSchedule of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    let hashInputs :: AccountMerkleHashInputs 'AccountV2
        hashInputs = AccountMerkleHashInputsV2{..}
        !paedHash = getHash hashInputs
        paedStakeCooldown = emptyCooldownQueue
    return $! PersistentAccountEnduringData{..}

-- | Construct a 'PersistentAccountEnduringData' from the components by computing the hash.
--   Used for 'AccountV3'.
--
--  Precondition: if the 'PersistentAccountEncryptedAmount' is present then it must not satisfy
--  'isInitialPersistentAccountEncryptedAmount'.
--
--  Precondition: if the 'AccountReleaseSchedule' is present, then it must have some releases
--  and the total amount of the releases must be the provided amount.
makeAccountEnduringDataAV3 ::
    ( MonadBlobStore m
    ) =>
    EagerBufferedRef PersistingAccountData ->
    Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) ->
    Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) ->
    PersistentAccountStakeEnduring 'AccountV3 ->
    CooldownQueue 'AccountV3 ->
    m (PersistentAccountEnduringData 'AccountV3)
makeAccountEnduringDataAV3 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown = do
    amhi3PersistingAccountDataHash <- getHashM paedPersistingData
    (amhi3AccountStakeHash :: AccountStakeHash 'AccountV3) <- getHashM paedStake
    amhi3EncryptedAmountHash <- case paedEncryptedAmount of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi3AccountReleaseScheduleHash <- case paedReleaseSchedule of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi3Cooldown <- getHashM paedStakeCooldown
    let hashInputs :: AccountMerkleHashInputs 'AccountV3
        hashInputs = AccountMerkleHashInputsV3{..}
        !paedHash = getHash hashInputs
    return $! PersistentAccountEnduringData{..}

-- | Construct a 'PersistentAccountEnduringData' from the components by computing the hash.
--   Used for 'AccountV4'.
--
--  Precondition: if the 'PersistentAccountEncryptedAmount' is present then it must not satisfy
--  'isInitialPersistentAccountEncryptedAmount'.
--
--  Precondition: if the 'AccountReleaseSchedule' is present, then it must have some releases
--  and the total amount of the releases must be the provided amount.
makeAccountEnduringDataAV4 ::
    ( MonadBlobStore m
    ) =>
    EagerBufferedRef PersistingAccountData ->
    Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) ->
    Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) ->
    PersistentAccountStakeEnduring 'AccountV4 ->
    CooldownQueue 'AccountV4 ->
    m (PersistentAccountEnduringData 'AccountV4)
makeAccountEnduringDataAV4 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown = do
    amhi4PersistingAccountDataHash <- getHashM paedPersistingData
    (amhi4AccountStakeHash :: AccountStakeHash 'AccountV4) <- getHashM paedStake
    amhi4EncryptedAmountHash <- case paedEncryptedAmount of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi4AccountReleaseScheduleHash <- case paedReleaseSchedule of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi4Cooldown <- getHashM paedStakeCooldown
    let hashInputs :: AccountMerkleHashInputs 'AccountV4
        hashInputs = AccountMerkleHashInputsV4{..}
        !paedHash = getHash hashInputs
    return $! PersistentAccountEnduringData{..}

-- | Construct a 'PersistentAccountEnduringData' from the components by computing the hash.
--   Used for 'AccountV5'.
--
--  Precondition: if the 'PersistentAccountEncryptedAmount' is present then it must not satisfy
--  'isInitialPersistentAccountEncryptedAmount'.
--
--  Precondition: if the 'AccountReleaseSchedule' is present, then it must have some releases
--  and the total amount of the releases must be the provided amount.
makeAccountEnduringDataAV5 ::
    ( MonadBlobStore m
    ) =>
    EagerBufferedRef PersistingAccountData ->
    Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) ->
    Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) ->
    PersistentAccountStakeEnduring 'AccountV5 ->
    CooldownQueue 'AccountV5 ->
    m (PersistentAccountEnduringData 'AccountV5)
makeAccountEnduringDataAV5 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown = do
    amhi5PersistingAccountDataHash <- getHashM paedPersistingData
    (amhi5AccountStakeHash :: AccountStakeHash 'AccountV5) <- getHashM paedStake
    amhi5EncryptedAmountHash <- case paedEncryptedAmount of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi5AccountReleaseScheduleHash <- case paedReleaseSchedule of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi5Cooldown <- getHashM paedStakeCooldown
    let hashInputs :: AccountMerkleHashInputs 'AccountV5
        hashInputs = AccountMerkleHashInputsV5{..}
        !paedHash = getHash hashInputs
    return $! PersistentAccountEnduringData{..}

-- | [For internal use in this module.] Recompute the Merkle hash of the enduring account data,
--  for 'AccountV2'.
rehashAccountEnduringDataAV2 ::
    (MonadBlobStore m) =>
    PersistentAccountEnduringData 'AccountV2 ->
    m (PersistentAccountEnduringData 'AccountV2)
rehashAccountEnduringDataAV2 ed = do
    amhi2PersistingAccountDataHash <- getHashM (paedPersistingData ed)
    (amhi2AccountStakeHash :: AccountStakeHash 'AccountV2) <- getHashM (paedStake ed)
    amhi2EncryptedAmountHash <- case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi2AccountReleaseScheduleHash <- case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    let hashInputs = AccountMerkleHashInputsV2{..}
    return $! ed{paedHash = getHash hashInputs}

-- | [For internal use in this module.] Recompute the Merkle hash of the enduring account data,
--  for 'AccountV3'.
rehashAccountEnduringDataAV3 ::
    (MonadBlobStore m) =>
    PersistentAccountEnduringData 'AccountV3 ->
    m (PersistentAccountEnduringData 'AccountV3)
rehashAccountEnduringDataAV3 ed = do
    amhi3PersistingAccountDataHash <- getHashM (paedPersistingData ed)
    (amhi3AccountStakeHash :: AccountStakeHash 'AccountV3) <- getHashM (paedStake ed)
    amhi3EncryptedAmountHash <- case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi3AccountReleaseScheduleHash <- case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi3Cooldown <- getHashM $ paedStakeCooldown ed
    let hashInputs = AccountMerkleHashInputsV3{..}
    return $! ed{paedHash = getHash hashInputs}

rehashAccountEnduringDataAV4 ::
    (MonadBlobStore m) =>
    PersistentAccountEnduringData 'AccountV4 ->
    m (PersistentAccountEnduringData 'AccountV4)
rehashAccountEnduringDataAV4 ed = do
    amhi4PersistingAccountDataHash <- getHashM (paedPersistingData ed)
    (amhi4AccountStakeHash :: AccountStakeHash 'AccountV4) <- getHashM (paedStake ed)
    amhi4EncryptedAmountHash <- case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi4AccountReleaseScheduleHash <- case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi4Cooldown <- getHashM $ paedStakeCooldown ed
    let hashInputs = AccountMerkleHashInputsV4{..}
    return $! ed{paedHash = getHash hashInputs}

rehashAccountEnduringDataAV5 ::
    (MonadBlobStore m) =>
    PersistentAccountEnduringData 'AccountV5 ->
    m (PersistentAccountEnduringData 'AccountV5)
rehashAccountEnduringDataAV5 ed = do
    amhi5PersistingAccountDataHash <- getHashM (paedPersistingData ed)
    (amhi5AccountStakeHash :: AccountStakeHash 'AccountV5) <- getHashM (paedStake ed)
    amhi5EncryptedAmountHash <- case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmountHash
        Some e -> getHash <$> (loadPersistentAccountEncryptedAmount =<< refLoad e)
    amhi5AccountReleaseScheduleHash <- case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseScheduleHashV1
        Some (rs, _) -> getHashM rs
    amhi5Cooldown <- getHashM $ paedStakeCooldown ed
    let hashInputs = AccountMerkleHashInputsV5{..}
    return $! ed{paedHash = getHash hashInputs}

-- | [For internal use in this module.] Recompute the Merkle hash of the enduring account data.
rehashAccountEnduringData ::
    forall m av.
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    PersistentAccountEnduringData av ->
    m (PersistentAccountEnduringData av)
rehashAccountEnduringData = case accountVersion @av of
    SAccountV2 -> rehashAccountEnduringDataAV2
    SAccountV3 -> rehashAccountEnduringDataAV3
    SAccountV4 -> rehashAccountEnduringDataAV4
    SAccountV5 -> rehashAccountEnduringDataAV5

-- | Compute the 'EnduringDataFlags' from a 'PersistentAccountEnduringData' for the purposes of
--  storing the account.
enduringDataFlags ::
    forall av.
    (IsAccountVersion av) =>
    PersistentAccountEnduringData av ->
    EnduringDataFlags av
enduringDataFlags PersistentAccountEnduringData{..} =
    EnduringDataFlags
        { edHasEncryptedAmount = isNotNull paedEncryptedAmount,
          edHasReleaseSchedule = isNotNull paedReleaseSchedule,
          edStakeFlags = stakeFlags paedStake,
          edHasCooldown =
            conditionally
                (sSupportsFlexibleCooldown (accountVersion @av))
                (not $ isCooldownQueueEmpty paedStakeCooldown)
        }

-- * Enduring account data storage helper definitions

-- | The nature of a pending stake change, abstracting the details.
--  This is stored as the low-order 2 bits of a 'Word8'.
--  The encoding is as follows:
--
--  - Bits 1 and 0 are both unset if there is no pending change.
--
--  - Bit 1 is unset and bit 0 is set if the stake is being reduced.
--
--  - Bit 1 is set and bit 0 is unset if the stake is being removed.
data PendingChangeFlags
    = -- | No change is pending
      PendingChangeNone
    | -- | A stake reduction is pending
      PendingChangeReduce
    | -- | Removal of stake is pending
      PendingChangeRemove
    deriving (Eq, Ord, Show)

-- | Get the 'PendingChangeFlags' for a 'StakePendingChange''.
stakePendingChangeFlags :: StakePendingChange av -> PendingChangeFlags
stakePendingChangeFlags NoChange = PendingChangeNone
stakePendingChangeFlags ReduceStake{} = PendingChangeReduce
stakePendingChangeFlags RemoveStake{} = PendingChangeRemove

-- | Store a 'PendingChangeFlags' as the low-order 2 bits of a 'Word8'.
pendingChangeFlagsToBits :: PendingChangeFlags -> Word8
pendingChangeFlagsToBits PendingChangeNone = 0b00
pendingChangeFlagsToBits PendingChangeReduce = 0b01
pendingChangeFlagsToBits PendingChangeRemove = 0b10

-- | Load a 'PendingChangeFlags' from the low-order 2 bits of a 'Word8'.
--  All other bits must be 0.
pendingChangeFlagsFromBits :: Word8 -> Either String PendingChangeFlags
pendingChangeFlagsFromBits 0b00 = return PendingChangeNone
pendingChangeFlagsFromBits 0b01 = return PendingChangeReduce
pendingChangeFlagsFromBits 0b10 = return PendingChangeRemove
pendingChangeFlagsFromBits _ = Left "Invalid pending change type"

-- | Flags that represent the nature of the stake on an account.
--  These are stored as the low-order 6 bits of a 'Word8'.
--  The encoding is as follows:
--
--  - Bits 5 and 4 indicate the staking status of the account:
--
--    - If bits 5 and 4 are unset, there is no staking.
--      All other bits are unset.
--
--    - If bit 5 is unset and bit 4 is set, the account is a baker. In this case
--
--      - Bit 3 is unset.
--
--      - Bit 2 is set if earnings are restaked.
--
--      - Bits 1 and 0 indicate the pending change as described in 'PendingChangeFlags'.
--
--     - If bit 5 is set and bit 4 is unset, the account is a delegator. In this case
--
--       - Bit 3 is set if the delegation is passive.
--
--      - Bit 2 is set if earnings are restaked.
--
--      - Bits 1 and 0 indicate the pending change as described in 'PendingChangeFlags'.
--
--  - If the account version supports flexible cooldown, then bits 1 and 0 are always unset.
data StakeFlags
    = -- | The account is not staking
      StakeFlagsNone
    | -- | The account is a baker
      StakeFlagsBaker
        { -- | Whether earnings are restaked
          sfRestake :: !Bool,
          -- | The pending stake change, if any
          sfChangeType :: !PendingChangeFlags
        }
    | -- | The account is a delegator
      StakeFlagsDelegator
        { -- | Whether delegation is passive
          sfPassive :: !Bool,
          -- | Whether earnings are restaked
          sfRestake :: !Bool,
          -- | The pending stake change, if any
          sfChangeType :: !PendingChangeFlags
        }
    deriving (Eq, Ord, Show)

-- | Get the 'StakeFlags' from a 'PersistentAccountStakeEnduring'.
stakeFlags :: PersistentAccountStakeEnduring av -> StakeFlags
stakeFlags PersistentAccountStakeEnduringNone = StakeFlagsNone
stakeFlags PersistentAccountStakeEnduringBaker{..} =
    StakeFlagsBaker
        { sfRestake = paseBakerRestakeEarnings,
          sfChangeType = stakePendingChangeFlags paseBakerPendingChange
        }
stakeFlags PersistentAccountStakeEnduringDelegator{..} =
    StakeFlagsDelegator
        { sfPassive = DelegatePassive == paseDelegatorTarget,
          sfRestake = paseDelegatorRestakeEarnings,
          sfChangeType = stakePendingChangeFlags paseDelegatorPendingChange
        }

-- | Store a 'StakeFlags' as the low-order 6 bits of a 'Word8'.
stakeFlagsToBits :: StakeFlags -> Word8
stakeFlagsToBits StakeFlagsNone =
    0b00_0000
stakeFlagsToBits StakeFlagsBaker{..} =
    0b01_0000
        .|. (if sfRestake then 0b00_0100 else 0b00_0000)
        .|. pendingChangeFlagsToBits sfChangeType
stakeFlagsToBits StakeFlagsDelegator{..} =
    0b10_0000
        .|. (if sfPassive then 0b00_1000 else 0b00_0000)
        .|. (if sfRestake then 0b00_0100 else 0b00_0000)
        .|. pendingChangeFlagsToBits sfChangeType

-- | Load a 'StakeFlags' from the low-order 6 bits of a 'Word8'.
--  All other bits must be 0.
stakeFlagsFromBits :: Word8 -> Either String StakeFlags
stakeFlagsFromBits 0b00_0000 = return StakeFlagsNone
stakeFlagsFromBits bs = case bs .&. 0b11_0000 of
    0b01_0000 -> do
        when sfPassive $ Left "Passive bit cannot be set for baker"
        sfChangeType <- pendingChangeFlagsFromBits (bs .&. 0b11)
        return StakeFlagsBaker{..}
    0b10_0000 -> do
        sfChangeType <- pendingChangeFlagsFromBits (bs .&. 0b11)
        return StakeFlagsDelegator{..}
    _ -> Left "Invalid staking type"
  where
    sfRestake = testBit bs 2
    sfPassive = testBit bs 3

-- | Flags that represent the nature of enduring account data.
--  These are stored as a 'Word8', in the following format:
--
--  - Bit 7 is set if the account has a (non-initial) encrypted amount.
--
--  - Bit 6 is set if the account has a (non-empty) release schedule.
--
--  - If the account version supports flexible cooldowns, then bit 0 is set if the account has
--    a cooldown.
--
--  - The remaining bits indicate the staking status of the account, in accordance with
--    'StakeFlags'. (Note that bits 0 and 1 are used for the pending change type if the account
--    version does not support flexible cooldowns.)
data EnduringDataFlags (av :: AccountVersion) = EnduringDataFlags
    { -- | Whether the enduring data includes a (non-initial) encrypted amount.
      edHasEncryptedAmount :: !Bool,
      -- | Whether the enduring data includes a (non-empty) release schedule.
      edHasReleaseSchedule :: !Bool,
      -- | Flags describing the stake (if any).
      edStakeFlags :: !StakeFlags,
      -- | If supported by the account version, whether the account has a cooldown.
      edHasCooldown :: !(Conditionally (SupportsFlexibleCooldown av) Bool)
    }
    deriving (Eq, Ord, Show)

-- | Encode an 'EnduringDataFlags' as a 'Word8'.
enduringDataFlagsToBits :: EnduringDataFlags av -> Word8
enduringDataFlagsToBits EnduringDataFlags{..} =
    (if edHasEncryptedAmount then 0b1000_0000 else 0b0000_0000)
        .|. (if edHasReleaseSchedule then 0b0100_0000 else 0b0000_0000)
        .|. stakeFlagsToBits edStakeFlags
        .|. cooldownBits
  where
    cooldownBits = case edHasCooldown of
        CTrue True -> 0b0000_0001
        _ -> 0b0000_0000

-- | Decode an 'EnduringDataFlags' from a 'Word8'.
enduringDataFlagsFromBits ::
    forall av.
    (IsAccountVersion av) =>
    Word8 ->
    Either String (EnduringDataFlags av)
enduringDataFlagsFromBits bs = do
    let edHasEncryptedAmount = testBit bs 7
    let edHasReleaseSchedule = testBit bs 6
    case sSupportsFlexibleCooldown (accountVersion @av) of
        STrue -> do
            let edHasCooldown = CTrue (testBit bs 0)
            when (testBit bs 1) $ Left "Bit 1 must be unset for flexible cooldown"
            edStakeFlags <- stakeFlagsFromBits (bs .&. 0b0011_1100)
            return EnduringDataFlags{..}
        SFalse -> do
            let edHasCooldown = CFalse
            edStakeFlags <- stakeFlagsFromBits (bs .&. 0b0011_1111)
            return EnduringDataFlags{..}

instance (IsAccountVersion av) => Serialize (EnduringDataFlags av) where
    put = putWord8 . enduringDataFlagsToBits
    get = label "EnduringDataFlags" $ do
        bs <- getWord8
        case enduringDataFlagsFromBits bs of
            Left e -> fail e
            Right r -> return r

-- | 'PersistentAccountEnduringData' is stored in the following format:
--
--  1. The 'AccountMerkleHash'.
--  2. The 'EnduringDataFlags' (1 byte), which indicate which other fields are present.
--  3. A reference to the 'PersistingAccountData'.
--  4. If 'edHasEncryptedAmount' is set, a reference to the 'PersistentAccountEncryptedAmount'.
--  5. If 'edHasReleaseSchedule' is set, a reference to the 'AccountReleaseSchedule'.
--  6. Depending on 'edStakeFlags', one of:
--    - If it is 'StakeFlagsNone' then nothing else (there is no staking on the account).
--    - If it is 'StakeFlagsBaker' then:
--      1. A reference to the 'BakerInfoEx'.
--      2. Depending on 'sfChangeType', one of:
--         - If it is 'PendingChangeNone' then nothing else.
--         - If it is 'PendingChangeReduce' then the target amount and effective time.
--         - If it is 'PendingChangeRemove' then the effective time.
--    - If it is 'StakeFlagsDelegator' then:
--      1. The delegator Id
--      2. If 'sfPassive' is not set, then the target baker id.
--      3. Depending on 'sfChangeType', one of:
--         - If it is 'PendingChangeNone' then nothing else.
--         - If it is 'PendingChangeReduce' then the target amount and effective time.
--         - If it is 'PendingChangeRemove' then the effective time.
--  7. Depending on 'edHasCooldown':
--     - If flexible cooldown is supported and the value is @True@, a reference to the
--       'CooldownQueue'.
--     - Otherwise, nothing.
instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccountEnduringData av) where
    storeUpdate paed@PersistentAccountEnduringData{..} = do
        (ppd, newPersistingData) <- storeUpdate paedPersistingData
        (pea, newEncryptedAmount) <- storeUpdate paedEncryptedAmount
        (prs, newReleaseSchedule) <- storeUpdate paedReleaseSchedule
        (ps, newStake) <- suStake paedStake
        (psc, newStakeCooldown) <-
            if isCooldownQueueEmpty paedStakeCooldown
                then return (return (), paedStakeCooldown)
                else storeUpdate paedStakeCooldown
        let p = do
                put paedHash
                put flags
                ppd
                when (edHasEncryptedAmount flags) pea
                when (edHasReleaseSchedule flags) prs
                ps
                psc
            newpaed =
                paed
                    { paedPersistingData = newPersistingData,
                      paedEncryptedAmount = newEncryptedAmount,
                      paedReleaseSchedule = newReleaseSchedule,
                      paedStake = newStake,
                      paedStakeCooldown = newStakeCooldown
                    }
        return $!! (p, newpaed)
      where
        flags = enduringDataFlags paed
        -- The flags encode the type of the pending change, so serPC only serializes the data.
        serPC NoChange = return ()
        serPC (ReduceStake amt et) = put amt >> put et
        serPC (RemoveStake et) = put et
        suStake s@PersistentAccountStakeEnduringNone = return (return (), s)
        suStake s@PersistentAccountStakeEnduringBaker{..} = do
            (pinfo, newInfo) <- storeUpdate paseBakerInfo
            let ppc = serPC paseBakerPendingChange
            return $!! (pinfo >> ppc, s{paseBakerInfo = newInfo})
        suStake s@PersistentAccountStakeEnduringDelegator{..} = do
            let ptarget = case paseDelegatorTarget of
                    DelegatePassive -> return ()
                    DelegateToBaker b -> put b
            let ppc = serPC paseDelegatorPendingChange
            return $!! (put paseDelegatorId >> ptarget >> ppc, s)
    load = do
        paedHash <- get
        EnduringDataFlags{..} <- get @(EnduringDataFlags av)
        mPersistingData <- load
        mEncryptedAmount <-
            if edHasEncryptedAmount
                then load
                else return (return Null)
        mReleaseSchedule <- if edHasReleaseSchedule then load else return (return Null)
        let getPC PendingChangeNone = return NoChange
            getPC PendingChangeReduce = ReduceStake <$> get <*> get
            getPC PendingChangeRemove = RemoveStake <$> get
        mStake <- case edStakeFlags of
            StakeFlagsNone{} -> return (return PersistentAccountStakeEnduringNone)
            StakeFlagsBaker{..} -> do
                let paseBakerRestakeEarnings = sfRestake
                mInfo <- load
                paseBakerPendingChange <- getPC sfChangeType
                return $! do
                    paseBakerInfo <- mInfo
                    return PersistentAccountStakeEnduringBaker{..}
            StakeFlagsDelegator{..} -> do
                paseDelegatorId <- get
                let paseDelegatorRestakeEarnings = sfRestake
                paseDelegatorTarget <- if sfPassive then return DelegatePassive else DelegateToBaker <$> get
                paseDelegatorPendingChange <- getPC sfChangeType
                return . return $! PersistentAccountStakeEnduringDelegator{..}
        mStakeCooldown <- case edHasCooldown of
            CTrue True -> load
            _ -> return (return emptyCooldownQueue)
        return $! do
            paedPersistingData <- mPersistingData
            paedEncryptedAmount <- mEncryptedAmount
            paedReleaseSchedule <- mReleaseSchedule
            paedStake <- mStake
            paedStakeCooldown <- mStakeCooldown
            return PersistentAccountEnduringData{..}

-- * Persistent account

-- | A persistent account.
--  The most commonly modified fields of an account (the next nonce, balance and staked balance)
--  are directly available.  The rest of the fields are stored as part of the enduring data,
--  under an 'EagerBufferedRef'.  This limits the amount that needs to be rewritten for the
--  most common updates.
data PersistentAccount av = PersistentAccount
    { -- | The next nonce for transactions on the account.
      accountNonce :: !Nonce,
      -- | The total balance of the account.
      accountAmount :: !Amount,
      -- | The actively staked balance of the account.
      --  INVARIANT: This is 0 if the account is not a baker or delegator.
      accountStakedAmount :: !Amount,
      -- | The state table of the protocol level tokens of the account in ascending order of the TokenIndex.
      accountTokenStateTable :: !(Conditionally (SupportsPLT av) (Nullable (HashedBufferedRef' TokenStateTableHash TokenAccountStateTable))),
      -- | The enduring account data.
      accountEnduringData :: !(EagerBufferedRef (PersistentAccountEnduringData av))
    }

instance HashableTo (AccountHash 'AccountV2) (PersistentAccount 'AccountV2) where
    getHash PersistentAccount{..} =
        makeAccountHash $
            AHIV2 $
                AccountHashInputsV2
                    { ahi2NextNonce = accountNonce,
                      ahi2AccountBalance = accountAmount,
                      ahi2StakedBalance = accountStakedAmount,
                      ahi2MerkleHash = getHash accountEnduringData
                    }

instance (Monad m) => MHashableTo m (AccountHash 'AccountV2) (PersistentAccount 'AccountV2)

instance HashableTo (AccountHash 'AccountV3) (PersistentAccount 'AccountV3) where
    getHash PersistentAccount{..} =
        makeAccountHash $
            AHIV3 $
                AccountHashInputsV2
                    { ahi2NextNonce = accountNonce,
                      ahi2AccountBalance = accountAmount,
                      ahi2StakedBalance = accountStakedAmount,
                      ahi2MerkleHash = getHash accountEnduringData
                    }

instance HashableTo (AccountHash 'AccountV4) (PersistentAccount 'AccountV4) where
    getHash PersistentAccount{..} =
        makeAccountHash $
            AHIV4 $
                AccountHashInputsV2
                    { ahi2NextNonce = accountNonce,
                      ahi2AccountBalance = accountAmount,
                      ahi2StakedBalance = accountStakedAmount,
                      ahi2MerkleHash = getHash accountEnduringData
                    }

instance (Monad m) => MHashableTo m (AccountHash 'AccountV3) (PersistentAccount 'AccountV3)
instance (Monad m) => MHashableTo m (AccountHash 'AccountV4) (PersistentAccount 'AccountV4)
instance (MonadBlobStore m) => MHashableTo m (AccountHash 'AccountV5) (PersistentAccount 'AccountV5) where
    getHashM PersistentAccount{..} = do
        h <- case uncond accountTokenStateTable of
            Null -> return $ TokenStateTableHash $ getHash $ runPutLazy $ put "Nothing"
            Some ref -> getHashM ref
        return $
            makeAccountHash $
                AHIV5 $
                    AccountHashInputsV3
                        { ahi3NextNonce = accountNonce,
                          ahi3AccountBalance = accountAmount,
                          ahi3StakedBalance = accountStakedAmount,
                          ahi3MerkleHash = getHash accountEnduringData,
                          ahi3TokenStateTableHash = h
                        }

instance HashableTo Hash.Hash (PersistentAccount 'AccountV2) where
    getHash = theAccountHash @'AccountV2 . getHash

instance (Monad m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV2)

instance HashableTo Hash.Hash (PersistentAccount 'AccountV3) where
    getHash = theAccountHash @'AccountV3 . getHash

instance HashableTo Hash.Hash (PersistentAccount 'AccountV4) where
    getHash = theAccountHash @'AccountV4 . getHash

instance (Monad m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV3)
instance (Monad m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV4)
instance (MonadBlobStore m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV5) where
    getHashM acc = theAccountHash @'AccountV5 <$> getHashM acc

instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccount av) where
    storeUpdate acc@PersistentAccount{..} = do
        (pAccountTokenStateTable :: Put, newAccountTokenStateTable) <- case accountTokenStateTable of
            CFalse -> return (return (), CFalse)
            CTrue tokens -> do
                (pTokens, newTokens) <- storeUpdate tokens
                return (pTokens, CTrue newTokens)
        (pEnduringData, newEnduringData) <- storeUpdate accountEnduringData
        let p = do
                put accountNonce
                put accountAmount
                put accountStakedAmount
                pAccountTokenStateTable
                pEnduringData
        return $!! (p, acc{accountEnduringData = newEnduringData, accountTokenStateTable = newAccountTokenStateTable})
    load = do
        accountNonce <- get
        accountAmount <- get
        accountStakedAmount <- get
        mAccountTokenStateTable <- case sSupportsPLT (accountVersion @av) of
            STrue -> do
                mAccountStateTable <- load
                return $ CTrue <$> mAccountStateTable
            SFalse -> return $ return CFalse
        mEnduringData <- load
        return $ do
            accountTokenStateTable <- mAccountTokenStateTable
            accountEnduringData <- mEnduringData
            return $! PersistentAccount{..}

-- | Get the enduring data for an account.
enduringData :: PersistentAccount av -> PersistentAccountEnduringData av
enduringData = eagerBufferedDeref . accountEnduringData

-- | Get the persisting data for an account.
persistingData :: PersistentAccount av -> PersistingAccountData
persistingData = eagerBufferedDeref . paedPersistingData . enduringData

-- ** Queries

-- | Get the canonical address of the account.
getCanonicalAddress :: (Monad m) => PersistentAccount av -> m AccountAddress
getCanonicalAddress acc = do
    let pd = persistingData acc
    return $! pd ^. accountAddress

-- | Get the current public account balance.
getAmount :: (Monad m) => PersistentAccount av -> m Amount
getAmount = pure . accountAmount

-- | Gets the amount of a baker's stake, or 'Nothing' if the account is not a baker.
getBakerStakeAmount :: (Monad m) => PersistentAccount av -> m (Maybe Amount)
getBakerStakeAmount acc = do
    let ed = enduringData acc
    return $! case paedStake ed of
        PersistentAccountStakeEnduringBaker{} -> Just $! accountStakedAmount acc
        _ -> Nothing

-- | Get the amount that is actively staked on the account.
getActiveStakedAmount :: (Monad m) => PersistentAccount av -> m Amount
getActiveStakedAmount acc = return $! accountStakedAmount acc

-- | Get the total amount that is staked on the account including the active stake (for a validator
-- or delegator) and the inactive stake (in cooldown).
-- For account versions prior to 'AccountV3', this is the same as 'getActiveStakedAmount'.
getTotalStakedAmount :: (Monad m) => PersistentAccount av -> m Amount
getTotalStakedAmount acc = return $! activeStake + inactiveStake
  where
    activeStake = accountStakedAmount acc
    inactiveStake = cooldownStake $ paedStakeCooldown (enduringData acc)

-- | Get the amount that is locked in scheduled releases on the account.
getLockedAmount :: (Monad m) => PersistentAccount av -> m Amount
getLockedAmount acc = do
    let ed = enduringData acc
    return $! paedLockedAmount ed

-- | Get the current public account available balance.
-- This accounts for lock-up and staked amounts.
-- @available = total - max locked staked@ where
-- @staked = active + inactive@.
getAvailableAmount :: (Monad m) => PersistentAccount av -> m Amount
getAvailableAmount acc = do
    let ed = enduringData acc
        activeStake = accountStakedAmount acc
        inactiveStake = cooldownStake $ paedStakeCooldown ed
        stake = activeStake + inactiveStake
    return $! accountAmount acc - max stake (paedLockedAmount ed)

-- | Get the next account nonce for transactions from this account.
getNonce :: (Monad m) => PersistentAccount av -> m Nonce
getNonce = pure . accountNonce

-- | Determine if a given operation is permitted for the account.
--
--  * For 'AllowedEncryptedTransfers' the account may only have 1 credential.
--
--  * For 'AllowedMultipleCredentials' the account must have the empty encrypted balance.
isAllowed :: (MonadBlobStore m) => PersistentAccount av -> AccountAllowance -> m Bool
isAllowed acc AllowedEncryptedTransfers = do
    creds <- getCredentials acc
    return $! Map.size creds == 1
isAllowed acc AllowedMultipleCredentials = do
    let ed = enduringData acc
    -- We use the invariant that if the encrypted amount is present then it will be non-empty.
    case paedEncryptedAmount ed of
        Null -> return True
        Some eaRef -> isZeroPersistentAccountEncryptedAmount =<< refLoad eaRef

-- | Get the credentials deployed on the account. This map is always non-empty and (presently)
--  will have a credential at index 'initialCredentialIndex' (0) that cannot be changed.
getCredentials :: (Monad m) => PersistentAccount av -> m (Map.Map CredentialIndex RawAccountCredential)
getCredentials acc = do
    let pd = persistingData acc
    return $! pd ^. accountCredentials

-- | Get the key used to verify transaction signatures, it records the signature scheme used as well.
getVerificationKeys :: (Monad m) => PersistentAccount av -> m AccountInformation
getVerificationKeys acc = do
    let pd = persistingData acc
    return $! pd ^. accountVerificationKeys

-- | Get the current encrypted amount on the account.
getEncryptedAmount :: (MonadBlobStore m) => PersistentAccount av -> m AccountEncryptedAmount
getEncryptedAmount acc = do
    let ed = enduringData acc
    case paedEncryptedAmount ed of
        Null -> return initialAccountEncryptedAmount
        Some ea -> loadPersistentAccountEncryptedAmount =<< refLoad ea

-- | Get the public key used to receive encrypted amounts.
getEncryptionKey :: (MonadBlobStore f) => PersistentAccount av -> f AccountEncryptionKey
getEncryptionKey acc = do
    let pd = persistingData acc
    -- The use of the unsafe @unsafeEncryptionKeyFromRaw@ function here is
    -- justified because the encryption key was validated when it was
    -- created/deployed (this is part of credential validation)
    return $! unsafeEncryptionKeyFromRaw (pd ^. accountEncryptionKey)

-- | Get the release schedule for an account.
getReleaseSummary :: (MonadBlobStore m) => PersistentAccount av -> m AccountReleaseSummary
getReleaseSummary acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return (AccountReleaseSummary 0 [])
        Some (rsRef, _) -> toAccountReleaseSummary =<< refLoad rsRef

-- | Get the release schedule for an account.
getReleaseSchedule :: (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) => PersistentAccount av -> m (TARS.AccountReleaseSchedule av)
getReleaseSchedule acc = do
    let ed = enduringData acc
    TARS.fromAccountReleaseScheduleV1 <$> case paedReleaseSchedule ed of
        Null -> return TARSV1.emptyAccountReleaseSchedule
        Some (rsRef, total) -> getAccountReleaseSchedule total =<< refLoad rsRef

-- | Get the timestamp at which the next scheduled release will occur (if any).
getNextReleaseTimestamp :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe Timestamp)
getNextReleaseTimestamp acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return Nothing
        Some (rsRef, _) -> nextReleaseTimestamp <$!> refLoad rsRef

-- | Get the baker (if any) attached to an account.
getBaker :: (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountBaker av))
getBaker acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> do
            abi <- refLoad paseBakerInfo
            let !bkr =
                    AccountBaker
                        { _stakedAmount = accountStakedAmount acc,
                          _stakeEarnings = paseBakerRestakeEarnings,
                          _accountBakerInfo = abi,
                          _bakerPendingChange = paseBakerPendingChange
                        }
            return $ Just bkr
        _ -> return Nothing

-- | Get a reference to the baker info (if any) attached to an account.
getBakerInfoRef ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    m (Maybe (PersistentBakerInfoEx av))
getBakerInfoRef acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> return $ Just paseBakerInfo
        _ -> return Nothing

-- | Get the baker and baker info reference (if any) attached to the account.
getBakerAndInfoRef :: (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountBaker av, PersistentBakerInfoEx av))
getBakerAndInfoRef acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} -> do
            bi <- refLoad paseBakerInfo
            let !bkr =
                    AccountBaker
                        { _stakedAmount = accountStakedAmount acc,
                          _stakeEarnings = paseBakerRestakeEarnings,
                          _accountBakerInfo = bi,
                          _bakerPendingChange = paseBakerPendingChange
                        }
            return $ Just (bkr, paseBakerInfo)
        _ -> return Nothing

-- | Get the delegator (if any) attached to the account.
getDelegator :: (MonadBlobStore m, AVSupportsDelegation av) => PersistentAccount av -> m (Maybe (AccountDelegation av))
getDelegator acc = do
    let ed = enduringData acc
    case paedStake ed of
        PersistentAccountStakeEnduringDelegator{..} -> do
            let !del =
                    AccountDelegationV1
                        { _delegationIdentity = paseDelegatorId,
                          _delegationStakedAmount = accountStakedAmount acc,
                          _delegationStakeEarnings = paseDelegatorRestakeEarnings,
                          _delegationTarget = paseDelegatorTarget,
                          _delegationPendingChange = paseDelegatorPendingChange
                        }
            return $ Just del
        _ -> return Nothing

-- | Get the baker or stake delegation information attached to an account.
getStake ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    PersistentAccount av ->
    m (AccountStake av)
getStake acc = do
    let ed = enduringData acc
    persistentToAccountStake (paedStake ed) (accountStakedAmount acc)

-- | Determine if an account has stake as a baker or delegator.
hasActiveStake :: PersistentAccount av -> Bool
hasActiveStake acc = case paedStake (enduringData acc) of
    PersistentAccountStakeEnduringNone -> False
    _ -> True

-- | Get details about an account's stake.
getStakeDetails :: (MonadBlobStore m, AVSupportsDelegation av) => PersistentAccount av -> m (StakeDetails av)
getStakeDetails acc = do
    let ed = enduringData acc
    return $! case paedStake ed of
        PersistentAccountStakeEnduringBaker{..} ->
            StakeDetailsBaker
                { sdStakedCapital = accountStakedAmount acc,
                  sdRestakeEarnings = paseBakerRestakeEarnings,
                  sdPendingChange = paseBakerPendingChange
                }
        PersistentAccountStakeEnduringDelegator{..} ->
            StakeDetailsDelegator
                { sdStakedCapital = accountStakedAmount acc,
                  sdRestakeEarnings = paseDelegatorRestakeEarnings,
                  sdPendingChange = paseDelegatorPendingChange,
                  sdDelegationTarget = paseDelegatorTarget
                }
        PersistentAccountStakeEnduringNone -> StakeDetailsNone

getStakeCooldown ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    m (CooldownQueue av)
getStakeCooldown acc = do
    let ed = enduringData acc
    return $ paedStakeCooldown ed

getCooldowns ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    PersistentAccount av ->
    m (Maybe Cooldowns)
getCooldowns =
    getStakeCooldown >=> \case
        EmptyCooldownQueue -> return Nothing
        CooldownQueue ref -> Just <$> refLoad ref

-- | Load the token state table (if present) of the given account in memory.
getTokenStateTable ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    m (Conditionally (SupportsPLT av) (Nullable (Map.Map TokenIndex TokenAccountState)))
getTokenStateTable acc = forM (accountTokenStateTable acc) $ \mbRef -> do
    mbTast <- forM mbRef refLoad
    forM mbTast $ \(TokenAccountStateTable tast) -> traverse refLoad tast

-- | Get the balance of a protocol-level token held by an account.
--  This is only available at account versions that support protocol-level tokens.
getTokenBalance ::
    (MonadBlobStore m, AVSupportsPLT av) =>
    PersistentAccount av ->
    TokenIndex ->
    m TokenRawAmount
getTokenBalance acc tokenIx = do
    case uncond (accountTokenStateTable acc) of
        Null -> return 0
        Some ref -> do
            table <- refLoad ref
            case Map.lookup tokenIx (tokenAccountStateTable table) of
                Nothing -> return 0
                Just tokenStateRef -> do
                    tokenState <- refLoad tokenStateRef
                    return $ tasBalance tokenState

-- | Look up the 'TokenStateValue' associated with a particular token and key on an account.
--  This is only available at account versions that support protocol-level tokens.
getTokenState ::
    (MonadBlobStore m, AVSupportsPLT av) =>
    PersistentAccount av ->
    TokenIndex ->
    TokenStateKey ->
    m (Maybe TokenStateValue)
getTokenState acc tokenIx key = do
    case uncond (accountTokenStateTable acc) of
        Null -> return Nothing
        Some ref -> do
            table <- refLoad ref
            case Map.lookup tokenIx (tokenAccountStateTable table) of
                Nothing -> return Nothing
                Just tokenStateRef -> do
                    tokenState <- refLoad tokenStateRef
                    return $ Map.lookup key (tasModuleState tokenState)

-- ** Updates

-- | Apply account updates to an account. It is assumed that the address in
--  account updates and account are the same.
updateAccount ::
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    AccountUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccount !upd !acc0 = do
    let ed0 = enduringData acc0
    (ed1, enduringRehash1, additionalLocked) <- case upd ^. auReleaseSchedule of
        Just l -> do
            let newLockedFunds = amountToDelta $ foldl' (+) 0 (concatMap (\(values, _) -> map snd values) l)
            (rData, oldLockedAmt) <- case paedReleaseSchedule ed0 of
                Null -> return (emptyAccountReleaseSchedule, 0)
                Some (rsRef, lockedAmt) -> (,lockedAmt) <$> refLoad rsRef
            newReleaseSchedule <- foldlM (flip addReleases) rData l
            releaseScheduleRef <-
                if isEmptyAccountReleaseSchedule newReleaseSchedule
                    then return Null
                    else do
                        !ref <- refMake newReleaseSchedule
                        let !lockedAmt = applyAmountDelta newLockedFunds oldLockedAmt
                        return (Some (ref, lockedAmt))
            let ed1 = ed0{paedReleaseSchedule = releaseScheduleRef}
            return (ed1, True, newLockedFunds)
        Nothing -> return (ed0, False, 0)
    (ed2, enduringRehash2) <- case upd ^. auEncrypted of
        Just encUpd -> do
            oldEncAmount <- case paedEncryptedAmount ed1 of
                Null -> initialPersistentAccountEncryptedAmount
                Some eaRef -> refLoad eaRef
            newEncryptedAmount <-
                ( case encUpd of
                    Add{..} -> addIncomingEncryptedAmount newAmount
                    ReplaceUpTo{..} -> replaceUpTo aggIndex newAmount
                    AddSelf{..} -> addToSelfEncryptedAmount newAmount
                    )
                    oldEncAmount
            isInitial <- isInitialPersistentAccountEncryptedAmount newEncryptedAmount
            encryptedAmountRef <-
                if isInitial
                    then return Null
                    else Some <$> refMake newEncryptedAmount
            let ed2 = ed1{paedEncryptedAmount = encryptedAmountRef}
            return (ed2, True)
        Nothing -> return (ed1, enduringRehash1)
    acc1 <-
        if enduringRehash2
            then do
                edRef <- refMake =<< rehashAccountEnduringData ed2
                return $! acc0{accountEnduringData = edRef}
            else return acc0
    let acc2 = case upd ^. auNonce of
            Nothing -> acc1
            Just n -> acc1{accountNonce = n}
    let !acc3 =
            acc2
                { accountAmount =
                    applyAmountDelta (additionalLocked + upd ^. auAmount . non 0) (accountAmount acc2)
                }
    return acc3

-- | Helper function. Apply an update to the 'PersistentAccountEnduringData' on an account,
--  recomputing the hash.
updateEnduringData ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    (PersistentAccountEnduringData av -> m (PersistentAccountEnduringData av)) ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateEnduringData f acc = do
    let ed = enduringData acc
    newEnduring <- refMake =<< rehashAccountEnduringData =<< f ed
    return $! acc{accountEnduringData = newEnduring}

-- | Apply an update to the 'PersistingAccountData' on an account, recomputing the hash.
updatePersistingData ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    (PersistingAccountData -> PersistingAccountData) ->
    PersistentAccount av ->
    m (PersistentAccount av)
updatePersistingData f = updateEnduringData $ \ed -> do
    let pd = eagerBufferedDeref (paedPersistingData ed)
    newPersisting <- refMake $! f pd
    return $! ed{paedPersistingData = newPersisting}

-- | Helper function. Update the 'PersistentAccountStakeEnduring' component of an account.
updateStake ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    (PersistentAccountStakeEnduring av -> m (PersistentAccountStakeEnduring av)) ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateStake f = updateEnduringData $ \ed -> do
    newStake <- f (paedStake ed)
    return $! ed{paedStake = newStake}

-- | Add or remove credentials on an account.
--  The caller must ensure the following, which are not checked:
--
--  * Any credential index that is removed must already exist.
--  * The credential with index 0 must not be removed.
--  * Any credential index that is added must not exist after the removals take effect.
--  * At least one credential remains after all removals and additions.
--  * Any new threshold is at most the number of accounts remaining (and at least 1).
updateAccountCredentials ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    -- | Credentials to remove
    [CredentialIndex] ->
    -- | Credentials to add
    Map.Map CredentialIndex AccountCredential ->
    -- | New account threshold
    AccountThreshold ->
    -- | Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentials cuRemove cuAdd cuAccountThreshold =
    updatePersistingData (updateCredentials cuRemove cuAdd cuAccountThreshold)

-- | Optionally update the verification keys and signature threshold for an account.
--  Precondition: The credential with given credential index exists.
updateAccountCredentialKeys ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    -- | Credential to update
    CredentialIndex ->
    -- | New public keys
    CredentialPublicKeys ->
    -- | Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentialKeys credIndex credKeys = updatePersistingData (updateCredentialKeys credIndex credKeys)

-- | Add an amount to the account's balance.
addAmount :: (Monad m) => Amount -> PersistentAccount av -> m (PersistentAccount av)
addAmount !amt acc = return $! acc{accountAmount = accountAmount acc + amt}

-- | Add a baker to an account for account version 1.
--  This will replace any existing staking information on the account.
addBakerV1 ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    -- | Extended baker info
    BakerInfoEx av ->
    -- | Baker's equity capital
    Amount ->
    -- | Whether earnings are restaked
    Bool ->
    -- | Account to add baker to
    PersistentAccount av ->
    m (PersistentAccount av)
addBakerV1 binfo stake restake acc = do
    let ed = enduringData acc
    binfoRef <- refMake $! binfo
    let baker =
            PersistentAccountStakeEnduringBaker
                { paseBakerRestakeEarnings = restake,
                  paseBakerInfo = binfoRef,
                  paseBakerPendingChange = NoChange
                }
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStake = baker}
    return $!
        acc
            { accountStakedAmount = stake,
              accountEnduringData = newEnduring
            }

-- | Add a delegator to an account.
--  This will replace any existing staking information on the account.
addDelegator ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    AccountDelegation av ->
    PersistentAccount av ->
    m (PersistentAccount av)
addDelegator AccountDelegationV1{..} acc = do
    let ed = enduringData acc
    let del =
            PersistentAccountStakeEnduringDelegator
                { paseDelegatorId = _delegationIdentity,
                  paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                  paseDelegatorTarget = _delegationTarget,
                  paseDelegatorPendingChange = NoChange
                }
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStake = del}
    return $!
        acc
            { accountStakedAmount = _delegationStakedAmount,
              accountEnduringData = newEnduring
            }

-- | Update the pool info on a baker account.
--  This MUST only be called with an account that is a baker.
updateBakerPoolInfo ::
    ( MonadBlobStore m,
      AVSupportsDelegation av,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    BakerPoolInfoUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateBakerPoolInfo upd = updateEnduringData $ \ed -> case paedStake ed of
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo = oldInfo & bieBakerPoolInfo %~ applyBakerPoolInfoUpdate upd
        newInfoRef <- refMake $! newInfo
        return $! ed{paedStake = baker{paseBakerInfo = newInfoRef}}
    PersistentAccountStakeEnduringDelegator{} ->
        error "updateBakerPoolInfo invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "updateBakerPoolInfo invariant violation: account is not a baker"

-- | Set the baker keys on a baker account.
--  This MUST only be called with an account that is a baker.
setBakerKeys ::
    ( MonadBlobStore m,
      AVSupportsDelegation av,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    BakerKeyUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
setBakerKeys upd = updateStake $ \case
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo =
                oldInfo
                    & bieBakerInfo
                        %~ ( (bakerAggregationVerifyKey .~ bkuAggregationKey upd)
                                . (bakerSignatureVerifyKey .~ bkuSignKey upd)
                                . (bakerElectionVerifyKey .~ bkuElectionKey upd)
                           )
        newInfoRef <- refMake $! newInfo
        return $! baker{paseBakerInfo = newInfoRef}
    PersistentAccountStakeEnduringDelegator{} ->
        error "setBakerKeys invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "setBakerKeys invariant violation: account is not a baker"

-- | Set the stake of a baker or delegator account.
--  This MUST only be called with an account that is either a baker or delegator.
--  This does no check that the staked amount is sensible, and has no effect on pending changes.
setStake ::
    (Monad m) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
setStake newStake acc = return $! acc{accountStakedAmount = newStake}

-- | Set the suspended state of a validator account.
--  This MUST only be called with an account that is a validator.
setValidatorSuspended ::
    forall av m.
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1,
      AVSupportsDelegation av,
      AVSupportsValidatorSuspension av
    ) =>
    Bool ->
    PersistentAccount av ->
    m (PersistentAccount av)
setValidatorSuspended isSusp = updateStake $ \case
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo = oldInfo & bieIsSuspended .~ isSusp
        newInfoRef <- refMake $! newInfo
        return $! baker{paseBakerInfo = newInfoRef}
    PersistentAccountStakeEnduringDelegator{} ->
        error "setValidatorSuspended invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "setValidatorSuspended invariant violation: account is not a baker"

-- | Add a specified amount to the pre-pre-cooldown inactive stake.
addPrePreCooldown ::
    forall m av.
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1,
      AVSupportsFlexibleCooldown av
    ) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
addPrePreCooldown amt = updateEnduringData $ \ed -> do
    newStakeCooldown <- CooldownQueue.addPrePreCooldown amt (paedStakeCooldown ed)
    return $! ed{paedStakeCooldown = newStakeCooldown}

-- | Remove up to the given amount from the cooldowns, starting with pre-pre-cooldown, then
--  pre-cooldown, and finally from the amounts in cooldown, in decreasing order of timestamp.
reactivateCooldownAmount ::
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1,
      AVSupportsFlexibleCooldown av
    ) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
reactivateCooldownAmount amt acc = case paedStakeCooldown (enduringData acc) of
    EmptyCooldownQueue -> return acc
    _ -> updateEnduringData reactivate acc
  where
    reactivate ed = do
        newStakeCooldown <- CooldownQueue.reactivateCooldownAmount amt (paedStakeCooldown ed)
        return $! ed{paedStakeCooldown = newStakeCooldown}

-- | Set whether a baker or delegator account restakes its earnings.
--  This MUST only be called with an account that is either a baker or delegator.
setRestakeEarnings ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    Bool ->
    PersistentAccount av ->
    m (PersistentAccount av)
setRestakeEarnings newRestake =
    updateStake $
        return . \case
            baker@PersistentAccountStakeEnduringBaker{} ->
                baker{paseBakerRestakeEarnings = newRestake}
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorRestakeEarnings = newRestake}
            PersistentAccountStakeEnduringNone -> error "setRestakeEarnings invariant violation: account is not a baker or delegator"

-- | Set the pending change on baker or delegator account.
--  This MUST only be called with an account that is either a baker or delegator.
setStakePendingChange ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    StakePendingChange av ->
    PersistentAccount av ->
    m (PersistentAccount av)
setStakePendingChange newPC =
    updateStake $
        return . \case
            baker@PersistentAccountStakeEnduringBaker{} ->
                baker{paseBakerPendingChange = newPC}
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorPendingChange = newPC}
            PersistentAccountStakeEnduringNone -> error "setStakePendingChange invariant violation: account is not a baker or delegator"

-- | Set the target of a delegating account.
--  This MUST only be called with an account that is a delegator.
setDelegationTarget ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    DelegationTarget ->
    PersistentAccount av ->
    m (PersistentAccount av)
setDelegationTarget newTarget =
    updateStake $
        return . \case
            del@PersistentAccountStakeEnduringDelegator{} ->
                del{paseDelegatorTarget = newTarget}
            PersistentAccountStakeEnduringBaker{} ->
                error "setDelegationTarget invariant violation: account is not a delegator"
            PersistentAccountStakeEnduringNone ->
                error "setDelegationTarget invariant violation: account is not a delegator"

-- | Remove any staking on an account.
removeStaking ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    PersistentAccount av ->
    m (PersistentAccount av)
removeStaking acc0 = do
    acc1 <- updateStake (const $ return PersistentAccountStakeEnduringNone) acc0
    return $! acc1{accountStakedAmount = 0}

-- | Set the commission rates on a baker account.
--  This MUST only be called with an account that is a baker.
setCommissionRates ::
    ( MonadBlobStore m,
      IsAccountVersion av,
      AVSupportsDelegation av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    CommissionRates ->
    PersistentAccount av ->
    m (PersistentAccount av)
setCommissionRates rates = updateStake $ \case
    baker@PersistentAccountStakeEnduringBaker{} -> do
        oldInfo <- refLoad (paseBakerInfo baker)
        let newInfo = oldInfo & bieBakerPoolInfo . poolCommissionRates .~ rates
        newInfoRef <- refMake $! newInfo
        return $! baker{paseBakerInfo = newInfoRef}
    PersistentAccountStakeEnduringDelegator{} ->
        error "setCommissionRates invariant violation: account is not a baker"
    PersistentAccountStakeEnduringNone ->
        error "setCommissionRates invariant violation: account is not a baker"

-- | Unlock scheduled releases on an account up to and including the given timestamp.
--  This returns the next timestamp at which a release is scheduled for the account, if any,
--  as well as the updated account.
unlockReleases ::
    (MonadBlobStore m, IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV1) =>
    Timestamp ->
    PersistentAccount av ->
    m (Maybe Timestamp, PersistentAccount av)
unlockReleases ts acc = do
    let ed = enduringData acc
    case paedReleaseSchedule ed of
        Null -> return (Nothing, acc)
        Some (oldRSRef, oldLockedAmt) -> do
            oldRS <- refLoad oldRSRef
            (unlockedAmt, nextTimestamp, newRS) <- unlockAmountsUntil ts oldRS
            newLocked <-
                if isEmptyAccountReleaseSchedule newRS
                    then return Null
                    else do
                        newRSRef <- refMake newRS
                        (return . Some) $!! (newRSRef, oldLockedAmt - unlockedAmt)
            newEnduring <- refMake =<< rehashAccountEnduringData ed{paedReleaseSchedule = newLocked}
            let !newAcc = acc{accountEnduringData = newEnduring}
            return (nextTimestamp, newAcc)

-- | Process the cooldowns on an account up to and including the given timestamp.
--  This returns the next timestamp at which a cooldown expires, if any.
--
--  Note: this should only be called if the account has cooldowns which expire at or before the
--  given timestamp, as otherwise the account will be updated unnecessarily.
processCooldownsUntil ::
    ( MonadBlobStore m,
      AVSupportsFlexibleCooldown av,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    -- | Release all cooldowns up to and including this timestamp.
    Timestamp ->
    PersistentAccount av ->
    m (Maybe Timestamp, PersistentAccount av)
processCooldownsUntil ts acc = do
    let ed = enduringData acc
    (nextTimestamp, newQueue) <- CooldownQueue.processCooldownsUntil ts (paedStakeCooldown ed)
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStakeCooldown = newQueue}
    return (nextTimestamp, acc{accountEnduringData = newEnduring})

-- | Move the pre-cooldown amount on an account into cooldown with the specified release time.
--  This returns @Just (Just ts)@ if the previous next cooldown time was @ts@, but the new next
--  cooldown (i.e. the supplied timestamp) time is earlier. It returns @Just Nothing@ if the account
--  did not have a cooldown but now does. Otherwise, it returns @Nothing@.
--
--  Note: this should only be called if the account has a pre-cooldown, as otherwise the account
--  will be updated unnecessarily.
processPreCooldown ::
    ( MonadBlobStore m,
      AVSupportsFlexibleCooldown av,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    Timestamp ->
    PersistentAccount av ->
    m (NextCooldownChange, PersistentAccount av)
processPreCooldown ts acc = do
    let ed = enduringData acc
    (res, newQueue) <- CooldownQueue.processPreCooldown ts (paedStakeCooldown ed)
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStakeCooldown = newQueue}
    return (res, acc{accountEnduringData = newEnduring})

-- | Move the pre-pre-cooldown amount on an account into pre-cooldown.
--  It should be the case that the account has a pre-pre-cooldown amount and no pre-cooldown amount.
--  However, if there is no pre-pre-cooldown amount, this will do nothing, and if there is already
--  a pre-cooldown amount, the pre-pre-cooldown amount will be added to it.
--
--  Note: this should only be called if the account has a pre-pre-cooldown, as otherwise the account
--  will be updated unnecessarily.
processPrePreCooldown ::
    ( MonadBlobStore m,
      AVSupportsFlexibleCooldown av,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    PersistentAccount av ->
    m (PersistentAccount av)
processPrePreCooldown acc = do
    let ed = enduringData acc
    newQueue <- CooldownQueue.processPrePreCooldown (paedStakeCooldown ed)
    newEnduring <- refMake =<< rehashAccountEnduringData ed{paedStakeCooldown = newQueue}
    return acc{accountEnduringData = newEnduring}

-- ** Creation

-- | Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount ::
    forall m av.
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1,
      TARS.AccountReleaseSchedule' av ~ TARSV1.AccountReleaseSchedule
    ) =>
    Transient.Account av ->
    m (PersistentAccount av)
makePersistentAccount Transient.Account{..} = do
    paedPersistingData :: EagerBufferedRef PersistingAccountData <- refMake $! _unhashed _accountPersisting
    (accountStakedAmount, !paedStake) <- case _accountStaking of
        AccountStakeNone -> return (0, PersistentAccountStakeEnduringNone)
        AccountStakeBaker AccountBaker{..} -> do
            paseBakerInfo <- refMake _accountBakerInfo
            let baker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = _stakeEarnings,
                          paseBakerPendingChange = _bakerPendingChange,
                          ..
                        }
            return (_stakedAmount, baker)
        AccountStakeDelegate AccountDelegationV1{..} -> do
            let del =
                    PersistentAccountStakeEnduringDelegator
                        { paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                          paseDelegatorId = _delegationIdentity,
                          paseDelegatorTarget = _delegationTarget,
                          paseDelegatorPendingChange = _delegationPendingChange
                        }
            return (_delegationStakedAmount, del)
    paedEncryptedAmount :: Nullable (LazyBufferedRef PersistentAccountEncryptedAmount) <- do
        ea <- storePersistentAccountEncryptedAmount _accountEncryptedAmount
        isInit <- isInitialPersistentAccountEncryptedAmount ea
        if isInit
            then return Null
            else Some <$!> refMake ea
    paedReleaseSchedule :: Nullable (LazyBufferedRef AccountReleaseSchedule, Amount) <- do
        rs <- makePersistentAccountReleaseSchedule (TARS.theAccountReleaseSchedule _accountReleaseSchedule)
        if isEmptyAccountReleaseSchedule rs
            then return Null
            else do
                rsRef <- refMake $! rs
                let !lockedBal = _accountReleaseSchedule ^. TARS.totalLockedUpBalance
                return (Some (rsRef, lockedBal))
    paedStakeCooldown <- makePersistentCooldownQueue @_ @av _accountStakeCooldown
    accountEnduringData <-
        refMake
            =<< case accountVersion @av of
                SAccountV2 ->
                    makeAccountEnduringDataAV2
                        paedPersistingData
                        paedEncryptedAmount
                        paedReleaseSchedule
                        paedStake
                SAccountV3 ->
                    makeAccountEnduringDataAV3
                        paedPersistingData
                        paedEncryptedAmount
                        paedReleaseSchedule
                        paedStake
                        paedStakeCooldown
                SAccountV4 ->
                    makeAccountEnduringDataAV4
                        paedPersistingData
                        paedEncryptedAmount
                        paedReleaseSchedule
                        paedStake
                        paedStakeCooldown
                SAccountV5 ->
                    makeAccountEnduringDataAV5
                        paedPersistingData
                        paedEncryptedAmount
                        paedReleaseSchedule
                        paedStake
                        paedStakeCooldown
    accountTokenStateTable <- forM _accountTokenStateTable $ \inMemoryTast -> do
        tast <-
            traverse makeHashedBufferedRef $
                Transient.inMemoryTokenStateTable $
                    _unhashed inMemoryTast
        fmap Some $ makeHashedBufferedRef $ TokenAccountStateTable tast
    return $!
        PersistentAccount
            { accountNonce = _accountNonce,
              accountAmount = _accountAmount,
              ..
            }

-- | Create an empty account with the given public key, address and credential.
newAccount ::
    forall m av.
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    GlobalContext ->
    AccountAddress ->
    AccountCredential ->
    m (PersistentAccount av)
newAccount cryptoParams _accountAddress credential = do
    let creds = Map.singleton initialCredentialIndex credential
    let newPData =
            PersistingAccountData
                { _accountEncryptionKey = toRawEncryptionKey (makeEncryptionKey cryptoParams (credId credential)),
                  _accountCredentials = toRawAccountCredential <$> creds,
                  _accountVerificationKeys = getAccountInformation 1 creds,
                  _accountRemovedCredentials = emptyHashedRemovedCredentials,
                  ..
                }
    paedPersistingData :: EagerBufferedRef PersistingAccountData <- refMake newPData
    accountEnduringData <-
        refMake
            =<< case accountVersion @av of
                SAccountV2 ->
                    makeAccountEnduringDataAV2
                        paedPersistingData
                        Null
                        Null
                        PersistentAccountStakeEnduringNone
                SAccountV3 ->
                    makeAccountEnduringDataAV3
                        paedPersistingData
                        Null
                        Null
                        PersistentAccountStakeEnduringNone
                        emptyCooldownQueue
                SAccountV4 ->
                    makeAccountEnduringDataAV4
                        paedPersistingData
                        Null
                        Null
                        PersistentAccountStakeEnduringNone
                        emptyCooldownQueue
                SAccountV5 ->
                    makeAccountEnduringDataAV5
                        paedPersistingData
                        Null
                        Null
                        PersistentAccountStakeEnduringNone
                        emptyCooldownQueue
    let accountTokenStateTable = conditionally (sSupportsPLT (accountVersion @av)) Null
    return $!
        PersistentAccount
            { accountNonce = minNonce,
              accountAmount = 0,
              accountStakedAmount = 0,
              ..
            }

-- | Make a persistent account from a genesis account.
--  The data is immediately flushed to disc and cached.
makeFromGenesisAccount ::
    forall pv av m.
    ( MonadBlobStore m,
      IsProtocolVersion pv,
      AccountVersionFor pv ~ av,
      AccountStructureVersionFor av ~ 'AccountStructureV1
    ) =>
    SProtocolVersion pv ->
    GlobalContext ->
    ChainParameters pv ->
    GenesisAccount ->
    m (PersistentAccount av)
makeFromGenesisAccount spv cryptoParams chainParameters GenesisAccount{..} = do
    paedPersistingData :: EagerBufferedRef PersistingAccountData <-
        refMakeFlushed $
            PersistingAccountData
                { _accountEncryptionKey =
                    toRawEncryptionKey $
                        makeEncryptionKey cryptoParams $
                            credId $
                                gaCredentials Map.! initialCredentialIndex,
                  _accountCredentials = toRawAccountCredential <$> gaCredentials,
                  _accountVerificationKeys = getAccountInformation gaThreshold gaCredentials,
                  _accountRemovedCredentials = emptyHashedRemovedCredentials,
                  _accountAddress = gaAddress
                }

    (accountStakedAmount, stakeEnduring) <- case gaBaker of
        Nothing -> return (0, PersistentAccountStakeEnduringNone)
        Just baker -> do
            paseBakerInfo <- refMakeFlushed $ genesisBakerInfoEx spv chainParameters baker
            let enduringBaker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = gbRestakeEarnings baker,
                          paseBakerPendingChange = NoChange,
                          ..
                        }
            return (gbStake baker, enduringBaker)

    accountEnduringData <-
        refMakeFlushed
            =<< case accountVersion @av of
                SAccountV2 ->
                    makeAccountEnduringDataAV2
                        paedPersistingData
                        Null
                        Null
                        stakeEnduring
                SAccountV3 ->
                    makeAccountEnduringDataAV3
                        paedPersistingData
                        Null
                        Null
                        stakeEnduring
                        emptyCooldownQueue
                SAccountV4 ->
                    makeAccountEnduringDataAV4
                        paedPersistingData
                        Null
                        Null
                        stakeEnduring
                        emptyCooldownQueue
                SAccountV5 ->
                    makeAccountEnduringDataAV5
                        paedPersistingData
                        Null
                        Null
                        stakeEnduring
                        emptyCooldownQueue
    let accountTokenStateTable = conditionally (sSupportsPLT (accountVersion @av)) Null
    return $!
        PersistentAccount
            { accountNonce = minNonce,
              accountAmount = gaBalance,
              ..
            }

-- ** Migration

migrateEnduringDataV2 ::
    (SupportMigration m t, MonadLogger (t m)) =>
    PersistentAccountEnduringData 'AccountV2 ->
    t m (PersistentAccountEnduringData 'AccountV2)
migrateEnduringDataV2 ed = do
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ migrateReference migratePersistentEncryptedAmount
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    paedStake <- migratePersistentAccountStakeEnduring (paedStake ed)
    let paedStakeCooldown = emptyCooldownQueue
    return $!
        PersistentAccountEnduringData
            { paedHash = paedHash ed,
              ..
            }

-- | Migrate enduring data from 'AccountV2' to 'AccountV3'. This uses 'StakedBalanceStateTT' to
--  track the staked balance of the account.
--
--   * If the account previously had a pending change, it will now have a pre-pre-cooldown, and
--     'addAccountInPrePreCooldown' is called (to register this globally). If the pending change
--     was a reduction in stake, the reduction is applied immediately to the active stake. If the
--     pending change was a removal, the baker or delegator record is removed altogether.
--
--   * If the account is still delegating but was delegating to a baker for which 'isBakerRemoved'
--     returns @True@, the delegation target is updated to passive delegation.
--
--   * If the account is still delegating, 'retainDelegator' is called to record the (new)
--     delegation amount and target globally.
migrateEnduringDataV2toV3 ::
    (SupportMigration m t, AccountMigration 'AccountV3 (t m), MonadLogger (t m)) =>
    -- | Current enduring data
    PersistentAccountEnduringData 'AccountV2 ->
    -- | New enduring data.
    StakedBalanceStateTT t m (PersistentAccountEnduringData 'AccountV3)
migrateEnduringDataV2toV3 ed = do
    logEvent GlobalState LLTrace "Migrating persisting data"
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ \e -> do
        logEvent GlobalState LLTrace "Migrating encrypted amount"
        migrateReference migratePersistentEncryptedAmount e
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        logEvent GlobalState LLTrace "Migrating release schedule"
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    logEvent GlobalState LLTrace "Migrating stake"
    (paedStake, paedStakeCooldown) <- migratePersistentAccountStakeEnduringV2toV3 (paedStake ed)
    logEvent GlobalState LLTrace "Reconstructing account enduring data"
    makeAccountEnduringDataAV3
        paedPersistingData
        paedEncryptedAmount
        paedReleaseSchedule
        paedStake
        paedStakeCooldown

migrateEnduringDataV3toV4 ::
    (SupportMigration m t, AccountMigration 'AccountV4 (t m), MonadLogger (t m)) =>
    -- | Current enduring data
    PersistentAccountEnduringData 'AccountV3 ->
    -- | New enduring data.
    t m (PersistentAccountEnduringData 'AccountV4)
migrateEnduringDataV3toV4 ed = do
    logEvent GlobalState LLTrace "Migrating persisting data"
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ \e -> do
        logEvent GlobalState LLTrace "Migrating encrypted amount"
        migrateReference migratePersistentEncryptedAmount e
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        logEvent GlobalState LLTrace "Migrating release schedule"
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    logEvent GlobalState LLTrace "Migrating stake"
    paedStake <- migratePersistentAccountStakeEnduringV3toV4 (paedStake ed)
    logEvent GlobalState LLTrace "Migrating cooldown queue"
    paedStakeCooldown <- migrateCooldownQueue (paedStakeCooldown ed)
    logEvent GlobalState LLTrace "Reconstructing account enduring data"
    makeAccountEnduringDataAV4
        paedPersistingData
        paedEncryptedAmount
        paedReleaseSchedule
        paedStake
        paedStakeCooldown

migrateEnduringDataV4toV5 ::
    (SupportMigration m t, MonadLogger (t m)) =>
    -- | Current enduring data
    PersistentAccountEnduringData 'AccountV4 ->
    -- | New enduring data.
    t m (PersistentAccountEnduringData 'AccountV5)
migrateEnduringDataV4toV5 ed = do
    logEvent GlobalState LLTrace "Migrating persisting data"
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ \e -> do
        logEvent GlobalState LLTrace "Migrating encrypted amount"
        migrateReference migratePersistentEncryptedAmount e
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        logEvent GlobalState LLTrace "Migrating release schedule"
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    logEvent GlobalState LLTrace "Migrating stake"
    paedStake <- migratePersistentAccountStakeEnduringAV4 (paedStake ed)
    logEvent GlobalState LLTrace "Migrating cooldown queue"
    paedStakeCooldown <- migrateCooldownQueue (paedStakeCooldown ed)
    logEvent GlobalState LLTrace "Reconstructing account enduring data"
    makeAccountEnduringDataAV5
        paedPersistingData
        paedEncryptedAmount
        paedReleaseSchedule
        paedStake
        paedStakeCooldown

-- | Migration for 'PersistentAccountEnduringData'. Only supports 'AccountV3'.
--  The data is unchanged in the migration.
migrateEnduringDataV3toV3 ::
    (SupportMigration m t, MonadLogger (t m)) =>
    PersistentAccountEnduringData 'AccountV3 ->
    t m (PersistentAccountEnduringData 'AccountV3)
migrateEnduringDataV3toV3 ed = do
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ migrateReference migratePersistentEncryptedAmount
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    paedStake <- migratePersistentAccountStakeEnduring (paedStake ed)
    paedStakeCooldown <- migrateCooldownQueue (paedStakeCooldown ed)
    makeAccountEnduringDataAV3 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown

-- | Migration for 'PersistentAccountEnduringData'. Only supports 'AccountV4'.
--  The data is unchanged in the migration.
migrateEnduringDataV4toV4 ::
    (SupportMigration m t) =>
    PersistentAccountEnduringData 'AccountV4 ->
    t m (PersistentAccountEnduringData 'AccountV4)
migrateEnduringDataV4toV4 ed = do
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ migrateReference migratePersistentEncryptedAmount
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    paedStake <- migratePersistentAccountStakeEnduring (paedStake ed)
    paedStakeCooldown <- migrateCooldownQueue (paedStakeCooldown ed)
    makeAccountEnduringDataAV4 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown

-- | Migration for 'PersistentAccountEnduringData'. Only supports 'AccountV5'.
--  The data is unchanged in the migration.
migrateEnduringDataV5toV5 ::
    (SupportMigration m t) =>
    PersistentAccountEnduringData 'AccountV5 ->
    t m (PersistentAccountEnduringData 'AccountV5)
migrateEnduringDataV5toV5 ed = do
    paedPersistingData <- migrateEagerBufferedRef return (paedPersistingData ed)
    paedEncryptedAmount <- forM (paedEncryptedAmount ed) $ migrateReference migratePersistentEncryptedAmount
    paedReleaseSchedule <- forM (paedReleaseSchedule ed) $ \(oldRSRef, lockedAmt) -> do
        newRSRef <- migrateReference migrateAccountReleaseSchedule oldRSRef
        return (newRSRef, lockedAmt)
    paedStake <- migratePersistentAccountStakeEnduring (paedStake ed)
    paedStakeCooldown <- migrateCooldownQueue (paedStakeCooldown ed)
    makeAccountEnduringDataAV5 paedPersistingData paedEncryptedAmount paedReleaseSchedule paedStake paedStakeCooldown

-- | A trivial migration from account version 2 to account version 2.
--  In particular the data is retained as-is.
migrateV2ToV2 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      MonadTrans t,
      MonadLogger (t m)
    ) =>
    PersistentAccount 'AccountV2 ->
    t m (PersistentAccount 'AccountV2)
migrateV2ToV2 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV2 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = CFalse,
              ..
            }

-- | Migrate from account version 2 to account version 3.
--
--   * If the account previously had a pending change, it will now have a pre-pre-cooldown, and
--     'addAccountInPrePreCooldown' is called (to register this globally). If the pending change
--     was a reduction in stake, the reduction is applied immediately to the active stake. If the
--     pending change was a removal, the baker or delegator record is removed altogether.
--
--   * If the account is still delegating but was delegating to a baker for which 'isBakerRemoved'
--     returns @True@, the delegation target is updated to passive delegation.
--
--   * If the account is still delegating, 'retainDelegator' is called to record the (new)
--     delegation amount and target globally.
--
--  Note: the global record of which bakers are retained and their stakes is determined a priori
--  (see "Concordium.GlobalState.Persistent.Account.MigrationState"). This is used to determine
--  whether a baker is removed.
migrateV2ToV3 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      AccountMigration 'AccountV3 (t m),
      MonadTrans t,
      MonadLogger (t m)
    ) =>
    PersistentAccount 'AccountV2 ->
    t m (PersistentAccount 'AccountV3)
migrateV2ToV3 acc = do
    (accountEnduringData, newStakedAmount) <-
        runStakedBalanceStateTT
            (migrateEagerBufferedRef migrateEnduringDataV2toV3 (accountEnduringData acc))
            (accountStakedAmount acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = newStakedAmount,
              accountTokenStateTable = CFalse,
              ..
            }

-- | A trivial migration from account version 3 to account version 3.
--  In particular the data is retained as-is.
migrateV3ToV3 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      MonadTrans t,
      MonadLogger (t m)
    ) =>
    PersistentAccount 'AccountV3 ->
    t m (PersistentAccount 'AccountV3)
migrateV3ToV3 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV3toV3 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = CFalse,
              ..
            }

migrateV3ToV4 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      AccountMigration 'AccountV4 (t m),
      MonadTrans t,
      MonadLogger (t m)
    ) =>
    PersistentAccount 'AccountV3 ->
    t m (PersistentAccount 'AccountV4)
migrateV3ToV4 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV3toV4 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = CFalse,
              ..
            }

-- | A trivial migration from account version 4 to account version 4.
--  In particular the data is retained as-is.
migrateV4ToV4 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      MonadTrans t
    ) =>
    PersistentAccount 'AccountV4 ->
    t m (PersistentAccount 'AccountV4)
migrateV4ToV4 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV4toV4 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = CFalse,
              ..
            }

migrateV4ToV5 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      MonadTrans t,
      MonadLogger (t m)
    ) =>
    PersistentAccount 'AccountV4 ->
    t m (PersistentAccount 'AccountV5)
migrateV4ToV5 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV4toV5 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = CTrue Null,
              ..
            }

-- | A trivial migration from account version 5 to account version 5.
--  In particular the data is retained as-is.
migrateV5ToV5 ::
    ( MonadBlobStore m,
      MonadBlobStore (t m),
      MonadTrans t
    ) =>
    PersistentAccount 'AccountV5 ->
    t m (PersistentAccount 'AccountV5)
migrateV5ToV5 acc = do
    accountEnduringData <- migrateEagerBufferedRef migrateEnduringDataV5toV5 (accountEnduringData acc)
    return $!
        PersistentAccount
            { accountNonce = accountNonce acc,
              accountAmount = accountAmount acc,
              accountStakedAmount = accountStakedAmount acc,
              accountTokenStateTable = accountTokenStateTable acc,
              ..
            }

-- | Migration for 'PersistentAccount'. Supports 'AccountV2', 'AccountV3', 'AccountV4'.
--
--  When migrating P6->P7 (account version 2 to 3), the 'AccountMigration' interface is used as
--  follows:
--
--   * Accounts that previously had a pending change are updated to have a pre-pre-cooldown, and
--     'addAccountInPrePreCooldown' is called. If the pending change is a reduction in stake,
--     the reduction is applied immediately to the active stake. If the pending change is a removal,
--     the baker or delegator record is removed altogether.
--
--   * Accounts that are still delegating but were delegating to a baker for which 'isBakerRemoved'
--     returns @True@ are updated to delegate to passive delegation.
--
--   * For accounts that are still delegating, 'retainDelegator' is called to record the (new)
--     delegation amount and target.
migratePersistentAccount ::
    forall m t oldpv pv.
    ( IsProtocolVersion oldpv,
      SupportMigration m t,
      AccountMigration (AccountVersionFor pv) (t m),
      AccountStructureVersionFor (AccountVersionFor oldpv) ~ 'AccountStructureV1,
      MonadLogger (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccount StateMigrationParametersTrivial acc = case accountVersion @(AccountVersionFor oldpv) of
    SAccountV2 -> migrateV2ToV2 acc
    SAccountV3 -> migrateV3ToV3 acc
    SAccountV4 -> migrateV4ToV4 acc
    SAccountV5 -> migrateV5ToV5 acc
migratePersistentAccount StateMigrationParametersP5ToP6{} acc = migrateV2ToV2 acc
migratePersistentAccount StateMigrationParametersP6ToP7{} acc = migrateV2ToV3 acc
migratePersistentAccount StateMigrationParametersP7ToP8{} acc = migrateV3ToV4 acc
migratePersistentAccount StateMigrationParametersP8ToP9{} acc = migrateV4ToV5 acc

-- | Migration for 'PersistentAccount' from 'V0.PersistentAccount'. This supports migration from
--  'P4' to 'P5'.
migratePersistentAccountFromV0 ::
    ( SupportMigration m t,
      AccountVersionFor oldpv ~ 'AccountV1,
      AccountVersionFor pv ~ 'AccountV2,
      MonadLogger (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    V0.PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccountFromV0 StateMigrationParametersP4ToP5{} V0.PersistentAccount{..} = do
    paedPersistingData <- migrateReference return _persistingData
    (accountStakedAmount, !paedStake) <- case _accountStake of
        V0.PersistentAccountStakeNone -> return (0, PersistentAccountStakeEnduringNone)
        V0.PersistentAccountStakeBaker bkrRef -> do
            V0.PersistentAccountBaker{..} <- lift $ refLoad bkrRef
            bkrInfoEx <- lift $ do
                bkrInfo <- refLoad _accountBakerInfo
                bkrPoolInfo <- refLoad (V0._theExtraBakerInfo _extraBakerInfo)
                return $! BakerInfoExV1 bkrInfo bkrPoolInfo CFalse
            paseBakerInfo' <- refMake bkrInfoEx
            (paseBakerInfo, _) <- refFlush paseBakerInfo'
            let baker =
                    PersistentAccountStakeEnduringBaker
                        { paseBakerRestakeEarnings = _stakeEarnings,
                          paseBakerPendingChange = coercePendingChangeEffectiveV1 <$> _bakerPendingChange,
                          ..
                        }
            return (_stakedAmount, baker)
        V0.PersistentAccountStakeDelegate dlgRef -> do
            AccountDelegationV1{..} <- lift $ refLoad dlgRef
            let del :: PersistentAccountStakeEnduring 'AccountV2
                del =
                    PersistentAccountStakeEnduringDelegator
                        { paseDelegatorRestakeEarnings = _delegationStakeEarnings,
                          paseDelegatorId = _delegationIdentity,
                          paseDelegatorTarget = _delegationTarget,
                          paseDelegatorPendingChange = coercePendingChangeEffectiveV1 <$> _delegationPendingChange
                        }
            return (_delegationStakedAmount, del)
    paedEncryptedAmount <- do
        mea <- lift $ do
            ea <- refLoad _accountEncryptedAmount
            isInit <- isInitialPersistentAccountEncryptedAmount ea
            return $ if isInit then Null else Some ea
        forM mea $ \ea -> do
            newEA <- migratePersistentEncryptedAmount ea
            refMake $! newEA
    paedReleaseSchedule <- do
        mrs <- lift $ do
            rs <- refLoad _accountReleaseSchedule
            return $ if ARSV0.isEmptyAccountReleaseSchedule rs then Null else Some rs
        forM mrs $ \rs -> do
            newRS <- migrateAccountReleaseScheduleFromV0 rs
            rsRef <- refMake $! newRS
            return (rsRef, ARSV0.releaseScheduleLockedBalance rs)
    (accountEnduringData, _) <-
        refFlush
            =<< refMake
            =<< makeAccountEnduringDataAV2
                paedPersistingData
                paedEncryptedAmount
                paedReleaseSchedule
                paedStake
    return $!
        PersistentAccount
            { accountNonce = _accountNonce,
              accountAmount = _accountAmount,
              accountTokenStateTable = CFalse,
              ..
            }

-- ** Conversion

-- | Converts an account to a transient (i.e. in memory) account. (Used for testing.)
toTransientAccount ::
    ( MonadBlobStore m,
      IsAccountVersion av,
      AccountStructureVersionFor av ~ 'AccountStructureV1,
      AVSupportsDelegation av
    ) =>
    PersistentAccount av ->
    m (Transient.Account av)
toTransientAccount acc = do
    let _accountPersisting = makeHashed $ persistingData acc
    _accountEncryptedAmount <- getEncryptedAmount acc
    _accountReleaseSchedule <- getReleaseSchedule acc
    _accountStaking <- getStake acc
    _accountStakeCooldown <- toTransientCooldownQueue <$> getStakeCooldown acc
    condTast <- getTokenStateTable acc
    let _accountTokenStateTable =
            fmap
                ( makeHashed . \case
                    Some tast -> Transient.InMemoryTokenStateTable tast
                    Null -> Transient.InMemoryTokenStateTable Map.empty
                )
                condTast
    return $
        Transient.Account
            { _accountNonce = accountNonce acc,
              _accountAmount = accountAmount acc,
              ..
            }
