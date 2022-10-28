{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |This module implements accounts for account versions 'AccountV0' (protocol 'P1' to 'P3') and
-- 'AccountV1' (protocol 'P4').
-- It should not be necessary to use this module directly, but instead through the interface
-- provided by 'Concordium.GlobalState.Persistent.Account'.
module Concordium.GlobalState.Persistent.Account.StructureV0 where

import Control.Arrow
import Control.Monad
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Serialize
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Genesis.Data.P4 as P4
import Concordium.ID.Parameters
import Concordium.ID.Types hiding (values)
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.Types.Accounts hiding (accountBakerInfo, bakerPendingChange, stakeEarnings, stakedAmount, _bakerPendingChange, _stakedAmount)
import qualified Concordium.Types.Accounts as BaseAccount hiding (bakerPendingChange, stakeEarnings)
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import qualified Concordium.Types.Migration as Migration
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put

import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount, replaceUpTo)
import Concordium.GlobalState.BakerInfo (BakerAdd (..), BakerKeyUpdate (..), bakerKeyUpdateToInfo)
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as Transient
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Account.EncryptedAmount
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Persistent.CachedRef

-- * A note on 'Cacheable' instances for persistent accounts

-- $PersistentAccountCacheable
--
-- 'PersistentAccount' and its constituents 'PersistentAccountEncryptedAmount',
-- 'PersistentExtraBakerInfo', 'PersistentBakerInfoEx', 'PersistentAccountBaker', and
-- 'AccountReleaseSchedule' internally use only 'EagerBufferedRef's, which means that when the
-- 'PersistentAccount' is loaded, so too are all of its component parts. Consequently, these
-- can have trivial instances of the 'Cacheable' typeclass, since the data is always cached.
-- (This also applies to 'Concordium.GlobalState.Persistent.Bakers.BakerInfos', which uses
-- 'PersistentBakerInfoEx'.)
--
-- The motivation for always loading accounts in their entirety is that when accounts are loaded
-- they will commonly also be updated, which requires recomputing their hashes, which in turn
-- requires all of the data associated with the account to be available. (For some portions,
-- having just the hash available is sufficient, but at present we retain the data instead of the
-- hash.)

-- * Account structure constraints

-- |Constrain the account structure version for an account version to 'AccountStructureV0'.
type AVStructureV0 (av :: AccountVersion) = AccountStructureVersionFor av ~ 'AccountStructureV0

-- |Constrain the account structure version for a protocol version to 'AccountStructureV0'.
type StructureV0 (pv :: ProtocolVersion) = AVStructureV0 (AccountVersionFor pv)

-- * 'PersistentExtraBakerInfo'

-- |Extra info (beyond 'BakerInfo') associated with a baker.
-- Before delegation ('AccountV0'), there is no extra info.
-- With delegation, this consists of (a reference to) the 'BakerPoolInfo'.
-- (This type is always fully cached in memory. See $PersistentAccountCacheable for details.)
type family PersistentExtraBakerInfo' (av :: AccountVersion) where
    PersistentExtraBakerInfo' 'AccountV0 = ()
    PersistentExtraBakerInfo' 'AccountV1 = EagerBufferedRef BakerPoolInfo

-- |Extra info (beyond 'BakerInfo') associated with a baker.
-- (This structure is always fully cached in memory. See $PersistentAccountCacheable for details.)
newtype PersistentExtraBakerInfo (av :: AccountVersion) = PersistentExtraBakerInfo
    { _theExtraBakerInfo :: PersistentExtraBakerInfo' av
    }

makeLenses ''PersistentExtraBakerInfo

instance forall av. (IsAccountVersion av, AVStructureV0 av) => Show (PersistentExtraBakerInfo av) where
    show = case accountVersion @av of
        SAccountV0 -> show . _theExtraBakerInfo
        SAccountV1 -> show . _theExtraBakerInfo

instance forall av m. (Applicative m) => Cacheable m (PersistentExtraBakerInfo av)

instance forall av m. (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) => BlobStorable m (PersistentExtraBakerInfo av) where
    storeUpdate =
        fmap (second PersistentExtraBakerInfo)
            . ( case accountVersion @av of
                    SAccountV0 -> storeUpdate
                    SAccountV1 -> storeUpdate
              )
            . _theExtraBakerInfo
    load =
        fmap PersistentExtraBakerInfo <$> case accountVersion @av of
            SAccountV0 -> load
            SAccountV1 -> load

makePersistentExtraBakerInfoV1 ::
    forall av.
    (IsAccountVersion av, AVStructureV0 av, AVSupportsDelegation av) =>
    EagerBufferedRef BakerPoolInfo ->
    PersistentExtraBakerInfo av
makePersistentExtraBakerInfoV1 = case accountVersion @av of
    SAccountV1 -> PersistentExtraBakerInfo

-- |See documentation of @migratePersistentBlockState@.
migratePersistentExtraBakerInfo' ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentExtraBakerInfo' (AccountVersionFor oldpv) ->
    t m (PersistentExtraBakerInfo' (AccountVersionFor pv))
migratePersistentExtraBakerInfo' migration bi = do
    case migration of
        StateMigrationParametersTrivial ->
            case accountVersion @(AccountVersionFor oldpv) of
                SAccountV0 -> return ()
                SAccountV1 -> migrateEagerBufferedRef return bi
        StateMigrationParametersP1P2 -> return ()
        StateMigrationParametersP2P3 -> return ()
        StateMigrationParametersP3ToP4 migrationData -> do
            let bpi = P4.defaultBakerPoolInfo migrationData
            (!newRef, _) <- refFlush =<< refMake bpi
            return newRef

-- |See documentation of @migratePersistentBlockState@.
migratePersistentExtraBakerInfo ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentExtraBakerInfo (AccountVersionFor oldpv) ->
    t m (PersistentExtraBakerInfo (AccountVersionFor pv))
migratePersistentExtraBakerInfo migration =
    fmap PersistentExtraBakerInfo
        . migratePersistentExtraBakerInfo' migration
        . _theExtraBakerInfo

-- * 'PersistentBakerInfoEx'

-- |A persistent version of 'BakerInfoEx'.
-- (This structure is always fully cached in memory. See $PersistentAccountCacheable for details.)
data PersistentBakerInfoEx av = PersistentBakerInfoEx
    { bakerInfoRef :: !(EagerBufferedRef BakerInfo),
      bakerInfoExtra :: !(PersistentExtraBakerInfo av)
    }

deriving instance (IsAccountVersion av, AVStructureV0 av) => Show (PersistentBakerInfoEx av)

instance forall m av. (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) => BlobStorable m (PersistentBakerInfoEx av) where
    storeUpdate PersistentBakerInfoEx{..} = do
        (pBakerInfo, newBakerInfo) <- storeUpdate bakerInfoRef
        (pExtraBakerInfo, newExtraBakerInfo) <- storeUpdate bakerInfoExtra
        let pab =
                PersistentBakerInfoEx
                    { bakerInfoRef = newBakerInfo,
                      bakerInfoExtra = newExtraBakerInfo,
                      ..
                    }
        return . (,pab) $ do
            pBakerInfo
            pExtraBakerInfo
    load = do
        rBakerInfo <- load
        rExtraBakerInfo <- load
        return $ do
            bakerInfoRef <- rBakerInfo
            bakerInfoExtra <- rExtraBakerInfo
            return PersistentBakerInfoEx{..}

instance (Applicative m) => Cacheable m (PersistentBakerInfoEx av)

-- ** Query

-- |Load 'BakerInfo' from a 'PersistentBakerInfoEx'.
loadBakerInfo :: MonadBlobStore m => PersistentBakerInfoEx av -> m BakerInfo
loadBakerInfo = refLoad . bakerInfoRef

-- |Load a 'BakerInfoEx' from a 'PersistentBakerInfoEx'.
loadPersistentBakerInfoEx ::
    forall av m.
    (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) =>
    PersistentBakerInfoEx av ->
    m (BakerInfoEx av)
loadPersistentBakerInfoEx PersistentBakerInfoEx{..} = do
    bkrInfo <- refLoad bakerInfoRef
    case accountVersion @av of
        SAccountV0 -> return $ BakerInfoExV0 bkrInfo
        SAccountV1 -> do
            bkrInfoEx <- refLoad (bakerInfoExtra ^. theExtraBakerInfo)
            return $ BakerInfoExV1 bkrInfo bkrInfoEx

-- |Load the baker id from the 'PersistentBakerInfoEx' structure.
loadBakerId :: MonadBlobStore m => PersistentBakerInfoEx av -> m BakerId
loadBakerId PersistentBakerInfoEx{..} = do
    bi <- refLoad bakerInfoRef
    return (_bakerIdentity bi)

-- ** Construction

-- |Construct a 'PersistentBakerInfoEx' from a 'BakerInfoEx'.
makePersistentBakerInfoEx :: (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) => BakerInfoEx av -> m (PersistentBakerInfoEx av)
makePersistentBakerInfoEx (BakerInfoExV0 bi) = do
    bakerInfoRef <- refMake bi
    return PersistentBakerInfoEx{bakerInfoExtra = PersistentExtraBakerInfo (), ..}
makePersistentBakerInfoEx (BakerInfoExV1 bi ebi) = do
    bakerInfoRef <- refMake bi
    bakerInfoExtra <- makePersistentExtraBakerInfoV1 <$> refMake ebi
    return PersistentBakerInfoEx{..}

-- ** Migration

-- |See documentation of @migratePersistentBlockState@.
migratePersistentBakerInfoEx ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoEx migration PersistentBakerInfoEx{..} = do
    newBakerInfoRef <- migrateEagerBufferedRef return bakerInfoRef
    newBakerInfoExtra <- migratePersistentExtraBakerInfo migration bakerInfoExtra
    return
        PersistentBakerInfoEx
            { bakerInfoRef = newBakerInfoRef,
              bakerInfoExtra = newBakerInfoExtra
            }

-- * Persistent account baker

-- |A baker associated with an account.
-- (This structure is always fully cached in memory. See $PersistentAccountCacheable for details.)
data PersistentAccountBaker (av :: AccountVersion) = PersistentAccountBaker
    { _stakedAmount :: !Amount,
      _stakeEarnings :: !Bool,
      _accountBakerInfo :: !(EagerBufferedRef BakerInfo),
      _extraBakerInfo :: !(PersistentExtraBakerInfo av),
      _bakerPendingChange :: !(StakePendingChange av)
    }

deriving instance (IsAccountVersion av, AVStructureV0 av) => Show (PersistentAccountBaker av)

makeLenses ''PersistentAccountBaker

instance forall m av. (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) => BlobStorable m (PersistentAccountBaker av) where
    storeUpdate PersistentAccountBaker{..} = do
        (pBakerInfo, newBakerInfo) <- storeUpdate _accountBakerInfo
        (pExtraBakerInfo, newExtraBakerInfo) <- storeUpdate _extraBakerInfo
        let pab =
                PersistentAccountBaker
                    { _accountBakerInfo = newBakerInfo,
                      _extraBakerInfo = newExtraBakerInfo,
                      ..
                    }
        return . (,pab) $ do
            put _stakedAmount
            put _stakeEarnings
            pBakerInfo
            pExtraBakerInfo
            put _bakerPendingChange
    load = do
        _stakedAmount <- get
        _stakeEarnings <- get
        rBakerInfo <- load
        rExtraBakerInfo <- load
        _bakerPendingChange <- get
        return $ do
            _accountBakerInfo <- rBakerInfo
            _extraBakerInfo <- rExtraBakerInfo
            return PersistentAccountBaker{..}

instance (Applicative m) => Cacheable m (PersistentAccountBaker av)

-- |Getter for accessing the 'PersistentBakerInfoEx' of a 'PersistentAccountBaker'.
accountBakerInfoEx :: Getting r (PersistentAccountBaker av) (PersistentBakerInfoEx av)
accountBakerInfoEx = to (\PersistentAccountBaker{..} -> PersistentBakerInfoEx _accountBakerInfo _extraBakerInfo)

-- |Lens for accessing the reference to the 'BakerPoolInfo' of a 'PersistentAccountBaker'.
bakerPoolInfoRef :: forall av. (IsAccountVersion av, AVStructureV0 av, AVSupportsDelegation av) => Lens' (PersistentAccountBaker av) (EagerBufferedRef BakerPoolInfo)
bakerPoolInfoRef = case accountVersion @av of
    SAccountV1 -> extraBakerInfo . theExtraBakerInfo

-- |Load a 'PersistentAccountBaker' to an 'AccountBaker'.
loadPersistentAccountBaker ::
    forall av m.
    (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) =>
    PersistentAccountBaker av ->
    m (AccountBaker av)
loadPersistentAccountBaker PersistentAccountBaker{..} = do
    _accountBakerInfo <-
        loadPersistentBakerInfoEx $
            PersistentBakerInfoEx _accountBakerInfo _extraBakerInfo
    return AccountBaker{..}

-- |Make a 'PersistentAccountBaker' from an 'AccountBaker'.
makePersistentAccountBaker ::
    forall av m.
    (IsAccountVersion av, AVStructureV0 av, MonadBlobStore m) =>
    AccountBaker av ->
    m (PersistentAccountBaker av)
makePersistentAccountBaker AccountBaker{..} = do
    case accountVersion @av of
        SAccountV0 -> do
            _accountBakerInfo <- refMake (_accountBakerInfo ^. bakerInfo)
            let _extraBakerInfo = PersistentExtraBakerInfo ()
            return PersistentAccountBaker{..}
        SAccountV1 -> do
            abi <- refMake (_accountBakerInfo ^. bakerInfo)
            ebi <- refMake (_bieBakerPoolInfo _accountBakerInfo)
            return
                PersistentAccountBaker
                    { _accountBakerInfo = abi,
                      _extraBakerInfo = PersistentExtraBakerInfo ebi,
                      ..
                    }

-- ** Migration

-- |See documentation of @migratePersistentBlockState@.
migratePersistentAccountBaker ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccountBaker (AccountVersionFor oldpv) ->
    t m (PersistentAccountBaker (AccountVersionFor pv))
migratePersistentAccountBaker migration PersistentAccountBaker{..} = do
    newAccountBakerInfo <- migrateEagerBufferedRef return _accountBakerInfo
    newExtraBakerInfo <- migratePersistentExtraBakerInfo migration _extraBakerInfo
    return
        PersistentAccountBaker
            { _stakedAmount = _stakedAmount,
              _stakeEarnings = _stakeEarnings,
              _accountBakerInfo = newAccountBakerInfo,
              _extraBakerInfo = newExtraBakerInfo,
              _bakerPendingChange = Migration.migrateStakePendingChange migration _bakerPendingChange
            }

-- * Persistent account stake

-- |Staking information associated with an account.
-- IMPORTANT NOTE: The 'Cacheable' instance relies on the fact that no recursive caching is
-- necessary (due to the use of 'EagerBufferedRef's). If this changes, the instance for
-- 'PersistentAccount' will also need to be updated.
data PersistentAccountStake (av :: AccountVersion) where
    PersistentAccountStakeNone :: PersistentAccountStake av
    PersistentAccountStakeBaker ::
        !(EagerBufferedRef (PersistentAccountBaker av)) ->
        PersistentAccountStake av
    PersistentAccountStakeDelegate ::
        AVSupportsDelegation av =>
        !(EagerBufferedRef (AccountDelegation av)) ->
        PersistentAccountStake av

deriving instance (IsAccountVersion av, AVStructureV0 av) => Show (PersistentAccountStake av)

instance forall m av. (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => BlobStorable m (PersistentAccountStake av) where
    storeUpdate = case accountVersion @av of
        SAccountV0 -> su0
        SAccountV1 -> su1
      where
        su0 :: PersistentAccountStake 'AccountV0 -> m (Put, PersistentAccountStake 'AccountV0)
        su0 pas@PersistentAccountStakeNone = return (put (refNull :: BlobRef (PersistentAccountBaker av)), pas)
        su0 (PersistentAccountStakeBaker bkrref) = do
            (r, bkrref') <- storeUpdate bkrref
            return (r, PersistentAccountStakeBaker bkrref')
        su1 :: PersistentAccountStake av -> m (Put, PersistentAccountStake av)
        su1 pas@PersistentAccountStakeNone = return (putWord8 0, pas)
        su1 (PersistentAccountStakeBaker bkrref) = do
            (r, bkrref') <- storeUpdate bkrref
            return (putWord8 1 >> r, PersistentAccountStakeBaker bkrref')
        su1 (PersistentAccountStakeDelegate dlgref) = do
            (r, dlgref') <- storeUpdate dlgref
            return (putWord8 2 >> r, PersistentAccountStakeDelegate dlgref')
    load = case accountVersion @av of
        SAccountV0 -> l0
        SAccountV1 -> l1
      where
        l0 :: Get (m (PersistentAccountStake av))
        l0 = do
            let toPASB Null = PersistentAccountStakeNone
                toPASB (Some br) = PersistentAccountStakeBaker br
            fmap toPASB <$> load
        l1 :: AVSupportsDelegation av => Get (m (PersistentAccountStake av))
        l1 =
            getWord8 >>= \case
                0 -> return (pure PersistentAccountStakeNone)
                1 -> fmap PersistentAccountStakeBaker <$> load
                2 -> fmap PersistentAccountStakeDelegate <$> load
                _ -> fail "Invalid staking type"

instance (Applicative m) => Cacheable m (PersistentAccountStake av)

instance (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => MHashableTo m (AccountStakeHash av) (PersistentAccountStake av) where
    getHashM PersistentAccountStakeNone = return $ getAccountStakeHash AccountStakeNone
    getHashM (PersistentAccountStakeBaker bkrref) =
        getAccountStakeHash . AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkrref)
    getHashM (PersistentAccountStakeDelegate dlgref) =
        getAccountStakeHash . AccountStakeDelegate <$> refLoad dlgref

-- |Load a 'PersistentAccountStake'.
loadAccountStake :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccountStake av -> m (AccountStake av)
loadAccountStake PersistentAccountStakeNone = return AccountStakeNone
loadAccountStake (PersistentAccountStakeBaker bkr) = AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkr)
loadAccountStake (PersistentAccountStakeDelegate dlg) = AccountStakeDelegate <$> refLoad dlg

-- |See documentation of @migratePersistentBlockState@.
migratePersistentAccountStake ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      StructureV0 oldpv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccountStake (AccountVersionFor oldpv) ->
    t m (PersistentAccountStake (AccountVersionFor pv))
migratePersistentAccountStake _ PersistentAccountStakeNone = return PersistentAccountStakeNone
migratePersistentAccountStake migration (PersistentAccountStakeBaker r) = PersistentAccountStakeBaker <$!> migrateEagerBufferedRef (migratePersistentAccountBaker migration) r
migratePersistentAccountStake StateMigrationParametersTrivial (PersistentAccountStakeDelegate r) =
    PersistentAccountStakeDelegate
        <$!> migrateEagerBufferedRef return r

-- * Persistent account

-- |A (persistent) account.
-- IMPORTANT NOTE: The 'Cacheable' instance relies on the fact that no recursive caching is
-- necessary (due to the use of 'EagerBufferedRef's). This fact is also important to the
-- implementation of 'load'.
data PersistentAccount (av :: AccountVersion) = PersistentAccount
    { -- |Next available nonce for this account.
      _accountNonce :: !Nonce,
      -- |Current public account balance.
      _accountAmount :: !Amount,
      -- |List of encrypted amounts on the account.
      _accountEncryptedAmount :: !(EagerBufferedRef PersistentAccountEncryptedAmount),
      -- |Schedule of releases on the account.
      _accountReleaseSchedule :: !(EagerBufferedRef AccountReleaseSchedule),
      -- |A pointer to account data that changes rarely
      _persistingData :: !(EagerlyHashedBufferedRef' PersistingAccountDataHash PersistingAccountData),
      -- |The baker info
      _accountStake :: !(PersistentAccountStake av)
    }

makeLenses ''PersistentAccount

accountBaker :: SimpleGetter (PersistentAccount av) (Nullable (EagerBufferedRef (PersistentAccountBaker av)))
accountBaker = to g
  where
    g PersistentAccount{_accountStake = PersistentAccountStakeBaker bkr} = Some bkr
    g _ = Null

deriving instance (IsAccountVersion av, AVStructureV0 av) => Show (PersistentAccount av)

instance (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => BlobStorable m (PersistentAccount av) where
    storeUpdate PersistentAccount{..} = do
        (pAccData :: Put, accData) <- storeUpdate _persistingData
        (pEnc, encData) <- storeUpdate _accountEncryptedAmount
        (pSched, schedData) <- storeUpdate _accountReleaseSchedule
        (pBkr, stakeData) <- storeUpdate _accountStake
        let !persistentAcc =
                PersistentAccount
                    { _persistingData = accData,
                      _accountEncryptedAmount = encData,
                      _accountReleaseSchedule = schedData,
                      _accountStake = stakeData,
                      ..
                    }
        let !putAccs = do
                put _accountNonce
                put _accountAmount
                pAccData
                pEnc
                pSched
                pBkr
        return (putAccs, persistentAcc)
    load = do
        _accountNonce <- get
        _accountAmount <- get
        mAccDataPtr <- load
        mAccountEncryptedAmountPtr <- load
        mAccountReleaseSchedulePtr <- load
        mAccountStake <- load
        return $ do
            -- Note: because of the use of 'EagerBufferedRef's, we do not have to cache these
            -- sub-structures here: they are cached by construction.
            _persistingData <- mAccDataPtr
            _accountEncryptedAmount <- mAccountEncryptedAmountPtr
            _accountReleaseSchedule <- mAccountReleaseSchedulePtr
            _accountStake <- mAccountStake
            return PersistentAccount{..}

instance (Applicative m) => Cacheable m (PersistentAccount av)

-- |Generate the inputs for computing the account hash at V0.
hashInputsV0 :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (AccountHashInputsV0 av)
hashInputsV0 PersistentAccount{..} = do
    eData <- refLoad _accountEncryptedAmount
    eData' <- loadPersistentAccountEncryptedAmount eData
    sHash <- getHashM _accountReleaseSchedule
    persistingHash <- getHashM _persistingData
    stakeHash <- getHashM _accountStake
    return $!
        AccountHashInputsV0
            { ahiNextNonce = _accountNonce,
              ahiAccountAmount = _accountAmount,
              ahiAccountEncryptedAmount = eData',
              ahiAccountReleaseScheduleHash = sHash,
              ahiPersistingAccountDataHash = persistingHash,
              ahiAccountStakeHash = stakeHash
            }

instance (MonadBlobStore m) => MHashableTo m (AccountHash 'AccountV0) (PersistentAccount 'AccountV0) where
    getHashM = fmap (makeAccountHash . AHIV0) . hashInputsV0

instance (MonadBlobStore m) => MHashableTo m (AccountHash 'AccountV1) (PersistentAccount 'AccountV1) where
    getHashM = fmap (makeAccountHash . AHIV1) . hashInputsV0

instance (MonadBlobStore m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV0) where
    getHashM = fmap (theAccountHash @'AccountV0) . getHashM

instance (MonadBlobStore m) => MHashableTo m Hash.Hash (PersistentAccount 'AccountV1) where
    getHashM = fmap (theAccountHash @'AccountV1) . getHashM

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^^. accountAddress@ returns the account's address.
(^^.) ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    Getting b PersistingAccountData b ->
    m b
acc ^^. l = (^. l) <$!> refLoad (acc ^. persistingData)
{-# INLINE (^^.) #-}

infixl 8 ^^.

-- |Update a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- Used to implement '.~~' and '%~~'.
setPAD ::
    forall m av.
    (MonadBlobStore m) =>
    (PersistingAccountData -> PersistingAccountData) ->
    PersistentAccount av ->
    m (PersistentAccount av)
setPAD f acc = do
    pData <- refLoad (acc ^. persistingData)
    let newPData = f pData
    newPDataRef <- refMake newPData
    return $! acc & persistingData .~ newPDataRef

-- |Set a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountStakeDelegate .~~ Nothing@ sets the
-- account's stake delegate to 'Nothing'.
(.~~) ::
    (MonadBlobStore m) =>
    ASetter PersistingAccountData PersistingAccountData a b ->
    b ->
    PersistentAccount av ->
    m (PersistentAccount av)
(.~~) l v = setPAD (l .~ v)
{-# INLINE (.~~) #-}

infixr 4 .~~

-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) ::
    (MonadBlobStore m) =>
    ASetter PersistingAccountData PersistingAccountData a b ->
    (a -> b) ->
    PersistentAccount av ->
    m (PersistentAccount av)
(%~~) l f = setPAD (l %~ f)
{-# INLINE (%~~) #-}

infixr 4 %~~

-- ** Queries

-- |Get the canonical address of the account.
getCanonicalAddress :: (MonadBlobStore m) => PersistentAccount av -> m AccountAddress
getCanonicalAddress = (^^. accountAddress)

-- |Get the current public account balance.
getAmount :: (Applicative m) => PersistentAccount av -> m Amount
getAmount = pure . view accountAmount

-- |Get the amount that is staked on the account.
getStakedAmount :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m Amount
getStakedAmount acc =
    getStakeDetails acc <&> \case
        StakeDetailsBaker{..} -> sdStakedCapital
        StakeDetailsDelegator{..} -> sdStakedCapital
        _ -> 0

-- |Get the amount that is locked in scheduled releases on the account.
getLockedAmount :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m Amount
getLockedAmount acc = Transient._totalLockedUpBalance <$> getReleaseSchedule acc

-- | Get the current public account available balance.
-- This accounts for lock-up and staked amounts.
-- @available = total - max locked staked@
getAvailableAmount :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m Amount
getAvailableAmount acc = do
    total <- getAmount acc
    lockedUp <- Transient._totalLockedUpBalance <$> getReleaseSchedule acc
    staked <- getStakeDetails acc <&> \case
        StakeDetailsBaker{..} -> sdStakedCapital
        StakeDetailsDelegator{..} -> sdStakedCapital
        _ -> 0
    return $ total - max lockedUp staked

-- |Get the next account nonce for transactions from this account.
getNonce :: (Applicative m) => PersistentAccount av -> m Nonce
getNonce = pure . view accountNonce

-- |Determine if a given operation is permitted for the account.
--
-- * For 'AllowedEncryptedTransfers' the account may only have 1 credential.
--
-- * For 'AllowedMultipleCredentials' the account must have the empty encrypted balance.
isAllowed :: MonadBlobStore m => PersistentAccount av -> AccountAllowance -> m Bool
isAllowed acc AllowedEncryptedTransfers = do
    creds <- getCredentials acc
    return $! Map.size creds == 1
isAllowed acc AllowedMultipleCredentials =
    isZeroPersistentAccountEncryptedAmount =<< refLoad (acc ^. accountEncryptedAmount)

-- |Get the credentials deployed on the account. This map is always non-empty and (presently)
-- will have a credential at index 'initialCredentialIndex' (0) that cannot be changed.
getCredentials :: (MonadBlobStore m) => PersistentAccount av -> m (Map.Map CredentialIndex RawAccountCredential)
getCredentials = (^^. accountCredentials)

-- |Get the key used to verify transaction signatures, it records the signature scheme used as well.
getVerificationKeys :: (MonadBlobStore m) => PersistentAccount av -> m AccountInformation
getVerificationKeys = (^^. accountVerificationKeys)

-- |Get the current encrypted amount on the account.
getEncryptedAmount :: MonadBlobStore m => PersistentAccount av -> m AccountEncryptedAmount
getEncryptedAmount acc = loadPersistentAccountEncryptedAmount =<< refLoad (acc ^. accountEncryptedAmount)

-- |Get the public key used to receive encrypted amounts.
getEncryptionKey :: MonadBlobStore f => PersistentAccount av -> f AccountEncryptionKey
-- The use of the unsafe @unsafeEncryptionKeyFromRaw@ function here is
-- justified because the encryption key was validated when it was
-- created/deployed (this is part of credential validation)
getEncryptionKey acc = ID.unsafeEncryptionKeyFromRaw <$> acc ^^. accountEncryptionKey

-- |Get the release schedule for an account.
getReleaseSchedule :: MonadBlobStore m => PersistentAccount av -> m Transient.AccountReleaseSchedule
getReleaseSchedule acc = loadPersistentAccountReleaseSchedule =<< refLoad (acc ^. accountReleaseSchedule)

-- |Get the timestamp at which the next scheduled release will occur (if any).
getNextReleaseTimestamp :: MonadBlobStore m => PersistentAccount av -> m (Maybe Timestamp)
getNextReleaseTimestamp acc = nextReleaseTimestamp <$!> refLoad (acc ^. accountReleaseSchedule)

-- |Get the baker and baker info reference (if any) attached to the account.
getBakerAndInfoRef :: forall m av. (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (Maybe (AccountBaker av, PersistentBakerInfoEx av))
getBakerAndInfoRef acc = case acc ^. accountBaker of
    Null -> return Nothing
    Some bref -> do
        pab@PersistentAccountBaker{..} <- refLoad bref
        abi <- refLoad (pab ^. accountBakerInfo)
        case delegationSupport @av of
            SAVDelegationNotSupported ->
                return $
                    Just
                        ( AccountBaker
                            { _accountBakerInfo = BakerInfoExV0 abi,
                              ..
                            },
                          pab ^. accountBakerInfoEx
                        )
            SAVDelegationSupported -> do
                ebi <- refLoad (pab ^. bakerPoolInfoRef)
                return $
                    Just
                        ( AccountBaker
                            { _accountBakerInfo = BakerInfoExV1 abi ebi,
                              ..
                            },
                          pab ^. accountBakerInfoEx
                        )

-- |Get the baker (if any) attached to an account.
getBaker :: forall m av. (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (Maybe (AccountBaker av))
getBaker acc = fmap fst <$> getBakerAndInfoRef acc

-- |Get the baker and baker info reference (if any) attached to the account.
getBakerInfoRef ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    PersistentAccount av ->
    m (Maybe (PersistentBakerInfoEx av))
getBakerInfoRef acc = case acc ^. accountBaker of
    Null -> return Nothing
    Some bref -> do
        pab <- refLoad bref
        return (Just (pab ^. accountBakerInfoEx))

-- |Get the delegator (if any) attached to the account.
getDelegator :: (MonadBlobStore m, IsAccountVersion av) => PersistentAccount av -> m (Maybe (AccountDelegation av))
getDelegator PersistentAccount{_accountStake = PersistentAccountStakeDelegate del} = Just <$> refLoad del
getDelegator _ = return Nothing

-- |Get the baker or stake delegation information attached to an account.
getStake :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (AccountStake av)
getStake acc = loadAccountStake (acc ^. accountStake)

-- |Determine if an account has stake as a baker or delegator.
hasStake :: PersistentAccount av -> Bool
hasStake acc = case acc ^. accountStake of
    PersistentAccountStakeNone -> False
    _ -> True

-- |Get details about an account's stake.
getStakeDetails :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (StakeDetails av)
getStakeDetails acc = case acc ^. accountStake of
    PersistentAccountStakeNone -> return StakeDetailsNone
    PersistentAccountStakeBaker bkrRef -> do
        baker <- refLoad bkrRef
        return $!
            StakeDetailsBaker
                { sdStakedCapital = baker ^. stakedAmount,
                  sdRestakeEarnings = baker ^. stakeEarnings,
                  sdPendingChange = baker ^. bakerPendingChange
                }
    PersistentAccountStakeDelegate dlgRef -> do
        dlg <- refLoad dlgRef
        return $!
            StakeDetailsDelegator
                { sdStakedCapital = dlg ^. delegationStakedAmount,
                  sdRestakeEarnings = dlg ^. delegationStakeEarnings,
                  sdPendingChange = dlg ^. delegationPendingChange,
                  sdDelegationTarget = dlg ^. delegationTarget
                }

-- |Gets the amount of a baker's stake, or 'Nothing' if the account is not a baker.
getBakerStakeAmount :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (Maybe Amount)
getBakerStakeAmount acc = case acc ^. accountBaker of
    Null -> return Nothing
    Some bref -> do
        bkr <- refLoad bref
        return . Just $! bkr ^. stakedAmount

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: forall m av. (MonadBlobStore m, AVStructureV0 av) => AccountUpdate -> PersistentAccount av -> m (PersistentAccount av)
updateAccount !upd !acc = do
    releaseScheduleUpdate <- case upd ^. auReleaseSchedule of
        Just l -> do
            rData <- refLoad (acc ^. accountReleaseSchedule)
            let newLockedFunds = amountToDelta $ foldl' (+) 0 (concatMap (\(values, _) -> map snd values) l)
            newReleaseSchedule <- foldlM (flip addReleases) rData l
            releaseScheduleRef <- refMake newReleaseSchedule
            return $ (accountAmount %~ applyAmountDelta newLockedFunds) . (accountReleaseSchedule .~ releaseScheduleRef)
        Nothing -> return id
    encryptedAmountUpdate <- case upd ^. auEncrypted of
        Just encUpd -> do
            encAmount <- refLoad (acc ^. accountEncryptedAmount)
            newEncryptedAmount <-
                ( case encUpd of
                    Add{..} -> addIncomingEncryptedAmount newAmount
                    ReplaceUpTo{..} -> replaceUpTo aggIndex newAmount
                    AddSelf{..} -> addToSelfEncryptedAmount newAmount
                    )
                    encAmount
            encryptedAmountRef <- refMake newEncryptedAmount
            return (accountEncryptedAmount .~ encryptedAmountRef)
        Nothing -> return id
    return $!
        acc
            & accountNonce %~ setMaybe (upd ^. auNonce)
            & accountAmount %~ applyAmountDelta (upd ^. auAmount . non 0)
            & releaseScheduleUpdate
            & encryptedAmountUpdate
  where
    setMaybe (Just x) _ = x
    setMaybe Nothing y = y

-- ** Updates

-- |Add or remove credentials on an account.
-- The caller must ensure the following, which are not checked:
--
-- * Any credential index that is removed must already exist.
-- * The credential with index 0 must not be removed.
-- * Any credential index that is added must not exist after the removals take effect.
-- * At least one credential remains after all removals and additions.
-- * Any new threshold is at most the number of accounts remaining (and at least 1).
updateAccountCredentials ::
    (MonadBlobStore m) =>
    -- |Credentials to remove
    [CredentialIndex] ->
    -- |Credentials to add
    Map.Map CredentialIndex AccountCredential ->
    -- |New account threshold
    AccountThreshold ->
    -- |Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentials cuRemove cuAdd cuAccountThreshold =
    setPAD (updateCredentials cuRemove cuAdd cuAccountThreshold)

-- |Optionally update the verification keys and signature threshold for an account.
-- Precondition: The credential with given credential index exists.
updateAccountCredentialKeys ::
    (MonadBlobStore m) =>
    -- |Credential to update
    CredentialIndex ->
    -- |New public keys
    CredentialPublicKeys ->
    -- |Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentialKeys credIndex credKeys = setPAD (updateCredentialKeys credIndex credKeys)

-- |Add an amount to the account's balance.
addAmount :: (MonadBlobStore m) => Amount -> PersistentAccount av -> m (PersistentAccount av)
addAmount !amt acc = return $! acc & accountAmount +~ amt

applyPendingStakeChange :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (PersistentAccount av)
applyPendingStakeChange acc = case acc ^. accountStake of
    PersistentAccountStakeNone -> noPendingChange
    PersistentAccountStakeBaker ebr -> do
        pab@PersistentAccountBaker{..} <- refLoad ebr
        case _bakerPendingChange of
            NoChange -> noPendingChange
            ReduceStake am _ -> do
                newBaker <- refMake pab{_bakerPendingChange = NoChange, _stakedAmount = am}
                return $! acc & accountStake .~ PersistentAccountStakeBaker newBaker
            RemoveStake _ -> return $! acc & accountStake .~ PersistentAccountStakeNone
    PersistentAccountStakeDelegate ebr -> do
        pad@AccountDelegationV1{..} <- refLoad ebr
        case _delegationPendingChange of
            NoChange -> noPendingChange
            ReduceStake am _ -> do
                newDelegator <- refMake pad{_delegationPendingChange = NoChange, _delegationStakedAmount = am}
                return $! acc & accountStake .~ PersistentAccountStakeDelegate newDelegator
            RemoveStake _ -> return $! acc & accountStake .~ PersistentAccountStakeNone
  where
    noPendingChange = error "Account has no pending stake change"

-- |Add a baker to an account for account version 0.
-- This will replace any existing staking information on the account.
addBakerV0 ::
    (MonadBlobStore m) =>
    BakerId ->
    BakerAdd ->
    PersistentAccount 'AccountV0 ->
    m (PersistentAccount 'AccountV0)
addBakerV0 bid BakerAdd{..} acc = do
    newBakerInfo <-
        refMake $
            bakerKeyUpdateToInfo bid baKeys
    newPAB <-
        refMake
            PersistentAccountBaker
                { _stakedAmount = baStake,
                  _stakeEarnings = baStakeEarnings,
                  _accountBakerInfo = newBakerInfo,
                  _extraBakerInfo = PersistentExtraBakerInfo (),
                  _bakerPendingChange = NoChange
                }
    return $! acc{_accountStake = PersistentAccountStakeBaker newPAB}

-- |Add a baker to an account for account version 1.
-- This will replace any existing staking information on the account.
addBakerV1 ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av, AVSupportsDelegation av) =>
    -- |Extended baker info
    BakerInfoEx av ->
    -- |Baker's equity capital
    Amount ->
    -- |Whether earnings are restaked
    Bool ->
    -- |Account to add baker to
    PersistentAccount av ->
    m (PersistentAccount av)
addBakerV1 BakerInfoExV1{..} stake restake acc = do
    poolInfoRef <- refMake _bieBakerPoolInfo
    bakerInfoRef <- refMake _bieBakerInfo
    bakerRef <-
        refMake
            PersistentAccountBaker
                { _stakedAmount = stake,
                  _stakeEarnings = restake,
                  _accountBakerInfo = bakerInfoRef,
                  _extraBakerInfo = makePersistentExtraBakerInfoV1 poolInfoRef,
                  _bakerPendingChange = NoChange
                }
    return $! acc{_accountStake = PersistentAccountStakeBaker bakerRef}

-- |Add a delegator to an account.
-- This will replace any existing staking information on the account.
addDelegator ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    AccountDelegation av ->
    PersistentAccount av ->
    m (PersistentAccount av)
addDelegator del acc = do
    delegatorRef <- refMake del
    return $! acc{_accountStake = PersistentAccountStakeDelegate delegatorRef}

-- |Update the pool info on a baker account.
-- This MUST only be called with an account that is a baker.
updateBakerPoolInfo ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av, AVSupportsDelegation av) =>
    BakerPoolInfoUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateBakerPoolInfo upd acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef}
    | upd == emptyBakerPoolInfoUpdate = return acc
    | otherwise = do
        oldBaker <- refLoad oldBakerRef
        oldPoolInfo <- refLoad (oldBaker ^. bakerPoolInfoRef)
        newPoolInfoRef <- refMake $ applyBakerPoolInfoUpdate upd oldPoolInfo
        newBakerRef <- refMake $! oldBaker & bakerPoolInfoRef .~ newPoolInfoRef
        return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
updateBakerPoolInfo _ _ = error "updateBakerPoolInfo invariant violation: account is not a baker"

-- |Set the baker keys on a baker account.
-- This MUST only be called with an account that is a baker.
setBakerKeys ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    BakerKeyUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
setBakerKeys upd acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef} = do
    oldBaker <- refLoad oldBakerRef
    oldBakerInfo <- refLoad (oldBaker ^. accountBakerInfo)
    newBakerInfoRef <-
        refMake $!
            BakerInfo
                { _bakerIdentity = _bakerIdentity oldBakerInfo,
                  _bakerAggregationVerifyKey = bkuAggregationKey upd,
                  _bakerSignatureVerifyKey = bkuSignKey upd,
                  _bakerElectionVerifyKey = bkuElectionKey upd
                }
    newBakerRef <- refMake $! oldBaker & accountBakerInfo .~ newBakerInfoRef
    return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
setBakerKeys _ _ = error "setBakerKeys invariant violation: account is not a baker"

-- |Set the stake of a baker or delegator account.
-- This MUST only be called with an account that is either a baker or delegator.
-- This does no check that the staked amount is sensible, and has no effect on pending changes.
setStake ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
setStake newAmount acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef} = do
    oldBaker <- refLoad oldBakerRef
    newBakerRef <- refMake $! oldBaker & stakedAmount .~ newAmount
    return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
setStake newAmount acc@PersistentAccount{_accountStake = PersistentAccountStakeDelegate oldDelRef} = do
    oldDel <- refLoad oldDelRef
    newDelRef <- refMake $ oldDel & delegationStakedAmount .~ newAmount
    return $! acc{_accountStake = PersistentAccountStakeDelegate newDelRef}
setStake _ _ = error "setStake invariant violation: account is not a baker or delegator"

-- |Set whether a baker or delegator account restakes its earnings.
-- This MUST only be called with an account that is either a baker or delegator.
setRestakeEarnings ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    Bool ->
    PersistentAccount av ->
    m (PersistentAccount av)
setRestakeEarnings restake acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef} = do
    oldBaker <- refLoad oldBakerRef
    newBakerRef <- refMake $! oldBaker & stakeEarnings .~ restake
    return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
setRestakeEarnings restake acc@PersistentAccount{_accountStake = PersistentAccountStakeDelegate oldDelRef} = do
    oldDel <- refLoad oldDelRef
    newDelRef <- refMake $ oldDel & delegationStakeEarnings .~ restake
    return $! acc{_accountStake = PersistentAccountStakeDelegate newDelRef}
setRestakeEarnings _ _ = error "setRestakeEarnings invariant violation: account is not a baker or delegator"

-- |Set the pending change on baker or delegator account.
-- This MUST only be called with an account that is either a baker or delegator.
setStakePendingChange ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    StakePendingChange av ->
    PersistentAccount av ->
    m (PersistentAccount av)
setStakePendingChange newPC acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef} = do
    oldBaker <- refLoad oldBakerRef
    newBakerRef <- refMake $! oldBaker & bakerPendingChange .~ newPC
    return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
setStakePendingChange newPC acc@PersistentAccount{_accountStake = PersistentAccountStakeDelegate oldDelRef} = do
    oldDel <- refLoad oldDelRef
    newDelRef <- refMake $ oldDel & delegationPendingChange .~ newPC
    return $! acc{_accountStake = PersistentAccountStakeDelegate newDelRef}
setStakePendingChange _ _ = error "setStakePendingChange invariant violation: account is not a baker or delegator"

-- |Set the target of a delegating account.
-- This MUST only be called with an account that is a delegator.
setDelegationTarget ::
    (MonadBlobStore m, IsAccountVersion av) =>
    DelegationTarget ->
    PersistentAccount av ->
    m (PersistentAccount av)
setDelegationTarget target acc@PersistentAccount{_accountStake = PersistentAccountStakeDelegate oldDelRef} = do
    oldDel <- refLoad oldDelRef
    newDelRef <- refMake $ oldDel & delegationTarget .~ target
    return $! acc{_accountStake = PersistentAccountStakeDelegate newDelRef}
setDelegationTarget _ _ = error "setDelegationTarget invariant violation: account is not a delegator"

-- |Remove any staking on an account.
removeStaking ::
    (MonadBlobStore m, IsAccountVersion av) =>
    PersistentAccount av ->
    m (PersistentAccount av)
removeStaking acc = return $! acc{_accountStake = PersistentAccountStakeNone}

-- |Set the commission rates on a baker account.
-- This MUST only be called with an account that is a baker.
setCommissionRates ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av, AVSupportsDelegation av) =>
    CommissionRates ->
    PersistentAccount av ->
    m (PersistentAccount av)
setCommissionRates rates acc@PersistentAccount{_accountStake = PersistentAccountStakeBaker oldBakerRef} = do
    oldBaker <- refLoad oldBakerRef
    oldPoolInfo <- refLoad (oldBaker ^. bakerPoolInfoRef)
    newPoolInfoRef <- refMake $! oldPoolInfo & poolCommissionRates .~ rates
    newBakerRef <- refMake $! oldBaker & bakerPoolInfoRef .~ newPoolInfoRef
    return $! acc{_accountStake = PersistentAccountStakeBaker newBakerRef}
setCommissionRates _ _ = error "setCommissionRates invariant violation: account is not a baker"

-- |Unlock scheduled releases on an account up to and including the given timestamp.
-- This returns the next timestamp at which a release is scheduled for the account, if any,
-- as well as the updated account.
unlockReleases :: (MonadBlobStore m) => Timestamp -> PersistentAccount av -> m (Maybe Timestamp, PersistentAccount av)
unlockReleases ts acc = do
    rData <- refLoad (acc ^. accountReleaseSchedule)
    (_, nextTs, rData') <- unlockAmountsUntil ts rData
    rDataRef <- refMake rData'
    let !acc' = acc & accountReleaseSchedule .~ rDataRef
    return (nextTs, acc')

-- ** Creation

-- |Create an empty account with the given public key, address and credential.
newAccount ::
    forall m av.
    (MonadBlobStore m) =>
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
                } ::
                PersistingAccountData
    _persistingData <- refMake newPData
    let _accountNonce = minNonce
        _accountAmount = 0
        _accountStake = PersistentAccountStakeNone @av
    accountEncryptedAmountData <- initialPersistentAccountEncryptedAmount
    _accountEncryptedAmount <- refMake accountEncryptedAmountData
    let relSched = emptyAccountReleaseSchedule
    _accountReleaseSchedule <- refMake relSched
    return PersistentAccount{..}

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => Transient.Account av -> m (PersistentAccount av)
makePersistentAccount tacc@Transient.Account{..} = do
    _persistingData <- refMake (tacc ^. persistingAccountData)
    _accountEncryptedAmount' <- refMake =<< storePersistentAccountEncryptedAmount _accountEncryptedAmount
    _accountReleaseSchedule' <- refMake =<< storePersistentAccountReleaseSchedule _accountReleaseSchedule
    _accountStake <- case _accountStaking of
        AccountStakeNone -> return PersistentAccountStakeNone
        AccountStakeBaker ab -> PersistentAccountStakeBaker <$> (refMake =<< makePersistentAccountBaker ab)
        AccountStakeDelegate ad@AccountDelegationV1{} -> PersistentAccountStakeDelegate <$> refMake ad
    return PersistentAccount{_accountEncryptedAmount = _accountEncryptedAmount', _accountReleaseSchedule = _accountReleaseSchedule', ..}

-- |Make a 'PersistentAccount' reference from a hashed 'Transient.Account'.
makePersistentAccountRef ::
    (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) =>
    Hashed' (AccountHash av) (Transient.Account av) ->
    m (HashedCachedRef c (PersistentAccount av))
makePersistentAccountRef (Hashed tacc acctHash) = do
    pacc <- makePersistentAccount tacc
    makeHashedCachedRef pacc (theAccountHash acctHash)

-- ** Migration

-- |See documentation of @migratePersistentBlockState@.
migratePersistentAccount ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      StructureV0 oldpv,
      StructureV0 pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccount migration PersistentAccount{..} = do
    !newAccountEncryptedAmount <- migrateEagerBufferedRef migratePersistentEncryptedAmount _accountEncryptedAmount
    !newAccountReleaseSchedule <- migrateEagerBufferedRef migratePersistentAccountReleaseSchedule _accountReleaseSchedule
    !newPersistingData <- migrateEagerlyHashedBufferedRefKeepHash return _persistingData
    !newAccountStake <- migratePersistentAccountStake migration _accountStake
    return
        PersistentAccount
            { _accountNonce = _accountNonce,
              _accountAmount = _accountAmount,
              _accountEncryptedAmount = newAccountEncryptedAmount,
              _accountReleaseSchedule = newAccountReleaseSchedule,
              _persistingData = newPersistingData,
              _accountStake = newAccountStake
            }

-- ** Serialization

-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
serializeAccount :: forall m av. (MonadBlobStore m, MonadPut m, IsAccountVersion av, AVStructureV0 av) => GlobalContext -> PersistentAccount av -> m ()
serializeAccount cryptoParams PersistentAccount{..} = do
    PersistingAccountData{..} <- refLoad _persistingData
    let initialCredId =
            credId
                ( Map.findWithDefault
                    (error "Account missing initial credential")
                    initialCredentialIndex
                    _accountCredentials
                )
        asfExplicitAddress = _accountAddress /= addressFromRegIdRaw initialCredId
        -- There is an opportunity for improvement here. There is no need to go
        -- through the deserialized key. The way the encryption key is formed is
        -- that the first half is the generator, the second half is the credId.
        -- So we could just concatenate them. This requires a bit of scaffolding
        -- to get the right component out of cryptoParams, so it is not yet
        -- done.
        asfExplicitEncryptionKey = _accountEncryptionKey /= toRawEncryptionKey (makeEncryptionKey cryptoParams (unsafeCredIdFromRaw initialCredId))
        (asfMultipleCredentials, putCredentials) = case Map.toList _accountCredentials of
            [(i, cred)] | i == initialCredentialIndex -> (False, put cred)
            _ -> (True, putSafeMapOf put put _accountCredentials)
        asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
        asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials
    aea <- refLoad _accountEncryptedAmount
    (asfExplicitEncryptedAmount, putEA) <-
        putAccountEncryptedAmountV0 aea <&> \case
            Nothing -> (False, return ())
            Just p -> (True, p)
    arSched <- loadPersistentAccountReleaseSchedule =<< refLoad _accountReleaseSchedule
    let asfExplicitReleaseSchedule = arSched /= Transient.emptyAccountReleaseSchedule
        asfHasBakerOrDelegation = case _accountStake of
            PersistentAccountStakeNone -> False
            _ -> True
    stake <- loadAccountStake _accountStake
    liftPut $ do
        put AccountSerializationFlags{..}
        when asfExplicitAddress $ put _accountAddress
        when asfExplicitEncryptionKey $ put _accountEncryptionKey
        unless asfThresholdIsOne $ put (aiThreshold _accountVerificationKeys)
        putCredentials
        when asfHasRemovedCredentials $ put (_accountRemovedCredentials ^. unhashed)
        put _accountNonce
        put _accountAmount
        putEA
        when asfExplicitReleaseSchedule $ put arSched
        when asfHasBakerOrDelegation $ serializeAccountStake stake

-- ** Conversion

-- |Converts an account to a transient (i.e. in memory) account. (Used for testing.)
toTransientAccount :: forall m av. (MonadBlobStore m, IsAccountVersion av, AVStructureV0 av) => PersistentAccount av -> m (Transient.Account av)
toTransientAccount PersistentAccount{..} = do
    _accountPersisting <- Transient.makeAccountPersisting <$> refLoad _persistingData
    _accountEncryptedAmount <- loadPersistentAccountEncryptedAmount =<< refLoad _accountEncryptedAmount
    _accountReleaseSchedule <- loadPersistentAccountReleaseSchedule =<< refLoad _accountReleaseSchedule
    _accountStaking <- case _accountStake of
        PersistentAccountStakeNone -> return AccountStakeNone
        PersistentAccountStakeBaker bkr -> AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkr)
        PersistentAccountStakeDelegate dlg -> AccountStakeDelegate <$> refLoad dlg
    return $ Transient.Account{..}
