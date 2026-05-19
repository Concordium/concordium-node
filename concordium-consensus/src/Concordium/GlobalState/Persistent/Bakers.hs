{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.GlobalState.Persistent.Bakers where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import qualified Concordium.Types.Accounts as BaseAccounts
import Concordium.Types.Execution (DelegationTarget (..))
import Concordium.Types.Migration
import Concordium.Types.Parameters
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Basic.BlockState.LFMBTree (hashAsLFMBTV1)
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put

-- | A list of references to 'BakerInfo's, ordered by increasing 'BakerId'.
--  (This structure is always fully cached in memory, so the 'Cacheable' instance is trivial. See

-- $Concordium.GlobalState.Persistent.Account.PersistentAccountCacheable for details.)

newtype BakerInfos (pv :: ProtocolVersion)
    = BakerInfos (Vec.Vector (PersistentBakerInfoRef (AccountVersionFor pv)))
    deriving (Show)

-- | See documentation of @migratePersistentBlockState@.
migrateBakerInfos ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    BakerInfos oldpv ->
    t m (BakerInfos pv)
migrateBakerInfos migration (BakerInfos inner) = BakerInfos <$> mapM (migratePersistentBakerInfoRef migration) inner

instance (IsProtocolVersion pv, MonadBlobStore m) => BlobStorable m (BakerInfos pv) where
    storeUpdate (BakerInfos v) = do
        v' <- mapM storeUpdate v
        let pv = do
                putLength (Vec.length v')
                mapM_ fst v'
        return (pv, BakerInfos (snd <$> v'))
    load = do
        len <- getLength
        v <- Vec.replicateM len load
        return $ BakerInfos <$> sequence v

-- | This hashing should match (part of) the hashing for 'Basic.EpochBakers'.
instance forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => MHashableTo m H.Hash (BakerInfos pv) where
    getHashM (BakerInfos infos) = do
        loadedInfos <- mapM loadPersistentBakerInfoRef infos
        case sBlockHashVersionFor (protocolVersion @pv) of
            SBlockHashVersion0 -> do
                return $ H.hashLazy $ runPutLazy $ mapM_ put loadedInfos
            SBlockHashVersion1 -> do
                return $ hashAsLFMBTV1 (H.hash "NoBakerInfos") $ H.hashLazy . runPutLazy . put <$> Vec.toList loadedInfos

instance (Applicative m) => Cacheable m (BakerInfos av)

-- | A list of stakes for bakers.
newtype BakerStakes = BakerStakes (Vec.Vector Amount) deriving (Show)

-- | This hashing should match (part of) the hashing for 'Basic.EpochBakers'.
instance HashableTo H.Hash BakerStakes where
    getHash (BakerStakes v) = H.hashLazy $ runPutLazy $ mapM_ put v

instance (Monad m) => MHashableTo m H.Hash BakerStakes

instance Serialize BakerStakes where
    put (BakerStakes v) = putLength (Vec.length v) >> mapM_ put v
    get = do
        len <- getLength
        BakerStakes <$> Vec.replicateM len get

instance (MonadBlobStore m) => BlobStorable m BakerStakes

instance (Applicative m) => Cacheable m BakerStakes

-- | The set of bakers that are eligible to bake in a particular epoch.
--
--  The hashing scheme separately hashes the baker info and baker stakes.
data PersistentEpochBakers (pv :: ProtocolVersion) = PersistentEpochBakers
    { _bakerInfos :: !(HashedBufferedRef (BakerInfos pv)),
      _bakerStakes :: !(HashedBufferedRef BakerStakes),
      _bakerTotalStake :: !Amount,
      _bakerFinalizationCommitteeParameters :: !(OFinalizationCommitteeParameters pv)
    }
    deriving (Show)

makeLenses ''PersistentEpochBakers

-- | Extract the list of pairs of (baker id, staked amount). The list is ordered
--  by increasing 'BakerId'.
--  The intention is that the list will be consumed immediately.
extractBakerStakes :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentEpochBakers pv -> m [(BakerId, Amount)]
extractBakerStakes PersistentEpochBakers{..} = do
    BakerInfos infos <- refLoad _bakerInfos
    BakerStakes stakes <- refLoad _bakerStakes
    zipWithM
        ( \bi bs -> do
            bid <- loadBakerId bi
            return (bid, bs)
        )
        (Vec.toList infos)
        (Vec.toList stakes)

-- | See documentation of @migratePersistentBlockState@.
migratePersistentEpochBakers ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentEpochBakers oldpv ->
    t m (PersistentEpochBakers pv)
migratePersistentEpochBakers migration PersistentEpochBakers{..} = do
    newBakerInfos <- migrateHashedBufferedRef (migrateBakerInfos migration) _bakerInfos
    newBakerStakes <- migrateHashedBufferedRefKeepHash _bakerStakes

    let newBakerFinalizationCommitteeParameters = case migration of
            StateMigrationParametersTrivial -> _bakerFinalizationCommitteeParameters
            StateMigrationParametersP1P2 -> NoParam
            StateMigrationParametersP2P3 -> NoParam
            StateMigrationParametersP3ToP4{} -> NoParam
            StateMigrationParametersP4ToP5 -> NoParam
            (StateMigrationParametersP5ToP6 P6.StateMigrationData{..}) ->
                SomeParam $ P6.updateFinalizationCommitteeParameters migrationProtocolUpdateData
            StateMigrationParametersP6ToP7{} -> _bakerFinalizationCommitteeParameters
            StateMigrationParametersP7ToP8{} ->
                SomeParam $ unOParam _bakerFinalizationCommitteeParameters
            StateMigrationParametersP8ToP9{} ->
                _bakerFinalizationCommitteeParameters
            StateMigrationParametersP9ToP10{} ->
                _bakerFinalizationCommitteeParameters
            StateMigrationParametersP10ToP11{} ->
                _bakerFinalizationCommitteeParameters
    return
        PersistentEpochBakers
            { _bakerInfos = newBakerInfos,
              _bakerStakes = newBakerStakes,
              _bakerFinalizationCommitteeParameters = newBakerFinalizationCommitteeParameters,
              ..
            }

-- | Look up a baker and its stake in a 'PersistentEpochBakers'.
epochBaker ::
    forall m pv.
    (IsProtocolVersion pv, MonadBlobStore m) =>
    BakerId ->
    PersistentEpochBakers pv ->
    m (Maybe (BaseAccounts.BakerInfoEx (AccountVersionFor pv), Amount))
epochBaker bid PersistentEpochBakers{..} = do
    (BakerInfos infoVec) <- refLoad _bakerInfos
    minfo <- binarySearchIM loadPersistentBakerInfoRef (^. BaseAccounts.bakerIdentity) infoVec bid
    forM minfo $ \(idx, binfo) -> do
        (BakerStakes stakeVec) <- refLoad _bakerStakes
        return (binfo, stakeVec Vec.! idx)

-- | Serialize 'PersistentEpochBakers'.
putEpochBakers :: (IsProtocolVersion pv, MonadBlobStore m, MonadPut m) => PersistentEpochBakers pv -> m ()
putEpochBakers peb = do
    BakerInfos bi <- refLoad (peb ^. bakerInfos)
    bInfos <- mapM loadBakerInfo bi
    BakerStakes bStakes <- refLoad (peb ^. bakerStakes)
    assert (Vec.length bInfos == Vec.length bStakes) $
        liftPut $
            putLength (Vec.length bInfos)
    mapM_ sPut bInfos
    mapM_ (liftPut . put) bStakes
    mapM_ (liftPut . put) (peb ^. bakerFinalizationCommitteeParameters)

instance (IsProtocolVersion pv, MonadBlobStore m) => MHashableTo m H.Hash (PersistentEpochBakers pv) where
    getHashM PersistentEpochBakers{..} = do
        hbkrInfos :: H.Hash <- getHashM _bakerInfos
        hbkrStakes :: H.Hash <- getHashM _bakerStakes
        case _bakerFinalizationCommitteeParameters of
            NoParam -> return $ H.hash $ runPut $ do
                put hbkrInfos
                put hbkrStakes
            SomeParam params ->
                return $
                    H.hashOfHashes
                        (H.hashOfHashes hbkrInfos hbkrStakes)
                        (getHash params)

instance (IsProtocolVersion pv, MonadBlobStore m) => BlobStorable m (PersistentEpochBakers pv) where
    storeUpdate PersistentEpochBakers{..} = do
        (pBkrInfos, newBkrInfos) <- storeUpdate _bakerInfos
        (pBkrStakes, newBkrStakes) <- storeUpdate _bakerStakes
        let pBkrs = do
                pBkrInfos
                pBkrStakes
                put _bakerTotalStake
                mapM_ put _bakerFinalizationCommitteeParameters
        return (pBkrs, PersistentEpochBakers{_bakerInfos = newBkrInfos, _bakerStakes = newBkrStakes, ..})
    load = do
        mBkrInfos <- load
        mBkrStakes <- load
        _bakerTotalStake <- get
        _bakerFinalizationCommitteeParameters <- whenSupportedA get
        return $ do
            _bakerInfos <- mBkrInfos
            _bakerStakes <- mBkrStakes
            return PersistentEpochBakers{..}

instance (IsProtocolVersion pv, MonadBlobStore m) => Cacheable m (PersistentEpochBakers pv) where
    cache peb = do
        cBkrInfos <- cache (_bakerInfos peb)
        cBkrStakes <- cache (_bakerStakes peb)
        return peb{_bakerInfos = cBkrInfos, _bakerStakes = cBkrStakes}

-- | Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentEpochBakers pv -> m FullBakers
epochToFullBakers PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM loadBakerInfo infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return
        FullBakers
            { fullBakerInfos = Vec.zipWith mkFullBakerInfo infos stakes,
              bakerTotalStake = _bakerTotalStake
            }
  where
    mkFullBakerInfo info stake = FullBakerInfo info stake

-- | Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakersEx ::
    forall m pv.
    (MonadBlobStore m, IsProtocolVersion pv, PVSupportsDelegation pv) =>
    PersistentEpochBakers pv ->
    m FullBakersEx
epochToFullBakersEx PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM loadPersistentBakerInfoRef infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return
        FullBakersEx
            { bakerInfoExs = Vec.zipWith mkFullBakerInfoEx infos stakes,
              bakerPoolTotalStake = _bakerTotalStake
            }
  where
    mkFullBakerInfoEx :: BaseAccounts.BakerInfoEx (AccountVersionFor pv) -> Amount -> FullBakerInfoEx
    mkFullBakerInfoEx (BaseAccounts.BakerInfoExV1 info extra _isSuspended) stake =
        FullBakerInfoEx (FullBakerInfo info stake) (extra ^. BaseAccounts.poolCommissionRates)

type DelegatorIdTrieSet = Trie.TrieN BufferedFix DelegatorId ()

type BakerIdTrieMap av = Trie.TrieN BufferedFix BakerId (PersistentActiveDelegators av)

-- | The set of delegators to a particular pool.
--  For 'AccountV0', delegation is not supported, and this is essentially the unit type.
data PersistentActiveDelegators (av :: AccountVersion) where
    PersistentActiveDelegatorsV0 :: PersistentActiveDelegators 'AccountV0
    PersistentActiveDelegatorsV1 ::
        (AVSupportsDelegation av) =>
        { -- | The set of delegators to this pool.
          adDelegators :: !DelegatorIdTrieSet,
          -- | The total capital of the delegators to this pool.
          adDelegatorTotalCapital :: !Amount
        } ->
        PersistentActiveDelegators av

-- | Lens to access the total capital of the delegators to the pool.
delegatorTotalCapital :: (AVSupportsDelegation av) => Lens' (PersistentActiveDelegators av) Amount
delegatorTotalCapital f (PersistentActiveDelegatorsV1{..}) =
    (\newDTC -> PersistentActiveDelegatorsV1{adDelegatorTotalCapital = newDTC, ..})
        <$> f adDelegatorTotalCapital

-- | Migrate the representation of a set of delegators to a particular pool.
-- In most cases, the migration is trivial, and the resulting structure is the same.
-- In the case of 'StateMigrationParametersP3ToP4', the set of delegators is introduced as empty,
-- and the total capital is introduced at 0.
migratePersistentActiveDelegators ::
    forall oldpv pv t m.
    (BlobStorable m (), BlobStorable (t m) (), MonadTrans t) =>
    StateMigrationParameters oldpv pv ->
    PersistentActiveDelegators (AccountVersionFor oldpv) ->
    t m (PersistentActiveDelegators (AccountVersionFor pv))
migratePersistentActiveDelegators m = case accountTypeMigrationFor m of
    AccountMigrationTrivial -> \case
        PersistentActiveDelegatorsV0 -> return PersistentActiveDelegatorsV0
        pad@PersistentActiveDelegatorsV1{} -> migrateSimpleV1 pad
    AccountMigrationV0ToV1 -> \case
        PersistentActiveDelegatorsV0 ->
            return
                PersistentActiveDelegatorsV1
                    { adDelegators = Trie.empty,
                      adDelegatorTotalCapital = 0
                    }
    AccountMigrationV1ToV2 -> migrateSimpleV1
    AccountMigrationV2ToV3 -> migrateSimpleV1
    AccountMigrationV3ToV4 -> migrateSimpleV1
    AccountMigrationV4ToV5 -> migrateSimpleV1
  where
    migrateSimpleV1 :: (AVSupportsDelegation oldav, AVSupportsDelegation av) => PersistentActiveDelegators oldav -> t m (PersistentActiveDelegators av)
    migrateSimpleV1 PersistentActiveDelegatorsV1{..} = do
        newDelegators <- Trie.migrateTrieN True return adDelegators
        return PersistentActiveDelegatorsV1{adDelegators = newDelegators, ..}

emptyPersistentActiveDelegators :: forall av. (IsAccountVersion av) => PersistentActiveDelegators av
emptyPersistentActiveDelegators =
    case delegationSupport @av of
        SAVDelegationNotSupported -> PersistentActiveDelegatorsV0
        SAVDelegationSupported -> PersistentActiveDelegatorsV1 Trie.empty 0

deriving instance Show (PersistentActiveDelegators av)

-- | This instance cases on the account version (hence the @IsAccountVersion av@ constraint).
--  The storage for each version is thus essentially independent.
instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentActiveDelegators av) where
    storeUpdate PersistentActiveDelegatorsV0 =
        return (return (), PersistentActiveDelegatorsV0)
    storeUpdate PersistentActiveDelegatorsV1{..} = do
        (pDas, newDs) <- storeUpdate adDelegators
        return (pDas <> put adDelegatorTotalCapital, PersistentActiveDelegatorsV1 newDs adDelegatorTotalCapital)
    load =
        case delegationSupport @av of
            SAVDelegationNotSupported -> return (return PersistentActiveDelegatorsV0)
            SAVDelegationSupported -> do
                madDelegators <- load
                adDelegatorTotalCapital <- get
                return $ do
                    adDelegators <- madDelegators
                    return PersistentActiveDelegatorsV1{..}

-- | The total active capital as stored as part of the 'PersistentActiveBakers' structure.
--  This is not stored for 'AccountV0', so behaves as the unit type in that case.
data TotalActiveCapital (av :: AccountVersion) where
    TotalActiveCapitalV0 :: TotalActiveCapital 'AccountV0
    TotalActiveCapitalV1 :: (AVSupportsDelegation av) => !Amount -> TotalActiveCapital av

deriving instance Show (TotalActiveCapital av)

-- | See documentation of @migratePersistentBlockState@.
migrateTotalActiveCapital ::
    StateMigrationParameters oldpv pv ->
    -- | The total amount staked by all the __bakers__.
    Amount ->
    TotalActiveCapital (AccountVersionFor oldpv) ->
    TotalActiveCapital (AccountVersionFor pv)
migrateTotalActiveCapital m = case accountTypeMigrationFor m of
    AccountMigrationTrivial -> \_ tac -> tac
    AccountMigrationV0ToV1 -> \bts _ -> TotalActiveCapitalV1 bts
    AccountMigrationV1ToV2 -> \_ (TotalActiveCapitalV1 bts) -> TotalActiveCapitalV1 bts
    AccountMigrationV2ToV3 -> \_ (TotalActiveCapitalV1 bts) -> TotalActiveCapitalV1 bts
    AccountMigrationV3ToV4 -> \_ (TotalActiveCapitalV1 bts) -> TotalActiveCapitalV1 bts
    AccountMigrationV4ToV5 -> \_ (TotalActiveCapitalV1 bts) -> TotalActiveCapitalV1 bts

instance (IsAccountVersion av) => Serialize (TotalActiveCapital av) where
    put TotalActiveCapitalV0 = return ()
    put (TotalActiveCapitalV1 amt) = put amt
    get = case delegationSupport @av of
        SAVDelegationNotSupported -> return TotalActiveCapitalV0
        SAVDelegationSupported -> TotalActiveCapitalV1 <$> get

-- | Add an amount to a 'TotalActiveCapital'.
addActiveCapital :: Amount -> TotalActiveCapital av -> TotalActiveCapital av
addActiveCapital _ TotalActiveCapitalV0 = TotalActiveCapitalV0
addActiveCapital amt0 (TotalActiveCapitalV1 amt1) = TotalActiveCapitalV1 $ amt0 + amt1

-- | Subtract a given 'Amount' from a 'TotalActiveCapital'.
--  The amount to subtract should not exceed the total active capital.
--  This is not checked, and could cause an underflow if violated.
subtractActiveCapital :: Amount -> TotalActiveCapital av -> TotalActiveCapital av
subtractActiveCapital _ TotalActiveCapitalV0 = TotalActiveCapitalV0
subtractActiveCapital amt0 (TotalActiveCapitalV1 amt1) = TotalActiveCapitalV1 $ amt1 - amt0

tacAmount :: (AVSupportsDelegation av) => Lens' (TotalActiveCapital av) Amount
tacAmount f (TotalActiveCapitalV1 amt) = TotalActiveCapitalV1 <$> f amt

type AggregationKeySet = Trie.TrieN BufferedFix BakerAggregationVerifyKey ()

-- | Persistent representation of the state of the active bakers and delegators.
data PersistentActiveBakers (av :: AccountVersion) = PersistentActiveBakers
    { -- | For each active baker, this records the set of delegators and their total stake.
      --  (This does not include the baker's own stake.)
      _activeBakers :: !(BakerIdTrieMap av),
      -- | The set of aggregation keys of all active bakers.
      -- This is used to prevent duplicate aggregation keys from being deployed.
      _aggregationKeys :: !AggregationKeySet,
      -- | The set of delegators to the passive pool, with their total stake.
      _passiveDelegators :: !(PersistentActiveDelegators av),
      -- | The total capital staked by all bakers and delegators.
      _totalActiveCapital :: !(TotalActiveCapital av)
    }
    deriving (Show)

makeLenses ''PersistentActiveBakers

-- | Migrate the representation of the active bakers and delegators on protocol update.
--  In most cases, the migration is trivial, and the resulting structure is the same.
--  The exception is migrating from P3 to P4 (where delegation is introduced), where
--  each pool's delegators are introduced as empty, and delegated capital is introduced at 0.
--  In that case, the total active capital is computed by summing the baker stake amounts
--  from the supplied accounts table.
migratePersistentActiveBakers ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      SupportMigration m t,
      Accounts.SupportsPersistentAccount pv (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    -- | Already migrated accounts.
    Accounts.Accounts pv ->
    PersistentActiveBakers (AccountVersionFor oldpv) ->
    t m (PersistentActiveBakers (AccountVersionFor pv))
migratePersistentActiveBakers migration accounts PersistentActiveBakers{..} = do
    newActiveBakers <- Trie.migrateTrieN True (migratePersistentActiveDelegators migration) _activeBakers
    newAggregationKeys <- Trie.migrateTrieN True return _aggregationKeys
    newPassiveDelegators <- migratePersistentActiveDelegators migration _passiveDelegators
    bakerIds <- Trie.keysAsc newActiveBakers
    bakerStakedAmount <-
        foldM
            ( \acc (BakerId aid) ->
                Accounts.indexedAccount aid accounts >>= \case
                    Nothing -> error "Baker account does not exist."
                    Just pa ->
                        accountBakerStakeAmount pa >>= \case
                            Nothing -> error "Baker account not a baker."
                            Just amt -> return $! (acc + amt)
            )
            0
            bakerIds
    let newTotalActiveCapital = migrateTotalActiveCapital migration bakerStakedAmount _totalActiveCapital
    return
        PersistentActiveBakers
            { _activeBakers = newActiveBakers,
              _aggregationKeys = newAggregationKeys,
              _passiveDelegators = newPassiveDelegators,
              _totalActiveCapital = newTotalActiveCapital
            }

-- | Construct a 'PersistentActiveBakers' with no bakers or delegators.
emptyPersistentActiveBakers :: forall av. (IsAccountVersion av) => PersistentActiveBakers av
emptyPersistentActiveBakers = case delegationSupport @av of
    SAVDelegationSupported ->
        PersistentActiveBakers
            { _activeBakers = Trie.empty,
              _aggregationKeys = Trie.empty,
              _passiveDelegators = PersistentActiveDelegatorsV1 Trie.empty 0,
              _totalActiveCapital = TotalActiveCapitalV1 0
            }
    SAVDelegationNotSupported ->
        PersistentActiveBakers
            { _activeBakers = Trie.empty,
              _aggregationKeys = Trie.empty,
              _passiveDelegators = PersistentActiveDelegatorsV0,
              _totalActiveCapital = TotalActiveCapitalV0
            }

totalActiveCapitalV1 :: (AVSupportsDelegation av) => Lens' (PersistentActiveBakers av) Amount
totalActiveCapitalV1 = totalActiveCapital . tac
  where
    tac :: (AVSupportsDelegation av) => Lens' (TotalActiveCapital av) Amount
    tac f (TotalActiveCapitalV1 v) = TotalActiveCapitalV1 <$> f v

-- | A helper function that adds a delegator to a 'PersistentActiveDelegators'.
--  It is assumed that the delegator is not already in the delegators.
addDelegatorHelper ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    DelegatorId ->
    Amount ->
    PersistentActiveDelegators av ->
    m (PersistentActiveDelegators av)
addDelegatorHelper did amt (PersistentActiveDelegatorsV1 dset tot) = do
    newDset <- Trie.insert did () dset
    return $ PersistentActiveDelegatorsV1 newDset (tot + amt)

-- | Add a delegator to the persistent active bakers at a particular target.
--  It is assumed that the delegator is not already delegated to this target.
--  If the target is not valid (i.e. it is a baker, but not in the active bakers) then the result
--  is @Left bid@, where @bid@ is the id of the target baker.  Otherwise, the return value is
--  @Right pab'@ where @pab'@ is the updated 'PersistentActiveBakers'.
--
--  IMPORTANT: This does not update the total active capital!
addDelegator ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    DelegationTarget ->
    DelegatorId ->
    Amount ->
    PersistentActiveBakers av ->
    m (Either BakerId (PersistentActiveBakers av))
addDelegator DelegatePassive did amt pab =
    Right <$> passiveDelegators (addDelegatorHelper did amt) pab
addDelegator (DelegateToBaker bid) did amt pab =
    Trie.lookup bid (pab ^. activeBakers) >>= \case
        Nothing -> return $ Left bid
        Just pad -> do
            pad' <- addDelegatorHelper did amt pad
            newActiveBakers <- Trie.insert bid pad' (pab ^. activeBakers)
            return $ Right $ pab & activeBakers .~ newActiveBakers

-- | Add a delegator to the persistent active bakers at a particular target.
--  It is assumed that the delegator is not already delegated to this target.
--  If the target is a baker, then the baker MUST be in the active bakers.
--
--  IMPORTANT: This does not update the total active capital!
addDelegatorUnsafe ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    DelegationTarget ->
    DelegatorId ->
    Amount ->
    PersistentActiveBakers av ->
    m (PersistentActiveBakers av)
addDelegatorUnsafe DelegatePassive did amt = passiveDelegators (addDelegatorHelper did amt)
addDelegatorUnsafe (DelegateToBaker bid) did amt = activeBakers (fmap snd . Trie.adjust upd bid)
  where
    upd Nothing = error "addDelegatorUnsafe: Baker not found"
    upd (Just pad) = ((),) . Trie.Insert <$> addDelegatorHelper did amt pad

-- | A helper function that removes a delegator from a 'PersistentActiveDelegators'.
--  It is assumed that the delegator is in the delegators with the specified amount.
removeDelegatorHelper ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    DelegatorId ->
    Amount ->
    PersistentActiveDelegators av ->
    m (PersistentActiveDelegators av)
removeDelegatorHelper did amt (PersistentActiveDelegatorsV1 dset tot) = do
    newDset <- Trie.delete did dset
    return $ PersistentActiveDelegatorsV1 newDset (tot - amt)

-- | Remove a delegator from 'PersistentActiveBakers'. It is assumed that the delegator
--  belongs to the pool, and the 'Amount' correctly represents the delegator's contribution.
--
--  IMPORTANT: This does not update the total active capital!
removeDelegator ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    DelegationTarget ->
    DelegatorId ->
    Amount ->
    PersistentActiveBakers av ->
    m (PersistentActiveBakers av)
removeDelegator DelegatePassive did amt pab = passiveDelegators (removeDelegatorHelper did amt) pab
removeDelegator (DelegateToBaker bid) did amt pab = do
    let rdh Nothing = return ((), Trie.NoChange)
        rdh (Just pad) = do
            pad' <- removeDelegatorHelper did amt pad
            return ((), Trie.Insert pad')
    newActiveBakers <- snd <$> Trie.adjust rdh bid (pab ^. activeBakers)
    return $ pab & activeBakers .~ newActiveBakers

-- | Modify the total capital of a pool. The pool MUST already exist.
--
--  IMPORTANT: This does not update the total active capital!
modifyPoolCapitalUnsafe ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    DelegationTarget ->
    (Amount -> Amount) ->
    PersistentActiveBakers av ->
    m (PersistentActiveBakers av)
modifyPoolCapitalUnsafe DelegatePassive change =
    pure . (passiveDelegators . delegatorTotalCapital %~ change)
modifyPoolCapitalUnsafe (DelegateToBaker bid) change =
    activeBakers (fmap snd . Trie.adjust upd bid)
  where
    upd Nothing = error "modifyPoolCapitalUnsafe: Baker not found"
    upd (Just pad) = pure . ((),) . Trie.Insert $ pad & delegatorTotalCapital %~ change

-- | Transfer all delegators from a baker to passive delegation in the 'PersistentActiveBakers'. This does
--  not affect the total stake, and does not remove the baker itself. This returns the list of
--  affected delegators.  (This will have no effect if the baker is not actually a baker, although
--  this function should not be used in that case.)
transferDelegatorsToPassive ::
    (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) =>
    BakerId ->
    PersistentActiveBakers av ->
    m ([DelegatorId], PersistentActiveBakers av)
transferDelegatorsToPassive bid pab = do
    (transferred, newAB) <- Trie.adjust extract bid (pab ^. activeBakers)
    transList <- Trie.keysAsc (adDelegators transferred)
    let oldLPD = pab ^. passiveDelegators . to adDelegators
    newLPD <- foldM (\a d -> Trie.insert d () a) oldLPD transList
    let pab' =
            pab
                & activeBakers .~ newAB
                & passiveDelegators
                    %~ ( \(PersistentActiveDelegatorsV1 _ tot) ->
                            PersistentActiveDelegatorsV1 newLPD (tot + adDelegatorTotalCapital transferred)
                       )
    return (transList, pab')
  where
    extract Nothing = return (emptyPersistentActiveDelegators, Trie.NoChange)
    extract (Just t) = return (t, Trie.Insert emptyPersistentActiveDelegators)

instance
    (IsAccountVersion av, MonadBlobStore m) =>
    BlobStorable m (PersistentActiveBakers av)
    where
    storeUpdate oldPAB@PersistentActiveBakers{..} = do
        (pActiveBakers, newActiveBakers) <- storeUpdate _activeBakers
        (pAggregationKeys, newAggregationKeys) <- storeUpdate _aggregationKeys
        (ppassiveDelegators, newpassiveDelegators) <- storeUpdate _passiveDelegators
        let pPAB = pActiveBakers >> pAggregationKeys >> ppassiveDelegators >> put _totalActiveCapital
        let newPAB =
                oldPAB
                    { _activeBakers = newActiveBakers,
                      _aggregationKeys = newAggregationKeys,
                      _passiveDelegators = newpassiveDelegators
                    }
        return (pPAB, newPAB)
    load = do
        mActiveBakers <- load
        mAggregationKeys <- load
        mpassiveDelegators <- load
        _totalActiveCapital <- get
        return $ do
            _activeBakers <- mActiveBakers
            _aggregationKeys <- mAggregationKeys
            _passiveDelegators <- mpassiveDelegators
            return PersistentActiveBakers{..}

instance (IsAccountVersion av, Applicative m) => Cacheable m (PersistentActiveBakers av)
