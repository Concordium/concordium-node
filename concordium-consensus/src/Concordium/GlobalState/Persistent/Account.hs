{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.Account where

import Data.Serialize
import Lens.Micro.Platform
import Data.Word
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map

import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.EncryptedTransfers
import Concordium.Types.HashableTo
import Concordium.Types hiding (_incomingEncryptedAmounts, _startIndex, _selfAmount, _aggregatedAmount)
import Concordium.Constants
import qualified Concordium.Types as TY
import Concordium.ID.Types
import Concordium.ID.Parameters
import qualified Concordium.Genesis.Data.P4 as P4
import qualified Concordium.Types.Migration as Migration

import Concordium.Types.Accounts hiding (_stakedAmount, _stakeEarnings, _accountBakerInfo)
import qualified Concordium.Types.Accounts as Transient
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)

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

-- * Encrypted amounts

-- | The persistent version of the encrypted amount structure per account.
-- We use 'EagerBufferedRef's for the encrypted amounts so that when a 'PersistentAccount' is
-- loaded, the entire encrypted amount will also be loaded.
-- This is useful, since the encrypted amount structure is used for computing the
-- hash of the account. (See $PersistentAccountCacheable.)
data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount {
  -- | Encrypted amount that is a result of this accounts' actions.
  -- In particular this list includes the aggregate of
  --
  -- - remaining amounts that result when transferring to public balance
  -- - remaining amounts when transferring to another account
  -- - encrypted amounts that are transferred from public balance
  --
  -- When a transfer is made all of these must always be used.
  _selfAmount :: !(EagerBufferedRef EncryptedAmount),
  -- | Starting index for incoming encrypted amounts. If an aggregated amount is present
  -- then this index is associated with such an amount and the list of incoming encrypted amounts
  -- starts at the index @_startIndex + 1@.
  _startIndex :: !EncryptedAmountAggIndex,
  -- | Amounts starting at @startIndex@ (or at @startIndex + 1@ if there is an aggregated amount present).
  -- They are assumed to be numbered sequentially. This list will never contain more than 'maxNumIncoming'
  -- (or @maxNumIncoming - 1@ if there is an aggregated amount present) values.
  _incomingEncryptedAmounts :: !(Seq.Seq (EagerBufferedRef EncryptedAmount)),
  -- |If 'Just', the amount that has resulted from aggregating other amounts and the
  -- number of aggregated amounts (must be at least 2 if present).
  _aggregatedAmount :: !(Maybe (EagerBufferedRef EncryptedAmount, Word32))
  } deriving Show

-- | Create an empty PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount :: MonadBlobStore m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
  _selfAmount <- refMake mempty
  return PersistentAccountEncryptedAmount {
    _startIndex = 0,
    _incomingEncryptedAmounts = Seq.Empty,
    _aggregatedAmount = Nothing,
    ..
    }

-- |Serialize a 'PersistentAccountEncryptedAmount' if it is not the empty
-- encrypted amount (in which case, @Nothing@ is returned).
--
-- This should match the serialization format of 'AccountEncryptedAmount' exactly.
putAccountEncryptedAmountV0 :: (MonadBlobStore m) => PersistentAccountEncryptedAmount -> m (Maybe Put)
putAccountEncryptedAmountV0 PersistentAccountEncryptedAmount{..} = do
    sAmt <- refLoad _selfAmount
    if isZeroEncryptedAmount sAmt && _startIndex == 0 && Seq.null _incomingEncryptedAmounts && isNothing _aggregatedAmount then
      return Nothing
    else do
      ieas <- mapM refLoad _incomingEncryptedAmounts
      putAgg <- case _aggregatedAmount of
          Nothing -> return $ putWord32be 0
          Just (eref, n) -> do
            e <- refLoad eref
            return $ do
              putWord32be n
              put e
      return . Just $ do
        put sAmt
        put _startIndex
        putWord32be (fromIntegral (Seq.length ieas))
        mapM_ put ieas
        putAgg

-- | Given an AccountEncryptedAmount, create a Persistent version of it
storePersistentAccountEncryptedAmount :: MonadBlobStore m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _selfAmount <- refMake _selfAmount
  _incomingEncryptedAmounts <- mapM refMake _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> refMake e
  return PersistentAccountEncryptedAmount{..}

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount :: MonadBlobStore m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- refLoad _selfAmount
  _incomingEncryptedAmounts <- mapM refLoad _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> refLoad e
  return AccountEncryptedAmount{..}

instance MonadBlobStore m => BlobStorable m PersistentAccountEncryptedAmount where
  storeUpdate PersistentAccountEncryptedAmount{..} = do
    (pSelf, _selfAmount) <- storeUpdate _selfAmount
    (pAmounts, _incomingEncryptedAmounts) <- Seq.unzip <$> mapM storeUpdate _incomingEncryptedAmounts
    (pAgg, _aggregatedAmount) <- case _aggregatedAmount of
      Nothing -> return (putWord32be  0, _aggregatedAmount)
      Just (e, n) -> do
        (pE, newE) <- storeUpdate e
        return (putWord32be n >> pE, Just (newE, n))
    return . (, PersistentAccountEncryptedAmount {..}) $ do
      pSelf
      put _startIndex
      putWord32be $ fromIntegral $ length pAmounts
      sequence_ pAmounts
      pAgg

  store a = fst <$> storeUpdate a
  load = do
    pSelf <- load
    _startIndex <- get
    numAmounts <- fromIntegral <$> getWord32be
    pAmounts <- replicateM numAmounts load
    numAggregatedAmount <- getWord32be
    agg <- case numAggregatedAmount of
      0 -> return Nothing
      n | n > 1 -> Just <$> load
        | otherwise -> fail "Number of aggregated amounts cannot be 1"
    return $ do
      _selfAmount <- pSelf
      _incomingEncryptedAmounts <- Seq.fromList <$> sequence pAmounts
      _aggregatedAmount <- case agg of
        Just v -> do
          vVal <- v
          return $ Just (vVal, numAggregatedAmount)
        Nothing -> return Nothing
      return PersistentAccountEncryptedAmount {..}

instance (MonadBlobStore m) => Cacheable m PersistentAccountEncryptedAmount where

-- * Staking information

-- |Extra info (beyond 'BakerInfo') associated with a baker.
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

instance forall av. IsAccountVersion av => Show (PersistentExtraBakerInfo av) where
    show = case accountVersion @av of
        SAccountV0 -> show . _theExtraBakerInfo
        SAccountV1 -> show . _theExtraBakerInfo

instance forall av m. (Applicative m) => Cacheable m (PersistentExtraBakerInfo av)

migratePersistentExtraBakerInfo' ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv
    , SupportMigration m t
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

migratePersistentExtraBakerInfo ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv
    , SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentExtraBakerInfo (AccountVersionFor oldpv) ->
    t m (PersistentExtraBakerInfo (AccountVersionFor pv))
migratePersistentExtraBakerInfo migration =
    fmap PersistentExtraBakerInfo
        . migratePersistentExtraBakerInfo' migration
        . _theExtraBakerInfo

-- |A persistent version of 'BakerInfoEx'.
-- (This structure is always fully cached in memory. See $PersistentAccountCacheable for details.)
data PersistentBakerInfoEx av = PersistentBakerInfoEx {
    bakerInfoRef :: !(EagerBufferedRef BakerInfo),
    bakerInfoExtra :: !(PersistentExtraBakerInfo av)
} deriving (Show)

loadBakerId :: MonadBlobStore m => PersistentBakerInfoEx av -> m BakerId
loadBakerId PersistentBakerInfoEx {..} = do
  bi <- refLoad bakerInfoRef
  return (_bakerIdentity bi)

migratePersistentBakerInfoEx ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv
    , SupportMigration m t
    ) => StateMigrationParameters oldpv pv ->
    PersistentBakerInfoEx (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoEx (AccountVersionFor pv))
migratePersistentBakerInfoEx migration PersistentBakerInfoEx {..} = do
  newBakerInfoRef <- migrateEagerBufferedRef return bakerInfoRef
  newBakerInfoExtra <- migratePersistentExtraBakerInfo migration bakerInfoExtra
  return PersistentBakerInfoEx {
    bakerInfoRef = newBakerInfoRef,
    bakerInfoExtra = newBakerInfoExtra
    }
  

-- |Load a 'BakerInfoEx' from a 'PersistentBakerInfoEx'.
loadPersistentBakerInfoEx :: forall av m. (IsAccountVersion av, MonadBlobStore m)
  => PersistentBakerInfoEx av -> m (BakerInfoEx av)
loadPersistentBakerInfoEx PersistentBakerInfoEx{..} = do
    bkrInfo <- refLoad bakerInfoRef
    case accountVersion @av of
      SAccountV0 -> return $ BakerInfoExV0 bkrInfo
      SAccountV1 -> do
        bkrInfoEx <- refLoad (bakerInfoExtra ^. theExtraBakerInfo)
        return $ BakerInfoExV1 bkrInfo bkrInfoEx

-- |Construct a 'PersistentBakerInfoEx' from a 'BakerInfoEx'.
makePersistentBakerInfoEx :: (MonadBlobStore m) => BakerInfoEx av -> m (PersistentBakerInfoEx av)
makePersistentBakerInfoEx (BakerInfoExV0 bi) = do
    bakerInfoRef <- refMake bi
    return PersistentBakerInfoEx{bakerInfoExtra = PersistentExtraBakerInfo (), ..}
makePersistentBakerInfoEx (BakerInfoExV1 bi ebi) = do
    bakerInfoRef <- refMake bi
    bakerInfoExtra <- PersistentExtraBakerInfo <$> refMake ebi
    return PersistentBakerInfoEx{..}

instance forall m av. (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentBakerInfoEx av) where
  storeUpdate PersistentBakerInfoEx{..} = do
    (pBakerInfo, newBakerInfo) <- storeUpdate bakerInfoRef
    (pExtraBakerInfo :: Put, newExtraBakerInfo) <- (case accountVersion @av of
        SAccountV0 -> storeUpdate
        SAccountV1 -> storeUpdate)
        $ _theExtraBakerInfo bakerInfoExtra
    let pab = PersistentBakerInfoEx{
            bakerInfoRef = newBakerInfo,
            bakerInfoExtra = PersistentExtraBakerInfo newExtraBakerInfo,
            ..}
    return . (, pab) $ do
      pBakerInfo
      pExtraBakerInfo
  store a = fst <$> storeUpdate a
  load = do
    rBakerInfo <- load
    rExtraBakerInfo <- case accountVersion @av of
        SAccountV0 -> load
        SAccountV1 -> load
    return $ do
      bakerInfoRef <- rBakerInfo
      bakerInfoExtra <- PersistentExtraBakerInfo <$> rExtraBakerInfo
      return PersistentBakerInfoEx{..}

instance (Applicative m) => Cacheable m (PersistentBakerInfoEx av)

-- |A baker associated with an account.
-- (This structure is always fully cached in memory. See $PersistentAccountCacheable for details.)
data PersistentAccountBaker (av :: AccountVersion) = PersistentAccountBaker
  { _stakedAmount :: !Amount
  , _stakeEarnings :: !Bool
  , _accountBakerInfo :: !(EagerBufferedRef BakerInfo)
  , _extraBakerInfo :: !(PersistentExtraBakerInfo av)
  , _bakerPendingChange :: !(StakePendingChange av)
  }

deriving instance (Show (PersistentExtraBakerInfo av)) => Show (PersistentAccountBaker av)

migratePersistentAccountBaker :: forall oldpv pv t m .
    (IsProtocolVersion pv,
     SupportMigration m t) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccountBaker (AccountVersionFor oldpv) -> t m (PersistentAccountBaker (AccountVersionFor pv))
migratePersistentAccountBaker migration PersistentAccountBaker{..} = do
  newAccountBakerInfo <- migrateEagerBufferedRef return _accountBakerInfo
  newExtraBakerInfo <- migratePersistentExtraBakerInfo migration _extraBakerInfo
  return PersistentAccountBaker{
    _stakedAmount = _stakedAmount,
    _stakeEarnings = _stakeEarnings,
    _accountBakerInfo = newAccountBakerInfo,
    _extraBakerInfo = newExtraBakerInfo,
    _bakerPendingChange = Migration.migrateStakePendingChange migration _bakerPendingChange
    }
    

makeLenses ''PersistentAccountBaker

-- |Getter for accessing the 'PersistentBakerInfoEx' of a 'PersistentAccountBaker'.
accountBakerInfoEx :: Getting r (PersistentAccountBaker av) (PersistentBakerInfoEx av)
accountBakerInfoEx = to (\PersistentAccountBaker{..} -> PersistentBakerInfoEx _accountBakerInfo _extraBakerInfo)

-- |Lens for accessing the reference to the 'BakerPoolInfo' of a 'PersistentAccountBaker'.
bakerPoolInfoRef :: Lens' (PersistentAccountBaker 'AccountV1) (EagerBufferedRef BakerPoolInfo)
bakerPoolInfoRef = extraBakerInfo . theExtraBakerInfo

-- |Load a 'PersistentAccountBaker' to an 'AccountBaker'.
loadPersistentAccountBaker :: forall av m. (IsAccountVersion av, MonadBlobStore m)
  => PersistentAccountBaker av
  -> m (AccountBaker av)
loadPersistentAccountBaker PersistentAccountBaker{..} = do
  abi' <- refLoad _accountBakerInfo
  case accountVersion @av of
    SAccountV0 ->
      return AccountBaker{_accountBakerInfo = BakerInfoExV0 abi', ..}
    SAccountV1 -> do
      _bieBakerPoolInfo <- refLoad (_theExtraBakerInfo _extraBakerInfo)
      return AccountBaker{_accountBakerInfo = BakerInfoExV1{_bieBakerInfo = abi', ..}, ..}

makePersistentAccountBaker :: forall av m. (IsAccountVersion av, MonadBlobStore m)
  => AccountBaker av
  -> m (PersistentAccountBaker av)
makePersistentAccountBaker AccountBaker{..} = do
  case accountVersion @av of
    SAccountV0 -> do
      _accountBakerInfo <- refMake (_accountBakerInfo ^. bakerInfo)
      let _extraBakerInfo = PersistentExtraBakerInfo ()
      return PersistentAccountBaker{..}
    SAccountV1 -> do
      abi <- refMake (_accountBakerInfo ^. bakerInfo)
      ebi <- refMake (_bieBakerPoolInfo _accountBakerInfo)
      return PersistentAccountBaker{
          _accountBakerInfo = abi,
          _extraBakerInfo = PersistentExtraBakerInfo ebi,
          ..}

instance forall m av. (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentAccountBaker av) where
  storeUpdate PersistentAccountBaker{..} = do
    (pBakerInfo, newBakerInfo) <- storeUpdate _accountBakerInfo
    (pExtraBakerInfo :: Put, newExtraBakerInfo) <- (case accountVersion @av of
        SAccountV0 -> storeUpdate
        SAccountV1 -> storeUpdate)
        $ _theExtraBakerInfo _extraBakerInfo
    let pab = PersistentAccountBaker{
            _accountBakerInfo = newBakerInfo,
            _extraBakerInfo = PersistentExtraBakerInfo newExtraBakerInfo,
            ..}
    return . (, pab) $ do
      put _stakedAmount
      put _stakeEarnings
      pBakerInfo
      pExtraBakerInfo
      put _bakerPendingChange
  store a = fst <$> storeUpdate a
  load = do
    _stakedAmount <- get
    _stakeEarnings <- get
    rBakerInfo <- load
    rExtraBakerInfo <- case accountVersion @av of
        SAccountV0 -> load
        SAccountV1 -> load
    _bakerPendingChange <- get
    return $ do
      _accountBakerInfo <- rBakerInfo
      _extraBakerInfo <- PersistentExtraBakerInfo <$> rExtraBakerInfo
      return PersistentAccountBaker{..}

instance (Applicative m) => Cacheable m (PersistentAccountBaker av) where

-- |Serialize a 'PersistentAccountBaker'.
putAccountBaker :: forall m av. (IsAccountVersion av, MonadBlobStore m, MonadPut m)
    => PersistentAccountBaker av
    -> m ()
putAccountBaker PersistentAccountBaker{..} = do
    abi <- refLoad _accountBakerInfo
    (abie :: BakerInfoEx av) <- case accountVersion @av of
        SAccountV0 -> return (BakerInfoExV0 abi)
        SAccountV1 -> do
            bpi <- refLoad $ _theExtraBakerInfo _extraBakerInfo
            return (BakerInfoExV1 abi bpi)
    liftPut $ do
      put _stakedAmount
      put _stakeEarnings
      put abie
      put _bakerPendingChange

-- |Staking information associated with an account.
-- IMPORTANT NOTE: The 'Cacheable' instance relies on the fact that no recursive caching is
-- necessary (due to the use of 'EagerBufferedRef's). If this changes, the instance for
-- 'PersistentAccount' will also need to be updated.
data PersistentAccountStake (av :: AccountVersion) where
    PersistentAccountStakeNone :: PersistentAccountStake av
    PersistentAccountStakeBaker :: !(EagerBufferedRef (PersistentAccountBaker av)) -> PersistentAccountStake av
    PersistentAccountStakeDelegate :: !(EagerBufferedRef (AccountDelegation av)) -> PersistentAccountStake av

deriving instance (Show (PersistentExtraBakerInfo av)) => Show (PersistentAccountStake av)

migratePersistentAccountStake :: forall oldpv pv t m .
    (IsProtocolVersion oldpv,
     IsProtocolVersion pv,
     SupportMigration m t) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccountStake (AccountVersionFor oldpv) -> t m (PersistentAccountStake (AccountVersionFor pv))
migratePersistentAccountStake _ PersistentAccountStakeNone = return PersistentAccountStakeNone
migratePersistentAccountStake migration (PersistentAccountStakeBaker r) = PersistentAccountStakeBaker <$!> migrateEagerBufferedRef (migratePersistentAccountBaker migration) r
migratePersistentAccountStake migration (PersistentAccountStakeDelegate r) = 
  case migration of
    StateMigrationParametersTrivial -> PersistentAccountStakeDelegate <$!> migrateEagerBufferedRef return r
    -- the other cases are impossible at the moment since protocols <= 3 do not have delegation.

instance forall m av. (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccountStake av) where
    storeUpdate = case accountVersion @av of
        SAccountV0 -> su0
        SAccountV1 -> su1
      where
        su0 :: PersistentAccountStake 'AccountV0 -> m (Put, PersistentAccountStake 'AccountV0)
        su0 pas@PersistentAccountStakeNone = return (put (refNull :: BlobRef (PersistentAccountBaker av)), pas)
        su0 (PersistentAccountStakeBaker bkrref) = do
          (r, bkrref') <- storeUpdate bkrref
          return (r, PersistentAccountStakeBaker bkrref')
        su1 :: PersistentAccountStake 'AccountV1 -> m (Put, PersistentAccountStake 'AccountV1)
        su1 pas@PersistentAccountStakeNone = return (putWord8 0, pas)
        su1 (PersistentAccountStakeBaker bkrref) = do
          (r, bkrref') <- storeUpdate bkrref
          return (putWord8 1 >> r, PersistentAccountStakeBaker bkrref')
        su1 (PersistentAccountStakeDelegate dlgref) = do
          (r, dlgref') <- storeUpdate dlgref
          return (putWord8 2 >> r, PersistentAccountStakeDelegate dlgref')
    store = fmap fst . storeUpdate
    load = case accountVersion @av of
        SAccountV0 -> l0
        SAccountV1 -> l1
      where
        l0 :: Get (m (PersistentAccountStake av))
        l0 = do
          let toPASB Null = PersistentAccountStakeNone
              toPASB (Some br) = PersistentAccountStakeBaker br
          fmap toPASB <$> load
        l1 :: Get (m (PersistentAccountStake av))
        l1 = getWord8 >>= \case
          0 -> return (pure PersistentAccountStakeNone)
          1 -> fmap PersistentAccountStakeBaker <$> load
          2 -> fmap PersistentAccountStakeDelegate <$> load
          _ -> fail "Invalid staking type"

loadAccountStake :: (MonadBlobStore m, IsAccountVersion av) => PersistentAccountStake av -> m (AccountStake av)
loadAccountStake PersistentAccountStakeNone = return AccountStakeNone
loadAccountStake (PersistentAccountStakeBaker bkr) = AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkr)
loadAccountStake (PersistentAccountStakeDelegate dlg) = AccountStakeDelegate <$> refLoad dlg

instance (Applicative m) => Cacheable m (PersistentAccountStake av) where

instance (MonadBlobStore m, IsAccountVersion av) => MHashableTo m (AccountStakeHash av) (PersistentAccountStake av) where
  getHashM PersistentAccountStakeNone = return $ getAccountStakeHash AccountStakeNone
  getHashM (PersistentAccountStakeBaker bkrref) =
      getAccountStakeHash . AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkrref)
  getHashM (PersistentAccountStakeDelegate dlgref) =
      getAccountStakeHash . AccountStakeDelegate <$> refLoad dlgref

-- * Persisting account data

-- |Type for a reference to an account's persisting data.
type AccountPersisting = EagerlyHashedBufferedRef PersistingAccountData

-- * Persistent account

-- |A (persistent) account.
-- IMPORTANT NOTE: The 'Cacheable' instance relies on the fact that no recursive caching is
-- necessary (due to the use of 'EagerBufferedRef's). This fact is also important to the
-- implementation of 'load'.
data PersistentAccount (av :: AccountVersion) = PersistentAccount {
  -- |Next available nonce for this account.
  _accountNonce :: !Nonce
  -- |Current public account balance.
  ,_accountAmount :: !Amount
  -- |List of encrypted amounts on the account.
  ,_accountEncryptedAmount :: !(EagerBufferedRef PersistentAccountEncryptedAmount)
  -- |Schedule of releases on the account.
  ,_accountReleaseSchedule :: !(EagerBufferedRef AccountReleaseSchedule)
  -- |A pointer to account data that changes rarely
  ,_persistingData :: !AccountPersisting
  -- |The baker info
  ,_accountStake :: !(PersistentAccountStake av)
  }

makeLenses ''PersistentAccount

migratePersistentAccount :: forall oldpv pv t m .
    (IsProtocolVersion oldpv,
     IsProtocolVersion pv,
     SupportMigration m t
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccount migration PersistentAccount {..} = do
  !newAccountEncryptedAmount <- migrateEagerBufferedRef migratePersistentEncryptedAmount _accountEncryptedAmount
  !newAccountReleaseSchedule <- migrateEagerBufferedRef migratePersistentAccountReleaseSchedule _accountReleaseSchedule
  !newPersistingData <- migrateEagerlyHashedBufferedRefKeepHash return _persistingData
  !newAccountStake <- migratePersistentAccountStake migration _accountStake
  return PersistentAccount {
    _accountNonce = _accountNonce,
    _accountAmount = _accountAmount,
    _accountEncryptedAmount = newAccountEncryptedAmount,
    _accountReleaseSchedule = newAccountReleaseSchedule,
    _persistingData = newPersistingData,
    _accountStake = newAccountStake
   }

migratePersistentEncryptedAmount :: (
     SupportMigration m t
    ) =>
    PersistentAccountEncryptedAmount -> t m PersistentAccountEncryptedAmount
migratePersistentEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  newSelfAmount <- migrateEagerBufferedRef return _selfAmount
  newIncomingEncryptedAmounts <- mapM (migrateEagerBufferedRef return) _incomingEncryptedAmounts
  newAggregatedAmount <- mapM (\(ea, numAgg) -> (, numAgg) <$> migrateEagerBufferedRef return ea) _aggregatedAmount
  return PersistentAccountEncryptedAmount{
    _selfAmount = newSelfAmount,
    _startIndex = _startIndex,
    _incomingEncryptedAmounts = newIncomingEncryptedAmounts,
    _aggregatedAmount = newAggregatedAmount
    }

accountBaker :: SimpleGetter (PersistentAccount av) (Nullable (EagerBufferedRef (PersistentAccountBaker av)))
accountBaker = to g
  where
    g PersistentAccount{_accountStake = PersistentAccountStakeBaker bkr} = Some bkr
    g _ = Null

accountDelegator :: SimpleGetter (PersistentAccount av) (Nullable (EagerBufferedRef (AccountDelegation av)))
accountDelegator = to g
  where
    g PersistentAccount{_accountStake = PersistentAccountStakeDelegate del} = Some del
    g _ = Null

deriving instance (IsAccountVersion av, Show (PersistentExtraBakerInfo av)) => Show (PersistentAccount av)

instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentAccount av) where
    storeUpdate PersistentAccount{..} = do
        (pAccData :: Put, accData) <- storeUpdate _persistingData
        (pEnc, encData) <- storeUpdate _accountEncryptedAmount
        (pSched, schedData) <- storeUpdate _accountReleaseSchedule
        (pBkr, stakeData) <- storeUpdate _accountStake
        let !persistentAcc = PersistentAccount {
                _persistingData = accData,
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
    store a = fst <$> storeUpdate a
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
          return PersistentAccount {..}

instance (Applicative m) => Cacheable m (PersistentAccount av) where

instance (MonadBlobStore m, IsAccountVersion av) => MHashableTo m (AccountHash av) (PersistentAccount av) where
    getHashM PersistentAccount{..} = do
        eData <- refLoad _accountEncryptedAmount
        eData' <- loadPersistentAccountEncryptedAmount eData
        sHash <- getHashM _accountReleaseSchedule
        persistingHash <- getHashM _persistingData
        stakeHash <- getHashM _accountStake
        return
            $! makeAccountHash
                AccountHashInputs
                    { ahiNextNonce = _accountNonce,
                      ahiAccountAmount = _accountAmount,
                      ahiAccountEncryptedAmount = eData',
                      ahiAccountReleaseScheduleHash = sHash,
                      ahiPersistingAccountDataHash = persistingHash,
                      ahiAccountStakeHash = stakeHash
                    }

instance (MonadBlobStore m, IsAccountVersion av) => MHashableTo m Hash.Hash (PersistentAccount av) where
    getHashM = fmap (theAccountHash @av) . getHashM

-- |Create an empty account with the given public key, address and credential.
newAccount :: forall m av. (MonadBlobStore m)
    => GlobalContext -> AccountAddress -> AccountCredential -> m (PersistentAccount av)
newAccount cryptoParams _accountAddress credential = do
  let creds = Map.singleton initialCredentialIndex credential
  let newPData = PersistingAccountData {
        _accountEncryptionKey = toRawEncryptionKey (makeEncryptionKey cryptoParams (credId credential)),
        _accountCredentials = toRawAccountCredential <$> creds,
        _accountVerificationKeys = getAccountInformation 1 creds,
        _accountRemovedCredentials = emptyHashedRemovedCredentials,
        ..
        } :: PersistingAccountData
  _persistingData <- refMake newPData
  let _accountNonce = minNonce
      _accountAmount = 0
      _accountStake = PersistentAccountStakeNone @av
  accountEncryptedAmountData <- initialPersistentAccountEncryptedAmount
  _accountEncryptedAmount <- refMake accountEncryptedAmountData
  let relSched = emptyAccountReleaseSchedule
  _accountReleaseSchedule <- refMake relSched
  return PersistentAccount {..}

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: (MonadBlobStore m, IsAccountVersion av) => Transient.Account av -> m (PersistentAccount av)
makePersistentAccount tacc@Transient.Account{..} = do
  _persistingData <- refMake (tacc ^. persistingAccountData)
  _accountEncryptedAmount' <- refMake =<< storePersistentAccountEncryptedAmount _accountEncryptedAmount
  _accountReleaseSchedule' <- refMake =<< storePersistentAccountReleaseSchedule _accountReleaseSchedule
  _accountStake <- case _accountStaking of
    AccountStakeNone -> return PersistentAccountStakeNone
    AccountStakeBaker ab ->  PersistentAccountStakeBaker <$> (refMake =<< makePersistentAccountBaker ab)
    AccountStakeDelegate ad -> PersistentAccountStakeDelegate <$> refMake ad
  return PersistentAccount {_accountEncryptedAmount = _accountEncryptedAmount', _accountReleaseSchedule = _accountReleaseSchedule', ..}

-- |Make a 'PersistentAccount' reference from a hashed 'Transient.Account'.
makePersistentAccountRef ::
    (MonadBlobStore m, IsAccountVersion av) =>
    Hashed' (AccountHash av) (Transient.Account av) ->
    m (HashedCachedRef c (PersistentAccount av))
makePersistentAccountRef (Hashed tacc acctHash) = do
    pacc <- makePersistentAccount tacc
    makeHashedCachedRef pacc (theAccountHash acctHash)

-- |Set the baker of an account.
setPersistentAccountStake :: forall m av. (Monad m)
  => PersistentAccount av
  -> PersistentAccountStake av
  -> m (PersistentAccount av)
setPersistentAccountStake pac newStake = do
  return $! pac{_accountStake = newStake}

-- |Checks whether the two arguments represent the same account. (Used for testing.)
sameAccount :: forall m av. (MonadBlobStore m, IsAccountVersion av) => Transient.Account av -> PersistentAccount av -> m Bool
sameAccount bAcc PersistentAccount{..} = do
  _accountPersisting <- Transient.makeAccountPersisting <$> refLoad _persistingData
  _accountEncryptedAmount <- loadPersistentAccountEncryptedAmount =<< refLoad _accountEncryptedAmount
  _accountReleaseSchedule <- loadPersistentAccountReleaseSchedule =<< refLoad _accountReleaseSchedule
  _accountStaking <-case _accountStake of
    PersistentAccountStakeNone -> return AccountStakeNone
    PersistentAccountStakeBaker bkr -> AccountStakeBaker <$> (loadPersistentAccountBaker =<< refLoad bkr)
    PersistentAccountStakeDelegate dlg -> AccountStakeDelegate <$> refLoad dlg
  return $ Transient.Account{..} == bAcc

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^^. accountAddress@ returns the account's address.
(^^.) :: (MonadBlobStore m)
      => PersistentAccount av
      -> Getting b PersistingAccountData b
      -> m b
acc ^^. l = (^. l) <$!> refLoad (acc ^. persistingData)

{-# INLINE (^^.) #-}
infixl 8 ^^.

-- |Update a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- Used to implement '.~~' and '%~~'.
setPAD :: forall m av. (MonadBlobStore m)
          => (PersistingAccountData -> PersistingAccountData)
          -> PersistentAccount av
          -> m (PersistentAccount av)
setPAD f acc = do
  pData <- refLoad (acc ^. persistingData)
  let newPData = f pData
  newPDataRef <- refMake newPData
  return $! acc & persistingData .~ newPDataRef

-- |Set a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountStakeDelegate .~~ Nothing@ sets the
-- account's stake delegate to 'Nothing'.
(.~~) :: (MonadBlobStore m)
      => ASetter PersistingAccountData PersistingAccountData a b
      -> b
      -> PersistentAccount av
      -> m (PersistentAccount av)
(.~~) l v = setPAD (l .~ v)

{-# INLINE (.~~) #-}
infixr 4 .~~

-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) :: (MonadBlobStore m)
      => ASetter PersistingAccountData PersistingAccountData a b
      -> (a -> b)
      -> PersistentAccount av
      -> m (PersistentAccount av)
(%~~) l f = setPAD (l %~ f)

{-# INLINE (%~~) #-}
infixr 4 %~~

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount :: MonadBlobStore m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addIncomingEncryptedAmount newAmount old = do
  newAmountRef <- refMake newAmount
  case _aggregatedAmount old of
      Nothing -> -- we need to aggregate if we have 'maxNumIncoming' or more incoming amounts
        if Seq.length (_incomingEncryptedAmounts old) >= maxNumIncoming then do
          -- irrefutable because of check above
          let ~(x Seq.:<| y Seq.:<| rest) = _incomingEncryptedAmounts old
          xVal <- refLoad x
          yVal <- refLoad y
          xPlusY <- refMake (xVal <> yVal)
          return old{_incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                     _aggregatedAmount = Just (xPlusY, 2),
                     _startIndex = _startIndex old + 1
                    }
        else return $ old {_incomingEncryptedAmounts = _incomingEncryptedAmounts old Seq.|> newAmountRef}
      Just (e, n) -> do -- we have to aggregate always
        -- irrefutable because of check above
        let ~(x Seq.:<| rest) = _incomingEncryptedAmounts old
        xVal <- refLoad x
        aggVal <- refLoad e
        xPlusY <- refMake (aggVal <> xVal)
        return old{_incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                   _aggregatedAmount = Just (xPlusY, n + 1),
                   _startIndex = _startIndex old + 1
                  }

-- | Drop the encrypted amount with indices up to (but not including) the given one, and add the new amount at the end.
-- This is used when an account is transfering from from an encrypted balance, and the newly added
-- amount is the remaining balance that was not used.
--
-- As mentioned above, the whole 'selfBalance' must always be used in any
-- outgoing action of the account.
replaceUpTo :: MonadBlobStore m => EncryptedAmountAggIndex -> EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
replaceUpTo newIndex newAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- refMake newAmount
  return PersistentAccountEncryptedAmount{
    _startIndex = newStartIndex,
    _incomingEncryptedAmounts = newEncryptedAmounts,
    _aggregatedAmount = newAggregatedAmount,
    ..
  }
  where (newStartIndex, toDrop, dropAggregated) =
          if newIndex > _startIndex
          then
            if isNothing _aggregatedAmount
            then
              (newIndex, fromIntegral (newIndex - _startIndex), False)
            else
              (newIndex, fromIntegral (newIndex - _startIndex) - 1, True)
          else (_startIndex, 0, False)
        newEncryptedAmounts = Seq.drop toDrop _incomingEncryptedAmounts
        newAggregatedAmount = if dropAggregated then Nothing else _aggregatedAmount

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount :: MonadBlobStore m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  newSelf <- refMake . (<> newAmount) =<< refLoad _selfAmount
  return old{_selfAmount = newSelf}


-- * Serialization

-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
serializeAccount :: forall m av. (MonadBlobStore m, MonadPut m, IsAccountVersion av) => GlobalContext -> PersistentAccount av -> m ()
serializeAccount cryptoParams PersistentAccount{..} = do
    PersistingAccountData {..} <- refLoad _persistingData
    let
        initialCredId = credId (Map.findWithDefault
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
    (asfExplicitEncryptedAmount, putEA) <- putAccountEncryptedAmountV0 aea <&> \case
        Nothing -> (False, return ())
        Just p -> (True, p)
    arSched <- loadPersistentAccountReleaseSchedule =<< refLoad _accountReleaseSchedule
    let
        asfExplicitReleaseSchedule = arSched /= Transient.emptyAccountReleaseSchedule
        asfHasBakerOrDelegation = case _accountStake of
          PersistentAccountStakeNone -> False
          _ -> True
    stake <- loadAccountStake _accountStake
    liftPut $ do
        put AccountSerializationFlags {..}
        when asfExplicitAddress $ put _accountAddress
        when asfExplicitEncryptionKey $ put _accountEncryptionKey
        unless asfThresholdIsOne $ put (aiThreshold _accountVerificationKeys)
        putCredentials
        when asfHasRemovedCredentials $ put (_accountRemovedCredentials ^. unhashed)
        put _accountNonce
        put _accountAmount
        putEA
        when asfExplicitReleaseSchedule $ put arSched
        when asfHasBakerOrDelegation $ putAccountStake stake
