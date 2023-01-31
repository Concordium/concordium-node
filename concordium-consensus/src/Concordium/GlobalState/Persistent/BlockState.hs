{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.GlobalState.Persistent.BlockState (
    PersistentBlockState,
    BlockStatePointers (..),
    HashedPersistentBlockState (..),
    hashBlockState,
    PersistentBirkParameters (..),
    initialPersistentState,
    emptyBlockState,
    emptyHashedEpochBlocks,
    emptyPersistentTransactionOutcomes,
    PersistentBlockStateContext (..),
    PersistentState,
    BlockRewardDetails (..),
    PersistentBlockStateMonad (..),
    withNewAccountCache,
    cacheStateAndGetTransactionTable,
    migratePersistentBlockState,
    SupportsPersistentState,
) where

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.Accounts (SupportsPersistentAccount)
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import Concordium.GlobalState.Persistent.Bakers
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import Concordium.GlobalState.Persistent.BlockState.Updates
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.Instances (PersistentInstance (..), PersistentInstanceParameters (..), PersistentInstanceV (..))
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT
import Concordium.GlobalState.Persistent.PoolRewards
import Concordium.GlobalState.Persistent.ReleaseSchedule
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.TransactionTable as TransactionTable
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.ID.Parameters as ID
import qualified Concordium.ID.Types as ID
import Concordium.Kontrol.Bakers
import Concordium.Logger (MonadLogger)
import Concordium.TimeMonad (TimeMonad)
import Concordium.Types
import Concordium.Types.Accounts (AccountBaker (..))
import qualified Concordium.Types.Accounts as BaseAccounts
import qualified Concordium.Types.AnonymityRevokers as ARS
import Concordium.Types.Execution (DelegationTarget (..), TransactionIndex, TransactionSummary)
import qualified Concordium.Types.Execution as Transactions
import Concordium.Types.HashableTo
import qualified Concordium.Types.IdentityProviders as IPS
import Concordium.Types.Queries (CurrentPaydayBakerPoolStatus (..), PoolStatus (..), RewardStatus' (..), makePoolPendingChange)
import Concordium.Types.SeedState
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.UpdateQueues as UQ
import Concordium.Types.Updates
import Concordium.Utils
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import qualified Concordium.Wasm as Wasm

import qualified Control.Monad.Except as MTL
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
import Data.IORef
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Sequence as Seq
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform

-- * Birk parameters

data PersistentBirkParameters (av :: AccountVersion) = PersistentBirkParameters
    { -- |The currently-registered bakers.
      _birkActiveBakers :: !(BufferedRef (PersistentActiveBakers av)),
      -- |The bakers that will be used for the next epoch.
      _birkNextEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers av)),
      -- |The bakers for the current epoch.
      _birkCurrentEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers av)),
      -- |The seed state used to derive the leadership election nonce.
      _birkSeedState :: !SeedState
    }
    deriving (Show)

makeLenses ''PersistentBirkParameters

-- |See documentation of @migratePersistentBlockState@.
--
-- Migrate the birk parameters assuming accounts have already been migrated.
migratePersistentBirkParameters ::
    forall oldpv pv t m.
    ( IsProtocolVersion pv,
      IsProtocolVersion oldpv,
      SupportMigration m t,
      SupportsPersistentAccount pv (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    Accounts.Accounts pv ->
    PersistentBirkParameters (AccountVersionFor oldpv) ->
    t m (PersistentBirkParameters (AccountVersionFor pv))
migratePersistentBirkParameters migration accounts PersistentBirkParameters{..} = do
    newActiveBakers <- migrateReference (migratePersistentActiveBakers migration accounts) _birkActiveBakers
    newNextEpochBakers <- migrateHashedBufferedRef (migratePersistentEpochBakers migration) _birkNextEpochBakers
    newCurrentEpochBakers <- migrateHashedBufferedRef (migratePersistentEpochBakers migration) _birkCurrentEpochBakers
    return
        PersistentBirkParameters
            { _birkActiveBakers = newActiveBakers,
              _birkNextEpochBakers = newNextEpochBakers,
              _birkCurrentEpochBakers = newCurrentEpochBakers,
              _birkSeedState = _birkSeedState
            }

-- |Accumulated state when iterating accounts, meant for constructing PersistentBirkParameters.
-- Used internally by initialBirkParameters.
data IBPFromAccountsAccum av = IBPFromAccountsAccum
    { -- | Collection of the IDs of the active bakers.
      aibpBakerIds :: !(BakerIdTrieMap av),
      -- | Collection of the aggregation keys of the active bakers.
      aibpBakerKeys :: !AggregationKeySet,
      -- | Total amount owned by accounts.
      aibpTotal :: !Amount,
      -- | Total staked amount by bakers.
      aibpStakedTotal :: !Amount,
      -- | List of baker info refs in incremental order of the baker ID.
      aibpBakerInfoRefs :: !(Vec.Vector (PersistentBakerInfoRef av)),
      -- | List of baker stake in incremental order of the baker ID.
      -- Entries in this list should have a matching entry in agsBakerCapitals.
      -- In the end result these are needed separately and are therefore constructed separately.
      aibpBakerStakes :: !(Vec.Vector Amount)
    }

-- |Initial state for iterating accounts.
initialIBPFromAccountsAccum :: IBPFromAccountsAccum pv
initialIBPFromAccountsAccum =
    IBPFromAccountsAccum
        { aibpBakerIds = Trie.empty,
          aibpBakerKeys = Trie.empty,
          aibpTotal = 0,
          aibpStakedTotal = 0,
          aibpBakerInfoRefs = Vec.empty,
          aibpBakerStakes = Vec.empty
        }

-- |Collections of delegators, grouped by the pool they are delegating to.
-- Used internally by initialBirkParameters.
data IBPCollectedDelegators av = IBPCollectedDelegators
    { -- |Delegators delegating to the passive pool.
      ibpcdToPassive :: !(PersistentActiveDelegators av),
      -- |Delegators delegating to bakers
      ibpcdToBaker :: !(Map.Map BakerId (PersistentActiveDelegators av))
    }

-- |Empty collections of delegators.
emptyIBPCollectedDelegators :: (IsAccountVersion av) => IBPCollectedDelegators av
emptyIBPCollectedDelegators =
    IBPCollectedDelegators
        { ibpcdToPassive = emptyPersistentActiveDelegators,
          ibpcdToBaker = Map.empty
        }

-- |Generate initial birk parameters from accounts and seed state.
initialBirkParameters ::
    forall av m.
    (MonadBlobStore m, IsAccountVersion av) =>
    -- |The accounts in ascending order of the account index.
    [PersistentAccount av] ->
    -- |The seed state
    SeedState ->
    m (PersistentBirkParameters av)
initialBirkParameters accounts seedState = do
    -- Iterate accounts and collect delegators.
    IBPCollectedDelegators{..} <- case delegationSupport @av of
        SAVDelegationNotSupported -> return emptyIBPCollectedDelegators
        SAVDelegationSupported -> foldM collectDelegator emptyIBPCollectedDelegators accounts

    -- Iterate the accounts again accumulate all relevant information.
    IBPFromAccountsAccum{..} <- foldM (accumFromAccounts ibpcdToBaker) initialIBPFromAccountsAccum accounts

    persistentActiveBakers <-
        refMake $!
            PersistentActiveBakers
                { _activeBakers = aibpBakerIds,
                  _aggregationKeys = aibpBakerKeys,
                  _passiveDelegators = ibpcdToPassive,
                  _totalActiveCapital = case delegationSupport @av of
                    SAVDelegationNotSupported -> TotalActiveCapitalV0
                    SAVDelegationSupported -> TotalActiveCapitalV1 aibpTotal
                }

    nextEpochBakers <- do
        _bakerInfos <- refMake $ BakerInfos aibpBakerInfoRefs
        _bakerStakes <- refMake $ BakerStakes aibpBakerStakes
        refMake PersistentEpochBakers{_bakerTotalStake = aibpStakedTotal, ..}

    return $!
        PersistentBirkParameters
            { _birkSeedState = seedState,
              _birkCurrentEpochBakers = nextEpochBakers,
              _birkNextEpochBakers = nextEpochBakers,
              _birkActiveBakers = persistentActiveBakers
            }
  where
    -- If the account is delegating, add it to the collection.
    collectDelegator ::
        (AVSupportsDelegation av) =>
        IBPCollectedDelegators av ->
        PersistentAccount av ->
        m (IBPCollectedDelegators av)
    collectDelegator accum account = do
        maybeDelegation <- accountDelegator account
        case maybeDelegation of
            Nothing -> return accum
            Just accountDelegationV1 ->
                let delegatorId = accountDelegationV1 ^. BaseAccounts.delegationIdentity
                    staked = accountDelegationV1 ^. BaseAccounts.delegationStakedAmount
                    insertDelegator = addDelegatorHelper delegatorId staked
                in  case accountDelegationV1 ^. BaseAccounts.delegationTarget of
                        DelegatePassive -> do
                            nextToPassive <- insertDelegator (ibpcdToPassive accum)
                            return accum{ibpcdToPassive = nextToPassive}
                        DelegateToBaker targetBaker -> do
                            let activeDelegators =
                                    fromMaybe emptyPersistentActiveDelegators $
                                        Map.lookup targetBaker $
                                            ibpcdToBaker accum
                            nextActiveDelegators <- insertDelegator activeDelegators
                            let nextToBaker =
                                    Map.insert targetBaker nextActiveDelegators $
                                        ibpcdToBaker accum
                            return accum{ibpcdToBaker = nextToBaker}

    -- Add account information to the state accumulator.
    accumFromAccounts ::
        Map.Map BakerId (PersistentActiveDelegators av) ->
        IBPFromAccountsAccum av ->
        PersistentAccount av ->
        m (IBPFromAccountsAccum av)
    accumFromAccounts delegatorMap accum account = do
        publicBalance <- accountAmount account
        let !updatedAccum = accum{aibpTotal = aibpTotal accum + publicBalance}

        maybeInfoRef <- accountBakerInfoRef account
        case maybeInfoRef of
            Just infoRef -> do
                bakerInfo <- loadBakerInfo infoRef

                let bakerId = bakerInfo ^. BaseAccounts.bakerIdentity
                let aggregationKey = bakerInfo ^. BaseAccounts.bakerAggregationVerifyKey
                let activeDelegators = fromMaybe emptyPersistentActiveDelegators $ Map.lookup bakerId delegatorMap

                nextBakerIds <- Trie.insert bakerId activeDelegators $ aibpBakerIds accum
                nextBakerKeys <- Trie.insert aggregationKey () $ aibpBakerKeys accum
                stake <- accountStakedAmount account

                return
                    updatedAccum
                        { aibpBakerIds = nextBakerIds,
                          aibpBakerKeys = nextBakerKeys,
                          aibpBakerInfoRefs = Vec.snoc (aibpBakerInfoRefs accum) infoRef,
                          aibpBakerStakes = Vec.snoc (aibpBakerStakes accum) stake,
                          aibpStakedTotal = aibpStakedTotal accum + stake
                        }
            Nothing -> return updatedAccum

freezeContractState :: forall v m. (Wasm.IsWasmVersion v, MonadBlobStore m) => UpdatableContractState v -> m (SHA256.Hash, Instances.InstanceStateV v)
freezeContractState cs = case Wasm.getWasmVersion @v of
    Wasm.SV0 -> return (getHash cs, Instances.InstanceStateV0 cs)
    Wasm.SV1 -> do
        (cbk, _) <- getCallbacks
        (hsh, persistent) <- liftIO (StateV1.freeze cbk cs)
        return (hsh, Instances.InstanceStateV1 persistent)

-- |Serialize 'PersistentBirkParameters' in V0 format.
putBirkParametersV0 :: forall m av. (IsAccountVersion av, MonadBlobStore m, MonadPut m) => PersistentBirkParameters av -> m ()
putBirkParametersV0 PersistentBirkParameters{..} = do
    sPut _birkSeedState
    putEpochBakers =<< refLoad _birkNextEpochBakers
    putEpochBakers =<< refLoad _birkCurrentEpochBakers

instance (IsAccountVersion av, MonadBlobStore m) => MHashableTo m H.Hash (PersistentBirkParameters av) where
    getHashM PersistentBirkParameters{..} = do
        nextHash <- getHashM _birkNextEpochBakers
        currentHash <- getHashM _birkCurrentEpochBakers
        let bpH0 = H.hash $ "SeedState" <> encode _birkSeedState
            bpH1 = H.hashOfHashes nextHash currentHash
        return $ H.hashOfHashes bpH0 bpH1

instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PersistentBirkParameters av) where
    storeUpdate bps@PersistentBirkParameters{..} = do
        (pabs, actBakers) <- storeUpdate _birkActiveBakers
        (pnebs, nextBakers) <- storeUpdate _birkNextEpochBakers
        (pcebs, currentBakers) <- storeUpdate _birkCurrentEpochBakers
        let putBSP = do
                pabs
                pnebs
                pcebs
                put _birkSeedState
        return $!!
            ( putBSP,
              bps
                { _birkActiveBakers = actBakers,
                  _birkNextEpochBakers = nextBakers,
                  _birkCurrentEpochBakers = currentBakers
                }
            )
    load = do
        mabs <- label "Active bakers" load
        mnebs <- label "Next epoch bakers" load
        mcebs <- label "Current epoch bakers" load
        _birkSeedState <- label "Seed state" get
        return $! do
            _birkActiveBakers <- mabs
            _birkNextEpochBakers <- mnebs
            _birkCurrentEpochBakers <- mcebs
            return PersistentBirkParameters{..}

instance (MonadBlobStore m, IsAccountVersion av) => Cacheable m (PersistentBirkParameters av) where
    cache PersistentBirkParameters{..} = do
        activeBaks <- cache _birkActiveBakers
        next <- cache _birkNextEpochBakers
        cur <- cache _birkCurrentEpochBakers
        return
            PersistentBirkParameters
                { _birkActiveBakers = activeBaks,
                  _birkNextEpochBakers = next,
                  _birkCurrentEpochBakers = cur,
                  ..
                }

-- * Epoch baked blocks

type EpochBlocks = Nullable (BufferedRef EpochBlock)

-- |Structure for tracking which bakers have baked blocks
-- in the current epoch.
data EpochBlock = EpochBlock
    { ebBakerId :: !BakerId,
      ebPrevious :: !EpochBlocks
    }

-- |Migrate the 'EpochBlocks' structure, reading it from context @m@ and writing
-- it to context @t m@.
migrateEpochBlocks :: (MonadTrans t, BlobStorable m EpochBlock, BlobStorable (t m) EpochBlock) => EpochBlocks -> t m EpochBlocks
migrateEpochBlocks Null = return Null
migrateEpochBlocks (Some inner) = Some <$> migrateReference go inner
  where
    go EpochBlock{..} = do
        newPrevious <- migrateEpochBlocks ebPrevious
        return EpochBlock{ebPrevious = newPrevious, ..}

-- |Return a map, mapping baker ids to the number of blocks they baked as they
-- appear in the 'EpochBlocks' structure.
bakersFromEpochBlocks :: (MonadBlobStore m) => EpochBlocks -> m (Map.Map BakerId Word64)
bakersFromEpochBlocks = go Map.empty
  where
    go m Null = return m
    go m (Some ref) = do
        EpochBlock{..} <- refLoad ref
        let !m' = m & at ebBakerId . non 0 +~ 1
        go m' ebPrevious

instance (MonadBlobStore m) => BlobStorable m EpochBlock where
    storeUpdate eb@EpochBlock{..} = do
        (ppref, ebPrevious') <- storeUpdate ebPrevious
        let putEB = put ebBakerId >> ppref
        let eb' = eb{ebPrevious = ebPrevious'}
        return $!! (putEB, eb')
    load = do
        ebBakerId <- get
        mPrevious <- load
        return $! do
            ebPrevious <- mPrevious
            return EpochBlock{..}

instance MonadBlobStore m => Cacheable m EpochBlock where
    cache eb = do
        ebPrevious' <- cache (ebPrevious eb)
        return eb{ebPrevious = ebPrevious'}

instance MonadBlobStore m => MHashableTo m Rewards.EpochBlocksHash EpochBlock where
    getHashM EpochBlock{..} = Rewards.epochBlockHash ebBakerId <$> getHashM ebPrevious

instance MonadBlobStore m => MHashableTo m Rewards.EpochBlocksHash EpochBlocks where
    getHashM Null = return Rewards.emptyEpochBlocksHash
    getHashM (Some r) = getHashM r

data HashedEpochBlocks = HashedEpochBlocks
    { hebBlocks :: !EpochBlocks,
      hebHash :: !Rewards.EpochBlocksHash
    }

-- |Like 'migrateEpochBlocks', but for hashed blocks. This makes use of the fact
-- that the hash does not change upon migration and so it is carried over.
--
-- See also documentation of @migratePersistentBlockState@.
migrateHashedEpochBlocks :: (MonadTrans t, BlobStorable m EpochBlock, BlobStorable (t m) EpochBlock) => HashedEpochBlocks -> t m HashedEpochBlocks
migrateHashedEpochBlocks HashedEpochBlocks{..} = do
    newHebBlocks <- migrateEpochBlocks hebBlocks
    return
        HashedEpochBlocks
            { hebBlocks = newHebBlocks,
              ..
            }

instance HashableTo Rewards.EpochBlocksHash HashedEpochBlocks where
    getHash = hebHash

instance MonadBlobStore m => BlobStorable m HashedEpochBlocks where
    storeUpdate heb = do
        (pblocks, blocks') <- storeUpdate (hebBlocks heb)
        return $!! (pblocks, heb{hebBlocks = blocks'})
    load = do
        mhebBlocks <- load
        return $! do
            hebBlocks <- mhebBlocks
            hebHash <- getHashM hebBlocks
            return HashedEpochBlocks{..}

instance MonadBlobStore m => Cacheable m HashedEpochBlocks where
    cache red = do
        blocks' <- cache (hebBlocks red)
        return $! red{hebBlocks = blocks'}

-- |The empty 'HashedEpochBlocks'.
emptyHashedEpochBlocks :: HashedEpochBlocks
emptyHashedEpochBlocks =
    HashedEpochBlocks
        { hebBlocks = Null,
          hebHash = Rewards.emptyEpochBlocksHash
        }

-- |Add a new 'BakerId' to the start of a 'HashedEpochBlocks'.
consEpochBlock :: (MonadBlobStore m) => BakerId -> HashedEpochBlocks -> m HashedEpochBlocks
consEpochBlock b hebbs = do
    mbr <-
        refMake
            EpochBlock
                { ebBakerId = b,
                  ebPrevious = hebBlocks hebbs
                }
    return
        HashedEpochBlocks
            { hebBlocks = Some mbr,
              hebHash = Rewards.epochBlockHash b (hebHash hebbs)
            }

-- |Serialize the 'HashedEpochBlocks' structure in V0 format.
putHashedEpochBlocksV0 :: (MonadBlobStore m, MonadPut m) => HashedEpochBlocks -> m ()
putHashedEpochBlocksV0 HashedEpochBlocks{..} = do
    ebs <- loadEB Seq.empty hebBlocks
    liftPut $ do
        putLength (Seq.length ebs)
        mapM_ put ebs
  where
    loadEB s Null = return s
    loadEB s (Some ebref) = do
        EpochBlock{..} <- refLoad ebref
        loadEB (s Seq.|> ebBakerId) ebPrevious

data BlockRewardDetails (av :: AccountVersion) where
    BlockRewardDetailsV0 :: !HashedEpochBlocks -> BlockRewardDetails 'AccountV0
    BlockRewardDetailsV1 :: (AVSupportsDelegation av) => !(HashedBufferedRef' Rewards.PoolRewardsHash PoolRewards) -> BlockRewardDetails av

-- |Migrate the block reward details.
-- When migrating to a 'P4' or later, this sets the 'nextPaydayEpoch' to the reward period length.
migrateBlockRewardDetails ::
    forall t m oldpv pv.
    ( MonadBlobStore (t m),
      MonadTrans t,
      SupportsPersistentAccount oldpv m
    ) =>
    StateMigrationParameters oldpv pv ->
    -- |Current epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- |Next epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    OParam 'PTTimeParameters (ChainParametersVersionFor pv) TimeParameters ->
    BlockRewardDetails (AccountVersionFor oldpv) ->
    t m (BlockRewardDetails (AccountVersionFor pv))
migrateBlockRewardDetails StateMigrationParametersTrivial _ _ tp = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
    (BlockRewardDetailsV1 hbr) -> case tp of
        SomeParam TimeParametersV1{..} ->
            BlockRewardDetailsV1
                <$> migrateHashedBufferedRef (migratePoolRewards (rewardPeriodEpochs _tpRewardPeriodLength)) hbr
        NoParam -> case protocolVersion @pv of {}
migrateBlockRewardDetails StateMigrationParametersP1P2 _ _ _ = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
migrateBlockRewardDetails StateMigrationParametersP2P3 _ _ _ = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
migrateBlockRewardDetails (StateMigrationParametersP3ToP4 _) curBakers nextBakers (SomeParam TimeParametersV1{..}) = \case
    (BlockRewardDetailsV0 heb) -> do
        blockCounts <- bakersFromEpochBlocks (hebBlocks heb)
        (!newRef, _) <- refFlush =<< refMake =<< migratePoolRewardsP1 curBakers nextBakers blockCounts (rewardPeriodEpochs _tpRewardPeriodLength) _tpMintPerPayday
        return (BlockRewardDetailsV1 newRef)
migrateBlockRewardDetails StateMigrationParametersP4ToP5{} _ _ (SomeParam TimeParametersV1{..}) = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewards (rewardPeriodEpochs _tpRewardPeriodLength)) hbr

instance MonadBlobStore m => MHashableTo m (Rewards.BlockRewardDetailsHash av) (BlockRewardDetails av) where
    getHashM (BlockRewardDetailsV0 heb) = return $ Rewards.BlockRewardDetailsHashV0 (getHash heb)
    getHashM (BlockRewardDetailsV1 pr) = Rewards.BlockRewardDetailsHashV1 <$> getHashM pr

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (BlockRewardDetails av) where
    storeUpdate (BlockRewardDetailsV0 heb) = fmap (fmap BlockRewardDetailsV0) $ storeUpdate heb
    storeUpdate (BlockRewardDetailsV1 hpr) = fmap (fmap BlockRewardDetailsV1) $ storeUpdate hpr
    load = case delegationSupport @av of
        SAVDelegationNotSupported -> fmap (fmap BlockRewardDetailsV0) load
        SAVDelegationSupported -> fmap (fmap BlockRewardDetailsV1) load

instance MonadBlobStore m => Cacheable m (BlockRewardDetails av) where
    cache (BlockRewardDetailsV0 heb) = BlockRewardDetailsV0 <$> cache heb
    cache (BlockRewardDetailsV1 hpr) = BlockRewardDetailsV1 <$> cache hpr

putBlockRewardDetails :: (MonadBlobStore m, MonadPut m) => BlockRewardDetails av -> m ()
putBlockRewardDetails (BlockRewardDetailsV0 heb) = putHashedEpochBlocksV0 heb
putBlockRewardDetails (BlockRewardDetailsV1 hpr) = refLoad hpr >>= putPoolRewards

-- |Extend a 'BlockRewardDetails' ''AccountV0' with an additional baker.
consBlockRewardDetails ::
    MonadBlobStore m =>
    BakerId ->
    BlockRewardDetails 'AccountV0 ->
    m (BlockRewardDetails 'AccountV0)
consBlockRewardDetails bid (BlockRewardDetailsV0 heb) = do
    BlockRewardDetailsV0 <$> consEpochBlock bid heb

-- |The empty 'BlockRewardDetails'.
emptyBlockRewardDetails ::
    forall av m.
    (MonadBlobStore m, IsAccountVersion av) =>
    m (BlockRewardDetails av)
emptyBlockRewardDetails =
    case delegationSupport @av of
        SAVDelegationNotSupported -> return $ BlockRewardDetailsV0 emptyHashedEpochBlocks
        SAVDelegationSupported -> BlockRewardDetailsV1 <$> (emptyPoolRewards >>= refMake)

-- * Block state

-- |Type representing a persistent block state. This is a 'BufferedRef' inside an 'IORef',
-- which supports making changes to the state without them (necessarily) being written to
-- disk.
type PersistentBlockState (pv :: ProtocolVersion) = IORef (BufferedRef (BlockStatePointers pv))

-- |Transaction outcomes stored in a merkle binary tree.
data MerkleTransactionOutcomes = MerkleTransactionOutcomes
    { -- |Normal transaction outcomes
      mtoOutcomes :: LFMBT.LFMBTree TransactionIndex HashedBufferedRef TransactionSummaryV1,
      -- |Special transaction outcomes
      mtoSpecials :: LFMBT.LFMBTree TransactionIndex HashedBufferedRef Transactions.SpecialTransactionOutcome
    }
    deriving (Show)

-- |Create an empty 'MerkleTransactionOutcomes'
emptyMerkleTransactionOutcomes :: MerkleTransactionOutcomes
emptyMerkleTransactionOutcomes =
    MerkleTransactionOutcomes
        { mtoOutcomes = LFMBT.empty,
          mtoSpecials = LFMBT.empty
        }

-- |Transaction outcomes stored in the 'Persistent' block state.
-- From PV1 to PV4 transaction outcomes are stored within a list 'Transactions.TransactionOutcomes'.
-- From PV5 and onwards the transaction outcomes are stored in a binary merkle tree.
-- Note. There are no difference in the actual stored 'TransactionSummary' however there
-- is a difference in the hashing scheme.
-- In PV1 to PV4 the transaction outcomes are hashed as a list based on all of the data
-- in the 'TransactionSummary'.
-- In PV5 and onwards the exact 'RejectReason's are omitted from the computed hash and moreover
-- the hashing scheme is not a hash list but a merkle tree, so it is the root hash that is
-- used in the final 'BlockHash'.
data PersistentTransactionOutcomes (tov :: TransactionOutcomesVersion) where
    PTOV0 :: Transactions.TransactionOutcomes -> PersistentTransactionOutcomes 'TOV0
    PTOV1 :: MerkleTransactionOutcomes -> PersistentTransactionOutcomes 'TOV1

-- |Create an empty persistent transaction outcome
emptyPersistentTransactionOutcomes :: forall tov. IsTransactionOutcomesVersion tov => PersistentTransactionOutcomes tov
emptyPersistentTransactionOutcomes = case transactionOutcomesVersion @tov of
    STOV0 -> PTOV0 Transactions.emptyTransactionOutcomesV0
    STOV1 -> PTOV1 emptyMerkleTransactionOutcomes

instance BlobStorable m TransactionSummaryV1 => MHashableTo m Transactions.TransactionOutcomesHash (PersistentTransactionOutcomes tov) where
    getHashM (PTOV0 bto) = return (getHash bto)
    getHashM (PTOV1 MerkleTransactionOutcomes{..}) = do
        out <- getHashM mtoOutcomes
        special <- getHashM mtoSpecials
        return $! Transactions.TransactionOutcomesHash (H.hashShort ("TransactionOutcomesHashV1" <> H.hashToShortByteString out <> H.hashToShortByteString special))

instance
    ( TransactionOutcomesVersionFor (MPV m) ~ tov,
      MonadBlobStore m,
      MonadProtocolVersion m
    ) =>
    BlobStorable m (PersistentTransactionOutcomes tov)
    where
    storeUpdate out@(PTOV0 bto) = return (Transactions.putTransactionOutcomes bto, out)
    storeUpdate (PTOV1 MerkleTransactionOutcomes{..}) = do
        (pout, mtoOutcomes') <- storeUpdate mtoOutcomes
        (pspecial, mtoSpecials') <- storeUpdate mtoSpecials
        return (pout <> pspecial, PTOV1 MerkleTransactionOutcomes{mtoOutcomes = mtoOutcomes', mtoSpecials = mtoSpecials'})

    load = do
        case transactionOutcomesVersion @(TransactionOutcomesVersionFor (MPV m)) of
            STOV0 -> do
                out <- PTOV0 <$!> Transactions.getTransactionOutcomes (protocolVersion @(MPV m))
                pure . pure $! out
            STOV1 -> do
                mout <- load
                mspecials <- load
                return $! do
                    mtoOutcomes <- mout
                    mtoSpecials <- mspecials
                    return $! PTOV1 MerkleTransactionOutcomes{..}

-- |Create an empty 'PersistentTransactionOutcomes' based on the 'ProtocolVersion'.
emptyTransactionOutcomes ::
    forall pv.
    (SupportsTransactionOutcomes pv) =>
    Proxy pv ->
    PersistentTransactionOutcomes (TransactionOutcomesVersionFor pv)
emptyTransactionOutcomes Proxy = case transactionOutcomesVersion @(TransactionOutcomesVersionFor pv) of
    STOV0 -> PTOV0 Transactions.emptyTransactionOutcomesV0
    STOV1 -> PTOV1 emptyMerkleTransactionOutcomes

-- |References to the components that make up the block state.
--
-- This type is parametric in the protocol version (as opposed to defined
-- as a data family) on the principle that the structure will be mostly
-- similar across versions. Where component change between versions,
-- those components themselves should be parametrised by the protocol
-- version.
data BlockStatePointers (pv :: ProtocolVersion) = BlockStatePointers
    { bspAccounts :: !(Accounts.Accounts pv),
      bspInstances :: !(Instances.Instances pv),
      bspModules :: !(HashedBufferedRef Modules.Modules),
      bspBank :: !(Hashed Rewards.BankStatus),
      bspIdentityProviders :: !(HashedBufferedRef IPS.IdentityProviders),
      bspAnonymityRevokers :: !(HashedBufferedRef ARS.AnonymityRevokers),
      bspBirkParameters :: !(PersistentBirkParameters (AccountVersionFor pv)),
      bspCryptographicParameters :: !(HashedBufferedRef CryptographicParameters),
      bspUpdates :: !(BufferedRef (Updates pv)),
      bspReleaseSchedule :: !(ReleaseSchedule pv),
      bspTransactionOutcomes :: !(PersistentTransactionOutcomes (TransactionOutcomesVersionFor pv)),
      -- |Details of bakers that baked blocks in the current epoch. This is
      -- used for rewarding bakers at the end of epochs.
      bspRewardDetails :: !(BlockRewardDetails (AccountVersionFor pv))
    }

-- |Lens for accessing the birk parameters of a 'BlockStatePointers' structure.
birkParameters :: Lens' (BlockStatePointers pv) (PersistentBirkParameters (AccountVersionFor pv))
birkParameters = lens bspBirkParameters (\bsp bp -> bsp{bspBirkParameters = bp})

-- |A hashed version of 'PersistingBlockState'.  This is used when the block state
-- is not being mutated so that the hash values are not recomputed constantly.
data HashedPersistentBlockState pv = HashedPersistentBlockState
    { hpbsPointers :: !(PersistentBlockState pv),
      hpbsHash :: !StateHash
    }

-- |Constraint for ensuring that @m@ supports both persistent accounts and persistent modules.
type SupportsPersistentState pv m = (MonadProtocolVersion m, MPV m ~ pv, SupportsPersistentAccount pv m, Modules.SupportsPersistentModule m)

-- |Convert a 'PersistentBlockState' to a 'HashedPersistentBlockState' by computing
-- the state hash.
hashBlockState :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (HashedPersistentBlockState pv)
hashBlockState hpbsPointers = do
    rbsp <- liftIO $ readIORef hpbsPointers
    bsp <- refLoad rbsp
    hpbsHash <- getHashM bsp
    return HashedPersistentBlockState{..}

instance (SupportsPersistentState pv m) => MHashableTo m StateHash (BlockStatePointers pv) where
    getHashM BlockStatePointers{..} = do
        bshBirkParameters <- getHashM bspBirkParameters
        bshCryptographicParameters <- getHashM bspCryptographicParameters
        bshIdentityProviders <- getHashM bspIdentityProviders
        bshAnonymityRevokers <- getHashM bspAnonymityRevokers
        bshModules <- getHashM bspModules
        let bshBankStatus = getHash bspBank
        bshAccounts <- getHashM bspAccounts
        bshInstances <- getHashM bspInstances
        bshUpdates <- getHashM bspUpdates
        bshBlockRewardDetails <- getHashM bspRewardDetails
        return $ makeBlockStateHash @pv BlockStateHashInputs{..}

instance (SupportsPersistentState pv m) => BlobStorable m (BlockStatePointers pv) where
    storeUpdate bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate bspAccounts
        (pinsts, bspInstances') <- storeUpdate bspInstances
        (pmods, bspModules') <- storeUpdate bspModules
        (pips, bspIdentityProviders') <- storeUpdate bspIdentityProviders
        (pars, bspAnonymityRevokers') <- storeUpdate bspAnonymityRevokers
        (pbps, bspBirkParameters') <- storeUpdate bspBirkParameters
        (pcryptps, bspCryptographicParameters') <- storeUpdate bspCryptographicParameters
        (poutcomes, bspTransactionOutcomes') <- storeUpdate bspTransactionOutcomes
        (pupdates, bspUpdates') <- storeUpdate bspUpdates
        (preleases, bspReleaseSchedule') <- storeUpdate bspReleaseSchedule
        (pRewardDetails, bspRewardDetails') <- storeUpdate bspRewardDetails
        let putBSP = do
                paccts
                pinsts
                pmods
                put $ _unhashed bspBank
                pips
                pars
                pbps
                pcryptps
                poutcomes
                pupdates
                preleases
                pRewardDetails
        return
            ( putBSP,
              bsp0
                { bspAccounts = bspAccounts',
                  bspInstances = bspInstances',
                  bspModules = bspModules',
                  bspIdentityProviders = bspIdentityProviders',
                  bspAnonymityRevokers = bspAnonymityRevokers',
                  bspBirkParameters = bspBirkParameters',
                  bspCryptographicParameters = bspCryptographicParameters',
                  bspTransactionOutcomes = bspTransactionOutcomes',
                  bspUpdates = bspUpdates',
                  bspReleaseSchedule = bspReleaseSchedule',
                  bspRewardDetails = bspRewardDetails'
                }
            )
    load = do
        maccts <- label "Accounts" load
        minsts <- label "Instances" load
        mmods <- label "Modules" load
        bspBank <- makeHashed <$> label "Bank" get
        mpips <- label "Identity providers" load
        mars <- label "Anonymity revokers" load
        mbps <- label "Birk parameters" load
        mcryptps <- label "Cryptographic parameters" load
        moutcomes <- label "Transaction outcomes" load
        mUpdates <- label "Updates" load
        mReleases <- label "Release schedule" load
        mRewardDetails <- label "Epoch blocks" load
        return $! do
            bspAccounts <- maccts
            bspInstances <- minsts
            bspModules <- mmods
            bspIdentityProviders <- mpips
            bspAnonymityRevokers <- mars
            bspBirkParameters <- mbps
            bspCryptographicParameters <- mcryptps
            bspTransactionOutcomes <- moutcomes
            bspUpdates <- mUpdates
            bspReleaseSchedule <- mReleases
            bspRewardDetails <- mRewardDetails
            return $! BlockStatePointers{..}

-- |Accessor for getting the pool rewards when supported by the protocol version.
bspPoolRewards :: PVSupportsDelegation pv => BlockStatePointers pv -> HashedBufferedRef' Rewards.PoolRewardsHash PoolRewards
bspPoolRewards bsp = case bspRewardDetails bsp of
    BlockRewardDetailsV1 pr -> pr

-- |An initial 'HashedPersistentBlockState', which may be used for testing purposes.
initialPersistentState ::
    (SupportsPersistentState pv m) =>
    SeedState ->
    CryptographicParameters ->
    [PersistentAccount (AccountVersionFor pv)] ->
    IPS.IdentityProviders ->
    ARS.AnonymityRevokers ->
    UpdateKeysCollection (AuthorizationsVersionForPV pv) ->
    ChainParameters pv ->
    m (HashedPersistentBlockState pv)
initialPersistentState seedState cryptoParams accounts ips ars keysCollection chainParams = do
    persistentBirkParameters <- initialBirkParameters accounts seedState
    modules <- refMake Modules.emptyModules
    identityProviders <- bufferHashed $ makeHashed ips
    anonymityRevokers <- bufferHashed $ makeHashed ars
    cryptographicParameters <- bufferHashed $ makeHashed cryptoParams
    blockAccounts <- Accounts.fromList accounts
    initialAmount <- foldM (\sumSoFar account -> (+ sumSoFar) <$> accountAmount account) 0 accounts
    updates <- refMake =<< initialUpdates keysCollection chainParams
    releaseSchedule <- emptyReleaseSchedule
    red <- emptyBlockRewardDetails

    bsp <-
        makeBufferedRef $
            BlockStatePointers
                { bspAccounts = blockAccounts,
                  bspInstances = Instances.emptyInstances,
                  bspModules = modules,
                  bspBank = makeHashed $ Rewards.makeGenesisBankStatus initialAmount,
                  bspIdentityProviders = identityProviders,
                  bspAnonymityRevokers = anonymityRevokers,
                  bspBirkParameters = persistentBirkParameters,
                  bspCryptographicParameters = cryptographicParameters,
                  bspTransactionOutcomes = emptyPersistentTransactionOutcomes,
                  bspUpdates = updates,
                  bspReleaseSchedule = releaseSchedule,
                  bspRewardDetails = red
                }
    bps <- liftIO $ newIORef $! bsp
    hashBlockState bps

-- |A mostly empty block state, but with the given birk parameters,
-- cryptographic parameters, update authorizations and chain parameters.
emptyBlockState ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBirkParameters (AccountVersionFor pv) ->
    CryptographicParameters ->
    UpdateKeysCollection (AuthorizationsVersionForPV pv) ->
    ChainParameters pv ->
    m (PersistentBlockState pv)
{-# WARNING emptyBlockState "should only be used for testing" #-}
emptyBlockState bspBirkParameters cryptParams keysCollection chainParams = do
    modules <- refMake Modules.emptyModules
    identityProviders <- refMake IPS.emptyIdentityProviders
    anonymityRevokers <- refMake ARS.emptyAnonymityRevokers
    cryptographicParameters <- refMake cryptParams
    bspUpdates <- refMake =<< initialUpdates keysCollection chainParams
    bspReleaseSchedule <- emptyReleaseSchedule
    bspRewardDetails <- emptyBlockRewardDetails
    bsp <-
        makeBufferedRef $
            BlockStatePointers
                { bspAccounts = Accounts.emptyAccounts,
                  bspInstances = Instances.emptyInstances,
                  bspModules = modules,
                  bspBank = makeHashed Rewards.emptyBankStatus,
                  bspIdentityProviders = identityProviders,
                  bspAnonymityRevokers = anonymityRevokers,
                  bspCryptographicParameters = cryptographicParameters,
                  bspTransactionOutcomes = emptyTransactionOutcomes (Proxy @pv),
                  ..
                }
    liftIO $ newIORef $! bsp

-- |Serialize the block state. The format may depend on the protocol version.
putBlockStateV0 :: (SupportsPersistentState pv m, MonadPut m) => PersistentBlockState pv -> m ()
putBlockStateV0 pbs = do
    BlockStatePointers{..} <- loadPBS pbs
    -- BirkParameters
    putBirkParametersV0 bspBirkParameters
    -- CryptographicParameters
    cryptoParams <- refLoad bspCryptographicParameters
    sPut cryptoParams
    -- IdentityProviders
    sPut =<< refLoad bspIdentityProviders
    -- AnonymityRevokers
    sPut =<< refLoad bspAnonymityRevokers
    -- Modules
    Modules.putModulesV0 =<< refLoad bspModules
    -- BankStatus
    sPut $ _unhashed bspBank
    -- Accounts
    Accounts.serializeAccounts cryptoParams bspAccounts
    -- Instances
    Instances.putInstancesV0 bspInstances
    -- Updates
    putUpdatesV0 =<< refLoad bspUpdates
    -- Epoch blocks / pool rewards
    putBlockRewardDetails bspRewardDetails

loadPBS :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (BlockStatePointers pv)
loadPBS = loadBufferedRef <=< liftIO . readIORef
{-# INLINE loadPBS #-}

storePBS :: SupportsPersistentAccount pv m => PersistentBlockState pv -> BlockStatePointers pv -> m (PersistentBlockState pv)
storePBS pbs bsp = liftIO $ do
    pbsp <- makeBufferedRef bsp
    writeIORef pbs pbsp
    return pbs
{-# INLINE storePBS #-}

-- | Get total delegated pool capital, sum of delegator stakes,
-- 'poolDelegatorCapital' @bsp@ @bid@, where
-- * @bsp@ is used to lookup accounts and active bakers,
-- * @bid@ is the baker.
-- If @bid@ is not a baker in @accounts@, then @0@ is returned.
-- If @bid@ is not an active baker in @ab@, then the baker's equity capital (stake) is returned.
-- It is assumed that all delegators to the baker @bid@ are delegator accounts in @accounts@.
poolDelegatorCapital ::
    forall pv m.
    (IsProtocolVersion pv, PVSupportsDelegation pv, SupportsPersistentAccount pv m) =>
    BlockStatePointers pv ->
    BakerId ->
    m Amount
poolDelegatorCapital bsp bid = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    Trie.lookup bid (pab ^. activeBakers) >>= \case
        Nothing -> return 0
        Just PersistentActiveDelegatorsV1{..} -> return adDelegatorTotalCapital

-- | Get the total passively-delegated capital.
passiveDelegationCapital ::
    (IsProtocolVersion pv, PVSupportsDelegation pv, SupportsPersistentAccount pv m) =>
    BlockStatePointers pv ->
    m Amount
passiveDelegationCapital bsp = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    return $! adDelegatorTotalCapital (pab ^. passiveDelegators)

-- | Get the total capital currently staked by bakers and delegators.
-- Note, this is separate from the stake and capital distribution used for the current payday, as
-- it reflects the current value of accounts.
totalCapital :: (IsProtocolVersion pv, PVSupportsDelegation pv, SupportsPersistentAccount pv m) => BlockStatePointers pv -> m Amount
totalCapital bsp = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    return $! pab ^. totalActiveCapitalV1

-- |Look up an account by index and run an operation on it.
-- This returns 'Nothing' if the account is not present or the operation returns 'Nothing'.
onAccount ::
    (SupportsPersistentAccount pv m) =>
    -- |Account index to resolve
    AccountIndex ->
    -- |Block state
    BlockStatePointers pv ->
    -- |Operation to apply to the account
    (PersistentAccount (AccountVersionFor pv) -> m (Maybe a)) ->
    m (Maybe a)
onAccount ai bsp f =
    Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
        Nothing -> return Nothing
        Just acc -> f acc

-- |Look up an account by index and run an operation on it.
-- This returns 'Nothing' if the account is not present.
onAccount' ::
    (SupportsPersistentAccount pv m) =>
    -- |Account index to resolve
    AccountIndex ->
    -- |Block state
    BlockStatePointers pv ->
    -- |Operation to apply to the account
    (PersistentAccount (AccountVersionFor pv) -> m a) ->
    m (Maybe a)
onAccount' ai bsp f = Accounts.indexedAccount ai (bspAccounts bsp) >>= mapM f

doGetModule :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ModuleRef -> m (Maybe (GSWasm.ModuleInterface Modules.PersistentInstrumentedModuleV))
doGetModule s modRef = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    Modules.getInterface modRef mods

doGetModuleArtifact :: (MonadBlobStore m, Wasm.IsWasmVersion v) => Modules.PersistentInstrumentedModuleV v -> m (GSWasm.InstrumentedModuleV v)
doGetModuleArtifact = Modules.loadInstrumentedModuleV

doGetModuleList :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [ModuleRef]
doGetModuleList s = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    return $ Modules.moduleRefList mods

doGetModuleSource :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ModuleRef -> m (Maybe Wasm.WasmModule)
doGetModuleSource s modRef = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    Modules.getSource modRef mods

doPutNewModule ::
    (Wasm.IsWasmVersion v, SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    (GSWasm.ModuleInterfaceV v, Wasm.WasmModuleV v) ->
    m (Bool, PersistentBlockState pv)
doPutNewModule pbs (pmInterface, pmSource) = do
    bsp <- loadPBS pbs
    mods <- refLoad (bspModules bsp)
    mMods' <- Modules.putInterface (pmInterface, pmSource) mods
    case mMods' of
        Nothing -> return (False, pbs)
        Just mods' -> do
            modules <- refMake mods'
            (True,) <$> storePBS pbs (bsp{bspModules = modules})

doGetSeedState :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m SeedState
doGetSeedState pbs = _birkSeedState . bspBirkParameters <$> loadPBS pbs

doSetSeedState :: (SupportsPersistentState pv m) => PersistentBlockState pv -> SeedState -> m (PersistentBlockState pv)
doSetSeedState pbs ss = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBirkParameters = (bspBirkParameters bsp){_birkSeedState = ss}}

doGetCurrentEpochBakers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m FullBakers
doGetCurrentEpochBakers pbs = epochToFullBakers =<< refLoad . _birkCurrentEpochBakers . bspBirkParameters =<< loadPBS pbs

doGetCurrentEpochFullBakersEx :: (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m FullBakersEx
doGetCurrentEpochFullBakersEx pbs = epochToFullBakersEx =<< refLoad . _birkCurrentEpochBakers . bspBirkParameters =<< loadPBS pbs

doGetCurrentCapitalDistribution :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m CapitalDistribution
doGetCurrentCapitalDistribution pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    poolRewards <- refLoad hpr
    refLoad $ currentCapital poolRewards

doGetNextEpochBakers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m FullBakers
doGetNextEpochBakers pbs = do
    bsp <- loadPBS pbs
    epochToFullBakers =<< refLoad (bspBirkParameters bsp ^. birkNextEpochBakers)

doGetSlotBakersP1 :: (AccountVersionFor pv ~ 'AccountV0, SupportsPersistentState pv m) => PersistentBlockState pv -> Slot -> m FullBakers
doGetSlotBakersP1 pbs slot = do
    bs <- loadPBS pbs
    let bps = bspBirkParameters bs
        SeedState{..} = bps ^. birkSeedState
        slotEpoch = fromIntegral $ slot `quot` epochLength
    case compare slotEpoch (epoch + 1) of
        LT -> epochToFullBakers =<< refLoad (bps ^. birkCurrentEpochBakers)
        EQ -> epochToFullBakers =<< refLoad (bps ^. birkNextEpochBakers)
        GT -> do
            activeBids <- Trie.keysAsc . _activeBakers =<< refLoad (bps ^. birkActiveBakers)
            let resolveBaker (BakerId aid) =
                    onAccount' aid bs accountBaker <&> \case
                        Just (Just pab) -> case _bakerPendingChange pab of
                            BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 remEpoch)
                                | remEpoch < slotEpoch -> Nothing
                            BaseAccounts.ReduceStake newAmt (BaseAccounts.PendingChangeEffectiveV0 redEpoch)
                                | redEpoch < slotEpoch -> Just $! FullBakerInfo (pab ^. BaseAccounts.bakerInfo) newAmt
                            _ -> Just $! FullBakerInfo (pab ^. BaseAccounts.bakerInfo) (pab ^. BaseAccounts.stakedAmount)
                        Just Nothing -> error "Persistent.getSlotBakers invariant violation: active baker account not a valid baker"
                        Nothing -> error "Persistent.getSlotBakers invariant violation: active baker account does not exist"
            futureBakers <- Vec.fromList . catMaybes <$> mapM resolveBaker activeBids
            return
                FullBakers
                    { fullBakerInfos = futureBakers,
                      bakerTotalStake = sum (_bakerStake <$> futureBakers)
                    }

doGetBakerAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> BakerId -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
doGetBakerAccount pbs (BakerId ai) = do
    bsp <- loadPBS pbs
    Accounts.indexedAccount ai (bspAccounts bsp)

doTransitionEpochBakers :: forall m pv. (SupportsPersistentState pv m, AccountVersionFor pv ~ 'AccountV0) => PersistentBlockState pv -> Epoch -> m (PersistentBlockState pv)
doTransitionEpochBakers pbs newEpoch = do
    bsp <- loadPBS pbs
    let oldBPs = bspBirkParameters bsp
    curActiveBIDs <- Trie.keysAsc . _activeBakers =<< refLoad (_birkActiveBakers oldBPs)
    -- Retrieve/update the baker info, accumulating the baker info to the list if it is still a
    -- baker after updating to account for any elapsed pending update.
    let accumBakers :: (BlockStatePointers pv, [(PersistentBakerInfoRef 'AccountV0, Amount)]) -> BakerId -> m (BlockStatePointers pv, [(PersistentBakerInfoRef 'AccountV0, Amount)])
        accumBakers (bs0, bkrs0) bkr@(BakerId aid) =
            onAccount aid bsp accountBakerAndInfoRef >>= \case
                Just (acctBkr, binfoRef) ->
                    case _bakerPendingChange acctBkr of
                        BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 remEpoch)
                            -- Removal takes effect next epoch, so exclude it from the list of bakers
                            | remEpoch == newEpoch + 1 -> return (bs0, bkrs0)
                            -- Removal complete, so update the active bakers and account as well
                            | remEpoch <= newEpoch -> do
                                -- Remove the baker from the active bakers
                                curABs <- refLoad (_birkActiveBakers (bspBirkParameters bs0))
                                newAB <- Trie.delete bkr (_activeBakers curABs)
                                let abi = acctBkr ^. BaseAccounts.bakerInfo
                                newAK <- Trie.delete (BaseAccounts._bakerAggregationVerifyKey abi) (_aggregationKeys curABs)
                                newABs <-
                                    refMake $
                                        PersistentActiveBakers
                                            { _activeBakers = newAB,
                                              _aggregationKeys = newAK,
                                              _passiveDelegators = curABs ^. passiveDelegators,
                                              _totalActiveCapital = TotalActiveCapitalV0
                                            }
                                -- Remove the baker from the account by applying the change
                                newAccounts <- Accounts.updateAccountsAtIndex' applyPendingStakeChange aid (bspAccounts bs0)
                                -- The baker is not included for this epoch
                                return
                                    ( bs0
                                        { bspBirkParameters = (bspBirkParameters bs0){_birkActiveBakers = newABs},
                                          bspAccounts = newAccounts
                                        },
                                      bkrs0
                                    )
                        BaseAccounts.ReduceStake newAmt (BaseAccounts.PendingChangeEffectiveV0 redEpoch)
                            -- Reduction takes effect next epoch, so apply it in the generated list
                            | redEpoch == newEpoch + 1 -> do
                                return (bs0, (binfoRef, newAmt) : bkrs0)
                            -- Reduction complete, so update the account as well
                            | redEpoch <= newEpoch -> do
                                -- Reduce the baker's stake on the account by applying the change
                                newAccounts <- Accounts.updateAccountsAtIndex' applyPendingStakeChange aid (bspAccounts bs0)
                                -- The baker is included with the revised stake
                                return (bs0{bspAccounts = newAccounts}, (binfoRef, newAmt) : bkrs0)
                        _ -> return (bs0, (binfoRef, _stakedAmount acctBkr) : bkrs0)
                Nothing -> error "Persistent.bsoTransitionEpochBakers invariant violation: active baker account not a valid baker"
    -- Get the baker info. The list of baker ids is reversed in the input so the accumulated list
    -- is in ascending order.
    (bsp', bkrs) <- foldM accumBakers (bsp, []) (reverse curActiveBIDs)
    newBakerInfos <- refMake . BakerInfos . Vec.fromList $ fst <$> bkrs
    let stakesVec = Vec.fromList $ snd <$> bkrs
    newBakerStakes <- refMake (BakerStakes stakesVec)
    let newCurrentBakers = oldBPs ^. birkNextEpochBakers
    neb <- refLoad newCurrentBakers
    -- If the baker infos structure has the same hash as the previous one,
    -- use that to avoid duplicate storage.
    _bakerInfos <- secondIfEqual newBakerInfos (_bakerInfos neb)
    -- Also for stakes. This is less likely to be useful, but it's pretty cheap to check,
    -- so why not?
    _bakerStakes <- secondIfEqual newBakerStakes (_bakerStakes neb)
    let _bakerTotalStake = sum stakesVec
    newNextBakers <- refMake PersistentEpochBakers{..}
    storePBS
        pbs
        bsp'
            { bspBirkParameters =
                (bspBirkParameters bsp')
                    { _birkCurrentEpochBakers = newCurrentBakers,
                      _birkNextEpochBakers = newNextBakers
                    }
            }
  where
    secondIfEqual a b = do
        h1 <- getHashM a
        h2 <- getHashM b
        return $ if (h1 :: H.Hash) == h2 then b else a

doGetActiveBakersAndDelegators ::
    forall pv m.
    ( IsProtocolVersion pv,
      SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      BakerInfoRef m ~ PersistentBakerInfoRef (AccountVersionFor pv)
    ) =>
    PersistentBlockState pv ->
    m ([ActiveBakerInfo m], [ActiveDelegatorInfo])
doGetActiveBakersAndDelegators pbs = do
    bsp <- loadPBS pbs
    ab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    abis <- Trie.toAscList (ab ^. activeBakers) >>= mapM (mkActiveBakerInfo bsp)
    let PersistentActiveDelegatorsV1 dset _ = ab ^. passiveDelegators
    lps <- Trie.keys dset >>= mapM (mkActiveDelegatorInfo bsp)
    return (abis, lps)
  where
    mkActiveBakerInfo bsp (BakerId acct, PersistentActiveDelegatorsV1 dlgs _) =
        onAccount acct bsp accountBakerAndInfoRef >>= \case
            Nothing -> error "Invariant violation: active baker is not a baker account"
            Just (theBaker, binfoRef) -> do
                dlglist <- Trie.keysAsc dlgs
                abd <- mapM (mkActiveDelegatorInfo bsp) dlglist
                return
                    ActiveBakerInfo
                        { activeBakerInfoRef = binfoRef,
                          activeBakerEquityCapital = theBaker ^. BaseAccounts.stakedAmount,
                          activeBakerPendingChange =
                            BaseAccounts.pendingChangeEffectiveTimestamp <$> theBaker ^. BaseAccounts.bakerPendingChange,
                          activeBakerDelegators = abd
                        }
    mkActiveDelegatorInfo :: BlockStatePointers pv -> DelegatorId -> m ActiveDelegatorInfo
    mkActiveDelegatorInfo bsp activeDelegatorId@(DelegatorId acct) =
        onAccount acct bsp accountDelegator >>= \case
            Nothing -> error "Invariant violation: active delegator is not a delegator account"
            Just theDelegator@BaseAccounts.AccountDelegationV1{} -> do
                return
                    ActiveDelegatorInfo
                        { activeDelegatorStake = theDelegator ^. BaseAccounts.delegationStakedAmount,
                          activeDelegatorPendingChange =
                            BaseAccounts.pendingChangeEffectiveTimestamp
                                <$> theDelegator ^. BaseAccounts.delegationPendingChange,
                          ..
                        }

-- |Get the registered delegators of a pool. Changes are reflected immediately here and will be effective in the next reward period.
-- The baker id is used to identify the pool and Nothing is used for the passive delegators.
-- Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
doGetActiveDelegators ::
    forall pv m.
    ( IsProtocolVersion pv,
      SupportsPersistentState pv m,
      PVSupportsDelegation pv
    ) =>
    PersistentBlockState pv ->
    Maybe BakerId ->
    m (Maybe [(AccountAddress, ActiveDelegatorInfo)])
doGetActiveDelegators pbs mPoolId = do
    bsp <- loadPBS pbs
    ab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    case mPoolId of
        Nothing -> do
            let PersistentActiveDelegatorsV1 dset _ = ab ^. passiveDelegators
            dids <- Trie.keys dset
            lps <- mapM (mkActiveDelegatorInfo bsp) dids
            return (Just lps)
        Just bid -> do
            Trie.lookup bid (ab ^. activeBakers) >>= \case
                Nothing -> return Nothing
                Just (PersistentActiveDelegatorsV1 dlgs _) -> do
                    lps <- Trie.keys dlgs >>= mapM (mkActiveDelegatorInfo bsp)
                    return (Just lps)
  where
    mkActiveDelegatorInfo :: BlockStatePointers pv -> DelegatorId -> m (AccountAddress, ActiveDelegatorInfo)
    mkActiveDelegatorInfo bsp activeDelegatorId@(DelegatorId acct) = do
        let myFromJust = fromMaybe (error "Invariant violation: active baker is not a baker account")
        theAcct <- myFromJust <$> Accounts.indexedAccount acct (bspAccounts bsp)
        addr <- accountCanonicalAddress theAcct
        theDelegator@BaseAccounts.AccountDelegationV1{} <- myFromJust <$> accountDelegator theAcct
        return
            ( addr,
              ActiveDelegatorInfo
                { activeDelegatorStake = theDelegator ^. BaseAccounts.delegationStakedAmount,
                  activeDelegatorPendingChange =
                    BaseAccounts.pendingChangeEffectiveTimestamp
                        <$> theDelegator ^. BaseAccounts.delegationPendingChange,
                  ..
                }
            )

-- |Get the delegators of a pool for the reward period. Changes are not reflected here until the next reward period.
-- The baker id is used to identify the pool and Nothing is used for the passive delegators.
-- Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
doGetCurrentDelegators ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv
    ) =>
    PersistentBlockState pv ->
    Maybe BakerId ->
    m (Maybe [(AccountAddress, DelegatorCapital)])
doGetCurrentDelegators pbs mPoolId = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    poolRewards <- refLoad hpr
    CapitalDistribution{..} <- refLoad $ currentCapital poolRewards
    let mkReturn dc@DelegatorCapital{dcDelegatorId = DelegatorId acctId} =
            Accounts.indexedAccount acctId (bspAccounts bsp) >>= \case
                Nothing -> error "Invariant violation: current delegator does not exist."
                Just acct -> do
                    addr <- accountCanonicalAddress acct
                    return (addr, dc)
    case mPoolId of
        Nothing -> do
            dlgs <- mapM mkReturn . Vec.toList $ passiveDelegatorsCapital
            return (Just dlgs)
        Just poolId ->
            case binarySearch bcBakerId bakerPoolCapital poolId of
                Nothing -> return Nothing
                Just BakerCapital{..} -> do
                    dlgs <- mapM mkReturn . Vec.toList $ bcDelegatorCapital
                    return (Just dlgs)

doAddBaker ::
    (SupportsPersistentState pv m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) =>
    PersistentBlockState pv ->
    AccountIndex ->
    BakerAdd ->
    m (BakerAddResult, PersistentBlockState pv)
doAddBaker pbs ai ba@BakerAdd{..} = do
    bsp <- loadPBS pbs
    Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
        -- Cannot resolve the account
        Nothing -> return (BAInvalidAccount, pbs)
        Just acct
            -- Account is already a baker
            | accountHasStake acct -> return (BAAlreadyBaker (BakerId ai), pbs)
            -- Account is not a baker
            | otherwise -> do
                cp <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> lookupCurrentParameters (bspUpdates bsp)
                if baStake < cp
                    then return (BAStakeUnderThreshold, pbs)
                    else do
                        let bid = BakerId ai
                        pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                        let updAgg Nothing = return (True, Trie.Insert ())
                            updAgg (Just ()) = return (False, Trie.NoChange)
                        Trie.adjust updAgg (bkuAggregationKey baKeys) (_aggregationKeys pab) >>= \case
                            -- Aggregation key is a duplicate
                            (False, _) -> return (BADuplicateAggregationKey, pbs)
                            (True, newAggregationKeys) -> do
                                newActiveBakers <- Trie.insert bid emptyPersistentActiveDelegators (_activeBakers pab)
                                newpabref <-
                                    refMake
                                        PersistentActiveBakers
                                            { _aggregationKeys = newAggregationKeys,
                                              _activeBakers = newActiveBakers,
                                              _passiveDelegators = pab ^. passiveDelegators,
                                              _totalActiveCapital = TotalActiveCapitalV0
                                            }
                                let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                                let updAcc = addAccountBakerV0 bid ba
                                -- This cannot fail to update the account, since we already looked up the account.
                                newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                                (BASuccess bid,)
                                    <$> storePBS
                                        pbs
                                        bsp
                                            { bspBirkParameters = newBirkParams,
                                              bspAccounts = newAccounts
                                            }

-- |Update an account's delegation to passive delegation. This only updates the account table,
-- and does not update the active baker index, which must be handled separately.
-- The account __must__ be an active delegator.
redelegatePassive ::
    forall pv m.
    (SupportsPersistentAccount pv m, PVSupportsDelegation pv) =>
    Accounts.Accounts pv ->
    DelegatorId ->
    m (Accounts.Accounts pv)
redelegatePassive accounts (DelegatorId accId) =
    Accounts.updateAccountsAtIndex'
        (setAccountDelegationTarget Transactions.DelegatePassive)
        accId
        accounts

doConfigureBaker ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState pv ->
    AccountIndex ->
    BakerConfigure ->
    m (BakerConfigureResult, PersistentBlockState pv)
doConfigureBaker pbs ai BakerConfigureAdd{..} = do
    -- It is assumed here that this account is NOT a baker and NOT a delegator.
    bsp <- loadPBS pbs
    Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
        -- Cannot resolve the account
        Nothing -> return (BCInvalidAccount, pbs)
        Just _ -> do
            chainParams <- lookupCurrentParameters (bspUpdates bsp)
            let poolParams = chainParams ^. cpPoolParameters
            let capitalMin = poolParams ^. ppMinimumEquityCapital
            let ranges = poolParams ^. ppCommissionBounds
            if
                    | bcaCapital < capitalMin -> return (BCStakeUnderThreshold, pbs)
                    | not (isInRange bcaTransactionFeeCommission (ranges ^. transactionCommissionRange)) ->
                        return (BCTransactionFeeCommissionNotInRange, pbs)
                    | not (isInRange bcaBakingRewardCommission (ranges ^. bakingCommissionRange)) ->
                        return (BCBakingRewardCommissionNotInRange, pbs)
                    | not (isInRange bcaFinalizationRewardCommission (ranges ^. finalizationCommissionRange)) ->
                        return (BCFinalizationRewardCommissionNotInRange, pbs)
                    | otherwise -> do
                        let bid = BakerId ai
                        pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                        let updAgg Nothing = return (True, Trie.Insert ())
                            updAgg (Just ()) = return (False, Trie.NoChange)
                        Trie.adjust updAgg (bkuAggregationKey bcaKeys) (_aggregationKeys pab) >>= \case
                            -- Aggregation key is a duplicate
                            (False, _) -> return (BCDuplicateAggregationKey (bkuAggregationKey bcaKeys), pbs)
                            (True, newAggregationKeys) -> do
                                newActiveBakers <- Trie.insert bid emptyPersistentActiveDelegators (_activeBakers pab)
                                newpabref <-
                                    refMake
                                        PersistentActiveBakers
                                            { _aggregationKeys = newAggregationKeys,
                                              _activeBakers = newActiveBakers,
                                              _passiveDelegators = pab ^. passiveDelegators,
                                              _totalActiveCapital = addActiveCapital bcaCapital (_totalActiveCapital pab)
                                            }
                                let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                                let cr =
                                        CommissionRates
                                            { _finalizationCommission = bcaFinalizationRewardCommission,
                                              _bakingCommission = bcaBakingRewardCommission,
                                              _transactionCommission = bcaTransactionFeeCommission
                                            }
                                    poolInfo =
                                        BaseAccounts.BakerPoolInfo
                                            { _poolOpenStatus = bcaOpenForDelegation,
                                              _poolMetadataUrl = bcaMetadataURL,
                                              _poolCommissionRates = cr
                                            }
                                    bakerInfo = bakerKeyUpdateToInfo bid bcaKeys
                                    bakerInfoEx =
                                        BaseAccounts.BakerInfoExV1
                                            { _bieBakerPoolInfo = poolInfo,
                                              _bieBakerInfo = bakerInfo
                                            }
                                    updAcc = addAccountBakerV1 bakerInfoEx bcaCapital bcaRestakeEarnings
                                -- This cannot fail to update the account, since we already looked up the account.
                                newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                                (BCSuccess [] bid,)
                                    <$> storePBS
                                        pbs
                                        bsp
                                            { bspBirkParameters = newBirkParams,
                                              bspAccounts = newAccounts
                                            }
doConfigureBaker pbs ai BakerConfigureUpdate{..} = do
    origBSP <- loadPBS pbs
    cp <- lookupCurrentParameters (bspUpdates origBSP)
    res <- MTL.runExceptT $ MTL.runWriterT $ flip MTL.execStateT origBSP $ do
        baker <- getAccountOrFail
        -- Check the various updates are OK, getting the transformation on the account
        -- implied by each.
        uKeys <- updateKeys baker
        uRestake <- updateRestakeEarnings baker
        uPoolInfo <- updateBakerPoolInfo baker cp
        uCapital <- updateCapital baker cp
        -- Compose together the transformations and apply them to the account.
        let updAcc = uKeys >=> uRestake >=> uPoolInfo >=> uCapital
        modifyAccount' updAcc
    case res of
        Left errorRes -> return (errorRes, pbs)
        Right (newBSP, changes) -> (BCSuccess changes bid,) <$> storePBS pbs newBSP
  where
    -- Lift a monadic action over the ExceptT, WriterT and StateT layers.
    liftBSO = lift . lift . lift
    bid = BakerId ai
    getAccountOrFail :: MTL.StateT (BlockStatePointers pv) (MTL.WriterT [BakerConfigureUpdateChange] (MTL.ExceptT BakerConfigureResult m)) (AccountBaker (AccountVersionFor pv))
    getAccountOrFail = do
        bsp <- MTL.get
        liftBSO (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
            Nothing -> MTL.throwError BCInvalidAccount
            Just acc ->
                accountBaker acc >>= \case
                    Nothing -> MTL.throwError BCInvalidBaker
                    Just bkr -> return bkr
    modifyAccount' updAcc = do
        bsp <- MTL.get
        newAccounts <- liftBSO $ Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
        MTL.put bsp{bspAccounts = newAccounts}
    ifPresent Nothing _ = return return
    ifPresent (Just v) k = k v
    updateKeys oldBkr = ifPresent bcuKeys $ \keys -> do
        bsp <- MTL.get
        pab <- liftBSO $ refLoad (_birkActiveBakers (bspBirkParameters bsp))
        let key = oldBkr ^. BaseAccounts.bakerAggregationVerifyKey
        -- Try updating the aggregation keys
        (keyOK, newAggregationKeys) <-
            -- If the aggregation key has not changed, we have nothing to do.
            if bkuAggregationKey keys == key
                then return (True, _aggregationKeys pab)
                else do
                    -- Remove the old key
                    ak1 <- liftBSO $ Trie.delete key (_aggregationKeys pab)
                    -- Add the new key and check that it is not already present
                    let updAgg Nothing = return (True, Trie.Insert ())
                        updAgg (Just ()) = return (False, Trie.NoChange)
                    liftBSO $ Trie.adjust updAgg (bkuAggregationKey keys) ak1
        unless keyOK (MTL.throwError (BCDuplicateAggregationKey key))
        newActiveBakers <- liftBSO $ refMake pab{_aggregationKeys = newAggregationKeys}
        let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newActiveBakers
        MTL.modify' $ \s -> s{bspBirkParameters = newBirkParams}
        MTL.tell [BakerConfigureUpdateKeys keys]
        -- Update the account with the new keys
        return (setAccountBakerKeys keys)
    updateRestakeEarnings oldBkr = ifPresent bcuRestakeEarnings $ \restakeEarnings -> do
        MTL.tell [BakerConfigureRestakeEarnings restakeEarnings]
        if oldBkr ^. BaseAccounts.stakeEarnings == restakeEarnings
            then return return
            else return $ setAccountRestakeEarnings restakeEarnings
    updateBakerPoolInfo ::
        AccountBaker (AccountVersionFor pv) ->
        ChainParameters pv ->
        MTL.StateT
            (BlockStatePointers pv)
            ( MTL.WriterT
                [BakerConfigureUpdateChange]
                (MTL.ExceptT BakerConfigureResult m)
            )
            (PersistentAccount (AccountVersionFor pv) -> m (PersistentAccount (AccountVersionFor pv)))
    updateBakerPoolInfo oldBkr cp = do
        let pu0 = emptyBakerPoolInfoUpdate
        pu1 <- condPoolInfoUpdate bcuOpenForDelegation (updateOpenForDelegation oldBkr) pu0
        pu2 <- condPoolInfoUpdate bcuMetadataURL (updateMetadataURL oldBkr) pu1
        pu3 <- condPoolInfoUpdate bcuTransactionFeeCommission (updateTransactionFeeCommission oldBkr cp) pu2
        pu4 <- condPoolInfoUpdate bcuBakingRewardCommission (updateBakingRewardCommission oldBkr cp) pu3
        pu5 <- condPoolInfoUpdate bcuFinalizationRewardCommission (updateFinalizationRewardCommission oldBkr cp) pu4
        return $ updateAccountBakerPoolInfo pu5
    condPoolInfoUpdate Nothing _ pu = return pu
    condPoolInfoUpdate (Just x) a pu = a x pu
    updateOpenForDelegation oldBkr openForDelegation pu = do
        MTL.tell [BakerConfigureOpenForDelegation openForDelegation]
        if oldBkr ^. BaseAccounts.poolOpenStatus == openForDelegation
            then return pu
            else do
                when (openForDelegation == Transactions.ClosedForAll) $ do
                    -- Transfer all existing delegators to passive delegation.
                    birkParams <- MTL.gets bspBirkParameters
                    activeBkrs <- liftBSO $ refLoad (birkParams ^. birkActiveBakers)
                    -- Update the active bakers
                    (delegators, newActiveBkrs) <- transferDelegatorsToPassive bid activeBkrs
                    newActiveBkrsRef <- refMake newActiveBkrs
                    MTL.modify $ \bsp -> bsp{bspBirkParameters = birkParams & birkActiveBakers .~ newActiveBkrsRef}
                    -- Update each baker account
                    accts0 <- MTL.gets bspAccounts
                    accts1 <- foldM redelegatePassive accts0 delegators
                    MTL.modify $ \bsp -> bsp{bspAccounts = accts1}
                return $! pu{updOpenForDelegation = Just openForDelegation}
    updateMetadataURL oldBkr metadataURL pu = do
        MTL.tell [BakerConfigureMetadataURL metadataURL]
        if oldBkr ^. BaseAccounts.poolMetadataUrl == metadataURL
            then return pu
            else return $! pu{updMetadataURL = Just metadataURL}
    updateTransactionFeeCommission oldBkr cp tfc pu = do
        let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
        unless (isInRange tfc range) (MTL.throwError BCTransactionFeeCommissionNotInRange)
        MTL.tell [BakerConfigureTransactionFeeCommission tfc]
        if oldBkr ^. BaseAccounts.poolCommissionRates . transactionCommission == tfc
            then return pu
            else return $! pu{updTransactionFeeCommission = Just tfc}
    updateBakingRewardCommission oldBkr cp brc pu = do
        let range = cp ^. cpPoolParameters . ppCommissionBounds . bakingCommissionRange
        unless (isInRange brc range) (MTL.throwError BCBakingRewardCommissionNotInRange)
        MTL.tell [BakerConfigureBakingRewardCommission brc]
        if oldBkr ^. BaseAccounts.poolCommissionRates . bakingCommission == brc
            then return pu
            else return $! pu{updBakingRewardCommission = Just brc}
    updateFinalizationRewardCommission oldBkr cp frc pu = do
        let range = cp ^. cpPoolParameters . ppCommissionBounds . finalizationCommissionRange
        unless (isInRange frc range) (MTL.throwError BCFinalizationRewardCommissionNotInRange)
        MTL.tell [BakerConfigureFinalizationRewardCommission frc]
        if oldBkr ^. BaseAccounts.poolCommissionRates . finalizationCommission == frc
            then return pu
            else return $! pu{updFinalizationRewardCommission = Just frc}
    updateCapital oldBkr cp = ifPresent bcuCapital $ \capital -> do
        when (_bakerPendingChange oldBkr /= BaseAccounts.NoChange) (MTL.throwError BCChangePending)
        let capitalMin = cp ^. cpPoolParameters . ppMinimumEquityCapital
        let cooldownDuration = cp ^. cpCooldownParameters . cpPoolOwnerCooldown
            cooldownElapsed = addDurationSeconds bcuSlotTimestamp cooldownDuration
        if capital == 0
            then do
                let bpc = BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                MTL.tell [BakerConfigureStakeReduced capital]
                return $ setAccountStakePendingChange bpc
            else do
                when (capital < capitalMin) (MTL.throwError BCStakeUnderThreshold)
                case compare capital (_stakedAmount oldBkr) of
                    LT -> do
                        let bpc = BaseAccounts.ReduceStake capital (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                        MTL.tell [BakerConfigureStakeReduced capital]
                        return $ setAccountStakePendingChange bpc
                    EQ -> do
                        MTL.tell [BakerConfigureStakeIncreased capital]
                        return return
                    GT -> do
                        birkParams <- MTL.gets bspBirkParameters
                        activeBkrs <- liftBSO $ refLoad (birkParams ^. birkActiveBakers)
                        newActiveBkrs <-
                            liftBSO $
                                refMake $
                                    activeBkrs
                                        & totalActiveCapital %~ addActiveCapital (capital - _stakedAmount oldBkr)
                        MTL.modify' $ \bsp -> bsp{bspBirkParameters = birkParams & birkActiveBakers .~ newActiveBkrs}
                        MTL.tell [BakerConfigureStakeIncreased capital]
                        return $ setAccountStake capital

doConstrainBakerCommission ::
    (SupportsPersistentState pv m, PVSupportsDelegation pv) =>
    PersistentBlockState pv ->
    AccountIndex ->
    CommissionRanges ->
    m (PersistentBlockState pv)
doConstrainBakerCommission pbs ai ranges = do
    bsp <- loadPBS pbs
    onAccount ai bsp accountBaker >>= \case
        Nothing -> return pbs
        Just bkr -> do
            let oldRates = bkr ^. BaseAccounts.poolCommissionRates
            let newRates = updateRates oldRates
            if oldRates == newRates
                then return pbs
                else do
                    newAccounts <- Accounts.updateAccountsAtIndex' (setAccountCommissionRates newRates) ai (bspAccounts bsp)
                    storePBS pbs bsp{bspAccounts = newAccounts}
  where
    updateRates = updateTransactionFeeCommission . updateBakingRewardCommission . updateFinalizationRewardCommission
    updateTransactionFeeCommission =
        transactionCommission %~ (`closestInRange` (ranges ^. transactionCommissionRange))
    updateBakingRewardCommission =
        bakingCommission %~ (`closestInRange` (ranges ^. bakingCommissionRange))
    updateFinalizationRewardCommission =
        finalizationCommission %~ (`closestInRange` (ranges ^. finalizationCommissionRange))

-- |Checks that the delegation target is not over-delegated.
-- This can throw one of the following 'DelegationConfigureResult's, in order:
--
--   * 'DCInvalidDelegationTarget' if the target baker is not a baker.
--   * 'DCPoolStakeOverThreshold' if the delegated amount puts the pool over the leverage bound.
--   * 'DCPoolOverDelegated' if the delegated amount puts the pool over the capital bound.
delegationConfigureDisallowOverdelegation ::
    (IsProtocolVersion pv, PVSupportsDelegation pv, MTL.MonadError DelegationConfigureResult m, SupportsPersistentAccount pv m) =>
    BlockStatePointers pv ->
    PoolParameters 'ChainParametersV1 ->
    DelegationTarget ->
    m ()
delegationConfigureDisallowOverdelegation bsp poolParams target = case target of
    Transactions.DelegatePassive -> return ()
    Transactions.DelegateToBaker bid@(BakerId baid) -> do
        bakerEquityCapital <-
            onAccount baid bsp accountBakerStakeAmount >>= \case
                Just amt -> return amt
                _ -> MTL.throwError (DCInvalidDelegationTarget bid)
        capitalTotal <- totalCapital bsp
        bakerDelegatedCapital <- poolDelegatorCapital bsp bid
        let PoolCaps{..} = delegatedCapitalCaps poolParams capitalTotal bakerEquityCapital bakerDelegatedCapital
        when (bakerDelegatedCapital > leverageCap) $ MTL.throwError DCPoolStakeOverThreshold
        when (bakerDelegatedCapital > boundCap) $ MTL.throwError DCPoolOverDelegated

-- |Check that a delegation target is open for delegation.
-- If the target is not a baker, this throws 'DCInvalidDelegationTarget'.
-- If the target is not open for all, this throws 'DCPoolClosed'.
delegationCheckTargetOpen ::
    (IsProtocolVersion pv, PVSupportsDelegation pv, MTL.MonadError DelegationConfigureResult m, SupportsPersistentAccount pv m) =>
    BlockStatePointers pv ->
    DelegationTarget ->
    m ()
delegationCheckTargetOpen _ Transactions.DelegatePassive = return ()
delegationCheckTargetOpen bsp (Transactions.DelegateToBaker bid@(BakerId baid)) = do
    onAccount baid bsp accountBaker >>= \case
        Just baker -> do
            case baker ^. BaseAccounts.poolOpenStatus of
                Transactions.OpenForAll -> return ()
                _ -> MTL.throwError DCPoolClosed
        _ -> MTL.throwError (DCInvalidDelegationTarget bid)

doConfigureDelegation ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState pv ->
    AccountIndex ->
    DelegationConfigure ->
    m (DelegationConfigureResult, PersistentBlockState pv)
doConfigureDelegation pbs ai DelegationConfigureAdd{..} = do
    -- It is assumed here that this account is NOT a baker and NOT a delegator.
    bsp <- loadPBS pbs
    poolParams <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
    result <- MTL.runExceptT $ do
        newBSP <- updateBlockState bsp
        delegationConfigureDisallowOverdelegation newBSP poolParams dcaDelegationTarget
        return newBSP
    case result of
        Left e -> return (e, pbs)
        Right newBirkParams -> (DCSuccess [] did,) <$> storePBS pbs newBirkParams
  where
    did = DelegatorId ai
    updateBlockState bsp =
        lift (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
            Nothing -> MTL.throwError DCInvalidAccount
            Just _ -> do
                delegationCheckTargetOpen bsp dcaDelegationTarget
                newBirkParams <- updateBirk bsp dcaDelegationTarget
                let dlg =
                        BaseAccounts.AccountDelegationV1
                            { BaseAccounts._delegationIdentity = did,
                              BaseAccounts._delegationStakedAmount = dcaCapital,
                              BaseAccounts._delegationStakeEarnings = dcaRestakeEarnings,
                              BaseAccounts._delegationTarget = dcaDelegationTarget,
                              BaseAccounts._delegationPendingChange = BaseAccounts.NoChange
                            }
                -- This cannot fail to update the accounts, since we already looked up the accounts:
                newAccounts <- lift $ Accounts.updateAccountsAtIndex' (addAccountDelegator dlg) ai (bspAccounts bsp)
                return bsp{bspBirkParameters = newBirkParams, bspAccounts = newAccounts}
    updateBirk bsp Transactions.DelegatePassive = lift $ do
        ab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        let PersistentActiveDelegatorsV1 dset tot = ab ^. passiveDelegators
        newDset <- Trie.insert did () dset
        newAB <-
            refMake
                ab
                    { _passiveDelegators = PersistentActiveDelegatorsV1 newDset (tot + dcaCapital),
                      _totalActiveCapital = addActiveCapital dcaCapital (_totalActiveCapital ab)
                    }
        return $! bspBirkParameters bsp & birkActiveBakers .~ newAB
    updateBirk bsp (Transactions.DelegateToBaker bid) = do
        pab <- lift $ refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        mDels <- lift $ Trie.lookup bid (pab ^. activeBakers)
        case mDels of
            Nothing -> MTL.throwError (DCInvalidDelegationTarget bid)
            Just (PersistentActiveDelegatorsV1 dels tot) -> do
                newDels <- lift $ flip PersistentActiveDelegatorsV1 (tot + dcaCapital) <$> (Trie.insert did () dels)
                newActiveBakers <- lift $ Trie.insert bid newDels (pab ^. activeBakers)
                newpabref <- lift $ refMake pab{_activeBakers = newActiveBakers, _totalActiveCapital = addActiveCapital dcaCapital (_totalActiveCapital pab)}
                return $! bspBirkParameters bsp & birkActiveBakers .~ newpabref
doConfigureDelegation pbs ai DelegationConfigureUpdate{..} = do
    origBSP <- loadPBS pbs
    cp <- lookupCurrentParameters (bspUpdates origBSP)
    res <- MTL.runExceptT $ MTL.runWriterT $ flip MTL.execStateT origBSP $ do
        oldTarget <- updateDelegationTarget
        updateRestakeEarnings
        oldCapital <- updateCapital cp
        checkOverdelegation oldCapital oldTarget cp
    case res of
        Left errorRes -> return (errorRes, pbs)
        Right (newBSP, changes) -> (DCSuccess changes did,) <$> storePBS pbs newBSP
  where
    did = DelegatorId ai
    getAccountOrFail = do
        bsp <- MTL.get
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            Nothing -> MTL.throwError DCInvalidAccount
            Just acc ->
                accountDelegator acc >>= \case
                    Just del -> return del
                    Nothing -> MTL.throwError DCInvalidDelegator
    modifyAccount updAcc = do
        bsp <- MTL.get
        newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
        MTL.put
            bsp
                { bspAccounts = newAccounts
                }
    updateDelegationTarget = do
        acctDlg <- getAccountOrFail
        let oldTarget = acctDlg ^. BaseAccounts.delegationTarget
        forM_ dcuDelegationTarget $ \target -> do
            unless (oldTarget == target) $ do
                -- Check that the target pool is open for delegation
                bsp0 <- MTL.get
                delegationCheckTargetOpen bsp0 target
                ab <- refLoad =<< use (to bspBirkParameters . birkActiveBakers)
                let stakedAmt = acctDlg ^. BaseAccounts.delegationStakedAmount
                -- Transfer the delegator in the active bakers from the old target to the new one.
                -- Note, these functions do not modify the total stake, but this is not being changed
                -- - just moved.
                ab1 <- removeDelegator oldTarget did stakedAmt ab
                ab2 <-
                    addDelegator target did stakedAmt ab1 >>= \case
                        Left bid -> MTL.throwError (DCInvalidDelegationTarget bid)
                        Right ab2 -> return ab2
                newActiveBakers <- refMake ab2
                MTL.modify' $ \bsp -> bsp{bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newActiveBakers}
                -- Update the account with the new delegation target.
                modifyAccount (setAccountDelegationTarget target)
            MTL.tell [DelegationConfigureDelegationTarget target]
        return oldTarget
    updateRestakeEarnings = forM_ dcuRestakeEarnings $ \restakeEarnings -> do
        acctDlg <- getAccountOrFail
        unless (acctDlg ^. BaseAccounts.delegationStakeEarnings == restakeEarnings) $ do
            modifyAccount (setAccountRestakeEarnings restakeEarnings)
        MTL.tell [DelegationConfigureRestakeEarnings restakeEarnings]
    updateCapital cp = do
        ad <- getAccountOrFail
        forM_ dcuCapital $ \capital -> do
            when (BaseAccounts._delegationPendingChange ad /= BaseAccounts.NoChange) (MTL.throwError DCChangePending)
            -- Cooldown time, used when the change reduces or removes the stake.
            let cooldownDuration = cp ^. cpCooldownParameters . cpDelegatorCooldown
                cooldownElapsed = addDurationSeconds dcuSlotTimestamp cooldownDuration
            if capital == 0
                then do
                    let dpc = BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                    modifyAccount $ setAccountStakePendingChange dpc
                    MTL.tell [DelegationConfigureStakeReduced capital]
                else case compare capital (BaseAccounts._delegationStakedAmount ad) of
                    LT -> do
                        let dpc = BaseAccounts.ReduceStake capital (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                        modifyAccount $ setAccountStakePendingChange dpc
                        MTL.tell [DelegationConfigureStakeReduced capital]
                    EQ ->
                        MTL.tell [DelegationConfigureStakeIncreased capital]
                    GT -> do
                        bsp1 <- MTL.get
                        ab <- refLoad (bspBirkParameters bsp1 ^. birkActiveBakers)
                        newActiveBakers <- addTotalsInActiveBakers ab ad (capital - BaseAccounts._delegationStakedAmount ad)
                        MTL.modify' $ \bsp -> bsp{bspBirkParameters = bspBirkParameters bsp1 & birkActiveBakers .~ newActiveBakers}
                        modifyAccount $ setAccountStake capital
                        MTL.tell [DelegationConfigureStakeIncreased capital]
        return $ BaseAccounts._delegationStakedAmount ad
    addTotalsInActiveBakers ab0 ad delta = do
        let ab1 = ab0 & totalActiveCapital %~ addActiveCapital delta
        case ad ^. BaseAccounts.delegationTarget of
            Transactions.DelegatePassive -> do
                let PersistentActiveDelegatorsV1 dset dtot = ab1 ^. passiveDelegators
                refMake $! ab1 & passiveDelegators .~ PersistentActiveDelegatorsV1 dset (dtot + delta)
            Transactions.DelegateToBaker bid -> do
                Trie.lookup bid (ab1 ^. activeBakers) >>= \case
                    Nothing -> error "Invariant violation: delegation target is not an active baker"
                    Just (PersistentActiveDelegatorsV1 dset dtot) -> do
                        newActiveMap <- Trie.insert bid (PersistentActiveDelegatorsV1 dset (dtot + delta)) (ab1 ^. activeBakers)
                        refMake $! ab1 & activeBakers .~ newActiveMap
    checkOverdelegation oldCapital oldTarget cp = do
        let doCheckOverDelegation = do
                let pp = cp ^. cpPoolParameters
                ad <- getAccountOrFail
                let target = ad ^. BaseAccounts.delegationTarget
                bsp <- MTL.get
                delegationConfigureDisallowOverdelegation bsp pp target
        case (dcuCapital, dcuDelegationTarget) of
            (Just newCapital, Just newTarget) -> unless (newCapital <= oldCapital && newTarget == oldTarget) doCheckOverDelegation
            (Just newCapital, Nothing) -> unless (newCapital <= oldCapital) doCheckOverDelegation
            (Nothing, Just newTarget) -> unless (newTarget == oldTarget) doCheckOverDelegation
            _ -> return ()

doUpdateBakerKeys ::
    (SupportsPersistentState pv m, AccountVersionFor pv ~ 'AccountV0) =>
    PersistentBlockState pv ->
    AccountIndex ->
    BakerKeyUpdate ->
    m (BakerKeyUpdateResult, PersistentBlockState pv)
doUpdateBakerKeys pbs ai bku@BakerKeyUpdate{..} = do
    bsp <- loadPBS pbs
    onAccount ai bsp accountBaker >>= \case
        -- The account is valid and has a baker
        Just bkr -> do
            pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
            let oldAggregationKey = bkr ^. BaseAccounts.bakerAggregationVerifyKey
            -- Try updating the aggregation keys
            (keyOK, newAggregationKeys) <-
                -- If the aggregation key has not changed, we have nothing to do.
                if bkuAggregationKey == oldAggregationKey
                    then return (True, _aggregationKeys pab)
                    else do
                        -- Remove the old key
                        ak1 <- Trie.delete oldAggregationKey (_aggregationKeys pab)
                        -- Add the new key and check that it is not already present
                        let updAgg Nothing = return (True, Trie.Insert ())
                            updAgg (Just ()) = return (False, Trie.NoChange)
                        Trie.adjust updAgg bkuAggregationKey ak1
            if keyOK
                then do
                    -- The new aggregation key is known to be unique
                    newActiveBakers <- refMake pab{_aggregationKeys = newAggregationKeys}
                    let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newActiveBakers
                    -- Update the account with the new keys
                    newAccounts <- Accounts.updateAccountsAtIndex' (setAccountBakerKeys bku) ai (bspAccounts bsp)
                    (BKUSuccess (BakerId ai),)
                        <$> storePBS
                            pbs
                            bsp
                                { bspBirkParameters = newBirkParams,
                                  bspAccounts = newAccounts
                                }
                else return (BKUDuplicateAggregationKey, pbs)
        -- Cannot resolve the account, or it is not a baker
        _ -> return (BKUInvalidBaker, pbs)

doUpdateBakerStake ::
    (SupportsPersistentState pv m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) =>
    PersistentBlockState pv ->
    AccountIndex ->
    Amount ->
    m (BakerStakeUpdateResult, PersistentBlockState pv)
doUpdateBakerStake pbs ai newStake = do
    bsp <- loadPBS pbs

    onAccount' ai bsp accountStakeDetails >>= \case
        Just StakeDetailsBaker{..} -> do
            if sdPendingChange /= BaseAccounts.NoChange
                then -- A change is already pending
                    return (BSUChangePending (BakerId ai), pbs)
                else -- We can make the change
                do
                    let curEpoch = epoch $ _birkSeedState (bspBirkParameters bsp)
                    upds <- refLoad (bspUpdates bsp)
                    cooldown <- (2 +) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized <$> refLoad (currentParameters upds)

                    bakerStakeThreshold <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> doGetChainParameters pbs
                    let applyUpdate updAcc = do
                            newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                            storePBS pbs bsp{bspAccounts = newAccounts}
                    case compare newStake sdStakedCapital of
                        LT ->
                            if newStake < bakerStakeThreshold
                                then return (BSUStakeUnderThreshold, pbs)
                                else
                                    (BSUStakeReduced (BakerId ai) (curEpoch + cooldown),)
                                        <$> applyUpdate
                                            ( setAccountStakePendingChange
                                                (BaseAccounts.ReduceStake newStake (BaseAccounts.PendingChangeEffectiveV0 $ curEpoch + cooldown))
                                            )
                        EQ -> return (BSUStakeUnchanged (BakerId ai), pbs)
                        GT -> (BSUStakeIncreased (BakerId ai),) <$> applyUpdate (setAccountStake newStake)
        _ -> return (BSUInvalidBaker, pbs)

doUpdateBakerRestakeEarnings ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    AccountIndex ->
    Bool ->
    m (BakerRestakeEarningsUpdateResult, PersistentBlockState pv)
doUpdateBakerRestakeEarnings pbs ai newRestakeEarnings = do
    bsp <- loadPBS pbs
    onAccount' ai bsp accountStakeDetails >>= \case
        Just StakeDetailsBaker{..} -> do
            if newRestakeEarnings == sdRestakeEarnings
                then return (BREUUpdated (BakerId ai), pbs)
                else do
                    let updAcc = setAccountRestakeEarnings newRestakeEarnings
                    newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                    (BREUUpdated (BakerId ai),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
        _ -> return (BREUInvalidBaker, pbs)

doRemoveBaker ::
    (SupportsPersistentState pv m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0) =>
    PersistentBlockState pv ->
    AccountIndex ->
    m (BakerRemoveResult, PersistentBlockState pv)
doRemoveBaker pbs ai = do
    bsp <- loadPBS pbs
    onAccount' ai bsp accountStakeDetails >>= \case
        -- The account is valid and has a baker
        Just StakeDetailsBaker{..} -> do
            if sdPendingChange /= BaseAccounts.NoChange
                then -- A change is already pending
                    return (BRChangePending (BakerId ai), pbs)
                else do
                    -- We can make the change
                    -- Note: this just sets the account to be removed at a future epoch
                    -- transition.
                    let curEpoch = epoch $ _birkSeedState (bspBirkParameters bsp)
                    upds <- refLoad (bspUpdates bsp)
                    cooldown <- (2 +) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized <$> refLoad (currentParameters upds)
                    let updAcc =
                            setAccountStakePendingChange $
                                BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 $ curEpoch + cooldown)
                    newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                    (BRRemoved (BakerId ai) (curEpoch + cooldown),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
        -- The account is not valid or has no baker
        _ -> return (BRInvalidBaker, pbs)

doRewardAccount :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountIndex -> Amount -> m (Maybe AccountAddress, PersistentBlockState pv)
doRewardAccount pbs ai reward = do
    bsp <- loadPBS pbs
    (mRes, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
    case mRes of
        Nothing -> return (Nothing, pbs)
        Just (addr, updActiveBkrs) -> do
            newActiveBkrs <- updActiveBkrs (bspBirkParameters bsp ^. birkActiveBakers)
            (Just addr,)
                <$> storePBS
                    pbs
                    bsp
                        { bspAccounts = newAccounts,
                          bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newActiveBkrs
                        }
  where
    updAcc acc = do
        addr <- accountCanonicalAddress acc
        stakeDetails <- accountStakeDetails acc
        (restaked, acc1) <- case stakeDetails of
            StakeDetailsBaker{..} | sdRestakeEarnings -> do
                acc' <- setAccountStake (sdStakedCapital + reward) acc
                let upd pActiveBkrs = do
                        activeBkrs <- refLoad pActiveBkrs
                        refMake $! activeBkrs & totalActiveCapital %~ addActiveCapital reward
                return (upd, acc')
            StakeDetailsDelegator{..} | sdRestakeEarnings -> do
                acc' <- setAccountStake (sdStakedCapital + reward) acc
                let upd pActiveBkrs = do
                        activeBkrs0 <- refLoad pActiveBkrs
                        activeBkrs1 <- updateDelegationPoolCapital activeBkrs0 sdDelegationTarget
                        refMake $! activeBkrs1 & totalActiveCapital %~ addActiveCapital reward
                return (upd, acc')
            _ -> return (return, acc)
        acc2 <- addAccountAmount reward acc1
        return ((addr, restaked), acc2)

    updateDelegationPoolCapital ::
        (AVSupportsDelegation av, IsAccountVersion av) =>
        PersistentActiveBakers av ->
        Transactions.DelegationTarget ->
        m (PersistentActiveBakers av)
    updateDelegationPoolCapital activeBkrs Transactions.DelegatePassive = do
        let tot = adDelegatorTotalCapital $ activeBkrs ^. passiveDelegators
        return $!
            activeBkrs
                & passiveDelegators %~ \dlgs ->
                    dlgs{adDelegatorTotalCapital = tot + reward}
    updateDelegationPoolCapital activeBkrs (Transactions.DelegateToBaker bid) = do
        let activeBkrsMap = activeBkrs ^. activeBakers
            adj Nothing = error "Invariant violation: active baker account is not in active bakers map"
            adj (Just dlgs) = do
                let tot = adDelegatorTotalCapital dlgs
                return ((), Trie.Insert $ dlgs{adDelegatorTotalCapital = tot + reward})
        (_, newActiveBkrsMap) <- Trie.adjust adj bid activeBkrsMap
        return $! activeBkrs & activeBakers .~ newActiveBkrsMap

doGetBakerPoolRewardDetails :: (PVSupportsDelegation pv, SupportsPersistentState pv m) => PersistentBlockState pv -> m (Map.Map BakerId BakerPoolRewardDetails)
doGetBakerPoolRewardDetails pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    poolRewards <- refLoad hpr
    rewardsList <- LFMBT.toAscList (bakerPoolRewardDetails poolRewards)
    capitals <- refLoad $ currentCapital poolRewards
    let bakerIdList = bcBakerId <$> Vec.toList (bakerPoolCapital capitals)
    -- The lists must be the same length since the rewards are reset when the current capital
    -- distribution is updated.
    return $! Map.fromList (zip bakerIdList rewardsList)

doGetRewardStatus :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> m (RewardStatus' Epoch)
doGetRewardStatus pbs = do
    bsp <- loadPBS pbs
    let bankStatus = _unhashed $ bspBank bsp
    let rewardsV0 :: RewardStatus' Epoch
        rewardsV0 =
            RewardStatusV0
                { rsTotalAmount = bankStatus ^. Rewards.totalGTU,
                  rsTotalEncryptedAmount = bankStatus ^. Rewards.totalEncryptedGTU,
                  rsBakingRewardAccount = bankStatus ^. Rewards.bakingRewardAccount,
                  rsFinalizationRewardAccount = bankStatus ^. Rewards.finalizationRewardAccount,
                  rsGasAccount = bankStatus ^. Rewards.gasAccount,
                  rsProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
                }
        rewardsV1 :: (PVSupportsDelegation pv) => m (RewardStatus' Epoch)
        rewardsV1 = do
            poolRewards <- refLoad (bspPoolRewards bsp)
            tc <- totalCapital bsp
            return
                RewardStatusV1
                    { rsTotalAmount = bankStatus ^. Rewards.totalGTU,
                      rsTotalEncryptedAmount = bankStatus ^. Rewards.totalEncryptedGTU,
                      rsBakingRewardAccount = bankStatus ^. Rewards.bakingRewardAccount,
                      rsFinalizationRewardAccount = bankStatus ^. Rewards.finalizationRewardAccount,
                      rsGasAccount = bankStatus ^. Rewards.gasAccount,
                      rsFoundationTransactionRewards = foundationTransactionRewards poolRewards,
                      rsNextPaydayTime = nextPaydayEpoch poolRewards,
                      rsNextPaydayMintRate = nextPaydayMintRate poolRewards,
                      rsTotalStakedCapital = tc,
                      rsProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
                    }
    case protocolVersion @pv of
        SP1 -> return rewardsV0
        SP2 -> return rewardsV0
        SP3 -> return rewardsV0
        SP4 -> rewardsV1
        SP5 -> rewardsV1
        SP6 -> rewardsV1

doRewardFoundationAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> Amount -> m (PersistentBlockState pv)
doRewardFoundationAccount pbs reward = do
    bsp <- loadPBS pbs
    let updAcc = addAccountAmount reward
    foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
    newAccounts <- Accounts.updateAccountsAtIndex' updAcc foundationAccount (bspAccounts bsp)
    storePBS pbs (bsp{bspAccounts = newAccounts})

doGetFoundationAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (PersistentAccount (AccountVersionFor pv))
doGetFoundationAccount pbs = do
    bsp <- loadPBS pbs
    foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
    macc <- Accounts.indexedAccount foundationAccount (bspAccounts bsp)
    case macc of
        Nothing -> error "bsoGetFoundationAccount: invalid foundation account"
        Just acc -> return acc

doMint :: (SupportsPersistentState pv m) => PersistentBlockState pv -> MintAmounts -> m (PersistentBlockState pv)
doMint pbs mint = do
    bsp <- loadPBS pbs
    let newBank =
            bspBank bsp
                & unhashed
                    %~ (Rewards.totalGTU +~ mintTotal mint)
                    . (Rewards.bakingRewardAccount +~ mintBakingReward mint)
                    . (Rewards.finalizationRewardAccount +~ mintFinalizationReward mint)
    let updAcc = addAccountAmount $ mintDevelopmentCharge mint
    foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
    newAccounts <- Accounts.updateAccountsAtIndex' updAcc foundationAccount (bspAccounts bsp)
    storePBS pbs (bsp{bspBank = newBank, bspAccounts = newAccounts})

doGetAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountAddress -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
doGetAccount pbs addr = do
    bsp <- loadPBS pbs
    Accounts.getAccountWithIndex addr (bspAccounts bsp)

doGetAccountExists :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountAddress -> m Bool
doGetAccountExists pbs aaddr = do
    bsp <- loadPBS pbs
    Accounts.exists aaddr (bspAccounts bsp)

doGetActiveBakers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [BakerId]
doGetActiveBakers pbs = do
    bsp <- loadPBS pbs
    ab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    Trie.keysAsc (ab ^. activeBakers)

doGetAccountByCredId :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ID.RawCredentialRegistrationID -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
doGetAccountByCredId pbs cid = do
    bsp <- loadPBS pbs
    Accounts.getAccountByCredId cid (bspAccounts bsp)

doGetAccountIndex :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountAddress -> m (Maybe AccountIndex)
doGetAccountIndex pbs addr = do
    bsp <- loadPBS pbs
    Accounts.getAccountIndex addr (bspAccounts bsp)

doGetAccountByIndex :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountIndex -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
doGetAccountByIndex pbs aid = do
    bsp <- loadPBS pbs
    Accounts.indexedAccount aid (bspAccounts bsp)

doGetIndexedAccountByIndex :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountIndex -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
doGetIndexedAccountByIndex pbs idx = fmap (idx,) <$> doGetAccountByIndex pbs idx

doAccountList :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [AccountAddress]
doAccountList pbs = do
    bsp <- loadPBS pbs
    Accounts.accountAddresses (bspAccounts bsp)

doAddressWouldClash :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountAddress -> m Bool
doAddressWouldClash pbs addr = do
    bsp <- loadPBS pbs
    Accounts.addressWouldClash addr (bspAccounts bsp)

doRegIdExists :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
    bsp <- loadPBS pbs
    isJust <$> Accounts.regIdExists regid (bspAccounts bsp)

doCreateAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ID.GlobalContext -> AccountAddress -> ID.AccountCredential -> m (Maybe (PersistentAccount (AccountVersionFor pv)), PersistentBlockState pv)
doCreateAccount pbs cryptoParams acctAddr credential = do
    acct <- newAccount cryptoParams acctAddr credential
    bsp <- loadPBS pbs
    -- Add the account
    (res, accts1) <- Accounts.putNewAccount acct (bspAccounts bsp)
    case res of
        Just idx -> do
            -- Record the RegId since we created a new account.
            accts2 <- Accounts.recordRegId (ID.credId credential) idx accts1
            (Just acct,) <$> storePBS pbs (bsp{bspAccounts = accts2})
        Nothing ->
            -- the account was not created
            return (Nothing, pbs)

doModifyAccount :: forall m pv. (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountUpdate -> m (PersistentBlockState pv)
doModifyAccount pbs aUpd@AccountUpdate{..} = do
    bsp <- loadPBS pbs
    -- Do the update to the account. The first component of the return value is a @Just@ when
    -- the release schedule for the account is updated. This is a triple of: the reference
    -- to the account (used by the release schedule index), the former first release timestamp
    -- (or @Nothing@ if there was none), and the new first release timestamp (or @Nothing@ if
    -- there is none). These are used to update the release schedule index as necessary.
    let doUpd ::
            PersistentAccount (AccountVersionFor pv) ->
            m (Maybe (RSAccountRef pv, Maybe Timestamp, Maybe Timestamp), PersistentAccount (AccountVersionFor pv))
        doUpd acc = do
            acc' <- updateAccount aUpd acc
            releaseChange <- forM _auReleaseSchedule $ \_ -> do
                acctRef <- case protocolVersion @pv of
                    SP1 -> accountCanonicalAddress acc'
                    SP2 -> accountCanonicalAddress acc'
                    SP3 -> accountCanonicalAddress acc'
                    SP4 -> accountCanonicalAddress acc'
                    SP5 -> return _auIndex
                    SP6 -> return _auIndex
                !oldRel <- accountNextReleaseTimestamp acc
                !newRel <- accountNextReleaseTimestamp acc'
                return (acctRef :: RSAccountRef pv, oldRel, newRel)
            return (releaseChange, acc')
    (releaseChange, accts1) <- Accounts.updateAccountsAtIndex doUpd _auIndex (bspAccounts bsp)
    newRS <- case releaseChange of
        Just (Just (aref, Nothing, Just ts)) -> addAccountRelease ts aref (bspReleaseSchedule bsp)
        Just (Just (aref, Just oldts, Just newts))
            | newts < oldts -> updateAccountRelease oldts newts aref (bspReleaseSchedule bsp)
        _ -> return $ bspReleaseSchedule bsp
    storePBS pbs (bsp{bspAccounts = accts1, bspReleaseSchedule = newRS})

doSetAccountCredentialKeys :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountIndex -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m (PersistentBlockState pv)
doSetAccountCredentialKeys pbs accIndex credIx credKeys = do
    bsp <- loadPBS pbs
    accts1 <- Accounts.updateAccountsAtIndex' upd accIndex (bspAccounts bsp)
    storePBS pbs (bsp{bspAccounts = accts1})
  where
    upd = updateAccountCredentialKeys credIx credKeys

doUpdateAccountCredentials ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    -- | Address of the account to update.
    AccountIndex ->
    -- | List of credential indices to remove.
    [ID.CredentialIndex] ->
    -- | New credentials to add.
    Map.Map ID.CredentialIndex ID.AccountCredential ->
    -- | New account threshold
    ID.AccountThreshold ->
    m (PersistentBlockState pv)
doUpdateAccountCredentials pbs accIndex remove add thrsh = do
    bsp <- loadPBS pbs
    (res, accts1) <- Accounts.updateAccountsAtIndex upd accIndex (bspAccounts bsp)
    case res of
        Just () -> do
            -- If we deploy a credential, record it
            accts2 <- Accounts.recordRegIds ((,accIndex) <$> Map.elems (ID.credId <$> add)) accts1
            storePBS pbs (bsp{bspAccounts = accts2})
        Nothing -> return pbs -- this should not happen, the precondition of this method is that the account exists. But doing nothing is safe.
  where
    upd oldAccount = ((),) <$> updateAccountCredentials remove add thrsh oldAccount

doGetInstance ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    ContractAddress ->
    m (Maybe (InstanceInfoType Modules.PersistentInstrumentedModuleV Instances.InstanceStateV))
doGetInstance pbs caddr = do
    bsp <- loadPBS pbs
    minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
    forM minst Instances.mkInstanceInfo

doContractInstanceList :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [ContractAddress]
doContractInstanceList pbs = do
    bsp <- loadPBS pbs
    Instances.allInstances (bspInstances bsp)

doPutNewInstance ::
    forall m pv v.
    (SupportsPersistentState pv m, Wasm.IsWasmVersion v) =>
    PersistentBlockState pv ->
    NewInstanceData (Modules.PersistentInstrumentedModuleV v) v ->
    m (ContractAddress, PersistentBlockState pv)
doPutNewInstance pbs NewInstanceData{..} = do
    bsp <- loadPBS pbs
    mods <- refLoad (bspModules bsp)
    -- Create the instance
    (ca, insts) <- Instances.newContractInstance (fnew mods) (bspInstances bsp)
    (ca,) <$> storePBS pbs bsp{bspInstances = insts}
  where
    fnew mods ca =
        case Wasm.getWasmVersion @v of
            Wasm.SV0 -> do
                let params =
                        PersistentInstanceParameters
                            { pinstanceAddress = ca,
                              pinstanceOwner = nidOwner,
                              pinstanceContractModule = GSWasm.miModuleRef nidInterface,
                              pinstanceReceiveFuns = nidEntrypoints,
                              pinstanceInitName = nidInitName,
                              pinstanceParameterHash = Instances.makeInstanceParameterHash ca nidOwner (GSWasm.miModuleRef nidInterface) nidInitName
                            }
                pinstanceParameters <- makeBufferedRef params
                -- We retrieve the module interface here so that we only have a single copy of it, meaning that
                -- all instances created from the same module share a reference to the module.
                -- Seeing that we know that the instance is V0, and that the module exists, this cannot fail.
                modRef <- fromJust <$> Modules.getModuleReference (GSWasm.miModuleRef nidInterface) mods
                (csHash, initialState) <- freezeContractState nidInitialState
                -- The module version is V0 because of the 'WasmVersion' is V0.
                return $!!
                    ( ca,
                      PersistentInstanceV0
                        Instances.PersistentInstanceV
                            { pinstanceModuleInterface = modRef,
                              pinstanceModel = initialState,
                              pinstanceAmount = nidInitialAmount,
                              pinstanceHash = Instances.makeInstanceHashV0 (pinstanceParameterHash params) csHash nidInitialAmount,
                              ..
                            }
                    )
            Wasm.SV1 -> do
                let params =
                        PersistentInstanceParameters
                            { pinstanceAddress = ca,
                              pinstanceOwner = nidOwner,
                              pinstanceContractModule = GSWasm.miModuleRef nidInterface,
                              pinstanceReceiveFuns = nidEntrypoints,
                              pinstanceInitName = nidInitName,
                              pinstanceParameterHash = Instances.makeInstanceParameterHash ca nidOwner (GSWasm.miModuleRef nidInterface) nidInitName
                            }
                pinstanceParameters <- makeBufferedRef params
                -- We retrieve the module interface here so that we only have a single copy of it, meaning that
                -- all instances created from the same module share a reference to the module.
                -- Seeing that we know that the instance is V1, and that the module exists, this cannot fail.
                modRef <- fromJust <$> Modules.getModuleReference (GSWasm.miModuleRef nidInterface) mods
                (csHash, initialState) <- freezeContractState nidInitialState
                let pinstanceHash = Instances.makeInstanceHashV1 (pinstanceParameterHash params) csHash nidInitialAmount
                -- The module version is V1 because of the 'WasmVersion' is V1.
                return $!!
                    ( ca,
                      PersistentInstanceV1
                        Instances.PersistentInstanceV
                            { pinstanceModuleInterface = modRef,
                              pinstanceModel = initialState,
                              pinstanceAmount = nidInitialAmount,
                              ..
                            }
                    )

doModifyInstance ::
    forall pv m v.
    (SupportsPersistentState pv m, Wasm.IsWasmVersion v) =>
    PersistentBlockState pv ->
    ContractAddress ->
    AmountDelta ->
    Maybe (UpdatableContractState v) ->
    Maybe (GSWasm.ModuleInterfaceA (Modules.PersistentInstrumentedModuleV v), Set.Set Wasm.ReceiveName) ->
    m (PersistentBlockState pv)
doModifyInstance pbs caddr deltaAmnt val newModule = do
    bsp <- loadPBS pbs
    -- Update the instance
    Instances.updateContractInstance (upd bsp) caddr (bspInstances bsp) >>= \case
        Nothing -> error "Invalid contract address"
        Just (_, insts) ->
            storePBS pbs bsp{bspInstances = insts}
  where
    upd :: BlockStatePointers pv -> PersistentInstance pv -> m ((), PersistentInstance pv)
    upd _ (PersistentInstanceV0 oldInst) = case Wasm.getWasmVersion @v of
        Wasm.SV0 -> do
            -- V0 instances cannot be upgraded, so we don't need to do any
            piParams <- loadBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0
                then case val of
                    Nothing -> return ((), PersistentInstanceV0 oldInst)
                    Just newVal -> do
                        (csHash, newModel) <- freezeContractState newVal
                        return
                            ( (),
                              PersistentInstanceV0 $
                                rehashV0
                                    (Just csHash)
                                    (pinstanceParameterHash piParams)
                                    (oldInst{pinstanceModel = newModel})
                            )
                else case val of
                    Nothing ->
                        return
                            ( (),
                              PersistentInstanceV0 $
                                rehashV0
                                    Nothing
                                    (pinstanceParameterHash piParams)
                                    oldInst{pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst)}
                            )
                    Just newVal -> do
                        (csHash, newModel) <- freezeContractState newVal
                        return
                            ( (),
                              PersistentInstanceV0 $
                                rehashV0
                                    (Just csHash)
                                    (pinstanceParameterHash piParams)
                                    oldInst
                                        { pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst),
                                          pinstanceModel = newModel
                                        }
                            )
        Wasm.SV1 -> error "Expected instance version V0, got V1."
    upd bsp (PersistentInstanceV1 oldInst) = case Wasm.getWasmVersion @v of
        Wasm.SV0 -> error "Expected V1 contract instance, got V0."
        Wasm.SV1 -> do
            -- V1 instances can be upgraded to other V1 instances.
            -- First compute whether the parameters and the in-memory module reference need to be updated.
            (piParams, newParamsRef, newModuleInterface) <- do
                (params, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
                case newModule of
                    Nothing -> return (params, newParamsRef, pinstanceModuleInterface oldInst)
                    Just (nm, newEntryPoints) -> do
                        let newParams' =
                                params
                                    { pinstanceContractModule = GSWasm.miModuleRef nm,
                                      pinstanceReceiveFuns = newEntryPoints
                                    }
                            -- compute the new hash of parameters since the module has changed
                            newHash = Instances.makeInstanceParameterHash (pinstanceAddress newParams') (pinstanceOwner newParams') (pinstanceContractModule newParams') (pinstanceInitName newParams')
                            newParams = newParams'{pinstanceParameterHash = newHash}
                        mods <- refLoad (bspModules bsp)
                        newModuleInterface <- fromMaybe (error "Cannot upgrade to a module that does not exist.") <$> (Modules.getModuleReference (GSWasm.miModuleRef nm) mods)
                        br <- makeBufferedRef newParams
                        return (newParams, br, newModuleInterface)
            if deltaAmnt == 0
                then -- there is no change in amount owned by the contract
                case val of
                    -- no change in either the state or the module. No need to change the instance
                    Nothing
                        | Nothing <- newModule -> return ((), PersistentInstanceV1 oldInst)
                        | otherwise ->
                            -- the module is the only thing that was updated, so change parameters, and rehash
                            rehashV1
                                Nothing
                                (pinstanceParameterHash piParams)
                                oldInst
                                    { pinstanceParameters = newParamsRef,
                                      pinstanceModuleInterface = newModuleInterface
                                    }
                    Just newVal -> do
                        -- the state has changed, we need to rehash the instance.
                        -- we also update parameters, but the update might be a no-op if newModel = Nothing,
                        -- since then newParamsRef = pinstanceParameters
                        (csHash, newModel) <- freezeContractState newVal
                        rehashV1
                            (Just csHash)
                            (pinstanceParameterHash piParams)
                            ( oldInst
                                { pinstanceParameters = newParamsRef,
                                  pinstanceModel = newModel,
                                  pinstanceModuleInterface = newModuleInterface
                                }
                            )
                else -- at least the amount has changed rehash in all cases
                case val of
                    Nothing ->
                        rehashV1
                            Nothing
                            (pinstanceParameterHash piParams)
                            oldInst
                                { pinstanceParameters = newParamsRef,
                                  pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst),
                                  pinstanceModuleInterface = newModuleInterface
                                }
                    Just newVal -> do
                        (csHash, newModel) <- freezeContractState newVal
                        rehashV1
                            (Just csHash)
                            (pinstanceParameterHash piParams)
                            oldInst
                                { pinstanceParameters = newParamsRef,
                                  pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst),
                                  pinstanceModel = newModel,
                                  pinstanceModuleInterface = newModuleInterface
                                }
    rehashV0 (Just csHash) iph inst@PersistentInstanceV{..} = inst{pinstanceHash = Instances.makeInstanceHashV0 iph csHash pinstanceAmount}
    rehashV0 Nothing iph inst@PersistentInstanceV{..} =
        inst{pinstanceHash = Instances.makeInstanceHashV0State iph pinstanceModel pinstanceAmount}
    rehashV1 (Just csHash) iph inst@PersistentInstanceV{..} =
        return ((), PersistentInstanceV1 inst{pinstanceHash = Instances.makeInstanceHashV1 iph csHash pinstanceAmount})
    rehashV1 Nothing iph inst@PersistentInstanceV{..} =
        (\newHash -> ((), PersistentInstanceV1 inst{pinstanceHash = newHash})) <$> Instances.makeInstanceHashV1State iph pinstanceModel pinstanceAmount

doGetIdentityProvider :: (SupportsPersistentState pv m) => PersistentBlockState pv -> ID.IdentityProviderIdentity -> m (Maybe IPS.IpInfo)
doGetIdentityProvider pbs ipId = do
    bsp <- loadPBS pbs
    ips <- refLoad (bspIdentityProviders bsp)
    return $! IPS.idProviders ips ^? ix ipId

doGetAllIdentityProvider :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [IPS.IpInfo]
doGetAllIdentityProvider pbs = do
    bsp <- loadPBS pbs
    ips <- refLoad (bspIdentityProviders bsp)
    return $! Map.elems $ IPS.idProviders ips

doGetAnonymityRevokers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> [ID.ArIdentity] -> m (Maybe [ARS.ArInfo])
doGetAnonymityRevokers pbs arIds = do
    bsp <- loadPBS pbs
    ars <- refLoad (bspAnonymityRevokers bsp)
    return $!
        let arsMap = ARS.arRevokers ars
        in  forM arIds (`Map.lookup` arsMap)

doGetAllAnonymityRevokers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [ARS.ArInfo]
doGetAllAnonymityRevokers pbs = do
    bsp <- loadPBS pbs
    ars <- refLoad (bspAnonymityRevokers bsp)
    return $! Map.elems $ ARS.arRevokers ars

doGetCryptoParams :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m CryptographicParameters
doGetCryptoParams pbs = do
    bsp <- loadPBS pbs
    refLoad (bspCryptographicParameters bsp)

doGetPaydayEpoch :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m Epoch
doGetPaydayEpoch pbs = do
    bsp <- loadPBS pbs
    case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> nextPaydayEpoch <$> refLoad hpr

doGetPaydayMintRate :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m MintRate
doGetPaydayMintRate pbs = do
    bsp <- loadPBS pbs
    case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> nextPaydayMintRate <$> refLoad hpr

doSetPaydayEpoch :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> Epoch -> m (PersistentBlockState pv)
doSetPaydayEpoch pbs e = do
    bsp <- loadPBS pbs
    case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> do
            pr <- refLoad hpr
            hpr' <- refMake pr{nextPaydayEpoch = e}
            storePBS pbs bsp{bspRewardDetails = BlockRewardDetailsV1 hpr'}

doSetPaydayMintRate :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> MintRate -> m (PersistentBlockState pv)
doSetPaydayMintRate pbs r = do
    bsp <- loadPBS pbs
    case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> do
            pr <- refLoad hpr
            hpr' <- refMake pr{nextPaydayMintRate = r}
            storePBS pbs bsp{bspRewardDetails = BlockRewardDetailsV1 hpr'}

doGetPoolStatus ::
    forall pv m.
    ( IsProtocolVersion pv,
      SupportsPersistentState pv m,
      PVSupportsDelegation pv
    ) =>
    PersistentBlockState pv ->
    Maybe BakerId ->
    m (Maybe PoolStatus)
doGetPoolStatus pbs Nothing = case delegationChainParameters @pv of
    DelegationChainParameters -> do
        bsp <- loadPBS pbs
        psDelegatedCapital <- passiveDelegationCapital bsp
        psCommissionRates <- _ppPassiveCommissions . _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
        poolRewards <- refLoad (bspPoolRewards bsp)
        let psCurrentPaydayTransactionFeesEarned = passiveDelegationTransactionRewards poolRewards
        psCurrentPaydayDelegatedCapital <- currentPassiveDelegationCapital poolRewards
        psAllPoolTotalCapital <- totalCapital bsp
        return $ Just PassiveDelegationStatus{..}
doGetPoolStatus pbs (Just psBakerId@(BakerId aid)) = case delegationChainParameters @pv of
    DelegationChainParameters -> do
        bsp <- loadPBS pbs
        Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
            Nothing -> return Nothing
            Just acct ->
                accountBaker acct >>= \case
                    Nothing -> return Nothing
                    Just baker -> do
                        let psBakerEquityCapital = baker ^. BaseAccounts.stakedAmount
                        psDelegatedCapital <- poolDelegatorCapital bsp psBakerId
                        poolParameters <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
                        psAllPoolTotalCapital <- totalCapital bsp
                        let psDelegatedCapitalCap =
                                delegatedCapitalCap
                                    poolParameters
                                    psAllPoolTotalCapital
                                    psBakerEquityCapital
                                    psDelegatedCapital
                        psBakerAddress <- accountCanonicalAddress acct
                        let psPoolInfo = baker ^. BaseAccounts.bakerPoolInfo
                        let psBakerStakePendingChange =
                                makePoolPendingChange $ BaseAccounts.pendingChangeEffectiveTimestamp <$> (baker ^. BaseAccounts.bakerPendingChange)
                        epochBakers <- refLoad (_birkCurrentEpochBakers $ bspBirkParameters bsp)
                        mepochBaker <- epochBaker psBakerId epochBakers
                        psCurrentPaydayStatus <- case mepochBaker of
                            Nothing -> return Nothing
                            Just (_, effectiveStake) -> do
                                poolRewards <- refLoad (bspPoolRewards bsp)
                                mbcr <- lookupBakerCapitalAndRewardDetails psBakerId poolRewards
                                case mbcr of
                                    Nothing -> return Nothing -- This should not happen
                                    Just (bc, BakerPoolRewardDetails{..}) -> do
                                        return $
                                            Just
                                                CurrentPaydayBakerPoolStatus
                                                    { bpsBlocksBaked = blockCount,
                                                      bpsFinalizationLive = finalizationAwake,
                                                      bpsTransactionFeesEarned = transactionFeesAccrued,
                                                      bpsEffectiveStake = effectiveStake,
                                                      bpsLotteryPower =
                                                        fromIntegral effectiveStake
                                                            / fromIntegral (_bakerTotalStake epochBakers),
                                                      bpsBakerEquityCapital = bcBakerEquityCapital bc,
                                                      bpsDelegatedCapital = bcTotalDelegatorCapital bc
                                                    }
                        return $ Just BakerPoolStatus{..}

doGetTransactionOutcome :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> Transactions.TransactionIndex -> m (Maybe TransactionSummary)
doGetTransactionOutcome pbs transHash = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return $! bto ^? ix transHash
        PTOV1 bto -> do
            fmap _transactionSummaryV1 <$> LFMBT.lookup transHash (mtoOutcomes bto)

doGetTransactionOutcomesHash :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> m Transactions.TransactionOutcomesHash
doGetTransactionOutcomesHash pbs = do
    bsp <- loadPBS pbs
    getHashM (bspTransactionOutcomes bsp)

doSetTransactionOutcomes :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> [TransactionSummary] -> m (PersistentBlockState pv)
doSetTransactionOutcomes pbs transList = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 _ ->
            storePBS pbs bsp{bspTransactionOutcomes = PTOV0 (Transactions.transactionOutcomesV0FromList transList)}
        PTOV1 _ -> do
            mtoOutcomes <- LFMBT.fromAscList . map TransactionSummaryV1 $ transList
            let mtoSpecials = LFMBT.empty
            storePBS pbs bsp{bspTransactionOutcomes = PTOV1 MerkleTransactionOutcomes{..}}

doNotifyEncryptedBalanceChange :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doNotifyEncryptedBalanceChange pbs amntDiff = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff}

doGetSpecialOutcomes :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> m (Seq.Seq Transactions.SpecialTransactionOutcome)
doGetSpecialOutcomes pbs = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return (bto ^. Transactions.outcomeSpecial)
        PTOV1 bto -> Seq.fromList <$> LFMBT.toAscList (mtoSpecials bto)

doGetOutcomes :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> m (Vec.Vector TransactionSummary)
doGetOutcomes pbs = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return (Transactions.outcomeValues bto)
        PTOV1 bto -> Vec.fromList . map _transactionSummaryV1 <$> LFMBT.toAscList (mtoOutcomes bto)

doAddSpecialTransactionOutcome :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> Transactions.SpecialTransactionOutcome -> m (PersistentBlockState pv)
doAddSpecialTransactionOutcome pbs !o = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto ->
            storePBS pbs $! bsp{bspTransactionOutcomes = PTOV0 (bto & Transactions.outcomeSpecial %~ (Seq.|> o))}
        PTOV1 bto -> do
            (_, newSpecials) <- LFMBT.append o (mtoSpecials bto)
            storePBS pbs $! bsp{bspTransactionOutcomes = PTOV1 (bto{mtoSpecials = newSpecials})}

doGetElectionDifficulty ::
    ( SupportsPersistentState pv m,
      ConsensusParametersVersionFor (ChainParametersVersionFor pv) ~ 'ConsensusParametersVersion0
    ) =>
    PersistentBlockState pv ->
    Timestamp ->
    m ElectionDifficulty
doGetElectionDifficulty pbs ts = do
    bsp <- loadPBS pbs
    futureElectionDifficulty (bspUpdates bsp) ts

doGetNextUpdateSequenceNumber :: (SupportsPersistentState pv m) => PersistentBlockState pv -> UpdateType -> m UpdateSequenceNumber
doGetNextUpdateSequenceNumber pbs uty = do
    bsp <- loadPBS pbs
    lookupNextUpdateSequenceNumber (bspUpdates bsp) uty

doGetCurrentElectionDifficulty ::
    ( SupportsPersistentState pv m,
      ConsensusParametersVersionFor
        (ChainParametersVersionFor pv)
        ~ 'ConsensusParametersVersion0
    ) =>
    PersistentBlockState pv ->
    m ElectionDifficulty
doGetCurrentElectionDifficulty pbs = do
    bsp <- loadPBS pbs
    upds <- refLoad (bspUpdates bsp)
    _cpElectionDifficulty . _cpConsensusParameters . unStoreSerialized <$> refLoad (currentParameters upds)

doGetUpdates :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (UQ.Updates pv)
doGetUpdates = makeBasicUpdates <=< refLoad . bspUpdates <=< loadPBS

doGetProtocolUpdateStatus :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m UQ.ProtocolUpdateStatus
doGetProtocolUpdateStatus = protocolUpdateStatus . bspUpdates <=< loadPBS

doProcessUpdateQueues ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    Timestamp ->
    m (Map.Map TransactionTime (UpdateValue (ChainParametersVersionFor pv)), PersistentBlockState pv)
doProcessUpdateQueues pbs ts = do
    bsp <- loadPBS pbs
    let (u, ars, ips) = (bspUpdates bsp, bspAnonymityRevokers bsp, bspIdentityProviders bsp)
    (changes, (u', ars', ips')) <- processUpdateQueues ts (u, ars, ips)
    (changes,) <$> storePBS pbs bsp{bspUpdates = u', bspAnonymityRevokers = ars', bspIdentityProviders = ips'}

doProcessReleaseSchedule :: forall m pv. (SupportsPersistentState pv m) => PersistentBlockState pv -> Timestamp -> m (PersistentBlockState pv)
doProcessReleaseSchedule pbs ts = do
    bsp <- loadPBS pbs
    (affectedAccounts, remRS) <- processReleasesUntil ts (bspReleaseSchedule bsp)
    if null affectedAccounts
        then return pbs
        else do
            let processAccountP1 ::
                    (RSAccountRef pv ~ AccountAddress) =>
                    (Accounts.Accounts pv, ReleaseSchedule pv) ->
                    RSAccountRef pv ->
                    m (Accounts.Accounts pv, ReleaseSchedule pv)
                processAccountP1 (accs, rs) addr = do
                    (reAdd, accs') <- Accounts.updateAccounts (unlockAccountReleases ts) addr accs
                    rs' <- case reAdd of
                        Just (_, Just nextTS) -> addAccountRelease nextTS addr rs
                        Just (_, Nothing) -> return rs
                        Nothing -> error "processReleaseSchedule: scheduled release for invalid account address"
                    return (accs', rs')
                processAccountP5 ::
                    (RSAccountRef pv ~ AccountIndex) =>
                    (Accounts.Accounts pv, ReleaseSchedule pv) ->
                    RSAccountRef pv ->
                    m (Accounts.Accounts pv, ReleaseSchedule pv)
                processAccountP5 (accs, rs) ai = do
                    (reAdd, accs') <- Accounts.updateAccountsAtIndex (unlockAccountReleases ts) ai accs
                    rs' <- case reAdd of
                        Just (Just nextTS) -> addAccountRelease nextTS ai rs
                        Just Nothing -> return rs
                        Nothing -> error "processReleaseSchedule: scheduled release for invalid account index"
                    return (accs', rs')
                processAccount :: (Accounts.Accounts pv, ReleaseSchedule pv) -> RSAccountRef pv -> m (Accounts.Accounts pv, ReleaseSchedule pv)
                processAccount = case protocolVersion @pv of
                    SP1 -> processAccountP1
                    SP2 -> processAccountP1
                    SP3 -> processAccountP1
                    SP4 -> processAccountP1
                    SP5 -> processAccountP5
                    SP6 -> processAccountP5
            (newAccs, newRS) <- foldM processAccount (bspAccounts bsp, remRS) affectedAccounts
            storePBS pbs (bsp{bspAccounts = newAccs, bspReleaseSchedule = newRS})

doGetUpdateKeyCollection ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m (UpdateKeysCollection (AuthorizationsVersionForPV pv))
doGetUpdateKeyCollection pbs = do
    bsp <- loadPBS pbs
    u <- refLoad (bspUpdates bsp)
    withIsAuthorizationsVersionForPV (protocolVersion @pv) $
        unStoreSerialized <$> refLoad (currentKeyCollection u)

doEnqueueUpdate ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    TransactionTime ->
    UpdateValue (ChainParametersVersionFor pv) ->
    m (PersistentBlockState pv)
doEnqueueUpdate pbs effectiveTime payload = do
    bsp <- loadPBS pbs
    u' <- enqueueUpdate effectiveTime payload (bspUpdates bsp)
    storePBS pbs bsp{bspUpdates = u'}

doOverwriteElectionDifficulty ::
    ( SupportsPersistentState pv m,
      ConsensusParametersVersionFor
        (ChainParametersVersionFor pv)
        ~ 'ConsensusParametersVersion0
    ) =>
    PersistentBlockState pv ->
    ElectionDifficulty ->
    m (PersistentBlockState pv)
doOverwriteElectionDifficulty pbs newElectionDifficulty = do
    bsp <- loadPBS pbs
    u' <- overwriteElectionDifficulty newElectionDifficulty (bspUpdates bsp)
    storePBS pbs bsp{bspUpdates = u'}

doClearProtocolUpdate :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (PersistentBlockState pv)
doClearProtocolUpdate pbs = do
    bsp <- loadPBS pbs
    u' <- clearProtocolUpdate (bspUpdates bsp)
    storePBS pbs bsp{bspUpdates = u'}

doSetNextCapitalDistribution ::
    forall pv m.
    (SupportsPersistentState pv m, PVSupportsDelegation pv) =>
    PersistentBlockState pv ->
    CapitalDistribution ->
    m (PersistentBlockState pv)
doSetNextCapitalDistribution pbs cd = do
    bsp <- loadPBS pbs
    capDist <- refMake cd
    newRewardDetails <- case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> do
            pr <- refLoad hpr
            BlockRewardDetailsV1 <$> refMake (pr{nextCapital = capDist})
    storePBS pbs bsp{bspRewardDetails = newRewardDetails}

doRotateCurrentCapitalDistribution ::
    (SupportsPersistentState pv m, PVSupportsDelegation pv) =>
    PersistentBlockState pv ->
    m (PersistentBlockState pv)
doRotateCurrentCapitalDistribution pbs = do
    bsp <- loadPBS pbs
    newRewardDetails <- case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> BlockRewardDetailsV1 <$> rotateCapitalDistribution hpr
    storePBS pbs bsp{bspRewardDetails = newRewardDetails}

doGetExchangeRates :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m ExchangeRates
doGetExchangeRates pbs = do
    bsp <- loadPBS pbs
    lookupExchangeRates (bspUpdates bsp)

doGetChainParameters :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (ChainParameters pv)
doGetChainParameters pbs = do
    bsp <- loadPBS pbs
    lookupCurrentParameters (bspUpdates bsp)

doGetPendingTimeParameters :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [(TransactionTime, TimeParameters)]
doGetPendingTimeParameters pbs = do
    bsp <- loadPBS pbs
    lookupPendingTimeParameters (bspUpdates bsp)

doGetPendingPoolParameters :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m [(TransactionTime, PoolParameters (ChainParametersVersionFor pv))]
doGetPendingPoolParameters pbs = do
    bsp <- loadPBS pbs
    lookupPendingPoolParameters (bspUpdates bsp)

doGetEpochBlocksBaked :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (Word64, [(BakerId, Word64)])
doGetEpochBlocksBaked pbs = do
    bsp <- loadPBS pbs
    case bspRewardDetails bsp of
        BlockRewardDetailsV0 heb ->
            accumBakersFromEpochBlocks (hebBlocks heb) 0 Map.empty
        BlockRewardDetailsV1 hpr -> do
            pr <- refLoad hpr
            bcs <- bakerBlockCounts pr
            return (sum (snd <$> bcs), bcs)
  where
    accumBakersFromEpochBlocks Null t m = return (t, Map.toList m)
    accumBakersFromEpochBlocks (Some ref) t m = do
        EpochBlock{..} <- refLoad ref
        let !t' = t + 1
            !m' = m & at ebBakerId . non 0 +~ 1
        accumBakersFromEpochBlocks ebPrevious t' m'

-- |This function updates the baker pool rewards details of a baker. It is a precondition that
-- the given baker is active.
modifyBakerPoolRewardDetailsInPoolRewards :: (SupportsPersistentAccount pv m, PVSupportsDelegation pv) => BlockStatePointers pv -> BakerId -> (BakerPoolRewardDetails -> BakerPoolRewardDetails) -> m (BlockStatePointers pv)
modifyBakerPoolRewardDetailsInPoolRewards bsp bid f = do
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    let bprs = bakerPoolRewardDetails pr
    bpc <- bakerPoolCapital <$> refLoad (currentCapital pr)
    case binarySearchI bcBakerId bpc bid of
        Nothing ->
            error "Invariant violation: unable to find baker in baker pool capital vector"
        Just (i, _) -> do
            newBPRs <- updateBPRs i bprs
            newBlockRewardDetails <- BlockRewardDetailsV1 <$> refMake pr{bakerPoolRewardDetails = newBPRs}
            return bsp{bspRewardDetails = newBlockRewardDetails}
  where
    updateBPRs i bprs = do
        mBPRs <- LFMBT.update (return . ((),) . f) (fromIntegral i) bprs
        case mBPRs of
            Nothing ->
                error "Invariant violation: unable to find baker in baker pool reward details tree"
            Just ((), newBPRs) ->
                return newBPRs

doNotifyBlockBaked :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> BakerId -> m (PersistentBlockState pv)
doNotifyBlockBaked pbs bid = do
    bsp <- loadPBS pbs
    case delegationSupport @(AccountVersionFor pv) of
        SAVDelegationNotSupported -> do
            newBlockRewardDetails <- consBlockRewardDetails bid (bspRewardDetails bsp)
            storePBS pbs bsp{bspRewardDetails = newBlockRewardDetails}
        SAVDelegationSupported ->
            let incBPR bpr = bpr{blockCount = blockCount bpr + 1}
            in  storePBS pbs =<< modifyBakerPoolRewardDetailsInPoolRewards bsp bid incBPR

doUpdateAccruedTransactionFeesBaker :: forall pv m. (PVSupportsDelegation pv, SupportsPersistentState pv m) => PersistentBlockState pv -> BakerId -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesBaker pbs bid delta = do
    bsp <- loadPBS pbs
    let accrueAmountBPR bpr = bpr{transactionFeesAccrued = applyAmountDelta delta (transactionFeesAccrued bpr)}
    storePBS pbs =<< modifyBakerPoolRewardDetailsInPoolRewards bsp bid accrueAmountBPR

doMarkFinalizationAwakeBakers :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> [BakerId] -> m (PersistentBlockState pv)
doMarkFinalizationAwakeBakers pbs bids = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    let bprs = bakerPoolRewardDetails pr
    bpc <- bakerPoolCapital <$> refLoad (currentCapital pr)
    newBPRs <- foldM (markFinalizerAwake bpc) bprs bids
    newBlockRewardDetails <- BlockRewardDetailsV1 <$> refMake pr{bakerPoolRewardDetails = newBPRs}
    newHash :: (Rewards.BlockRewardDetailsHash (AccountVersionFor pv)) <-
        getHashM newBlockRewardDetails
    oldHash <- getHashM (bspRewardDetails bsp)
    if newHash == oldHash
        then return pbs
        else storePBS pbs bsp{bspRewardDetails = newBlockRewardDetails}
  where
    markFinalizerAwake bpc bprs bid = do
        case binarySearchI bcBakerId bpc bid of
            Nothing -> return bprs
            Just (i, _) -> do
                mBPRs <- LFMBT.update setAwake (fromIntegral i) bprs
                case mBPRs of
                    Nothing ->
                        error "Invariant violation: unable to find baker in baker pool reward details tree"
                    Just ((), newBPRs) ->
                        return newBPRs
    setAwake bpr = return ((), bpr{finalizationAwake = True})

doUpdateAccruedTransactionFeesPassive :: forall pv m. (PVSupportsDelegation pv, SupportsPersistentState pv m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesPassive pbs delta = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    newBlockRewardDetails <-
        BlockRewardDetailsV1
            <$> refMake
                pr
                    { passiveDelegationTransactionRewards = applyAmountDelta delta (passiveDelegationTransactionRewards pr)
                    }
    storePBS pbs $ bsp{bspRewardDetails = newBlockRewardDetails}

doGetAccruedTransactionFeesPassive :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m Amount
doGetAccruedTransactionFeesPassive pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    passiveDelegationTransactionRewards <$> refLoad hpr

doUpdateAccruedTransactionFeesFoundationAccount :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesFoundationAccount pbs delta = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    newBlockRewardDetails <-
        BlockRewardDetailsV1
            <$> refMake
                pr
                    { foundationTransactionRewards = applyAmountDelta delta (foundationTransactionRewards pr)
                    }
    storePBS pbs $ bsp{bspRewardDetails = newBlockRewardDetails}

doGetAccruedTransactionFeesFoundationAccount :: forall pv m. (SupportsPersistentState pv m, PVSupportsDelegation pv) => PersistentBlockState pv -> m Amount
doGetAccruedTransactionFeesFoundationAccount pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    foundationTransactionRewards <$> refLoad hpr

doClearEpochBlocksBaked :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (PersistentBlockState pv)
doClearEpochBlocksBaked pbs = do
    bsp <- loadPBS pbs
    rewardDetails <- emptyBlockRewardDetails
    storePBS pbs bsp{bspRewardDetails = rewardDetails}

doRotateCurrentEpochBakers ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m (PersistentBlockState pv)
doRotateCurrentEpochBakers pbs = do
    bsp <- loadPBS pbs
    let oldBirkParams = bspBirkParameters bsp
        newBirkParams = oldBirkParams & birkCurrentEpochBakers .~ (oldBirkParams ^. birkNextEpochBakers)
    storePBS pbs bsp{bspBirkParameters = newBirkParams}

doSetNextEpochBakers ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    [(PersistentBakerInfoRef (AccountVersionFor pv), Amount)] ->
    m (PersistentBlockState pv)
doSetNextEpochBakers pbs bakers = do
    bsp <- loadPBS pbs
    _bakerInfos <- refMake (BakerInfos preBakerInfos)
    _bakerStakes <- refMake (BakerStakes preBakerStakes)
    let _bakerTotalStake = sum preBakerStakes
    pebRef <- refMake PersistentEpochBakers{..}
    let newBirkParams = (bspBirkParameters bsp){_birkNextEpochBakers = pebRef}
    storePBS pbs bsp{bspBirkParameters = newBirkParams}
  where
    bakers' = Vec.fromList bakers
    preBakerInfos = fst <$> bakers'
    preBakerStakes = snd <$> bakers'

doProcessPendingChanges ::
    forall pv m.
    (SupportsPersistentState pv m, PVSupportsDelegation pv) =>
    PersistentBlockState pv ->
    -- |Guard determining if a change is effective
    (Timestamp -> Bool) ->
    m (PersistentBlockState pv)
doProcessPendingChanges persistentBS isEffective = do
    bsp <- loadPBS persistentBS
    newBSP <- processPendingChanges bsp
    storePBS persistentBS newBSP
  where
    processPendingChanges bsp0 = do
        pab0 <- refLoad (bsp0 ^. birkParameters . birkActiveBakers)
        (ps1, a1) <- modifyPassiveDelegation (pab0, bspAccounts bsp0)
        ((pab2, accts2), a2) <- modifyBakers ps1
        -- Update the total active capital to the sum of the new passive delegators and bakers.
        newAB <- refMake $! (pab2 & totalActiveCapital .~ TotalActiveCapitalV1 (a1 + a2))
        return $ bsp0{bspAccounts = accts2} & (birkParameters . birkActiveBakers .~ newAB)

    -- Process the passive delegators, handling any cooldowns.  This updates the active
    -- bakers index, as well as the accounts themselves.
    -- The returned amount is the total capital now staked by the passive delegators
    -- (before any other delegators have been moved to passive delegation).
    modifyPassiveDelegation (pab, accts) = do
        (pab', accts') <- MTL.runStateT (passiveDelegators processDelegators pab) accts
        return ((pab', accts'), adDelegatorTotalCapital (pab' ^. passiveDelegators))

    -- Process the baker pools, handling any cooldowns.  This updates the active
    -- bakers index, as well as the accounts themselves.
    -- The returned amount is the total capital now delegated by the processed bakers
    -- and their delegators.
    modifyBakers (pab, accts) = do
        ((newBakers, total), (accts', newAggs, newPassive)) <-
            MTL.runStateT
                (processBakers (pab ^. activeBakers))
                (accts, pab ^. aggregationKeys, pab ^. passiveDelegators)
        let pab' =
                pab
                    & activeBakers .~ newBakers
                    & aggregationKeys .~ newAggs
                    & passiveDelegators .~ newPassive
        return ((pab', accts'), total)

    -- Process a set of delegators for elapsed cooldowns, updating the total delegated amount
    -- in the process. This does not update the active bakers, but should be used to modify
    -- an entry for a particular pool.
    processDelegators ::
        PersistentActiveDelegators (AccountVersionFor pv) ->
        MTL.StateT (Accounts.Accounts pv) m (PersistentActiveDelegators (AccountVersionFor pv))
    processDelegators (PersistentActiveDelegatorsV1 dset _) = do
        (newDlgs, newAmt) <- MTL.runWriterT $ Trie.filterKeysM processDelegator dset
        return (PersistentActiveDelegatorsV1 newDlgs newAmt)

    -- Update the delegator on an account if its cooldown has expired.
    -- This only updates the account table, and not the active bakers index.
    -- This also 'MTL.tell's the (updated) staked amount of the account.
    processDelegator :: DelegatorId -> MTL.WriterT Amount (MTL.StateT (Accounts.Accounts pv) m) Bool
    processDelegator (DelegatorId accId) = do
        accounts <- MTL.get
        Accounts.indexedAccount accId accounts >>= \case
            Just acct -> updateAccountDelegator accId acct
            Nothing -> error "Invariant violation: active delegator account was not found"

    -- Update the delegator on a given account if its cooldown has expired.
    -- This only updates the account table, and not the active bakers index.
    -- This also 'MTL.tell's the (updated) staked amount of the account.
    -- The boolean return value indicates if the delegator is still a delegator.
    updateAccountDelegator ::
        AccountIndex ->
        PersistentAccount (AccountVersionFor pv) ->
        MTL.WriterT Amount (MTL.StateT (Accounts.Accounts pv) m) Bool
    updateAccountDelegator accId acct =
        accountDelegator acct >>= \case
            Just BaseAccounts.AccountDelegationV1{..} -> do
                case BaseAccounts.pendingChangeEffectiveTimestamp <$> _delegationPendingChange of
                    BaseAccounts.RemoveStake pet | isEffective pet -> do
                        lift $ removeDelegatorStake accId
                        return False
                    BaseAccounts.ReduceStake newAmt pet | isEffective pet -> do
                        MTL.tell newAmt
                        lift $ reduceDelegatorStake accId newAmt
                        return True
                    _ -> do
                        -- No change to the stake
                        MTL.tell _delegationStakedAmount
                        return True
            Nothing ->
                error "Invariant violation: active delegator is not a delegation account"

    -- Remove a delegator from an account.
    -- This only affects the account, and does not affect the active bakers index.
    removeDelegatorStake :: AccountIndex -> MTL.StateT (Accounts.Accounts pv) m ()
    removeDelegatorStake accId = do
        accounts <- MTL.get
        newAccounts <- Accounts.updateAccountsAtIndex' removeAccountStaking accId accounts
        MTL.put newAccounts

    -- Reduce the stake of a delegator account to the new amount, given the current delegation.
    -- This also removes the pending change on the account.
    -- This only affects the account, and does not affect the active bakers index.
    reduceDelegatorStake ::
        AccountIndex ->
        Amount ->
        MTL.StateT (Accounts.Accounts pv) m ()
    reduceDelegatorStake accId newAmt = do
        accounts <- MTL.get
        let updAcc = setAccountStake newAmt >=> setAccountStakePendingChange BaseAccounts.NoChange
        newAccounts <- Accounts.updateAccountsAtIndex' updAcc accId accounts
        MTL.put newAccounts

    -- Traverse over the active baker index, processing bakers and delegators that have elapsed cooldowns.
    -- Changes to the account table, aggregation keys set, and passive delegation set are
    -- applied to the state.
    -- The new total capital staked by the bakers and their original delegators is returned.
    -- (Note that stakes may have been reduced or removed, or moved to passive delegation.)
    processBakers ::
        BakerIdTrieMap (AccountVersionFor pv) ->
        MTL.StateT
            (Accounts.Accounts pv, AggregationKeySet, PersistentActiveDelegators (AccountVersionFor pv))
            m
            (BakerIdTrieMap (AccountVersionFor pv), Amount)
    processBakers = MTL.runWriterT . Trie.alterMapM processBaker

    -- Process a baker's entry in the active baker pools table, updating the account table,
    -- aggregation key set, and passive delegators accordingly.
    -- If a baker's cooldown is elapsed, its stake is reduced or the pool is removed.
    -- The new delegated amount of the baker and its delegators is accumulated.
    -- The return value indicates how the active baker pool table should be updated.
    processBaker ::
        BakerId ->
        PersistentActiveDelegators (AccountVersionFor pv) ->
        MTL.WriterT
            Amount
            (MTL.StateT (Accounts.Accounts pv, AggregationKeySet, PersistentActiveDelegators (AccountVersionFor pv)) m)
            (Trie.Alteration (PersistentActiveDelegators (AccountVersionFor pv)))
    processBaker bid@(BakerId accId) oldDelegators = do
        accts0 <- use _1
        (newDelegators, accts1) <- lift $ lift $ MTL.runStateT (processDelegators oldDelegators) accts0
        _1 .=! accts1
        MTL.tell (adDelegatorTotalCapital newDelegators)
        let trieInsert
                | adDelegatorTotalCapital oldDelegators /= adDelegatorTotalCapital newDelegators =
                    return (Trie.Insert newDelegators)
                | otherwise = do
                    oldKeys <- Trie.keys (adDelegators oldDelegators)
                    newKeys <- Trie.keys (adDelegators newDelegators)
                    if newKeys == oldKeys
                        then return Trie.NoChange
                        else return (Trie.Insert newDelegators)
        Accounts.indexedAccount accId accts1 >>= \case
            Just acct ->
                accountStakeDetails acct >>= \case
                    StakeDetailsBaker{..} -> do
                        case BaseAccounts.pendingChangeEffectiveTimestamp <$> sdPendingChange of
                            BaseAccounts.RemoveStake pet | isEffective pet -> do
                                -- This will not fail, since we know the account is a baker
                                acctBkr <- fromJust <$> accountBaker acct
                                lift $ removeBaker bid acctBkr newDelegators
                                return Trie.Remove
                            BaseAccounts.ReduceStake newAmt pet | isEffective pet -> do
                                MTL.tell newAmt
                                lift $ reduceBakerStake bid newAmt
                                trieInsert
                            _ -> do
                                MTL.tell sdStakedCapital
                                trieInsert
                    _ ->
                        error "Persistent.bsoProcessPendingChanges invariant violation: active baker account not a baker"
            Nothing ->
                error "Persistent.bsoProcessPendingChanges invariant violation: active baker account not valid"

    -- Remove the baker, transferring its delegators to passive delegation.
    -- The three components of the state are updated as follows:
    -- 1. The accounts table is updated to reflect the change both for the account and delegators.
    -- 2. The baker's aggregation key is removed from the aggregation key set.
    -- 3. The delegators are added to the passive delegators.
    removeBaker ::
        BakerId ->
        AccountBaker av ->
        PersistentActiveDelegators (AccountVersionFor pv) ->
        MTL.StateT (Accounts.Accounts pv, AggregationKeySet, PersistentActiveDelegators (AccountVersionFor pv)) m ()
    removeBaker (BakerId accId) acctBkr (PersistentActiveDelegatorsV1 dset dcapital) = do
        accounts0 <- use _1
        -- Update the baker's account to have no delegation
        accounts1 <- Accounts.updateAccountsAtIndex' removeAccountStaking accId accounts0
        -- Update the delegators' accounts to delegate to passive
        dlist <- Trie.keysAsc dset
        accounts2 <- foldM redelegatePassive accounts1 dlist
        _1 .=! accounts2

        -- Remove the baker's aggregation key from the aggregation keys set
        (_2 .=!) =<< Trie.delete (acctBkr ^. BaseAccounts.bakerAggregationVerifyKey) =<< use _2

        -- Add the delegators to the passive delegators
        oldPAD <- use _3
        newDset <- foldM (\t d -> Trie.insert d () t) (adDelegators oldPAD) dlist
        _3 .=! PersistentActiveDelegatorsV1 newDset (adDelegatorTotalCapital oldPAD + dcapital)

    -- Reduce the baker's stake, making the update to the account table.
    reduceBakerStake ::
        BakerId ->
        Amount ->
        MTL.StateT (Accounts.Accounts pv, a, b) m ()
    reduceBakerStake (BakerId accId) newAmt = do
        let updAcc = setAccountStake newAmt >=> setAccountStakePendingChange BaseAccounts.NoChange
        accounts <- use _1
        newAccounts <- lift $ Accounts.updateAccountsAtIndex' updAcc accId accounts
        _1 .=! newAccounts

doGetBankStatus :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m Rewards.BankStatus
doGetBankStatus pbs = _unhashed . bspBank <$> loadPBS pbs

doSetRewardAccounts :: (SupportsPersistentState pv m) => PersistentBlockState pv -> Rewards.RewardAccounts -> m (PersistentBlockState pv)
doSetRewardAccounts pbs rewards = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.rewardAccounts .~ rewards}

-- |Context that supports the persistent block state.
data PersistentBlockStateContext pv = PersistentBlockStateContext
    { -- |The 'BlobStore' used for storing the persistent state.
      pbscBlobStore :: !BlobStore,
      -- |Cache used for caching accounts.
      pbscAccountCache :: !(AccountCache (AccountVersionFor pv)),
      -- |Cache used for caching modules.
      pbscModuleCache :: !Modules.ModuleCache
    }

instance HasBlobStore (PersistentBlockStateContext av) where
    blobStore = bscBlobStore . pbscBlobStore
    blobLoadCallback = bscLoadCallback . pbscBlobStore
    blobStoreCallback = bscStoreCallback . pbscBlobStore

instance AccountVersionFor pv ~ av => Cache.HasCache (AccountCache av) (PersistentBlockStateContext pv) where
    projectCache = pbscAccountCache

instance Cache.HasCache Modules.ModuleCache (PersistentBlockStateContext pv) where
    projectCache = pbscModuleCache

instance IsProtocolVersion pv => MonadProtocolVersion (BlobStoreT (PersistentBlockStateContext pv) m) where
    type MPV (BlobStoreT (PersistentBlockStateContext pv) m) = pv

-- |Create a new account cache of the specified size for running the given monadic operation by
-- extending the 'BlobStore' context to a 'PersistentBlockStateContext'.
withNewAccountCache :: (MonadIO m) => Int -> BlobStoreT (PersistentBlockStateContext pv) m a -> BlobStoreT BlobStore m a
withNewAccountCache size bsm = do
    ac <- liftIO $ newAccountCache size
    mc <- liftIO $ Modules.newModuleCache 100
    alterBlobStoreT (\bs -> PersistentBlockStateContext bs ac mc) bsm

newtype PersistentBlockStateMonad (pv :: ProtocolVersion) (r :: Type) (m :: Type -> Type) (a :: Type) = PersistentBlockStateMonad {runPersistentBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadLogger, TimeMonad, MTL.MonadState s)

type PersistentState av pv r m =
    ( MonadIO m,
      MonadReader r m,
      HasBlobStore r,
      AccountVersionFor pv ~ av,
      Cache.HasCache (AccountCache av) r,
      Cache.HasCache Modules.ModuleCache r
    )

instance PersistentState av pv r m => MonadBlobStore (PersistentBlockStateMonad pv r m)
instance PersistentState av pv r m => MonadBlobStore (PutT (PersistentBlockStateMonad pv r m))
instance PersistentState av pv r m => MonadBlobStore (PutH (PersistentBlockStateMonad pv r m))

instance PersistentState av pv r m => Cache.MonadCache (AccountCache av) (PersistentBlockStateMonad pv r m)
instance PersistentState av pv r m => Cache.MonadCache Modules.ModuleCache (PersistentBlockStateMonad pv r m)

type instance BlockStatePointer (PersistentBlockState pv) = BlobRef (BlockStatePointers pv)
type instance BlockStatePointer (HashedPersistentBlockState pv) = BlobRef (BlockStatePointers pv)

instance (IsProtocolVersion pv) => MonadProtocolVersion (PersistentBlockStateMonad pv r m) where
    type MPV (PersistentBlockStateMonad pv r m) = pv

instance BlockStateTypes (PersistentBlockStateMonad pv r m) where
    type BlockState (PersistentBlockStateMonad pv r m) = HashedPersistentBlockState pv
    type UpdatableBlockState (PersistentBlockStateMonad pv r m) = PersistentBlockState pv
    type Account (PersistentBlockStateMonad pv r m) = PersistentAccount (AccountVersionFor pv)
    type BakerInfoRef (PersistentBlockStateMonad pv r m) = PersistentBakerInfoRef (AccountVersionFor pv)
    type ContractState (PersistentBlockStateMonad pv r m) = Instances.InstanceStateV
    type InstrumentedModuleRef (PersistentBlockStateMonad pv r m) = Modules.PersistentInstrumentedModuleV

instance (PersistentState av pv r m) => ModuleQuery (PersistentBlockStateMonad pv r m) where
    getModuleArtifact = doGetModuleArtifact

instance (IsProtocolVersion pv, PersistentState av pv r m) => BlockStateQuery (PersistentBlockStateMonad pv r m) where
    getModule = doGetModuleSource . hpbsPointers
    getModuleInterface pbs mref = doGetModule (hpbsPointers pbs) mref
    getAccount = doGetAccount . hpbsPointers
    accountExists = doGetAccountExists . hpbsPointers
    getActiveBakers = doGetActiveBakers . hpbsPointers
    getActiveBakersAndDelegators = doGetActiveBakersAndDelegators . hpbsPointers
    getActiveDelegators = doGetActiveDelegators . hpbsPointers
    getCurrentDelegators = doGetCurrentDelegators . hpbsPointers
    getAccountByCredId = doGetAccountByCredId . hpbsPointers
    getAccountByIndex = doGetIndexedAccountByIndex . hpbsPointers
    getContractInstance = doGetInstance . hpbsPointers
    getModuleList = doGetModuleList . hpbsPointers
    getAccountList = doAccountList . hpbsPointers
    getContractInstanceList = doContractInstanceList . hpbsPointers
    getSeedState = doGetSeedState . hpbsPointers
    getCurrentEpochBakers = doGetCurrentEpochBakers . hpbsPointers
    getNextEpochBakers = doGetNextEpochBakers . hpbsPointers
    getSlotBakersP1 = doGetSlotBakersP1 . hpbsPointers
    getBakerAccount = doGetBakerAccount . hpbsPointers
    getRewardStatus = doGetRewardStatus . hpbsPointers
    getTransactionOutcome = doGetTransactionOutcome . hpbsPointers
    getTransactionOutcomesHash = doGetTransactionOutcomesHash . hpbsPointers
    getStateHash = return . hpbsHash
    getSpecialOutcomes = doGetSpecialOutcomes . hpbsPointers
    getOutcomes = doGetOutcomes . hpbsPointers
    getAllIdentityProviders = doGetAllIdentityProvider . hpbsPointers
    getAllAnonymityRevokers = doGetAllAnonymityRevokers . hpbsPointers
    getElectionDifficulty = doGetElectionDifficulty . hpbsPointers
    getNextUpdateSequenceNumber = doGetNextUpdateSequenceNumber . hpbsPointers
    getCurrentElectionDifficulty = doGetCurrentElectionDifficulty . hpbsPointers
    getUpdates = doGetUpdates . hpbsPointers
    getPendingTimeParameters = doGetPendingTimeParameters . hpbsPointers
    getPendingPoolParameters = doGetPendingPoolParameters . hpbsPointers
    getProtocolUpdateStatus = doGetProtocolUpdateStatus . hpbsPointers
    getCryptographicParameters = doGetCryptoParams . hpbsPointers
    getIdentityProvider = doGetIdentityProvider . hpbsPointers
    getAnonymityRevokers = doGetAnonymityRevokers . hpbsPointers
    getUpdateKeysCollection = doGetUpdateKeyCollection . hpbsPointers
    getExchangeRates = doGetExchangeRates . hpbsPointers
    getPaydayEpoch = doGetPaydayEpoch . hpbsPointers
    getPoolStatus = doGetPoolStatus . hpbsPointers

instance (MonadIO m, PersistentState av pv r m) => ContractStateOperations (PersistentBlockStateMonad pv r m) where
    thawContractState (Instances.InstanceStateV0 inst) = return inst
    thawContractState (Instances.InstanceStateV1 inst) = liftIO . flip StateV1.thaw inst . fst =<< getCallbacks
    externalContractState (Instances.InstanceStateV0 inst) = return inst
    externalContractState (Instances.InstanceStateV1 inst) = return inst
    stateSizeV0 (Instances.InstanceStateV0 inst) = return (Wasm.contractStateSize inst)
    getV1StateContext = asks blobLoadCallback
    contractStateToByteString (Instances.InstanceStateV0 st) = return (encode st)
    contractStateToByteString (Instances.InstanceStateV1 st) = runPut . putByteStringLen <$> StateV1.toByteString st
    {-# INLINE thawContractState #-}
    {-# INLINE stateSizeV0 #-}
    {-# INLINE getV1StateContext #-}
    {-# INLINE contractStateToByteString #-}

instance (PersistentState av pv r m, IsProtocolVersion pv) => AccountOperations (PersistentBlockStateMonad pv r m) where
    getAccountCanonicalAddress = accountCanonicalAddress

    getAccountAmount = accountAmount

    getAccountStakedAmount = accountStakedAmount

    getAccountLockedAmount = accountLockedAmount

    getAccountAvailableAmount = accountAvailableAmount

    getAccountNonce = accountNonce

    checkAccountIsAllowed = accountIsAllowed

    getAccountCredentials = Concordium.GlobalState.Persistent.Account.accountCredentials

    getAccountVerificationKeys = Concordium.GlobalState.Persistent.Account.accountVerificationKeys

    getAccountEncryptedAmount = accountEncryptedAmount

    getAccountEncryptionKey = Concordium.GlobalState.Persistent.Account.accountEncryptionKey

    getAccountReleaseSummary = accountReleaseSummary

    getAccountBaker = accountBaker

    getAccountDelegator = accountDelegator

    getAccountStake = accountStake

    getAccountBakerInfoRef = accountBakerInfoRef

    derefBakerInfo = loadBakerInfo

    getAccountHash = accountHash

instance (IsProtocolVersion pv, PersistentState av pv r m) => BlockStateOperations (PersistentBlockStateMonad pv r m) where
    bsoGetModule pbs mref = doGetModule pbs mref
    bsoGetAccount bs = doGetAccount bs
    bsoGetAccountIndex = doGetAccountIndex
    bsoGetAccountByIndex = doGetAccountByIndex
    bsoGetInstance = doGetInstance
    bsoAddressWouldClash = doAddressWouldClash
    bsoRegIdExists = doRegIdExists
    bsoCreateAccount = doCreateAccount
    bsoPutNewInstance = doPutNewInstance
    bsoPutNewModule = doPutNewModule
    bsoModifyAccount = doModifyAccount
    bsoSetAccountCredentialKeys = doSetAccountCredentialKeys
    bsoUpdateAccountCredentials = doUpdateAccountCredentials
    bsoModifyInstance = doModifyInstance
    bsoNotifyEncryptedBalanceChange = doNotifyEncryptedBalanceChange
    bsoGetSeedState = doGetSeedState
    bsoSetSeedState = doSetSeedState
    bsoTransitionEpochBakers = doTransitionEpochBakers
    bsoGetActiveBakers = doGetActiveBakers
    bsoGetActiveBakersAndDelegators = doGetActiveBakersAndDelegators
    bsoGetCurrentEpochBakers = doGetCurrentEpochBakers
    bsoGetCurrentEpochFullBakersEx = doGetCurrentEpochFullBakersEx
    bsoGetCurrentCapitalDistribution = doGetCurrentCapitalDistribution
    bsoAddBaker = doAddBaker
    bsoConfigureBaker = case delegationChainParameters @pv of
        DelegationChainParameters -> doConfigureBaker
    bsoConstrainBakerCommission = doConstrainBakerCommission
    bsoConfigureDelegation = case delegationChainParameters @pv of
        DelegationChainParameters -> doConfigureDelegation
    bsoUpdateBakerKeys = doUpdateBakerKeys
    bsoUpdateBakerStake = doUpdateBakerStake
    bsoUpdateBakerRestakeEarnings = doUpdateBakerRestakeEarnings
    bsoRemoveBaker = doRemoveBaker
    bsoRewardAccount = doRewardAccount
    bsoGetBakerPoolRewardDetails = doGetBakerPoolRewardDetails
    bsoRewardFoundationAccount = doRewardFoundationAccount
    bsoGetFoundationAccount = doGetFoundationAccount
    bsoMint = doMint
    bsoGetIdentityProvider = doGetIdentityProvider
    bsoGetAnonymityRevokers = doGetAnonymityRevokers
    bsoGetCryptoParams = doGetCryptoParams
    bsoGetPaydayEpoch = doGetPaydayEpoch
    bsoGetPaydayMintRate = doGetPaydayMintRate
    bsoSetPaydayEpoch = doSetPaydayEpoch
    bsoSetPaydayMintRate = doSetPaydayMintRate
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoProcessUpdateQueues = doProcessUpdateQueues
    bsoProcessReleaseSchedule = doProcessReleaseSchedule
    bsoGetUpdateKeyCollection = doGetUpdateKeyCollection
    bsoGetNextUpdateSequenceNumber = doGetNextUpdateSequenceNumber
    bsoEnqueueUpdate = doEnqueueUpdate
    bsoOverwriteElectionDifficulty = doOverwriteElectionDifficulty
    bsoClearProtocolUpdate = doClearProtocolUpdate
    bsoSetNextCapitalDistribution = doSetNextCapitalDistribution
    bsoRotateCurrentCapitalDistribution = doRotateCurrentCapitalDistribution
    bsoGetExchangeRates = doGetExchangeRates
    bsoGetChainParameters = doGetChainParameters
    bsoGetEpochBlocksBaked = doGetEpochBlocksBaked
    bsoNotifyBlockBaked = doNotifyBlockBaked
    bsoUpdateAccruedTransactionFeesBaker = doUpdateAccruedTransactionFeesBaker
    bsoMarkFinalizationAwakeBakers = doMarkFinalizationAwakeBakers
    bsoUpdateAccruedTransactionFeesPassive = doUpdateAccruedTransactionFeesPassive
    bsoGetAccruedTransactionFeesPassive = doGetAccruedTransactionFeesPassive
    bsoUpdateAccruedTransactionFeesFoundationAccount = doUpdateAccruedTransactionFeesFoundationAccount
    bsoGetAccruedTransactionFeesFoundationAccount = doGetAccruedTransactionFeesFoundationAccount
    bsoClearEpochBlocksBaked = doClearEpochBlocksBaked
    bsoRotateCurrentEpochBakers = doRotateCurrentEpochBakers
    bsoSetNextEpochBakers = doSetNextEpochBakers
    bsoProcessPendingChanges = doProcessPendingChanges
    bsoGetBankStatus = doGetBankStatus
    bsoSetRewardAccounts = doSetRewardAccounts

instance (IsProtocolVersion pv, PersistentState av pv r m) => BlockStateStorage (PersistentBlockStateMonad pv r m) where
    thawBlockState HashedPersistentBlockState{..} =
        liftIO $ newIORef =<< readIORef hpbsPointers

    freezeBlockState pbs = hashBlockState pbs

    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    purgeBlockState _ = return ()
    {-# INLINE purgeBlockState #-}

    archiveBlockState HashedPersistentBlockState{..} = do
        inner <- liftIO $ readIORef hpbsPointers
        !inner' <- uncacheBufferedRef inner
        liftIO $ writeIORef hpbsPointers inner'

    saveBlockState HashedPersistentBlockState{..} = do
        inner <- liftIO $ readIORef hpbsPointers
        (!inner', !ref) <- flushBufferedRef inner
        liftIO $ writeIORef hpbsPointers inner'
        flushStore
        return ref

    loadBlockState hpbsHash ref = do
        hpbsPointers <- liftIO $ newIORef $ blobRefToBufferedRef ref
        return HashedPersistentBlockState{..}

    serializeBlockState hpbs = do
        p <- runPutT (putBlockStateV0 (hpbsPointers hpbs))
        return $ runPut p

    blockStateLoadCallback = asks blobLoadCallback
    {-# INLINE blockStateLoadCallback #-}

    collapseCaches = do
        Cache.collapseCache (Proxy :: Proxy (AccountCache av))
        Cache.collapseCache (Proxy :: Proxy Modules.ModuleCache)

-- |Migrate the block state from the representation used by protocol version
-- @oldpv@ to the one used by protocol version @pv@. The migration is done gradually,
-- and that is the reason for the monad @m@ and the transformer @t@. The inner monad @m@ is
-- used to __load__ the state, only the loading part of the 'MonadBlobStore' is used.
-- The outer monad @t m@ is used to __write__ the new state after migration.
--
-- The intention is that the inner @m@ is @BlobStoreT
-- (PersistentBlockStateContext oldpv) IO@, whereas the @t@ is @BlobStoreT
-- (PersistentBlockStateContext pv)@. Since they both implement the
-- 'MonadBlobStore' interface some care must be observed to read and write in
-- the correct context in the implementation of this function. Typically,
-- reading from the old state requires @lift@ing the relevant operation.
--
-- The migration function should not do non-trivial changes, i.e., it should
-- only migrate representation, and fill in the defaults as specified by the
-- state migration parameters.
migratePersistentBlockState ::
    forall oldpv pv t m.
    ( MonadTrans t,
      MonadBlobStore (t m),
      SupportsPersistentAccount oldpv m,
      SupportsPersistentAccount pv (t m),
      Modules.SupportsPersistentModule m,
      Modules.SupportsPersistentModule (t m),
      MonadProtocolVersion (t m),
      MPV (t m) ~ pv,
      MonadProtocolVersion m,
      MPV m ~ oldpv
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentBlockState oldpv ->
    t m (PersistentBlockState pv)
migratePersistentBlockState migration oldState = do
    !newState <- migrateBlockPointers migration =<< lift . refLoad =<< liftIO (readIORef oldState)
    newStateRef <- refMake newState
    (newStateRefFlushed, _) <- refFlush newStateRef
    liftIO . newIORef $! newStateRefFlushed

migrateBlockPointers ::
    forall oldpv pv t m.
    ( SupportMigration m t,
      SupportsPersistentAccount oldpv m,
      SupportsPersistentAccount pv (t m),
      Modules.SupportsPersistentModule m,
      Modules.SupportsPersistentModule (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    BlockStatePointers oldpv ->
    t m (BlockStatePointers pv)
migrateBlockPointers migration BlockStatePointers{..} = do
    -- We migrate the release schedule first because we may need to access the
    -- accounts in the process.
    let rsMigration = case migration of
            StateMigrationParametersTrivial -> trivialReleaseScheduleMigration
            StateMigrationParametersP1P2 -> RSMLegacyToLegacy
            StateMigrationParametersP2P3 -> RSMLegacyToLegacy
            StateMigrationParametersP3ToP4{} -> RSMLegacyToLegacy
            StateMigrationParametersP4ToP5{} -> RSMLegacyToNew $ \addr ->
                Accounts.getAccountIndex addr bspAccounts <&> \case
                    Nothing -> error "Account with release schedule does not exist"
                    Just ai -> ai
    newReleaseSchedule <- migrateReleaseSchedule rsMigration bspReleaseSchedule
    newAccounts <- Accounts.migrateAccounts migration bspAccounts
    newModules <- migrateHashedBufferedRef Modules.migrateModules bspModules
    modules <- refLoad newModules
    newInstances <- Instances.migrateInstances modules bspInstances
    let newBank = bspBank
    newIdentityProviders <- migrateHashedBufferedRefKeepHash bspIdentityProviders
    newAnonymityRevokers <- migrateHashedBufferedRefKeepHash bspAnonymityRevokers
    newBirkParameters <- migratePersistentBirkParameters migration newAccounts bspBirkParameters
    newCryptographicParameters <- migrateHashedBufferedRefKeepHash bspCryptographicParameters
    newUpdates <- migrateReference (migrateUpdates migration) bspUpdates
    curBakers <- extractBakerStakes =<< refLoad (_birkCurrentEpochBakers newBirkParameters)
    nextBakers <- extractBakerStakes =<< refLoad (_birkNextEpochBakers newBirkParameters)
    -- clear transaction outcomes.
    let newTransactionOutcomes = emptyTransactionOutcomes (Proxy @pv)
    chainParams <- refLoad . currentParameters =<< refLoad newUpdates
    let timeParams = _cpTimeParameters . unStoreSerialized $ chainParams
    newRewardDetails <- migrateBlockRewardDetails migration curBakers nextBakers timeParams bspRewardDetails

    return $!
        BlockStatePointers
            { bspAccounts = newAccounts,
              bspInstances = newInstances,
              bspModules = newModules,
              bspBank = newBank,
              bspIdentityProviders = newIdentityProviders,
              bspAnonymityRevokers = newAnonymityRevokers,
              bspBirkParameters = newBirkParameters,
              bspCryptographicParameters = newCryptographicParameters,
              bspUpdates = newUpdates,
              bspReleaseSchedule = newReleaseSchedule,
              bspTransactionOutcomes = newTransactionOutcomes,
              bspRewardDetails = newRewardDetails
            }

-- |Cache the block state and get the initial (empty) transaction table with the next account nonces
-- and update sequence numbers populated.
cacheStateAndGetTransactionTable ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    HashedPersistentBlockState pv ->
    m TransactionTable.TransactionTable
cacheStateAndGetTransactionTable hpbs = do
    BlockStatePointers{..} <- loadPBS (hpbsPointers hpbs)
    -- When caching the accounts, we populate the transaction table with the next account nonces.
    -- This is done by using 'liftCache' on the account table with a custom cache function that
    -- records the nonces.
    let perAcct acct = do
            -- Note: we do not need to cache the account because a loaded account is already fully
            -- cached. (Indeed, 'cache' is defined to be 'pure'.)
            nonce <- accountNonce acct
            unless (nonce == minNonce) $ do
                addr <- accountCanonicalAddress acct
                MTL.modify
                    ( TransactionTable.ttNonFinalizedTransactions . at' (accountAddressEmbed addr)
                        ?~ TransactionTable.emptyANFTWithNonce nonce
                    )
            return acct
    (accts, tt0) <- MTL.runStateT (liftCache perAcct bspAccounts) TransactionTable.emptyTransactionTable
    -- first cache the modules
    mods <- cache bspModules
    -- then cache the instances, but don't cache the modules again. Instead
    -- share the references in memory we have already constructed by caching
    -- modules above. Loading the modules here is cheap since we cached them.
    insts <- runReaderT (cache bspInstances) =<< refLoad mods
    ips <- cache bspIdentityProviders
    ars <- cache bspAnonymityRevokers
    birkParams <- cache bspBirkParameters
    cryptoParams <- cache bspCryptographicParameters
    upds <- cache bspUpdates
    -- Update the transaction table with the sequence numbers for chain updates.
    let updInTT tt uty = do
            sn <- lookupNextUpdateSequenceNumber bspUpdates uty
            if sn /= minUpdateSequenceNumber
                then
                    return $!
                        tt
                            & TransactionTable.ttNonFinalizedChainUpdates . at' uty
                                ?~ TransactionTable.emptyNFCUWithSequenceNumber sn
                else return tt
    tt <- foldM updInTT tt0 [minBound ..]
    rels <- cache bspReleaseSchedule
    red <- cache bspRewardDetails
    _ <-
        storePBS
            (hpbsPointers hpbs)
            BlockStatePointers
                { bspAccounts = accts,
                  bspInstances = insts,
                  bspModules = mods,
                  bspBank = bspBank,
                  bspIdentityProviders = ips,
                  bspAnonymityRevokers = ars,
                  bspBirkParameters = birkParams,
                  bspCryptographicParameters = cryptoParams,
                  bspUpdates = upds,
                  bspReleaseSchedule = rels,
                  bspTransactionOutcomes = bspTransactionOutcomes,
                  bspRewardDetails = red
                }
    return tt
