{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
    initialBirkParameters,
    initialPersistentState,
    emptyBlockState,
    emptyHashedEpochBlocks,
    emptyPersistentTransactionOutcomes,
    PersistentBlockStateContext (..),
    PersistentState,
    BlockRewardDetails' (..),
    BlockRewardDetails,
    PersistentBlockStateMonad (..),
    withNewAccountCacheAndLMDBAccountMap,
    cacheState,
    cacheStateAndGetTransactionTable,
    migratePersistentBlockState,
    SupportsPersistentState,
    loadPBS,
    storePBS,
) where

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.AccountMap.ModuleMap (MonadModuleMapStore)
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.CooldownQueue as CooldownQueue
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.Account.CooldownQueue (NextCooldownChange (..))
import qualified Concordium.GlobalState.Persistent.Account.MigrationState as MigrationState
import Concordium.GlobalState.Persistent.Accounts (SupportsPersistentAccount)
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import qualified Concordium.GlobalState.Persistent.Accounts as LMDBAccountMap
import Concordium.GlobalState.Persistent.Bakers
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlockState.Updates
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.Cooldown
import Concordium.GlobalState.Persistent.Instances (PersistentInstance (..), PersistentInstanceParameters (..), PersistentInstanceV (..))
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT
import Concordium.GlobalState.Persistent.PoolRewards
import Concordium.GlobalState.Persistent.ReleaseSchedule
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.PoolRewards
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.TransactionTable as TransactionTable
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.ID.Parameters as ID
import qualified Concordium.ID.Types as ID
import Concordium.Kontrol.Bakers
import Concordium.Logger
import Concordium.TimeMonad (TimeMonad)
import Concordium.Types
import Concordium.Types.Accounts (AccountBaker (..))
import qualified Concordium.Types.Accounts as BaseAccounts
import qualified Concordium.Types.AnonymityRevokers as ARS
import Concordium.Types.Conditionally
import Concordium.Types.Execution (DelegationTarget (..), TransactionIndex, TransactionSummary)
import qualified Concordium.Types.Execution as Transactions
import Concordium.Types.HashableTo
import qualified Concordium.Types.IdentityProviders as IPS
import Concordium.Types.Option
import Concordium.Types.Queries (
    ActiveBakerPoolStatus (..),
    BakerPoolStatus (..),
    CurrentPaydayBakerPoolStatus (..),
    PassiveDelegationStatus (..),
    RewardStatus' (..),
    makePoolPendingChange,
 )
import Concordium.Types.SeedState
import qualified Concordium.Types.TransactionOutcomes as TransactionOutcomes
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.UpdateQueues as UQ
import Concordium.Types.Updates
import Concordium.Utils
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import qualified Concordium.Wasm as Wasm
import Control.Exception
import Control.Monad
import qualified Control.Monad.Catch as MonadCatch
import qualified Control.Monad.Except as MTL
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
import Data.Bool.Singletons
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
import System.Directory (removeDirectoryRecursive)

-- * Birk parameters

data PersistentBirkParameters (pv :: ProtocolVersion) = PersistentBirkParameters
    { -- | The currently-registered bakers.
      _birkActiveBakers :: !(BufferedRef (PersistentActiveBakers (AccountVersionFor pv))),
      -- | The bakers that will be used for the next epoch.
      _birkNextEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers pv)),
      -- | The bakers for the current epoch.
      _birkCurrentEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers pv)),
      -- | The seed state used to derive the leadership election nonce.
      _birkSeedState :: !(SeedState (SeedStateVersionFor pv))
    }
    deriving (Show)

makeLenses ''PersistentBirkParameters

-- | Migrate a 'SeedState' between protocol versions.
--  For migrations in consensus version 0, changes to the seed state are handled prior to state
--  migration.
--
--  For migrations to consensus version 1, changes to the seed state are handled here as follows:
--
--   * P5 to P6: The new initial seed state is constructed with
--       - the initial nonce @H.hash $ "Regenesis" <> encode ss0CurrentLeadershipElectionNonce@, and
--       - the first epoch trigger block timestamp determined by the state migration data, which
--         should be one epoch after the regenesis time.
--
--   * P6 to P6: The new seed state is constructed with
--       - the initial nonce @H.hash $ "Regenesis" <> encode ss1UpdatedNonce@,
--       - the epoch reset to 0,
--       - the first epoch trigger block time the same as the prior seed state,
--       - the epoch transition triggered flag set, and
--       - the shutdown triggered flag cleared.
migrateSeedState ::
    forall oldpv pv.
    (IsProtocolVersion pv) =>
    StateMigrationParameters oldpv pv ->
    SeedState (SeedStateVersionFor oldpv) ->
    SeedState (SeedStateVersionFor pv)
migrateSeedState StateMigrationParametersTrivial{} ss = case ss of
    SeedStateV0{} -> ss -- In consensus v0, seed state update is handled prior to migration
    SeedStateV1{} -> migrateSeedStateV1Trivial ss
migrateSeedState StateMigrationParametersP1P2{} ss = ss
migrateSeedState StateMigrationParametersP2P3{} ss = ss
migrateSeedState StateMigrationParametersP3ToP4{} ss = ss
migrateSeedState StateMigrationParametersP4ToP5{} ss = ss
migrateSeedState (StateMigrationParametersP5ToP6 (P6.StateMigrationData _ time)) SeedStateV0{..} =
    let seed = H.hash $ "Regenesis" <> encode ss0CurrentLeadershipElectionNonce
    in  initialSeedStateV1 seed time
migrateSeedState StateMigrationParametersP6ToP7{} ss = migrateSeedStateV1Trivial ss
migrateSeedState StateMigrationParametersP7ToP8{} ss = migrateSeedStateV1Trivial ss
migrateSeedState StateMigrationParametersP8ToP9{} ss = migrateSeedStateV1Trivial ss

-- | Trivial migration of a 'SeedStateV1' between protocol versions.
migrateSeedStateV1Trivial :: SeedState 'SeedStateVersion1 -> SeedState 'SeedStateVersion1
migrateSeedStateV1Trivial SeedStateV1{..} =
    SeedStateV1
        { -- Reset the epoch to 0.
          ss1Epoch = 0,
          ss1CurrentLeadershipElectionNonce = newNonce,
          ss1UpdatedNonce = newNonce,
          -- We maintain the trigger block time. This forces an epoch transition as soon as possible
          -- which will effectively substitute for the epoch transition that would have happened
          -- on the previous consensus, had it not shut down.
          ss1TriggerBlockTime = ss1TriggerBlockTime,
          -- We flag the epoch transition as triggered so that the epoch transition will happen
          -- as soon as possible.
          ss1EpochTransitionTriggered = True,
          -- We clear the shutdown flag.
          ss1ShutdownTriggered = False
        }
  where
    -- We derive the new nonce from the updated nonce on the basis that it was fixed
    -- at the trigger block from the previous consensus.
    newNonce = H.hash $ "Regenesis" <> encode ss1UpdatedNonce

-- | See documentation of @migratePersistentBlockState@.
--
--  Migrate the birk parameters assuming accounts have already been migrated.
migratePersistentBirkParameters ::
    forall c oldpv pv t m.
    ( IsProtocolVersion pv,
      IsProtocolVersion oldpv,
      SupportMigration m t,
      SupportsPersistentAccount pv (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    Accounts.Accounts pv ->
    Conditionally c (PersistentActiveBakers (AccountVersionFor pv)) ->
    PersistentBirkParameters oldpv ->
    t m (PersistentBirkParameters pv)
migratePersistentBirkParameters migration accounts mActiveBakers PersistentBirkParameters{..} = do
    newActiveBakers <- case mActiveBakers of
        CTrue ab -> refMake ab
        CFalse -> migrateReference (migratePersistentActiveBakers migration accounts) _birkActiveBakers
    newNextEpochBakers <- migrateHashedBufferedRef (migratePersistentEpochBakers migration) _birkNextEpochBakers
    newCurrentEpochBakers <- migrateHashedBufferedRef (migratePersistentEpochBakers migration) _birkCurrentEpochBakers
    return
        PersistentBirkParameters
            { _birkActiveBakers = newActiveBakers,
              _birkNextEpochBakers = newNextEpochBakers,
              _birkCurrentEpochBakers = newCurrentEpochBakers,
              _birkSeedState = migrateSeedState migration _birkSeedState
            }

-- | Accumulated state when iterating accounts, meant for constructing PersistentBirkParameters.
--  Used internally by initialBirkParameters.
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

-- | Initial state for iterating accounts.
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

-- | Collections of delegators, grouped by the pool they are delegating to.
--  Used internally by initialBirkParameters.
data IBPCollectedDelegators av = IBPCollectedDelegators
    { -- | Delegators delegating to the passive pool.
      ibpcdToPassive :: !(PersistentActiveDelegators av),
      -- | Delegators delegating to bakers
      ibpcdToBaker :: !(Map.Map BakerId (PersistentActiveDelegators av))
    }

-- | Empty collections of delegators.
emptyIBPCollectedDelegators :: (IsAccountVersion av) => IBPCollectedDelegators av
emptyIBPCollectedDelegators =
    IBPCollectedDelegators
        { ibpcdToPassive = emptyPersistentActiveDelegators,
          ibpcdToBaker = Map.empty
        }

-- | Generate initial birk parameters from accounts and seed state.
initialBirkParameters ::
    forall pv av m.
    (MonadBlobStore m, IsProtocolVersion pv, av ~ AccountVersionFor pv) =>
    -- | The accounts in ascending order of the account index.
    [PersistentAccount av] ->
    -- | The seed state
    SeedState (SeedStateVersionFor pv) ->
    -- | The finalization committee parameters (if relevant)
    OFinalizationCommitteeParameters pv ->
    m (PersistentBirkParameters pv)
initialBirkParameters accounts seedState _bakerFinalizationCommitteeParameters = do
    -- Iterate accounts and collect delegators.
    IBPCollectedDelegators{..} <- case delegationSupport @av of
        SAVDelegationNotSupported -> return emptyIBPCollectedDelegators
        SAVDelegationSupported -> foldM collectDelegator emptyIBPCollectedDelegators accounts

    -- Iterate the accounts again accumulate all relevant information.
    IBPFromAccountsAccum{..} <- foldM (accumFromAccounts ibpcdToBaker) initialIBPFromAccountsAccum accounts

    -- The total stake from bakers and delegators
    let totalStake = case delegationSupport @av of
            SAVDelegationNotSupported -> aibpStakedTotal
            SAVDelegationSupported ->
                aibpStakedTotal
                    + sum ((^. delegatorTotalCapital) <$> ibpcdToBaker)
                    + ibpcdToPassive ^. delegatorTotalCapital

    persistentActiveBakers <-
        refMake $!
            PersistentActiveBakers
                { _activeBakers = aibpBakerIds,
                  _aggregationKeys = aibpBakerKeys,
                  _passiveDelegators = ibpcdToPassive,
                  _totalActiveCapital = case delegationSupport @av of
                    SAVDelegationNotSupported -> TotalActiveCapitalV0
                    SAVDelegationSupported -> TotalActiveCapitalV1 totalStake
                }

    nextEpochBakers <- do
        _bakerInfos <- refMake $ BakerInfos aibpBakerInfoRefs
        _bakerStakes <- refMake $ BakerStakes aibpBakerStakes
        refMake PersistentEpochBakers{_bakerTotalStake = totalStake, ..}

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
                stake <- accountActiveStakedAmount account

                return
                    updatedAccum
                        { aibpBakerIds = nextBakerIds,
                          aibpBakerKeys = nextBakerKeys,
                          aibpBakerInfoRefs = Vec.snoc (aibpBakerInfoRefs accum) infoRef,
                          aibpBakerStakes = Vec.snoc (aibpBakerStakes accum) stake,
                          aibpStakedTotal = aibpStakedTotal accum + stake
                        }
            Nothing -> return updatedAccum

freezeContractState :: forall v m. (Wasm.IsWasmVersion v, MonadBlobStore m) => UpdatableContractState v -> m (H.Hash, Instances.InstanceStateV v)
freezeContractState cs = case Wasm.getWasmVersion @v of
    Wasm.SV0 -> return (getHash cs, Instances.InstanceStateV0 cs)
    Wasm.SV1 -> do
        (cbk, _) <- getCallbacks
        (hsh, persistent) <- liftIO (StateV1.freeze cbk cs)
        return (hsh, Instances.InstanceStateV1 persistent)

instance (IsProtocolVersion pv, MonadBlobStore m) => MHashableTo m H.Hash (PersistentBirkParameters pv) where
    getHashM PersistentBirkParameters{..} = withIsSeedStateVersionFor (protocolVersion @pv) $ do
        nextHash <- getHashM _birkNextEpochBakers
        currentHash <- getHashM _birkCurrentEpochBakers
        let bpH0 = H.hash $ "SeedState" <> encode _birkSeedState
            bpH1 = H.hashOfHashes nextHash currentHash
        return $ H.hashOfHashes bpH0 bpH1

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (PersistentBirkParameters pv) where
    storeUpdate bps@PersistentBirkParameters{..} = withIsSeedStateVersionFor (protocolVersion @pv) $ do
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
    load = withIsSeedStateVersionFor (protocolVersion @pv) $ do
        mabs <- label "Active bakers" load
        mnebs <- label "Next epoch bakers" load
        mcebs <- label "Current epoch bakers" load
        _birkSeedState <- label "Seed state" get
        return $! do
            _birkActiveBakers <- mabs
            _birkNextEpochBakers <- mnebs
            _birkCurrentEpochBakers <- mcebs
            return PersistentBirkParameters{..}

instance (MonadBlobStore m, IsProtocolVersion pv) => Cacheable m (PersistentBirkParameters pv) where
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

-- | Structure for tracking which bakers have baked blocks
--  in the current epoch.
data EpochBlock = EpochBlock
    { ebBakerId :: !BakerId,
      ebPrevious :: !EpochBlocks
    }

-- | Migrate the 'EpochBlocks' structure, reading it from context @m@ and writing
--  it to context @t m@.
migrateEpochBlocks :: (MonadTrans t, BlobStorable m EpochBlock, BlobStorable (t m) EpochBlock) => EpochBlocks -> t m EpochBlocks
migrateEpochBlocks Null = return Null
migrateEpochBlocks (Some inner) = Some <$> migrateReference go inner
  where
    go EpochBlock{..} = do
        newPrevious <- migrateEpochBlocks ebPrevious
        return EpochBlock{ebPrevious = newPrevious, ..}

-- | Return a map, mapping baker ids to the number of blocks they baked as they
--  appear in the 'EpochBlocks' structure.
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

instance (MonadBlobStore m) => Cacheable m EpochBlock where
    cache eb = do
        ebPrevious' <- cache (ebPrevious eb)
        return eb{ebPrevious = ebPrevious'}

instance (MonadBlobStore m) => MHashableTo m Rewards.EpochBlocksHash EpochBlock where
    getHashM EpochBlock{..} = Rewards.epochBlockHash ebBakerId <$> getHashM ebPrevious

instance (MonadBlobStore m) => MHashableTo m Rewards.EpochBlocksHash EpochBlocks where
    getHashM Null = return Rewards.emptyEpochBlocksHash
    getHashM (Some r) = getHashM r

data HashedEpochBlocks = HashedEpochBlocks
    { hebBlocks :: !EpochBlocks,
      hebHash :: !Rewards.EpochBlocksHash
    }

-- | Like 'migrateEpochBlocks', but for hashed blocks. This makes use of the fact
--  that the hash does not change upon migration and so it is carried over.
--
--  See also documentation of @migratePersistentBlockState@.
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

instance (MonadBlobStore m) => BlobStorable m HashedEpochBlocks where
    storeUpdate heb = do
        (pblocks, blocks') <- storeUpdate (hebBlocks heb)
        return $!! (pblocks, heb{hebBlocks = blocks'})
    load = do
        mhebBlocks <- load
        return $! do
            hebBlocks <- mhebBlocks
            hebHash <- getHashM hebBlocks
            return HashedEpochBlocks{..}

instance (MonadBlobStore m) => Cacheable m HashedEpochBlocks where
    cache red = do
        blocks' <- cache (hebBlocks red)
        return $! red{hebBlocks = blocks'}

-- | The empty 'HashedEpochBlocks'.
emptyHashedEpochBlocks :: HashedEpochBlocks
emptyHashedEpochBlocks =
    HashedEpochBlocks
        { hebBlocks = Null,
          hebHash = Rewards.emptyEpochBlocksHash
        }

-- | Add a new 'BakerId' to the start of a 'HashedEpochBlocks'.
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

data BlockRewardDetails' (av :: AccountVersion) (bhv :: BlockHashVersion) where
    BlockRewardDetailsV0 :: !HashedEpochBlocks -> BlockRewardDetails' 'AccountV0 bhv
    BlockRewardDetailsV1 :: (AVSupportsDelegation av) => !(HashedBufferedRef' (Rewards.PoolRewardsHash bhv) (PoolRewards bhv av)) -> BlockRewardDetails' av bhv

type BlockRewardDetails pv = BlockRewardDetails' (AccountVersionFor pv) (BlockHashVersionFor pv)

-- | Migrate the block reward details.
--  When migrating to 'P4' or 'P5', or from 'P5' to 'P6', this sets the 'nextPaydayEpoch' to the
--  reward period length. Migrations from 'P6' onwards (consensus protocol version 1) will set the
--  'nextPaydayEpoch' to occur at the same time as it would have before the protocol update.
migrateBlockRewardDetails ::
    forall t m oldpv pv.
    ( MonadBlobStore (t m),
      MonadTrans t,
      SupportsPersistentAccount oldpv m
    ) =>
    StateMigrationParameters oldpv pv ->
    -- | Current epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- | Next epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- | The time parameters (where supported by the new protocol version).
    OParam 'PTTimeParameters (ChainParametersVersionFor pv) TimeParameters ->
    -- | The epoch number before the protocol update.
    Epoch ->
    BlockRewardDetails oldpv ->
    t m (BlockRewardDetails pv)
migrateBlockRewardDetails StateMigrationParametersTrivial _ _ tp oldEpoch = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
    (BlockRewardDetailsV1 hbr) -> case tp of
        SomeParam TimeParametersV1{..} ->
            BlockRewardDetailsV1
                <$> migrateHashedBufferedRef migratePR hbr
          where
            migratePR = migratePoolRewardsP6 oldEpoch _tpRewardPeriodLength
        NoParam -> case protocolVersion @pv of {}
migrateBlockRewardDetails StateMigrationParametersP1P2 _ _ _ _ = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
migrateBlockRewardDetails StateMigrationParametersP2P3 _ _ _ _ = \case
    (BlockRewardDetailsV0 heb) -> BlockRewardDetailsV0 <$> migrateHashedEpochBlocks heb
migrateBlockRewardDetails (StateMigrationParametersP3ToP4 _) curBakers nextBakers (SomeParam TimeParametersV1{..}) _ = \case
    (BlockRewardDetailsV0 heb) -> do
        blockCounts <- bakersFromEpochBlocks (hebBlocks heb)
        (!newRef, _) <- refFlush =<< refMake =<< migratePoolRewardsP1 curBakers nextBakers blockCounts (rewardPeriodEpochs _tpRewardPeriodLength) _tpMintPerPayday
        return (BlockRewardDetailsV1 newRef)
migrateBlockRewardDetails StateMigrationParametersP4ToP5{} _ _ (SomeParam TimeParametersV1{..}) _ = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewards (rewardPeriodEpochs _tpRewardPeriodLength)) hbr
migrateBlockRewardDetails StateMigrationParametersP5ToP6{} _ _ (SomeParam TimeParametersV1{..}) _ = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewards (rewardPeriodEpochs _tpRewardPeriodLength)) hbr
migrateBlockRewardDetails StateMigrationParametersP6ToP7{} _ _ (SomeParam TimeParametersV1{..}) oldEpoch = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewardsP6 oldEpoch _tpRewardPeriodLength) hbr
migrateBlockRewardDetails StateMigrationParametersP7ToP8{} _ _ (SomeParam TimeParametersV1{..}) oldEpoch = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewardsP6 oldEpoch _tpRewardPeriodLength) hbr
migrateBlockRewardDetails StateMigrationParametersP8ToP9{} _ _ (SomeParam TimeParametersV1{..}) oldEpoch = \case
    (BlockRewardDetailsV1 hbr) ->
        BlockRewardDetailsV1
            <$> migrateHashedBufferedRef (migratePoolRewardsP6 oldEpoch _tpRewardPeriodLength) hbr

instance
    (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) =>
    MHashableTo m (Rewards.BlockRewardDetailsHash' av bhv) (BlockRewardDetails' av bhv)
    where
    getHashM (BlockRewardDetailsV0 heb) = return $ Rewards.BlockRewardDetailsHashV0 (getHash heb)
    getHashM (BlockRewardDetailsV1 pr) = Rewards.BlockRewardDetailsHashV1 <$> getHashM pr

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (BlockRewardDetails' av bhv) where
    storeUpdate (BlockRewardDetailsV0 heb) = fmap (fmap BlockRewardDetailsV0) $ storeUpdate heb
    storeUpdate (BlockRewardDetailsV1 hpr) = fmap (fmap BlockRewardDetailsV1) $ storeUpdate hpr
    load = case delegationSupport @av of
        SAVDelegationNotSupported -> fmap (fmap BlockRewardDetailsV0) load
        SAVDelegationSupported -> fmap (fmap BlockRewardDetailsV1) load

instance (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) => Cacheable m (BlockRewardDetails' av bhv) where
    cache (BlockRewardDetailsV0 heb) = BlockRewardDetailsV0 <$> cache heb
    cache (BlockRewardDetailsV1 hpr) = BlockRewardDetailsV1 <$> cache hpr

-- | Extend a 'BlockRewardDetails' ''AccountV0' with an additional baker.
consBlockRewardDetails ::
    (MonadBlobStore m) =>
    BakerId ->
    BlockRewardDetails' 'AccountV0 bhv ->
    m (BlockRewardDetails' 'AccountV0 bhv)
consBlockRewardDetails bid (BlockRewardDetailsV0 heb) = do
    BlockRewardDetailsV0 <$> consEpochBlock bid heb

-- | The empty 'BlockRewardDetails'.
emptyBlockRewardDetails ::
    forall av bhv m.
    (MonadBlobStore m, IsAccountVersion av, IsBlockHashVersion bhv) =>
    m (BlockRewardDetails' av bhv)
emptyBlockRewardDetails =
    case delegationSupport @av of
        SAVDelegationNotSupported -> return $ BlockRewardDetailsV0 emptyHashedEpochBlocks
        SAVDelegationSupported -> BlockRewardDetailsV1 <$> (emptyPoolRewards >>= refMake)

-- * Block state

-- | Type representing a persistent block state. This is a 'BufferedRef' inside an 'IORef',
--  which supports making changes to the state without them (necessarily) being written to
--  disk.
type PersistentBlockState (pv :: ProtocolVersion) = IORef (BufferedRef (BlockStatePointers pv))

-- | Transaction outcomes stored in a merkle binary tree.
data MerkleTransactionOutcomes = MerkleTransactionOutcomes
    { -- | Normal transaction outcomes
      mtoOutcomes :: LFMBT.LFMBTree TransactionIndex HashedBufferedRef TransactionSummaryV1,
      -- | Special transaction outcomes
      mtoSpecials :: LFMBT.LFMBTree TransactionIndex HashedBufferedRef Transactions.SpecialTransactionOutcome
    }
    deriving (Show)

-- | Create an empty 'MerkleTransactionOutcomes'
emptyMerkleTransactionOutcomes :: MerkleTransactionOutcomes
emptyMerkleTransactionOutcomes =
    MerkleTransactionOutcomes
        { mtoOutcomes = LFMBT.empty,
          mtoSpecials = LFMBT.empty
        }

-- | Transaction outcomes stored in the 'Persistent' block state.
--  From PV1 to PV4 transaction outcomes are stored within a list 'Transactions.TransactionOutcomes'.
--  From PV5 and onwards the transaction outcomes are stored in a binary merkle tree.
--  Note. There are no difference in the actual stored 'TransactionSummary' however there
--  is a difference in the hashing scheme.
--  In PV1 to PV4 the transaction outcomes are hashed as a list based on all of the data
--  in the 'TransactionSummary'.
--  In PV5 and onwards the exact 'RejectReason's are omitted from the computed hash and moreover
--  the hashing scheme is not a hash list but a merkle tree, so it is the root hash that is
--  used in the final 'BlockHash'.
data PersistentTransactionOutcomes (tov :: TransactionOutcomesVersion) where
    PTOV0 :: TransactionOutcomes.TransactionOutcomes -> PersistentTransactionOutcomes 'TOV0
    PTOV1 :: MerkleTransactionOutcomes -> PersistentTransactionOutcomes 'TOV1
    PTOV2 :: MerkleTransactionOutcomes -> PersistentTransactionOutcomes 'TOV2

-- | Create an empty persistent transaction outcome
emptyPersistentTransactionOutcomes :: forall tov. (IsTransactionOutcomesVersion tov) => PersistentTransactionOutcomes tov
emptyPersistentTransactionOutcomes = case transactionOutcomesVersion @tov of
    STOV0 -> PTOV0 TransactionOutcomes.emptyTransactionOutcomesV0
    STOV1 -> PTOV1 emptyMerkleTransactionOutcomes
    STOV2 -> PTOV2 emptyMerkleTransactionOutcomes

instance
    (BlobStorable m TransactionSummaryV1) =>
    MHashableTo m (TransactionOutcomes.TransactionOutcomesHashV tov) (PersistentTransactionOutcomes tov)
    where
    getHashM (PTOV0 bto) = return (getHash bto)
    getHashM (PTOV1 MerkleTransactionOutcomes{..}) = do
        out <- getHashM @_ @(LFMBT.LFMBTreeHash' 'BlockHashVersion0) mtoOutcomes
        special <- getHashM @_ @(LFMBT.LFMBTreeHash' 'BlockHashVersion0) mtoSpecials
        return $!
            TransactionOutcomes.TransactionOutcomesHashV . H.hashLazy . runPutLazy $ do
                putShortByteString "TransactionOutcomesHashV1"
                put out
                put special
    getHashM (PTOV2 MerkleTransactionOutcomes{..}) = do
        out <- getHashM @_ @(LFMBT.LFMBTreeHash' 'BlockHashVersion1) mtoOutcomes
        special <- getHashM @_ @(LFMBT.LFMBTreeHash' 'BlockHashVersion1) mtoSpecials
        return $!
            TransactionOutcomes.TransactionOutcomesHashV $
                H.hashOfHashes (LFMBT.theLFMBTreeHash out) (LFMBT.theLFMBTreeHash special)

instance
    ( TransactionOutcomesVersionFor (MPV m) ~ tov,
      MonadBlobStore m,
      MonadProtocolVersion m
    ) =>
    BlobStorable m (PersistentTransactionOutcomes tov)
    where
    storeUpdate out@(PTOV0 bto) = return (TransactionOutcomes.putTransactionOutcomes bto, out)
    storeUpdate out = case out of
        PTOV1 mto -> (_2 %~ PTOV1) <$> inner mto
        PTOV2 mto -> (_2 %~ PTOV2) <$> inner mto
      where
        inner MerkleTransactionOutcomes{..} = do
            (pout, mtoOutcomes') <- storeUpdate mtoOutcomes
            (pspecial, mtoSpecials') <- storeUpdate mtoSpecials
            return (pout <> pspecial, MerkleTransactionOutcomes{mtoOutcomes = mtoOutcomes', mtoSpecials = mtoSpecials'})

    load = do
        case transactionOutcomesVersion @(TransactionOutcomesVersionFor (MPV m)) of
            STOV0 -> do
                out <- PTOV0 <$!> TransactionOutcomes.getTransactionOutcomes (protocolVersion @(MPV m))
                pure . pure $! out
            STOV1 -> do
                mout <- load
                mspecials <- load
                return $! do
                    mtoOutcomes <- mout
                    mtoSpecials <- mspecials
                    return $! PTOV1 MerkleTransactionOutcomes{..}
            STOV2 -> do
                mout <- load
                mspecials <- load
                return $! do
                    mtoOutcomes <- mout
                    mtoSpecials <- mspecials
                    return $! PTOV2 MerkleTransactionOutcomes{..}

-- | Create an empty 'PersistentTransactionOutcomes' based on the 'ProtocolVersion'.
emptyTransactionOutcomes ::
    forall pv.
    (SupportsTransactionOutcomes pv) =>
    Proxy pv ->
    PersistentTransactionOutcomes (TransactionOutcomesVersionFor pv)
emptyTransactionOutcomes Proxy = case transactionOutcomesVersion @(TransactionOutcomesVersionFor pv) of
    STOV0 -> PTOV0 TransactionOutcomes.emptyTransactionOutcomesV0
    STOV1 -> PTOV1 emptyMerkleTransactionOutcomes
    STOV2 -> PTOV2 emptyMerkleTransactionOutcomes

-- | References to the components that make up the block state.
--
--  This type is parametric in the protocol version (as opposed to defined
--  as a data family) on the principle that the structure will be mostly
--  similar across versions. Where component change between versions,
--  those components themselves should be parametrised by the protocol
--  version.
data BlockStatePointers (pv :: ProtocolVersion) = BlockStatePointers
    { bspAccounts :: !(Accounts.Accounts pv),
      bspInstances :: !(Instances.Instances pv),
      bspModules :: !(HashedBufferedRef' (ModulesHash pv) Modules.Modules),
      bspBank :: !(Hashed Rewards.BankStatus),
      bspIdentityProviders :: !(HashedBufferedRef IPS.IdentityProviders),
      bspAnonymityRevokers :: !(HashedBufferedRef ARS.AnonymityRevokers),
      bspBirkParameters :: !(PersistentBirkParameters pv),
      bspCryptographicParameters :: !(HashedBufferedRef CryptographicParameters),
      bspUpdates :: !(BufferedRef (Updates pv)),
      bspReleaseSchedule :: !(ReleaseSchedule pv),
      bspAccountsInCooldown :: !(AccountsInCooldownForPV pv),
      bspTransactionOutcomes :: !(PersistentTransactionOutcomes (TransactionOutcomesVersionFor pv)),
      -- | Details of bakers that baked blocks in the current epoch. This is
      --  used for rewarding bakers at the end of epochs.
      bspRewardDetails :: !(BlockRewardDetails pv),
      -- | The global state of protocol-level tokens.
      bspProtocolLevelTokens :: !(ProtocolLevelTokensForPV pv)
    }

-- | Lens for accessing the birk parameters of a 'BlockStatePointers' structure.
birkParameters :: Lens' (BlockStatePointers pv) (PersistentBirkParameters pv)
birkParameters = lens bspBirkParameters (\bsp bp -> bsp{bspBirkParameters = bp})

-- | A hashed version of 'PersistingBlockState'.  This is used when the block state
--  is not being mutated so that the hash values are not recomputed constantly.
data HashedPersistentBlockState pv = HashedPersistentBlockState
    { hpbsPointers :: !(PersistentBlockState pv),
      hpbsHash :: !StateHash
    }

instance HashableTo StateHash (HashedPersistentBlockState pv) where
    getHash = hpbsHash

instance (Monad m) => MHashableTo m StateHash (HashedPersistentBlockState pv)

-- | Constraint for ensuring that @m@ supports both persistent accounts and persistent modules.
type SupportsPersistentState pv m = (MonadProtocolVersion m, MPV m ~ pv, SupportsPersistentAccount pv m, Modules.SupportsPersistentModule m)

-- | Convert a 'PersistentBlockState' to a 'HashedPersistentBlockState' by computing
--  the state hash.
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
        bshProtocolLevelTokens <- getHashM bspProtocolLevelTokens
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
        (pAccountsInCooldown, bspAccountInCooldown') <- storeUpdate bspAccountsInCooldown
        (pRewardDetails, bspRewardDetails') <- storeUpdate bspRewardDetails
        (pProtocolLevelTokens, bspProtocolLevelTokens') <- storeUpdate bspProtocolLevelTokens
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
                pAccountsInCooldown
                pRewardDetails
                pProtocolLevelTokens
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
                  bspAccountsInCooldown = bspAccountInCooldown',
                  bspRewardDetails = bspRewardDetails',
                  bspProtocolLevelTokens = bspProtocolLevelTokens'
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
        mAccountsInCooldown <- label "Accounts in cooldown" load
        mRewardDetails <- label "Epoch blocks" load
        mProtocolLevelTokens <- label "Protocol-level tokens" load
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
            bspAccountsInCooldown <- mAccountsInCooldown
            bspRewardDetails <- mRewardDetails
            bspProtocolLevelTokens <- mProtocolLevelTokens
            return $! BlockStatePointers{..}

-- | Accessor for getting the pool rewards when supported by the protocol version.
bspPoolRewards ::
    (PVSupportsDelegation pv, bhv ~ BlockHashVersionFor pv) =>
    BlockStatePointers pv ->
    HashedBufferedRef' (Rewards.PoolRewardsHash bhv) (PoolRewards bhv (AccountVersionFor pv))
bspPoolRewards bsp = case bspRewardDetails bsp of
    BlockRewardDetailsV1 pr -> pr

-- | An initial 'HashedPersistentBlockState', which may be used for testing purposes.
-- This assumes that among the initial accounts, none are in (pre)*cooldown.
{-# WARNING initialPersistentState "should only be used for testing" #-}
initialPersistentState ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    SeedState (SeedStateVersionFor pv) ->
    CryptographicParameters ->
    [PersistentAccount (AccountVersionFor pv)] ->
    IPS.IdentityProviders ->
    ARS.AnonymityRevokers ->
    UpdateKeysCollection (AuthorizationsVersionForPV pv) ->
    ChainParameters pv ->
    m (HashedPersistentBlockState pv)
initialPersistentState seedState cryptoParams accounts ips ars keysCollection chainParams = do
    persistentBirkParameters <- initialBirkParameters accounts seedState (chainParams ^. cpFinalizationCommitteeParameters)
    modules <- refMake =<< Modules.emptyModules
    identityProviders <- bufferHashed $ makeHashed ips
    anonymityRevokers <- bufferHashed $ makeHashed ars
    cryptographicParameters <- bufferHashed $ makeHashed cryptoParams
    blockAccounts <- Accounts.fromList accounts
    initialAmount <- foldM (\sumSoFar account -> (+ sumSoFar) <$> accountAmount account) 0 accounts
    updates <- refMake =<< initialUpdates keysCollection chainParams
    releaseSchedule <- emptyReleaseSchedule
    acctsInCooldown <- initialAccountsInCooldown accounts
    red <- emptyBlockRewardDetails
    plts <- emptyProtocolLevelTokensForPV
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
                  bspAccountsInCooldown = acctsInCooldown,
                  bspRewardDetails = red,
                  bspProtocolLevelTokens = plts
                }
    bps <- liftIO $ newIORef $! bsp
    hashBlockState bps

-- | A mostly empty block state, but with the given birk parameters,
--  cryptographic parameters, update authorizations and chain parameters.
emptyBlockState ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBirkParameters pv ->
    CryptographicParameters ->
    UpdateKeysCollection (AuthorizationsVersionForPV pv) ->
    ChainParameters pv ->
    m (PersistentBlockState pv)
{-# WARNING emptyBlockState "should only be used for testing" #-}
emptyBlockState bspBirkParameters cryptParams keysCollection chainParams = do
    modules <- refMake =<< Modules.emptyModules
    identityProviders <- refMake IPS.emptyIdentityProviders
    anonymityRevokers <- refMake ARS.emptyAnonymityRevokers
    cryptographicParameters <- refMake cryptParams
    bspUpdates <- refMake =<< initialUpdates keysCollection chainParams
    bspReleaseSchedule <- emptyReleaseSchedule
    bspRewardDetails <- emptyBlockRewardDetails
    bspAccounts <- Accounts.emptyAccounts
    bspProtocolLevelTokens <- emptyProtocolLevelTokensForPV
    bsp <-
        makeBufferedRef $
            BlockStatePointers
                { bspAccounts = bspAccounts,
                  bspInstances = Instances.emptyInstances,
                  bspModules = modules,
                  bspBank = makeHashed Rewards.emptyBankStatus,
                  bspIdentityProviders = identityProviders,
                  bspAnonymityRevokers = anonymityRevokers,
                  bspCryptographicParameters = cryptographicParameters,
                  bspAccountsInCooldown = emptyAccountsInCooldownForPV,
                  bspTransactionOutcomes = emptyTransactionOutcomes (Proxy @pv),
                  ..
                }
    liftIO $ newIORef $! bsp

-- | Load 'BlockStatePointers' from a 'PersistentBlockState'.
loadPBS :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (BlockStatePointers pv)
loadPBS = loadBufferedRef <=< liftIO . readIORef
{-# INLINE loadPBS #-}

-- | Update the 'BlockStatePointers' stored in a 'PersistentBlockState'.
storePBS :: (SupportsPersistentAccount pv m) => PersistentBlockState pv -> BlockStatePointers pv -> m (PersistentBlockState pv)
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

-- | Look up an account by index and run an operation on it.
--  This returns 'Nothing' if the account is not present or the operation returns 'Nothing'.
onAccount ::
    (SupportsPersistentAccount pv m) =>
    -- | Account index to resolve
    AccountIndex ->
    -- | Block state
    BlockStatePointers pv ->
    -- | Operation to apply to the account
    (PersistentAccount (AccountVersionFor pv) -> m (Maybe a)) ->
    m (Maybe a)
onAccount ai bsp f =
    Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
        Nothing -> return Nothing
        Just acc -> f acc

-- | Look up an account by index and run an operation on it.
--  This returns 'Nothing' if the account is not present.
onAccount' ::
    (SupportsPersistentAccount pv m) =>
    -- | Account index to resolve
    AccountIndex ->
    -- | Block state
    BlockStatePointers pv ->
    -- | Operation to apply to the account
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
    Modules.moduleRefList mods

-- | Get the size of the module table.
doGetModuleCount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m Word64
doGetModuleCount s = do
    bsp <- loadPBS s
    Modules.moduleCount <$> refLoad (bspModules bsp)

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

doGetSeedState ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m (SeedState (SeedStateVersionFor pv))
doGetSeedState pbs = _birkSeedState . bspBirkParameters <$> loadPBS pbs

doSetSeedState ::
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    SeedState (SeedStateVersionFor pv) ->
    m (PersistentBlockState pv)
doSetSeedState pbs ss = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBirkParameters = (bspBirkParameters bsp){_birkSeedState = ss}}

doGetCurrentEpochFinalizationCommitteeParameters ::
    ( SupportsPersistentState pv m,
      IsSupported 'PTFinalizationCommitteeParameters (ChainParametersVersionFor pv) ~ 'True
    ) =>
    PersistentBlockState pv ->
    m FinalizationCommitteeParameters
doGetCurrentEpochFinalizationCommitteeParameters pbs = do
    eb <- refLoad . _birkCurrentEpochBakers . bspBirkParameters =<< loadPBS pbs
    return $! eb ^. bakerFinalizationCommitteeParameters . supportedOParam

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

doGetNextEpochFinalizationCommitteeParameters ::
    ( SupportsPersistentState pv m,
      IsSupported 'PTFinalizationCommitteeParameters (ChainParametersVersionFor pv) ~ 'True
    ) =>
    PersistentBlockState pv ->
    m FinalizationCommitteeParameters
doGetNextEpochFinalizationCommitteeParameters pbs = do
    eb <- refLoad . _birkNextEpochBakers . bspBirkParameters =<< loadPBS pbs
    return $! eb ^. bakerFinalizationCommitteeParameters . supportedOParam

doGetNextEpochBakers :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m FullBakers
doGetNextEpochBakers pbs = do
    bsp <- loadPBS pbs
    epochToFullBakers =<< refLoad (bspBirkParameters bsp ^. birkNextEpochBakers)

doGetSlotBakersP1 ::
    ( AccountVersionFor pv ~ 'AccountV0,
      SeedStateVersionFor pv ~ 'SeedStateVersion0,
      SupportsPersistentState pv m
    ) =>
    PersistentBlockState pv ->
    Slot ->
    m FullBakers
doGetSlotBakersP1 pbs slot = do
    bs <- loadPBS pbs
    let bps = bspBirkParameters bs
        SeedStateV0{..} = bps ^. birkSeedState
        slotEpoch = fromIntegral $ slot `quot` ss0EpochLength
    case compare slotEpoch (ss0Epoch + 1) of
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
        _bakerFinalizationCommitteeParameters = case protocolVersion @pv of
            SP1 -> NoParam
            SP2 -> NoParam
            SP3 -> NoParam
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
                          activeBakerDelegators = abd,
                          activeBakerIsSuspended =
                            fromCondDef
                                ( BaseAccounts._bieIsSuspended $
                                    BaseAccounts._accountBakerInfo $
                                        theBaker
                                )
                                False,
                          activeBakerId =
                            BaseAccounts._bakerIdentity $
                                BaseAccounts._bieBakerInfo $
                                    BaseAccounts._accountBakerInfo $
                                        theBaker
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

-- | Get the registered delegators of a pool. Changes are reflected immediately here and will be effective in the next reward period.
--  The baker id is used to identify the pool and Nothing is used for the passive delegators.
--  Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
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

-- | Get the delegators of a pool for the reward period. Changes are not reflected here until the next reward period.
--  The baker id is used to identify the pool and Nothing is used for the passive delegators.
--  Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
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
            -- Account is already a baker. (NB: cannot be a delegator at AccountV0.)
            | accountHasActiveStake acct -> return (BAAlreadyBaker (BakerId ai), pbs)
            -- Account is not a baker
            | otherwise -> do
                cp <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> lookupCurrentParameters (bspUpdates bsp)
                if baStake < max 1 cp
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

-- | Update an account's delegation to passive delegation. This only updates the account table,
--  and does not update the active baker index, which must be handled separately.
--  The account __must__ be an active delegator.
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

-- | Check the conditions required for successfully adding a validator.
--  This function does not modify the block state.
--
--  The function behaves as follows:
--
--  1. If the baker's capital is 0, or less than the minimum threshold, throw
--     'VCFStakeUnderThreshold'.
--  2. If the transaction fee commission is not in the acceptable range, throw
--     'VCFTransactionFeeCommissionNotInRange'.
--  3. If the baking reward commission is not in the acceptable range, throw
--     'VCFBakingRewardCommissionNotInRange'.
--  4. If the finalization reward commission is not in the acceptable range, throw
--     'VCFFinalizationRewardCommissionNotInRange'.
--  5. If the aggregation key is a duplicate, throw 'VCFDuplicateAggregationKey'.
addValidatorChecks ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    BlockStatePointers pv ->
    ValidatorAdd ->
    MTL.ExceptT ValidatorConfigureFailure m ()
addValidatorChecks bsp ValidatorAdd{..} = do
    chainParams <- lookupCurrentParameters (bspUpdates bsp)
    let
        poolParams = chainParams ^. cpPoolParameters
        capitalMin = poolParams ^. ppMinimumEquityCapital
        ranges = poolParams ^. ppCommissionBounds
    -- Check if the equity capital is below the minimum threshold.
    when (vaCapital < max 1 capitalMin) $ MTL.throwError VCFStakeUnderThreshold
    -- Check if the transaction fee commission rate is in the acceptable range.
    unless
        ( isInRange
            (vaCommissionRates ^. transactionCommission)
            (ranges ^. transactionCommissionRange)
        )
        $ MTL.throwError VCFTransactionFeeCommissionNotInRange
    -- Check if the baking reward commission rate is in the acceptable range.
    unless
        ( isInRange
            (vaCommissionRates ^. bakingCommission)
            (ranges ^. bakingCommissionRange)
        )
        $ MTL.throwError VCFBakingRewardCommissionNotInRange
    -- Check if the finalization reward commission rate is in the acceptable range.
    unless
        ( isInRange
            (vaCommissionRates ^. finalizationCommission)
            (ranges ^. finalizationCommissionRange)
        )
        $ MTL.throwError VCFFinalizationRewardCommissionNotInRange
    -- Check if the aggregation key is fresh.
    pab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    existingAggKey <- isJust <$> Trie.lookup (bkuAggregationKey vaKeys) (pab ^. aggregationKeys)
    when existingAggKey $
        MTL.throwError (VCFDuplicateAggregationKey (bkuAggregationKey vaKeys))

-- | From chain parameters version >= 1, this adds a validator for an account. This is used to
--  implement 'bsoAddValidator'.
--
--  PRECONDITIONS:
--
--  * the account is valid;
--  * the account is not a baker;
--  * the account is not a delegator;
--  * the account has sufficient balance to cover the stake.
--
--  The function behaves as follows:
--
--  1. If the baker's capital is 0, or less than the minimum threshold, return
--     'VCFStakeUnderThreshold'.
--  2. If the transaction fee commission is not in the acceptable range, return
--     'VCFTransactionFeeCommissionNotInRange'.
--  3. If the baking reward commission is not in the acceptable range, return
--     'VCFBakingRewardCommissionNotInRange'.
--  4. If the finalization reward commission is not in the acceptable range, return
--     'VCFFinalizationRewardCommissionNotInRange'.
--  5. If the aggregation key is a duplicate, return 'VCFDuplicateAggregationKey'.
--  6. Add the baker to the account. If flexible cooldowns are supported by the protocol
--     version, then the capital in cooldown is reactivated. The indexes are updated as follows:
--
--       * add an empty pool for the baker in the active bakers;
--       * add the baker's equity capital to the total active capital;
--       * add the baker's aggregation key to the aggregation key set;
--       * the cooldown indexes are updated to reflect any reactivation of capital.
--
--  7. Return the updated block state.
newAddValidator ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState (MPV m) ->
    AccountIndex ->
    ValidatorAdd ->
    MTL.ExceptT ValidatorConfigureFailure m (PersistentBlockState (MPV m))
newAddValidator pbs ai va@ValidatorAdd{..} = do
    bsp <- loadPBS pbs
    addValidatorChecks bsp va
    newBirkParams <- do
        pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        newAggregationKeys <- Trie.insert (bkuAggregationKey vaKeys) () (pab ^. aggregationKeys)
        newActiveBakers <- Trie.insert bid emptyPersistentActiveDelegators (pab ^. activeBakers)
        newPABref <-
            refMake $
                pab
                    & aggregationKeys .~ newAggregationKeys
                    & activeBakers .~ newActiveBakers
                    & totalActiveCapital %~ addActiveCapital vaCapital
        return $ bspBirkParameters bsp & birkActiveBakers .~ newPABref
    let poolInfo =
            BaseAccounts.BakerPoolInfo
                { _poolOpenStatus = vaOpenForDelegation,
                  _poolMetadataUrl = vaMetadataURL,
                  _poolCommissionRates = vaCommissionRates
                }
    let bakerInfo = bakerKeyUpdateToInfo bid vaKeys
    let bakerInfoEx =
            BaseAccounts.BakerInfoExV1
                { _bieBakerPoolInfo = poolInfo,
                  _bieBakerInfo = bakerInfo,
                  _bieIsSuspended = conditionally hasValidatorSuspension vaSuspended
                }
    -- The precondition guaranties that the account exists
    acc <- fromJust <$> Accounts.indexedAccount ai (bspAccounts bsp)
    -- Add the baker to the account.
    accWithBaker <- addAccountBakerV1 bakerInfoEx vaCapital vaRestakeEarnings acc
    (accUpdated, newAIC) <- case flexibleCooldowns of
        SFalse -> return (accWithBaker, bspAccountsInCooldown bsp)
        STrue -> do
            -- Reactivate stake in cooldown to cover the new stake.
            oldCooldowns <- accountCooldowns accWithBaker
            accUpdated <- reactivateCooldownAmount vaCapital accWithBaker
            newCooldowns <- accountCooldowns accUpdated
            let removals = cooldownRemovals oldCooldowns newCooldowns
            newAIC <- applyCooldownRemovalsGlobally ai removals (bspAccountsInCooldown bsp)
            return (accUpdated, newAIC)
    newAccounts <- Accounts.setAccountAtIndex ai accUpdated (bspAccounts bsp)
    storePBS pbs $
        bsp
            { bspBirkParameters = newBirkParams,
              bspAccounts = newAccounts,
              bspAccountsInCooldown = newAIC
            }
  where
    bid = BakerId ai
    flexibleCooldowns = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))
    hasValidatorSuspension = sSupportsValidatorSuspension (accountVersion @(AccountVersionFor pv))

-- | Check the conditions required for successfully updating a validator. This does not modify
--  the block state.
--
--  1. If keys are supplied: if the aggregation key duplicates an existing aggregation key @key@
--     (except the accounts's current aggregation key), throw @VCFDuplicateAggregationKey key@.
--
--  2. If the transaction fee commission is supplied, and the commission does not fall within the
--     current range according to the chain parameters, throw
--     @VCFTransactionFeeCommissionNotInRange@.
--
--  3. If the baking reward commission is supplied, and the commission does not fall within the
--     current range according to the chain parameters, throw @VCFBakingRewardCommissionNotInRange@.
--
--  4. If the finalization reward commission is supplied, and the commission does not fall within
--     the current range according to the chain parameters, throw
--     @VCFFinalizationRewardCommissionNotInRange@.
--
--  5. If the capital is supplied:
--
--       * If there is a pending change to the baker's capital, throw @VCFChangePending@.
--
--       * If the capital is non-zero, and less than the current minimum equity capital, throw
--         @BCStakeUnderThreshold@.
updateValidatorChecks ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    BlockStatePointers pv ->
    -- | The current baker on the account being updated
    AccountBaker (AccountVersionFor pv) ->
    ValidatorUpdate ->
    MTL.ExceptT ValidatorConfigureFailure m ()
updateValidatorChecks bsp baker ValidatorUpdate{..} = do
    chainParams <- lookupCurrentParameters (bspUpdates bsp)
    let poolParams = chainParams ^. cpPoolParameters
        capitalMin = poolParams ^. ppMinimumEquityCapital
        ranges = poolParams ^. ppCommissionBounds
    -- Check if the aggregation key is fresh (or the same as the baker's existing one).
    forM_ vuKeys $ \BakerKeyUpdate{..} ->
        when (baker ^. BaseAccounts.bakerAggregationVerifyKey /= bkuAggregationKey) $ do
            pab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
            existingAggKey <- isJust <$> Trie.lookup bkuAggregationKey (pab ^. aggregationKeys)
            when existingAggKey $ MTL.throwError (VCFDuplicateAggregationKey bkuAggregationKey)
    -- Check if the transaction fee commission rate is in the acceptable range.
    forM_ vuTransactionFeeCommission $ \tfc ->
        unless (isInRange tfc (ranges ^. transactionCommissionRange)) $
            MTL.throwError VCFTransactionFeeCommissionNotInRange
    -- Check if the baking reward commission rate is in the acceptable range.
    forM_ vuBakingRewardCommission $ \brc ->
        unless (isInRange brc (ranges ^. bakingCommissionRange)) $
            MTL.throwError VCFBakingRewardCommissionNotInRange
    -- Check if the finalization reward commission rate is in the acceptable range.
    forM_ vuFinalizationRewardCommission $ \frc ->
        unless (isInRange frc (ranges ^. finalizationCommissionRange)) $
            MTL.throwError VCFFinalizationRewardCommissionNotInRange
    forM_ vuCapital $ \capital -> do
        -- Check that there is no pending change on the account already.
        when (baker ^. BaseAccounts.bakerPendingChange /= BaseAccounts.NoChange) $
            MTL.throwError VCFChangePending
        -- Check that the baker's equity capital is above the minimum threshold, unless it
        -- is being removed.
        when (capital /= 0 && capital < capitalMin) $
            MTL.throwError VCFStakeUnderThreshold

-- | Update the validator for an account.
--
--  PRECONDITIONS:
--
--  * the account is valid;
--  * the account is a baker;
--  * if the stake is being updated, then the account balance is at least the new stake.
--
--  The function behaves as follows, building a list @events@:
--
--  1. If keys are supplied: if the aggregation key duplicates an existing aggregation key @key@
--     (except the accounts's current aggregation key), return @VCFDuplicateAggregationKey key@;
--     otherwise, update the keys with the supplied @keys@, update the aggregation key index
--     (removing the old key and adding the new one), and append @BakerConfigureUpdateKeys keys@
--     to @events@.
--
--  2. If the restake earnings flag is supplied: update the account's flag to the supplied value
--     @restakeEarnings@ and append @BakerConfigureRestakeEarnings restakeEarnings@ to @events@.
--
--  3. If the open-for-delegation configuration is supplied:
--
--         (1) update the account's configuration to the supplied value @openForDelegation@;
--
--         (2) if @openForDelegation == ClosedForAll@, transfer all delegators in the baker's pool to
--             passive delegation; and
--
--         (3) append @BakerConfigureOpenForDelegation openForDelegation@ to @events@.
--
--  4. If the metadata URL is supplied: update the account's metadata URL to the supplied value
--     @metadataURL@ and append @BakerConfigureMetadataURL metadataURL@ to @events@.
--
--  5. If the transaction fee commission is supplied:
--
--        (1) if the commission does not fall within the current range according to the chain
--            parameters, return @VCFTransactionFeeCommissionNotInRange@; otherwise,
--
--        (2) update the account's transaction fee commission rate to the the supplied value @tfc@;
--
--        (3) append @BakerConfigureTransactionFeeCommission tfc@ to @events@.
--
--  6. If the baking reward commission is supplied:
--
--        (1) if the commission does not fall within the current range according to the chain
--            parameters, return @VCFBakingRewardCommissionNotInRange@; otherwise,
--
--        (2) update the account's baking reward commission rate to the the supplied value @brc@;
--
--        (3) append @BakerConfigureBakingRewardCommission brc@ to @events@.
--
--  7. If the finalization reward commission is supplied:
--
--        (1) if the commission does not fall within the current range according to the chain
--            parameters, return @VCFFinalizationRewardCommissionNotInRange@; otherwise,
--
--        (2) update the account's finalization reward commission rate to the the supplied value @frc@;
--
--        (3) append @BakerConfigureFinalizationRewardCommission frc@ to @events@.
--
--  8. (>= P8) If the suspended/resumed flag is set:

--        (1) Suspend/resume the validator according to the flag.

--        (2) Append @BakerConfigureSuspended@ or @BakerConfigureResumed@ accordingly to @events@.
--
--  9. If the capital is supplied: if there is a pending change to the baker's capital, return
--     @VCFChangePending@; otherwise:
--
--       * if the capital is 0
--
--           - (< P7) mark the baker as pending removal at @bcuSlotTimestamp@ plus the
--           the current baker cooldown period according to the chain parameters
--
--           - (>= P7) transfer the existing staked capital to pre-pre-cooldown, and mark the
--           account as in pre-pre-cooldown (in the global index) if it wasn't already, and
--           update the active bakers index to reflect the change, including removing the baker's
--           aggregation key from the in-use set
--
--           - append @BakerConfigureStakeReduced 0@ to @events@;
--
--       * if the capital is less than the current minimum equity capital, return @BCStakeUnderThreshold@;
--
--       * if the capital is (otherwise) less than the current equity capital of the baker
--
--           - (< P7) mark the baker as pending stake reduction to the new capital at
--             @bcuSlotTimestamp@ plus the current baker cooldown period according to the chain
--             parameters
--
--           - (>= P7) transfer the decrease in staked capital to pre-pre-cooldown, mark the
--             account as in pre-pre-cooldown (in the global index) if it wasn't already, and
--             update the active bakers index to reflect the change
--
--           - append @BakerConfigureStakeReduced capital@ to @events@;
--
--       * if the capital is equal to the baker's current equity capital, do nothing, append
--         @BakerConfigureStakeIncreased capital@ to @events@;
--
--       * if the capital is greater than the baker's current equity capital, increase the baker's
--         equity capital to the new capital (updating the total active capital in the active baker
--         index by adding the difference between the new and old capital) and append
--         @BakerConfigureStakeIncreased capital@ to @events@.

--  10. Return @events@ with the updated block state.
newUpdateValidator ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState (MPV m) ->
    -- | Current block timestamp
    Timestamp ->
    AccountIndex ->
    ValidatorUpdate ->
    MTL.ExceptT ValidatorConfigureFailure m ([BakerConfigureUpdateChange], PersistentBlockState (MPV m))
newUpdateValidator pbs curTimestamp ai vu@ValidatorUpdate{..} = do
    bsp <- loadPBS pbs
    -- Cannot fail: The precondition guaranties that the account exists
    acc <- fromJust <$> Accounts.indexedAccount ai (bspAccounts bsp)
    -- Cannot fail: account must be a registered baker.
    existingBaker <- fromJust <$> accountBaker acc
    updateValidatorChecks bsp existingBaker vu
    (newBSP, events) <- lift . MTL.runWriterT $ do
        (newBSP, newAcc) <-
            updateKeys existingBaker (bsp, acc)
                >>= updateRestakeEarnings
                >>= updatePoolInfo existingBaker
                -- NOTE: updateSuspend needs to be executed before updateCapital.
                -- Because if we update the stake to 0, the validator gets
                -- removed. After this, a call to updateSuspend will error.
                >>= updateSuspend
                >>= updateCapital existingBaker
        newAccounts <- Accounts.setAccountAtIndex ai newAcc (bspAccounts newBSP)
        return newBSP{bspAccounts = newAccounts}
    (events,) <$> storePBS pbs newBSP
  where
    bid = BakerId ai
    -- Only do the given update if specified.
    ifPresent Nothing _ = return
    ifPresent (Just v) k = k v
    updateSuspend =
        ifPresent vuSuspend $ \suspend (bsp, acc) -> do
            case sSupportsValidatorSuspension (accountVersion @(AccountVersionFor pv)) of
                STrue -> do
                    acc1 <- setAccountValidatorSuspended suspend acc
                    MTL.tell
                        [ if suspend
                            then BakerConfigureSuspended
                            else BakerConfigureResumed
                        ]
                    return (bsp, acc1)
                SFalse -> return (bsp, acc)
    updateKeys oldBaker = ifPresent vuKeys $ \keys (bsp, acc) -> do
        let oldAggrKey = oldBaker ^. BaseAccounts.bakerAggregationVerifyKey
        bsp1 <-
            if bkuAggregationKey keys == oldAggrKey
                then return bsp
                else do
                    pab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
                    newAggregationKeys <-
                        Trie.insert (bkuAggregationKey keys) ()
                            =<< Trie.delete oldAggrKey (pab ^. aggregationKeys)
                    newPABref <- refMake $ pab & aggregationKeys .~ newAggregationKeys
                    return $
                        bsp{bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newPABref}
        acc1 <- setAccountBakerKeys keys acc
        MTL.tell [BakerConfigureUpdateKeys keys]
        return (bsp1, acc1)
    updateRestakeEarnings = ifPresent vuRestakeEarnings $ \restakeEarnings (bsp, acc) -> do
        acc1 <- setAccountRestakeEarnings restakeEarnings acc
        MTL.tell [BakerConfigureRestakeEarnings restakeEarnings]
        return (bsp, acc1)
    updatePoolInfo oldBaker (bsp0, acc) = do
        let pu0 = emptyBakerPoolInfoUpdate
        (bsp1, pu1) <-
            updateOpenForDelegation oldBaker (bsp0, pu0)
                >>= updateMetadataURL oldBaker
                >>= updateTransactionFeeCommission oldBaker
                >>= updateBakingRewardCommission oldBaker
                >>= updateFinalizationRewardCommission oldBaker
        acc1 <- updateAccountBakerPoolInfo pu1 acc
        return (bsp1, acc1)
    updateOpenForDelegation oldBaker = ifPresent vuOpenForDelegation $ \openForDelegation (bsp, pu) -> do
        MTL.tell [BakerConfigureOpenForDelegation openForDelegation]
        if oldBaker ^. BaseAccounts.poolOpenStatus == openForDelegation
            then return (bsp, pu)
            else do
                bsp1 <-
                    if openForDelegation == Transactions.ClosedForAll
                        then moveDelegatorsToPassive bsp Nothing
                        else return bsp
                return (bsp1, pu{updOpenForDelegation = Just openForDelegation})
    updateMetadataURL oldBaker = ifPresent vuMetadataURL $ \metadataUrl (bsp, pu) -> do
        MTL.tell [BakerConfigureMetadataURL metadataUrl]
        if oldBaker ^. BaseAccounts.poolMetadataUrl == metadataUrl
            then return (bsp, pu)
            else return (bsp, pu{updMetadataURL = Just metadataUrl})
    updateTransactionFeeCommission oldBaker =
        ifPresent vuTransactionFeeCommission $ \tfc (bsp, pu) -> do
            MTL.tell [BakerConfigureTransactionFeeCommission tfc]
            if oldBaker ^. BaseAccounts.poolCommissionRates . transactionCommission == tfc
                then return (bsp, pu)
                else return (bsp, pu{updTransactionFeeCommission = Just tfc})
    updateBakingRewardCommission oldBaker =
        ifPresent vuBakingRewardCommission $ \brc (bsp, pu) -> do
            MTL.tell [BakerConfigureBakingRewardCommission brc]
            if oldBaker ^. BaseAccounts.poolCommissionRates . bakingCommission == brc
                then return (bsp, pu)
                else return (bsp, pu{updBakingRewardCommission = Just brc})
    updateFinalizationRewardCommission oldBaker =
        ifPresent vuFinalizationRewardCommission $ \frc (bsp, pu) -> do
            MTL.tell [BakerConfigureFinalizationRewardCommission frc]
            if oldBaker ^. BaseAccounts.poolCommissionRates . finalizationCommission == frc
                then return (bsp, pu)
                else return (bsp, pu{updFinalizationRewardCommission = Just frc})
    updateCapital = updateCapital' (sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)))
    updateCapital' SFalse oldBaker = ifPresent vuCapital $ \capital (bsp, acc) -> do
        -- No flexible cooldowns. Reducing stake creates a pending change.
        cp <- lookupCurrentParameters (bspUpdates bsp)
        let cooldownDuration = cp ^. cpCooldownParameters . cpPoolOwnerCooldown
            cooldownElapsed =
                BaseAccounts.PendingChangeEffectiveV1 $
                    addDurationSeconds curTimestamp cooldownDuration
        let oldCapital = oldBaker ^. BaseAccounts.stakedAmount
        if capital == 0
            then do
                -- Validator is being removed. (Removal occurs after cooldown.)
                MTL.tell [BakerConfigureStakeReduced capital]
                let bpc = BaseAccounts.RemoveStake cooldownElapsed
                (bsp,) <$> setAccountStakePendingChange bpc acc
            else case compare capital oldCapital of
                LT -> do
                    -- Stake reduced.
                    MTL.tell [BakerConfigureStakeReduced capital]
                    let bpc = BaseAccounts.ReduceStake capital cooldownElapsed
                    (bsp,) <$> setAccountStakePendingChange bpc acc
                EQ -> do
                    -- Stake unchanged: record as if increased.
                    MTL.tell [BakerConfigureStakeIncreased capital]
                    return (bsp, acc)
                GT -> do
                    -- Stake increased
                    MTL.tell [BakerConfigureStakeIncreased capital]
                    bsp1 <-
                        modifyActiveCapital
                            (addActiveCapital $ capital - oldCapital)
                            bsp
                    acc1 <- setAccountStake capital acc
                    return (bsp1, acc1)
    updateCapital' STrue oldBaker = ifPresent vuCapital $ \capital (bsp, acc) -> do
        -- Flexible cooldowns. Reducing stake goes into cooldown, and increasing stake reactivates
        -- stake from cooldown.
        let oldCapital = oldBaker ^. BaseAccounts.stakedAmount
        if capital == 0
            then do
                MTL.tell [BakerConfigureStakeReduced 0]
                alreadyInPrePreCooldown <- accountHasPrePreCooldown acc
                acc1 <- removeAccountStaking acc >>= addAccountPrePreCooldown oldCapital
                let oldKeys =
                        maybe
                            (oldBaker ^. BaseAccounts.bakerAggregationVerifyKey)
                            bkuAggregationKey
                            vuKeys
                bsp1 <- moveDelegatorsToPassive bsp (Just (oldCapital, oldKeys))
                bsp2 <- (if alreadyInPrePreCooldown then return else addToPrePreCooldowns) bsp1
                return (bsp2, acc1)
            else case compare capital oldCapital of
                LT -> do
                    MTL.tell [BakerConfigureStakeReduced capital]
                    alreadyInPrePreCooldown <- accountHasPrePreCooldown acc
                    acc1 <-
                        setAccountStake capital acc
                            >>= addAccountPrePreCooldown (oldCapital - capital)
                    bsp1 <- modifyActiveCapital (subtractActiveCapital $ oldCapital - capital) bsp
                    bsp2 <- (if alreadyInPrePreCooldown then return else addToPrePreCooldowns) bsp1
                    return (bsp2, acc1)
                EQ -> do
                    MTL.tell [BakerConfigureStakeIncreased capital]
                    return (bsp, acc)
                GT -> do
                    MTL.tell [BakerConfigureStakeIncreased capital]
                    oldCooldowns <- accountCooldowns acc
                    acc1 <-
                        setAccountStake capital acc
                            >>= reactivateCooldownAmount (capital - oldCapital)
                    newCooldowns <- accountCooldowns acc1
                    let removals = cooldownRemovals oldCooldowns newCooldowns
                    bsp1 <- modifyActiveCapital (addActiveCapital $ capital - oldCapital) bsp
                    newAIC <- applyCooldownRemovalsGlobally ai removals (bspAccountsInCooldown bsp1)
                    let bsp2 = bsp1{bspAccountsInCooldown = newAIC}
                    return (bsp2, acc1)
    -- Move all @bid@'s current delegators into passive delegation.
    -- If the amount (the baker's prior stake) and key (the bakers prior aggregation key) are
    -- specified, then @bid@ is removed from the active bakers, the total active capital is reduced
    -- accordingly, and the aggregation key is removed from the set of keys in use.
    moveDelegatorsToPassive bsp mAmountAndKey = do
        pab0 <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        (delegators, pab1) <- transferDelegatorsToPassive bid pab0
        pab2 <- case mAmountAndKey of
            Nothing -> return pab1
            Just (amount, aggKey) -> do
                newActiveBakers <- Trie.delete bid (pab1 ^. activeBakers)
                newAggregationKeys <- Trie.delete aggKey (pab1 ^. aggregationKeys)
                return $
                    pab1
                        & totalActiveCapital %~ subtractActiveCapital amount
                        & activeBakers .~ newActiveBakers
                        & aggregationKeys .~ newAggregationKeys
        newPABref <- refMake pab2
        let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newPABref
        newAccounts <- foldM redelegatePassive (bspAccounts bsp) delegators
        return bsp{bspBirkParameters = newBirkParams, bspAccounts = newAccounts}
    modifyActiveCapital upd bsp = do
        pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        newPABref <- refMake $ pab & totalActiveCapital %~ upd
        return bsp{bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newPABref}
    addToPrePreCooldowns ::
        (MonadBlobStore m', PVSupportsFlexibleCooldown pv) =>
        BlockStatePointers pv ->
        m' (BlockStatePointers pv)
    addToPrePreCooldowns bsp = do
        -- Add the account to the pre-pre-cooldowns list.
        newAccountsInCooldown <-
            (accountsInCooldown . prePreCooldown)
                (consAccountList ai)
                (bspAccountsInCooldown bsp)
        return bsp{bspAccountsInCooldown = newAccountsInCooldown}

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

-- | Check the conditions required to successfully add a delegator to an account:
--
--      * the delegation target is passive delegation; or
--
--      * the delegation target is a baker (otherwise, throw 'DCFInvalidDelegationTarget') and:
--
--           - the baker's pool is open for all (otherwise, throw 'DCFPoolClosed'),
--
--           - the delegation would not put the pool over the leverage bound (otherwise, throw
--             'DCFPoolStakeOverThreshold'), and
--
--           - the delegation would not put the pool over the capital bound (otherwise, throw
--             'DCFPoolOverDelegated').
addDelegatorChecks ::
    ( IsProtocolVersion pv,
      PVSupportsDelegation pv,
      MTL.MonadError DelegatorConfigureFailure m,
      SupportsPersistentAccount pv m,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    BlockStatePointers pv ->
    DelegatorAdd ->
    m ()
addDelegatorChecks _ DelegatorAdd{daDelegationTarget = Transactions.DelegatePassive} = return ()
addDelegatorChecks bsp DelegatorAdd{daDelegationTarget = Transactions.DelegateToBaker bid, ..} = do
    onAccount baid bsp accountBaker >>= \case
        Nothing -> MTL.throwError (DCFInvalidDelegationTarget bid)
        Just baker -> do
            unless (baker ^. BaseAccounts.poolOpenStatus == Transactions.OpenForAll) $
                MTL.throwError DCFPoolClosed
            poolParams <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
            let bakerEquityCapital = baker ^. BaseAccounts.stakedAmount
            bakerDelegatedCapital <- (daCapital +) <$> poolDelegatorCapital bsp bid
            capitalTotal <- (daCapital +) <$> totalCapital bsp
            let PoolCaps{..} = delegatedCapitalCaps poolParams capitalTotal bakerEquityCapital bakerDelegatedCapital
            when (bakerDelegatedCapital > leverageCap) $ MTL.throwError DCFPoolStakeOverThreshold
            when (bakerDelegatedCapital > boundCap) $ MTL.throwError DCFPoolOverDelegated
  where
    BakerId baid = bid

-- | From chain parameters version >= 1, this operation is used to add a delegator.
--  When adding delegator, it is assumed that 'AccountIndex' account is NOT a baker and NOT a delegator.
--
--  PRECONDITIONS:
--
--  * the account is valid;
--  * the account is not a baker;
--  * the account is not a delegator;
--  * the delegated amount does not exceed the account's balance;
--  * the delegated stake is > 0.
--
--  The function behaves as follows:
--
--  1. If the delegation target is a valid baker that is not 'OpenForAll', return 'DCFPoolClosed'.
--
--  2. If the delegation target is baker id @bid@, but the baker does not exist, return
--     @DCFInvalidDelegationTarget bid@.
--
--  3. Update the active bakers index to record:
--
--       * the delegator delegates to the target pool;
--       * the target pool's delegated capital is increased by the delegated amount;
--       * the total active capital is increased by the delegated amount.
--
--  4. Update the account to record the specified delegation.
--
--  5. If the amount delegated to the delegation target exceeds the leverage bound, return
--     'DCFPoolStakeOverThreshold' and revert any changes.
--
--  6. If the amount delegated to the delegation target exceed the capital bound, return
--     'DCFPoolOverDelegated' and revert any changes.
--
--  7. Return the updated state.
newAddDelegator ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState (MPV m) ->
    AccountIndex ->
    DelegatorAdd ->
    MTL.ExceptT DelegatorConfigureFailure m (PersistentBlockState (MPV m))
newAddDelegator pbs ai da@DelegatorAdd{..} = do
    bsp <- loadPBS pbs
    addDelegatorChecks bsp da
    newBirkParameters <- do
        pab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
        -- Cannot fail: the delegation target is valid because it is checked in 'addDelegatorChecks'.
        newActiveBakers <-
            addDelegatorUnsafe daDelegationTarget did daCapital pab
                <&> totalActiveCapital %~ addActiveCapital daCapital
        newPABRef <- refMake newActiveBakers
        return $ bspBirkParameters bsp & birkActiveBakers .~ newPABRef
    case flexibleCooldown of
        SFalse -> do
            newAccounts <-
                Accounts.updateAccountsAtIndex'
                    (addAccountDelegator newDelegator)
                    ai
                    (bspAccounts bsp)
            storePBS pbs $
                bsp
                    { bspAccounts = newAccounts,
                      bspBirkParameters = newBirkParameters
                    }
        STrue -> do
            acc <- fromJust <$> Accounts.indexedAccount ai (bspAccounts bsp)
            maybeCooldownsBefore <- accountCooldowns acc
            newAcc <- (addAccountDelegator newDelegator >=> reactivateCooldownAmount daCapital) acc
            maybeCooldownsAfter <- accountCooldowns newAcc
            let removals = cooldownRemovals maybeCooldownsBefore maybeCooldownsAfter
            newCooldowns <- applyCooldownRemovalsGlobally ai removals (bspAccountsInCooldown bsp)
            newAccounts <- Accounts.setAccountAtIndex ai newAcc (bspAccounts bsp)
            storePBS pbs $
                bsp
                    { bspAccounts = newAccounts,
                      bspBirkParameters = newBirkParameters,
                      bspAccountsInCooldown = newCooldowns
                    }
  where
    did = DelegatorId ai
    flexibleCooldown = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))
    newDelegator :: BaseAccounts.AccountDelegation (AccountVersionFor pv)
    newDelegator =
        BaseAccounts.AccountDelegationV1
            { _delegationTarget = daDelegationTarget,
              _delegationStakedAmount = daCapital,
              _delegationStakeEarnings = daRestakeEarnings,
              _delegationPendingChange = BaseAccounts.NoChange,
              _delegationIdentity = did
            }

-- | Check the conditions required to successfully update a delegator.
--
--  1. If the delegation target is neither passive nor a valid baker, throw
--     'DCFInvalidDelegationTarget'.
--
--  2. If the delegation target is a valid baker that is different from the previous target, but
--     the pool is not open for all, throw 'DCFPoolClosed'.
--
--  3. If the delegated capital is specified and there is a pending change to the delegator's
--     stake, throw 'DCFChangePending'.
--
--  4. If the delegation target is being changed or the delegated capital is being increased:
--
--            * If the amount delegated to the delegation target would exceed the leverage bound,
--              throw 'DCFPoolStakeOverThreshold'.
--
--            * If the amount delegated to the delegation target would exceed the capital bound,
--              throw 'DCFPoolOverDelegated'.
updateDelegatorChecks ::
    forall pv m.
    ( IsProtocolVersion pv,
      PVSupportsDelegation pv,
      MTL.MonadError DelegatorConfigureFailure m,
      SupportsPersistentAccount pv m,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    BlockStatePointers pv ->
    -- | The current delegation status of the account.
    BaseAccounts.AccountDelegation (AccountVersionFor pv) ->
    DelegatorUpdate ->
    m ()
updateDelegatorChecks bsp oldDelegator DelegatorUpdate{..} = do
    -- Check that the delegation target is valid and open.
    -- This returns @Just (baker, sameBaker)@ if the (new) delegation target is a baker (@baker@),
    -- with @sameBaker@ indicating whether the delegator is still delegating to the same pool.
    -- If the target is passive delegation, it returns @Nothing@.
    mTargetBaker <- case duDelegationTarget of
        Nothing -> case oldDelegator ^. BaseAccounts.delegationTarget of
            Transactions.DelegatePassive -> return Nothing
            Transactions.DelegateToBaker (BakerId baid) -> do
                -- Cannot fail: the account is already delegating to a baker that can thus be looked up.
                baker <- fromJust <$> onAccount baid bsp accountBaker
                -- Since it wasn't changed, the baker is the same as before.
                return (Just (baker, True))
        Just Transactions.DelegatePassive -> return Nothing
        Just (Transactions.DelegateToBaker bid@(BakerId baid)) -> do
            onAccount baid bsp accountBaker >>= \case
                Nothing -> MTL.throwError (DCFInvalidDelegationTarget bid)
                Just baker -> do
                    let sameBaker =
                            Transactions.DelegateToBaker bid
                                == oldDelegator ^. BaseAccounts.delegationTarget
                    unless
                        ( sameBaker
                            || baker ^. BaseAccounts.poolOpenStatus == Transactions.OpenForAll
                        )
                        $ MTL.throwError DCFPoolClosed
                    return $ Just (baker, sameBaker)
    -- If the capital is being changed, check there is not a pending change.
    let hasPendingChange =
            oldDelegator ^. BaseAccounts.delegationPendingChange /= BaseAccounts.NoChange
    when (isJust duCapital && hasPendingChange) $ MTL.throwError DCFChangePending
    -- If the target is a baker pool, check that the delegation amount is within bounds.
    forM_ mTargetBaker $ \(baker, sameBaker) -> do
        let oldStake = oldDelegator ^. BaseAccounts.delegationStakedAmount
        -- The new effective stake is the old stake if:
        --   - no new stake is provided, or
        --   - the new stake is less than or equal to the old stake and the protocol version does
        --     not support flexible cooldown. (In this case, the change will be pending on the
        --     account.)
        let newEffectiveStake = case duCapital of
                Nothing -> oldStake
                Just newStake -> case flexibleCooldown of
                    SFalse -> max newStake oldStake -- If the stake is reduced, the change is pending.
                    STrue -> newStake
        -- We only check for over-delegation if the stake is being increased or the target is
        -- is changed and the effective stake is non-zero.
        unless (newEffectiveStake == 0 || sameBaker && newEffectiveStake <= oldStake) $ do
            -- The change to the total staked capital.
            let delta = newEffectiveStake `amountDiff` oldStake
            -- The change to the pool's staked capital. This depends on whether the delegator is
            -- switching pools.
            let poolDelta = if sameBaker then delta else amountToDelta newEffectiveStake
            poolParams <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
            let bakerEquityCapital = baker ^. BaseAccounts.stakedAmount
            let bid = baker ^. BaseAccounts.bakerIdentity
            bakerDelegatedCapital <- applyAmountDelta poolDelta <$> poolDelegatorCapital bsp bid
            capitalTotal <- applyAmountDelta delta <$> totalCapital bsp
            let PoolCaps{..} =
                    delegatedCapitalCaps poolParams capitalTotal bakerEquityCapital bakerDelegatedCapital
            when (bakerDelegatedCapital > leverageCap) $ MTL.throwError DCFPoolStakeOverThreshold
            when (bakerDelegatedCapital > boundCap) $ MTL.throwError DCFPoolOverDelegated
  where
    flexibleCooldown = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))

-- | From chain parameters version >= 1, this operation is used to update or remove a delegator.
--  This is used to implement 'bsoUpdateDelegator'.
--
--  PRECONDITIONS:
--
--  * the account is valid;
--  * the account is a delegator;
--  * if the delegated amount is updated, it does not exceed the account's balance.
--
--  The function behaves as follows, building a list @events@:
--
--  1. If the delegation target is specified as @target@:
--
--       (1) If the delegation target is changed and is a valid baker that is not 'OpenForAll',
--           return 'DCFPoolClosed'. [Note, it is allowed for the target to be the same baker,
--           which is 'ClosedForNew'.]
--
--       (2) If the delegation target is baker id @bid@, but the baker does not exist, return
--           @DCFInvalidDelegationTarget bid@.
--
--       (3) Update the active bakers index to: remove the delegator and delegated amount from the
--           old baker pool, and add the delegator and delegated amount to the new baker pool.
--           (Note, the total delegated amount is unchanged at this point.)
--
--       (4) Update the account to record the new delegation target.
--
--       (5) Append @DelegationConfigureDelegationTarget target@ to @events@. [N.B. if the target
--           pool is the same as the previous value, steps (1)-(4) will do nothing and may be skipped
--           by the implementation. This relies on the invariant that delegators delegate only to
--           valid pools.]
--
--  2. If the "restake earnings" flag is specified as @restakeEarnings@:
--
--       (1) Update the restake earnings flag on the account to match @restakeEarnings@.
--
--       (2) Append @DelegationConfigureRestakeEarnings restakeEarnings@ to @events@.
--
--  3. If the delegated capital is specified as @capital@: if there is a pending change to the
--     delegator's stake, return 'DCFChangePending'; otherwise:
--
--       * If the new capital is 0, mark the delegator as pending removal at the slot timestamp
--         plus the delegator cooldown chain parameter, and append
--         @DelegationConfigureStakeReduced capital@ to @events@; otherwise
--
--       * If the new capital is less than the current staked capital (but not 0), mark the
--         delegator as pending stake reduction to @capital@ at the slot timestamp plus the
--         delegator cooldown chain parameter, and append @DelegationConfigureStakeReduced capital@
--         to @events@;
--
--       * If the new capital is equal to the current staked capital, append
--         @DelegationConfigureStakeIncreased capital@ to @events@.
--
--       * If the new capital is greater than the current staked capital by @delta > 0@:
--
--             * increase the total active capital by @delta@,
--
--             * increase the delegator's target pool delegated capital by @delta@,
--
--             * set the baker's delegated capital to @capital@, and
--
--             * append @DelegationConfigureStakeIncreased capital@ to @events@.
--
--  4. If the delegation target has changed or the delegated capital is increased:
--
--            * If the amount delegated to the delegation target exceeds the leverage bound,
--              return 'DCFPoolStakeOverThreshold' and revert any changes.
--
--            * If the amount delegated to the delegation target exceed the capital bound,
--              return 'DCFPoolOverDelegated' and revert any changes.
--
--  6. Return @events@ with the updated state.
newUpdateDelegator ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsDelegation pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1,
      CooldownParametersVersionFor (ChainParametersVersionFor pv) ~ 'CooldownParametersVersion1
    ) =>
    PersistentBlockState (MPV m) ->
    -- | Current block timestamp
    Timestamp ->
    AccountIndex ->
    DelegatorUpdate ->
    MTL.ExceptT DelegatorConfigureFailure m ([DelegationConfigureUpdateChange], PersistentBlockState (MPV m))
newUpdateDelegator pbs blockTimestamp ai du@DelegatorUpdate{..} = do
    bsp <- loadPBS pbs
    -- Cannot fail: The precondition guarantees that the account exists.
    acc <- fromJust <$> Accounts.indexedAccount ai (bspAccounts bsp)
    -- Cannot fail: the account must already be a delegator.
    existingDelegator <- fromJust <$> accountDelegator acc
    updateDelegatorChecks bsp existingDelegator du
    (newBSP, events) <- lift . MTL.runWriterT $ do
        (newBSP, newAcc) <-
            updateDelegationTarget (existingDelegator ^. BaseAccounts.delegationTarget) (bsp, acc)
                >>= updateRestakeEarnings
                >>= updateCapital

        newAccounts <- Accounts.setAccountAtIndex ai newAcc (bspAccounts newBSP)
        return newBSP{bspAccounts = newAccounts}
    (events,) <$> storePBS pbs newBSP
  where
    did = DelegatorId ai
    flexibleCooldown = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))
    -- Only do the update if specified.
    ifPresent Nothing _ = return
    ifPresent (Just v) k = k v
    updateDelegationTarget oldTarget = ifPresent duDelegationTarget $ \target (bsp, acc) -> do
        MTL.tell [DelegationConfigureDelegationTarget target]
        stakedAmount <- accountActiveStakedAmount acc
        if target == oldTarget || stakedAmount == 0
            then return (bsp, acc)
            else do
                bsp1 <-
                    onActiveBakers bsp $
                        removeDelegator oldTarget did stakedAmount
                            >=> addDelegatorUnsafe target did stakedAmount
                acc1 <- setAccountDelegationTarget target acc
                return (bsp1, acc1)
    updateRestakeEarnings = ifPresent duRestakeEarnings $ \restakeEarnings (bsp, acc) -> do
        MTL.tell [DelegationConfigureRestakeEarnings restakeEarnings]
        acc1 <- setAccountRestakeEarnings restakeEarnings acc
        return (bsp, acc1)
    updateCapital = ifPresent duCapital $ \capital (bsp, acc) -> case flexibleCooldown of
        SFalse -> do
            chainParams <- lookupCurrentParameters (bspUpdates bsp)
            oldCapital <- accountActiveStakedAmount acc
            let cooldownDuration = chainParams ^. cpCooldownParameters . cpDelegatorCooldown
                cooldownElapsed = addDurationSeconds blockTimestamp cooldownDuration
                changeEffective = BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed
            if capital == 0
                then do
                    MTL.tell [DelegationConfigureStakeReduced capital]
                    let dpc = BaseAccounts.RemoveStake changeEffective
                    acc1 <- setAccountStakePendingChange dpc acc
                    return (bsp, acc1)
                else case compare capital oldCapital of
                    LT -> do
                        MTL.tell [DelegationConfigureStakeReduced capital]
                        let dpc = BaseAccounts.ReduceStake capital changeEffective
                        acc1 <- setAccountStakePendingChange dpc acc
                        return (bsp, acc1)
                    EQ -> do
                        MTL.tell [DelegationConfigureStakeIncreased capital]
                        return (bsp, acc)
                    GT -> do
                        MTL.tell [DelegationConfigureStakeIncreased capital]
                        -- Cannot fail: account must already be a delegator.
                        target <- BaseAccounts._delegationTarget . fromJust <$> accountDelegator acc
                        let change = capital - oldCapital
                        bsp1 <-
                            onActiveBakers bsp $
                                fmap (totalActiveCapital %~ addActiveCapital change)
                                    . modifyPoolCapitalUnsafe target (+ change)
                        acc1 <- setAccountStake capital acc
                        return (bsp1, acc1)
        STrue -> do
            oldCapital <- accountActiveStakedAmount acc
            target <- BaseAccounts._delegationTarget . fromJust <$> accountDelegator acc
            if capital == 0
                then do
                    MTL.tell [DelegationConfigureStakeReduced 0]
                    bsp1 <-
                        onActiveBakers bsp $
                            removeDelegator target did oldCapital
                                . (totalActiveCapital %~ subtractActiveCapital oldCapital)

                    alreadyInPrePreCooldown <- accountHasPrePreCooldown acc
                    acc1 <- removeAccountStaking acc >>= addAccountPrePreCooldown oldCapital
                    bsp2 <- (if alreadyInPrePreCooldown then return else addToPrePreCooldowns) bsp1
                    return (bsp2, acc1)
                else case compare capital oldCapital of
                    LT -> do
                        MTL.tell [DelegationConfigureStakeReduced capital]
                        let delta = oldCapital - capital
                        bsp1 <-
                            onActiveBakers bsp $
                                fmap (totalActiveCapital %~ subtractActiveCapital delta)
                                    . modifyPoolCapitalUnsafe target (subtract delta)
                        alreadyInPrePreCooldown <- accountHasPrePreCooldown acc
                        acc1 <- setAccountStake capital acc >>= addAccountPrePreCooldown delta
                        bsp2 <- (if alreadyInPrePreCooldown then return else addToPrePreCooldowns) bsp1
                        return (bsp2, acc1)
                    EQ -> do
                        MTL.tell [DelegationConfigureStakeIncreased capital]
                        return (bsp, acc)
                    GT -> do
                        MTL.tell [DelegationConfigureStakeIncreased capital]
                        let delta = capital - oldCapital
                        bsp1 <-
                            onActiveBakers bsp $
                                fmap (totalActiveCapital %~ addActiveCapital delta)
                                    . modifyPoolCapitalUnsafe target (+ delta)
                        maybeCooldownsBefore <- accountCooldowns acc
                        acc1 <-
                            setAccountStake capital acc
                                >>= reactivateCooldownAmount delta
                        maybeCooldownsAfter <- accountCooldowns acc1
                        let removals = cooldownRemovals maybeCooldownsBefore maybeCooldownsAfter
                        newCooldowns <- applyCooldownRemovalsGlobally ai removals (bspAccountsInCooldown bsp1)
                        return (bsp1{bspAccountsInCooldown = newCooldowns}, acc1)
    onActiveBakers bsp f = do
        newPABRef <- refMake =<< f =<< refLoad (bspBirkParameters bsp ^. birkActiveBakers)
        return bsp{bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newPABRef}
    addToPrePreCooldowns ::
        (MonadBlobStore m', PVSupportsFlexibleCooldown pv) =>
        BlockStatePointers pv ->
        m' (BlockStatePointers pv)
    addToPrePreCooldowns bsp = do
        -- Add the account to the pre-pre-cooldowns list.
        newAccountsInCooldown <-
            (accountsInCooldown . prePreCooldown)
                (consAccountList ai)
                (bspAccountsInCooldown bsp)
        return bsp{bspAccountsInCooldown = newAccountsInCooldown}

-- | Whether a (pre-)(pre-)cooldown was removed on an account. Used by 'applyCooldownRemovalsGlobally'
-- to then also remove the account from the global list of accounts in cooldown.
data CooldownRemovals = CooldownRemovals
    { -- | Whether the pre-pre cooldown was removed.
      crPrePreCooldown :: !Bool,
      -- | Whether the pre cooldown was removed.
      crPreCooldown :: !Bool,
      -- | If all cooldowns were removed, this is the previous timestamp of the earliest cooldown.
      crCooldown :: !(Maybe Timestamp)
    }

-- | Determine if a change in cooldowns requires global updates to the indexes.
--  The change should arise from (possibly) reactivating stake from cooldown.
--  The first input is old 'Cooldowns' on the account, and the second input is the new 'Cooldowns' on
--  the account after possibly reactivating stake.
cooldownRemovals ::
    Maybe CooldownQueue.Cooldowns -> Maybe CooldownQueue.Cooldowns -> CooldownRemovals
cooldownRemovals Nothing _ = CooldownRemovals False False Nothing
cooldownRemovals (Just cd1) Nothing =
    CooldownRemovals
        { crPrePreCooldown = isPresent (CooldownQueue.prePreCooldown cd1),
          crPreCooldown = isPresent (CooldownQueue.preCooldown cd1),
          crCooldown = fst <$> Map.lookupMin (CooldownQueue.inCooldown cd1)
        }
cooldownRemovals (Just cd1) (Just cd2) =
    CooldownRemovals
        { crPrePreCooldown = isPresent (CooldownQueue.prePreCooldown cd1) && isAbsent (CooldownQueue.prePreCooldown cd2),
          crPreCooldown = isPresent (CooldownQueue.preCooldown cd1) && isAbsent (CooldownQueue.preCooldown cd2),
          crCooldown = do
            guard (Map.null (CooldownQueue.inCooldown cd2))
            fst <$> Map.lookupMin (CooldownQueue.inCooldown cd1)
        }

-- | Apply cooldown removals for an account to the global indexes.
applyCooldownRemovalsGlobally ::
    (MonadBlobStore m, PVSupportsFlexibleCooldown pv) =>
    AccountIndex ->
    CooldownRemovals ->
    AccountsInCooldownForPV pv ->
    m (AccountsInCooldownForPV pv)
applyCooldownRemovalsGlobally ai CooldownRemovals{..} =
    doIf crPrePreCooldown ((accountsInCooldown . prePreCooldown) (removeAccountFromAccountList ai))
        >=> doIf crPreCooldown ((accountsInCooldown . preCooldown) (removeAccountFromAccountList ai))
        >=> case crCooldown of
            Just ts -> (accountsInCooldown . cooldown) (removeAccountFromReleaseSchedule ts ai)
            Nothing -> return
  where
    doIf True f = f
    doIf False _ = return

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
                    let curEpoch = bspBirkParameters bsp ^. birkSeedState . epoch
                    upds <- refLoad (bspUpdates bsp)
                    cooldownEpochs <-
                        (2 +) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized
                            <$> refLoad (currentParameters upds)

                    bakerStakeThreshold <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> doGetChainParameters pbs
                    let applyUpdate updAcc = do
                            newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                            storePBS pbs bsp{bspAccounts = newAccounts}
                    case compare newStake sdStakedCapital of
                        LT ->
                            if newStake < max 1 bakerStakeThreshold
                                then return (BSUStakeUnderThreshold, pbs)
                                else
                                    (BSUStakeReduced (BakerId ai) (curEpoch + cooldownEpochs),)
                                        <$> applyUpdate
                                            ( setAccountStakePendingChange
                                                (BaseAccounts.ReduceStake newStake (BaseAccounts.PendingChangeEffectiveV0 $ curEpoch + cooldownEpochs))
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
                    let curEpoch = bspBirkParameters bsp ^. birkSeedState . epoch
                    upds <- refLoad (bspUpdates bsp)
                    cooldownEpochs <-
                        (2 +) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized
                            <$> refLoad (currentParameters upds)
                    let updAcc =
                            setAccountStakePendingChange $
                                BaseAccounts.RemoveStake $
                                    BaseAccounts.PendingChangeEffectiveV0 $
                                        curEpoch + cooldownEpochs
                    newAccounts <- Accounts.updateAccountsAtIndex' updAcc ai (bspAccounts bsp)
                    (BRRemoved (BakerId ai) (curEpoch + cooldownEpochs),)
                        <$> storePBS pbs bsp{bspAccounts = newAccounts}
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

doGetBakerPoolRewardDetails :: (PVSupportsDelegation pv, SupportsPersistentState pv m) => PersistentBlockState pv -> m (Map.Map BakerId (BakerPoolRewardDetails (AccountVersionFor pv)))
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
        SP7 -> rewardsV1
        SP8 -> rewardsV1
        SP9 -> rewardsV1

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

doSafeMintToAccount :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AccountIndex -> Amount -> m (Either Amount (PersistentBlockState pv))
doSafeMintToAccount pbs acctIdx mintAmt = do
    bsp <- loadPBS pbs
    let currentSupply = bspBank bsp ^. unhashed . Rewards.totalGTU
    let maxMintAmount = maxBound - currentSupply
    if maxMintAmount >= mintAmt
        then do
            let newBank = bspBank bsp & unhashed . Rewards.totalGTU +~ mintAmt
            let updAcc = addAccountAmount mintAmt
            newAccounts <- Accounts.updateAccountsAtIndex' updAcc acctIdx (bspAccounts bsp)
            Right <$> storePBS pbs (bsp{bspBank = newBank, bspAccounts = newAccounts})
        else return $ Left maxMintAmount

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
                    SP7 -> return _auIndex
                    SP8 -> return _auIndex
                    SP9 -> return _auIndex
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

-- | Get the status of passive delegation.
--  Used to implement 'getPassiveDelegationStatus'.
doGetPassiveDelegationStatus ::
    forall pv m.
    (IsProtocolVersion pv, SupportsPersistentState pv m, PVSupportsDelegation pv) =>
    PersistentBlockState pv ->
    m PassiveDelegationStatus
doGetPassiveDelegationStatus pbs = case delegationChainParameters @pv of
    DelegationChainParameters -> do
        bsp <- loadPBS pbs
        pdsDelegatedCapital <- passiveDelegationCapital bsp
        pdsCommissionRates <- _ppPassiveCommissions . _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
        poolRewards <- refLoad (bspPoolRewards bsp)
        let pdsCurrentPaydayTransactionFeesEarned = passiveDelegationTransactionRewards poolRewards
        pdsCurrentPaydayDelegatedCapital <- currentPassiveDelegationCapital poolRewards
        pdsAllPoolTotalCapital <- totalCapital bsp
        return $! PassiveDelegationStatus{..}

-- | Get a 'BakerPoolStatus' record describing the status of a baker pool. The result is
--  'Nothing' if the 'BakerId' is not an active or current-epoch baker.
--  Used to implement 'getPoolStatus'.
doGetPoolStatus ::
    forall pv m.
    ( IsProtocolVersion pv,
      SupportsPersistentState pv m,
      PVSupportsDelegation pv
    ) =>
    PersistentBlockState pv ->
    BakerId ->
    m (Maybe BakerPoolStatus)
doGetPoolStatus pbs psBakerId@(BakerId aid) = case delegationChainParameters @pv of
    DelegationChainParameters -> do
        bsp <- loadPBS pbs
        Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
            Nothing -> return Nothing
            Just acct -> do
                psBakerAddress <- accountCanonicalAddress acct
                psAllPoolTotalCapital <- totalCapital bsp
                mBaker <- accountBaker acct
                psActiveStatus <- forM mBaker $ \baker -> do
                    let abpsBakerEquityCapital = baker ^. BaseAccounts.stakedAmount
                    abpsDelegatedCapital <- poolDelegatorCapital bsp psBakerId
                    poolParameters <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
                    let abpsDelegatedCapitalCap =
                            delegatedCapitalCap
                                poolParameters
                                psAllPoolTotalCapital
                                abpsBakerEquityCapital
                                abpsDelegatedCapital
                    let abpsPoolInfo = baker ^. BaseAccounts.bakerPoolInfo
                    let abpsBakerStakePendingChange =
                            makePoolPendingChange $ BaseAccounts.pendingChangeEffectiveTimestamp <$> (baker ^. BaseAccounts.bakerPendingChange)
                    let abpsIsSuspended =
                            fromCondDef
                                (Just <$> BaseAccounts._bieIsSuspended (baker ^. BaseAccounts.accountBakerInfo))
                                Nothing
                    return $! ActiveBakerPoolStatus{..}
                epochBakers <- refLoad (_birkCurrentEpochBakers $ bspBirkParameters bsp)
                mepochBaker <- epochBaker psBakerId epochBakers
                psCurrentPaydayStatus <- case mepochBaker of
                    Nothing -> return Nothing
                    Just (currentEpochBaker, effectiveStake) -> do
                        poolRewards <- refLoad (bspPoolRewards bsp)
                        mbcr <- lookupBakerCapitalAndRewardDetails psBakerId poolRewards
                        case mbcr of
                            Nothing ->
                                error $
                                    "doGetPoolStatus: invariant violation: baker "
                                        ++ show psBakerId
                                        ++ " is present in the current epoch bakers, but not \
                                           \the current epoch capital distribution."
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
                                              bpsDelegatedCapital = bcTotalDelegatorCapital bc,
                                              bpsCommissionRates =
                                                currentEpochBaker
                                                    ^. BaseAccounts.bieBakerPoolInfo
                                                        . BaseAccounts.poolCommissionRates,
                                              bpsIsPrimedForSuspension =
                                                fromCondDef (fmap (Just . primedForSuspension) suspensionInfo) Nothing,
                                              bpsMissedRounds =
                                                fromCondDef (fmap (Just . missedRounds) suspensionInfo) Nothing
                                            }
                if isJust psActiveStatus || isJust psCurrentPaydayStatus
                    then return $ Just BakerPoolStatus{..}
                    else return Nothing

doGetTransactionOutcome :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> Transactions.TransactionIndex -> m (Maybe TransactionSummary)
doGetTransactionOutcome pbs transHash = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return $! bto ^? ix transHash
        PTOV1 bto -> fmap _transactionSummaryV1 <$> LFMBT.lookup transHash (mtoOutcomes bto)
        PTOV2 bto -> fmap _transactionSummaryV1 <$> LFMBT.lookup transHash (mtoOutcomes bto)

doGetTransactionOutcomesHash ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m TransactionOutcomes.TransactionOutcomesHash
doGetTransactionOutcomesHash pbs = do
    bsp <- loadPBS pbs
    TransactionOutcomes.toTransactionOutcomesHash @(TransactionOutcomesVersionFor pv)
        <$> getHashM (bspTransactionOutcomes bsp)

doSetTransactionOutcomes :: forall pv m. (SupportsPersistentState pv m) => PersistentBlockState pv -> [TransactionSummary] -> m (PersistentBlockState pv)
doSetTransactionOutcomes pbs transList = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 _ ->
            storePBS
                pbs
                bsp
                    { bspTransactionOutcomes =
                        PTOV0 (TransactionOutcomes.transactionOutcomesV0FromList transList)
                    }
        PTOV1 _ -> do
            mto <- makeMTO
            storePBS pbs bsp{bspTransactionOutcomes = PTOV1 mto}
        PTOV2 _ -> do
            mto <- makeMTO
            storePBS pbs bsp{bspTransactionOutcomes = PTOV2 mto}
  where
    makeMTO :: m MerkleTransactionOutcomes
    makeMTO = do
        mtoOutcomes <- LFMBT.fromAscList . map TransactionSummaryV1 $ transList
        return MerkleTransactionOutcomes{mtoSpecials = LFMBT.empty, ..}

doNotifyEncryptedBalanceChange :: (SupportsPersistentState pv m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doNotifyEncryptedBalanceChange pbs amntDiff = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff}

doGetSpecialOutcomes :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> m (Seq.Seq Transactions.SpecialTransactionOutcome)
doGetSpecialOutcomes pbs = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return (bto ^. TransactionOutcomes.outcomeSpecial)
        PTOV1 bto -> Seq.fromList <$> LFMBT.toAscList (mtoSpecials bto)
        PTOV2 bto -> Seq.fromList <$> LFMBT.toAscList (mtoSpecials bto)

doGetOutcomes :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> m (Vec.Vector TransactionSummary)
doGetOutcomes pbs = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto -> return (TransactionOutcomes.outcomeValues bto)
        PTOV1 bto -> Vec.fromList . map _transactionSummaryV1 <$> LFMBT.toAscList (mtoOutcomes bto)
        PTOV2 bto -> Vec.fromList . map _transactionSummaryV1 <$> LFMBT.toAscList (mtoOutcomes bto)

doAddSpecialTransactionOutcome :: (SupportsPersistentState pv m, MonadProtocolVersion m) => PersistentBlockState pv -> Transactions.SpecialTransactionOutcome -> m (PersistentBlockState pv)
doAddSpecialTransactionOutcome pbs !o = do
    bsp <- loadPBS pbs
    case bspTransactionOutcomes bsp of
        PTOV0 bto ->
            storePBS pbs $!
                bsp
                    { bspTransactionOutcomes =
                        PTOV0 (bto & TransactionOutcomes.outcomeSpecial %~ (Seq.|> o))
                    }
        PTOV1 bto -> do
            (_, newSpecials) <- LFMBT.append o (mtoSpecials bto)
            storePBS pbs $! bsp{bspTransactionOutcomes = PTOV1 (bto{mtoSpecials = newSpecials})}
        PTOV2 bto -> do
            (_, newSpecials) <- LFMBT.append o (mtoSpecials bto)
            storePBS pbs $! bsp{bspTransactionOutcomes = PTOV2 (bto{mtoSpecials = newSpecials})}

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

doIsProtocolUpdateEffective :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m Bool
doIsProtocolUpdateEffective = isProtocolUpdateEffective . bspUpdates <=< loadPBS

doUpdateMissedRounds ::
    ( PVSupportsDelegation pv,
      SupportsPersistentState pv m
    ) =>
    PersistentBlockState pv ->
    Map.Map BakerId Word64 ->
    m (PersistentBlockState pv)
doUpdateMissedRounds pbs rds = do
    bsp <- loadPBS pbs
    bsp' <-
        foldM
            ( \bsp0 (bId, newMissedRounds) ->
                modifyBakerPoolRewardDetailsInPoolRewards
                    bsp0
                    bId
                    ( \bprd ->
                        bprd
                            { suspensionInfo =
                                (\SuspensionInfo{..} -> SuspensionInfo{missedRounds = missedRounds + newMissedRounds, ..})
                                    <$> suspensionInfo bprd
                            }
                    )
            )
            bsp
            (Map.toList rds)
    storePBS pbs bsp'

-- | Prime validators for suspension. Returns the subset of the given validator
--  ids whose missed rounds exceeded the given threshold and are now primed for
--  suspension.
doPrimeForSuspension ::
    ( PVSupportsDelegation pv,
      SupportsPersistentState pv m
    ) =>
    PersistentBlockState pv ->
    Word64 ->
    m ([BakerId], PersistentBlockState pv)
doPrimeForSuspension pbs threshold = do
    bprds <- doGetBakerPoolRewardDetails pbs
    bsp0 <- loadPBS pbs
    (bidsUpd, bsp') <- do
        foldM
            ( \res@(acc, bsp) (bId, bprd) -> do
                case suspensionInfo bprd of
                    CTrue SuspensionInfo{..} | missedRounds > threshold -> do
                        bsp' <-
                            modifyBakerPoolRewardDetailsInPoolRewards
                                bsp
                                bId
                                (\bpr -> bpr{suspensionInfo = (\suspInfo -> suspInfo{primedForSuspension = True}) <$> suspensionInfo bpr})
                        return (bId : acc, bsp')
                    _otherwise -> return res
            )
            ([], bsp0)
            (Map.toList bprds)
    pbs' <- storePBS pbs bsp'
    return (bidsUpd, pbs')

-- | Suspend validators with the given account indices, if
--  1) the account index points to an existing account
--  2) the account belongs to a validator
--  3) the account was not already suspended
--  Returns the subset of account indices that were suspended together with their canonical account
--  addresses.
doSuspendValidators ::
    forall pv m.
    ( SupportsPersistentState pv m
    ) =>
    PersistentBlockState pv ->
    [AccountIndex] ->
    m ([(AccountIndex, AccountAddress)], PersistentBlockState pv)
doSuspendValidators pbs ais =
    case hasValidatorSuspension of
        STrue -> do
            bsp0 <- loadPBS pbs
            (aisSusp, bspUpd) <-
                foldM
                    ( \res@(aisSusp, bsp) ai -> do
                        mAcc <- Accounts.indexedAccount ai (bspAccounts bsp)
                        case mAcc of
                            Nothing -> return res
                            Just acc -> do
                                mValidatorExists <- accountBaker acc
                                case mValidatorExists of
                                    Nothing -> return res
                                    Just ba
                                        -- The validator is not yet suspended
                                        | False <-
                                            uncond $ BaseAccounts._bieIsSuspended $ _accountBakerInfo ba -> do
                                            newAcc <- setAccountValidatorSuspended True acc
                                            newAccounts <- Accounts.setAccountAtIndex ai newAcc (bspAccounts bsp)
                                            address <- accountCanonicalAddress newAcc
                                            return ((ai, address) : aisSusp, bsp{bspAccounts = newAccounts})
                                        -- The validator is already suspended, nothing to do
                                        | otherwise -> return res
                    )
                    ([], bsp0)
                    ais
            pbsUpd <- storePBS pbs bspUpd
            return (aisSusp, pbsUpd)
        SFalse -> return ([], pbs)
  where
    hasValidatorSuspension = sSupportsValidatorSuspension (accountVersion @(AccountVersionFor pv))

doProcessUpdateQueues ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    Timestamp ->
    m ([(TransactionTime, UpdateValue (ChainParametersVersionFor pv))], PersistentBlockState pv)
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
                    SP7 -> processAccountP5
                    SP8 -> processAccountP5
                    SP9 -> processAccountP5
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

-- | This function updates the baker pool rewards details of a baker. It is a precondition that
--  the given baker is active.
modifyBakerPoolRewardDetailsInPoolRewards :: (SupportsPersistentAccount pv m, PVSupportsDelegation pv) => BlockStatePointers pv -> BakerId -> ((BakerPoolRewardDetails (AccountVersionFor pv)) -> (BakerPoolRewardDetails (AccountVersionFor pv))) -> m (BlockStatePointers pv)
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
            let incBPR bpr =
                    bpr
                        { blockCount = blockCount bpr + 1,
                          suspensionInfo = emptySuspensionInfo <$ suspensionInfo bpr
                        }
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
    newHash :: (Rewards.BlockRewardDetailsHash pv) <-
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
                mBPRs <- MTL.runExceptT $ LFMBT.update setAwake (fromIntegral i) bprs
                case mBPRs of
                    -- error is used to signal that there is no change
                    Left () -> return bprs
                    Right Nothing ->
                        error "Invariant violation: unable to find baker in baker pool reward details tree"
                    Right (Just ((), newBPRs)) ->
                        return newBPRs
    setAwake bpr = do
        -- If there's nothing to do, use throwError to skip the update.
        when
            ( finalizationAwake bpr
                && all (== emptySuspensionInfo) (suspensionInfo bpr)
            )
            $ MTL.throwError ()
        return
            ( (),
              bpr
                { finalizationAwake = True,
                  suspensionInfo = emptySuspensionInfo <$ suspensionInfo bpr
                }
            )

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
    OFinalizationCommitteeParameters pv ->
    m (PersistentBlockState pv)
doSetNextEpochBakers pbs bakers _bakerFinalizationCommitteeParameters = do
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
    -- | Guard determining if a change is effective
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

-- | Process cooldowns on accounts that have expired, and move pre-cooldowns into cooldown.
doProcessCooldowns ::
    forall pv m.
    (SupportsPersistentState pv m, PVSupportsFlexibleCooldown pv) =>
    PersistentBlockState pv ->
    -- | Timestamp for expiring cooldowns.
    Timestamp ->
    -- | Timestamp for pre-cooldowns entering cooldown.
    Timestamp ->
    m (PersistentBlockState pv)
doProcessCooldowns pbs now newExpiry = do
    bsp <- loadPBS pbs
    (newAIC, newAccts) <-
        MTL.execStateT
            process
            (bspAccountsInCooldown bsp ^. accountsInCooldown, bspAccounts bsp)
    storePBS pbs $
        bsp
            { bspAccountsInCooldown = AccountsInCooldownForPV (CTrue newAIC),
              bspAccounts = newAccts
            }
  where
    -- Perform a monadic update on the global cooldown release schedule.
    withCooldown a = (_1 . cooldown .=) =<< a =<< use (_1 . cooldown)
    -- Perform a monadic update an the accounts table.
    withAccounts a = (_2 .=) =<< a =<< use _2
    process = do
        -- Determine which accounts have cooldowns that have expired and remove them from the
        -- cooldown schedule.
        cooldown0 <- use (_1 . cooldown)
        (cooldownList, cooldown1) <- processReleasesUntil now cooldown0
        _1 . cooldown .= cooldown1
        -- Process the cooldowns for the accounts that have expired cooldowns, adding them back
        -- to the cooldown schedule if they have remaining cooldowns.
        forM_ cooldownList $ \acc -> do
            withAccounts (Accounts.updateAccountsAtIndex' (processCooldownForAccount acc) acc)
        -- Fetch and clear the list of accounts in pre-cooldown.
        preCooldownAL <- _1 . preCooldown <<.= Null
        preCooldowns <- loadAccountList preCooldownAL
        -- Process the pre-cooldowns, moving them into cooldown.
        forM_ preCooldowns $ \acc -> do
            withAccounts (Accounts.updateAccountsAtIndex' (processPreCooldownForAccount acc) acc)
    processCooldownForAccount acc pa = do
        -- Release the elapsed cooldowns on the account.
        (mNextCooldown, newPA) <- processAccountCooldownsUntil now pa
        -- If there are remaining cooldowns, add the account back to the cooldown schedule.
        forM_ mNextCooldown $ \nextCooldown -> withCooldown $ addAccountRelease nextCooldown acc
        return newPA
    processPreCooldownForAccount acc pa = do
        -- Process the pre-cooldowns on the account.
        (res, newPA) <- processAccountPreCooldown newExpiry pa
        case res of
            -- In this case, the account already had cooldowns, but the new cooldown expires
            -- earlier, so the release schedule needs to be updated.
            EarlierNextCooldown oldTS -> withCooldown $ updateAccountRelease oldTS newExpiry acc
            -- In this case, the account did not have cooldowns, so the new cooldown is added.
            NewNextCooldown -> withCooldown $ addAccountRelease newExpiry acc
            -- In this case, the earliest cooldown on the account has not changed.
            NextCooldownUnchanged -> return ()
        return newPA

-- | Move all pre-pre-cooldowns into pre-cooldown.
--
-- PRECONDITION: there are no pre-cooldowns.
doProcessPrePreCooldowns ::
    forall pv m.
    (SupportsPersistentState pv m, PVSupportsFlexibleCooldown pv) =>
    PersistentBlockState pv ->
    m (PersistentBlockState pv)
doProcessPrePreCooldowns pbs = do
    bsp <- loadPBS pbs
    let oldAIC = bspAccountsInCooldown bsp ^. accountsInCooldown
    -- The new pre-cooldown list is the old pre-pre-cooldown list.
    let !newPreCooldown = assert (isNull (oldAIC ^. preCooldown)) $ oldAIC ^. prePreCooldown
    let newAIC =
            oldAIC
                & preCooldown .~ newPreCooldown
                & prePreCooldown .~ Null
    accounts <- loadAccountList newPreCooldown
    -- Process the pre-pre-cooldown on each account, moving it to pre-cooldown.
    let processAccount = flip $ Accounts.updateAccountsAtIndex' processAccountPrePreCooldown
    newAccts <- foldM processAccount (bspAccounts bsp) accounts
    storePBS pbs $
        bsp
            { bspAccountsInCooldown = AccountsInCooldownForPV (CTrue newAIC),
              bspAccounts = newAccts
            }

doGetBankStatus :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m Rewards.BankStatus
doGetBankStatus pbs = _unhashed . bspBank <$> loadPBS pbs

doSetRewardAccounts :: (SupportsPersistentState pv m) => PersistentBlockState pv -> Rewards.RewardAccounts -> m (PersistentBlockState pv)
doSetRewardAccounts pbs rewards = do
    bsp <- loadPBS pbs
    storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.rewardAccounts .~ rewards}

-- | Get the index of accounts with scheduled releases.
doGetScheduledReleaseAccounts :: (SupportsPersistentState pv m) => PersistentBlockState pv -> m (Map.Map Timestamp (Set.Set AccountIndex))
doGetScheduledReleaseAccounts pbs = do
    bsp <- loadPBS pbs
    let resolveAddress addr = do
            mIndex <- Accounts.getAccountIndex addr (bspAccounts bsp)
            case mIndex of
                Just index -> return index
                Nothing -> error "Invariant violation: account address not found"
    releasesMap resolveAddress (bspReleaseSchedule bsp)

-- | Get the index of accounts with stake in cooldown.
doGetCooldownAccounts ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m (Map.Map Timestamp (Set.Set AccountIndex))
doGetCooldownAccounts pbs = case sSupportsFlexibleCooldown sav of
    STrue -> do
        bsp <- loadPBS pbs
        newReleasesMap (bspAccountsInCooldown bsp ^. accountsInCooldown . cooldown)
    SFalse -> return Map.empty
  where
    sav = sAccountVersionFor (protocolVersion @pv)

-- | Get the index of accounts in pre-cooldown.
doGetPreCooldownAccounts ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m [AccountIndex]
doGetPreCooldownAccounts pbs = case sSupportsFlexibleCooldown sav of
    STrue -> do
        bsp <- loadPBS pbs
        loadAccountList $ bspAccountsInCooldown bsp ^. accountsInCooldown . preCooldown
    SFalse -> return []
  where
    sav = sAccountVersionFor (protocolVersion @pv)

-- | Get the index of accounts in pre-pre-cooldown.
doGetPrePreCooldownAccounts ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    PersistentBlockState pv ->
    m [AccountIndex]
doGetPrePreCooldownAccounts pbs = case sSupportsFlexibleCooldown sav of
    STrue -> do
        bsp <- loadPBS pbs
        loadAccountList $ bspAccountsInCooldown bsp ^. accountsInCooldown . prePreCooldown
    SFalse -> return []
  where
    sav = sAccountVersionFor (protocolVersion @pv)

-- | Context that supports the persistent block state.
data PersistentBlockStateContext pv = PersistentBlockStateContext
    { -- | The 'BlobStore' used for storing the persistent state.
      pbscBlobStore :: !BlobStore,
      -- | Cache used for caching accounts.
      pbscAccountCache :: !(AccountCache (AccountVersionFor pv)),
      -- | Cache used for caching modules.
      pbscModuleCache :: !Modules.ModuleCache,
      -- | LMDB account map
      pbscAccountMap :: !LMDBAccountMap.DatabaseHandlers
    }

instance LMDBAccountMap.HasDatabaseHandlers (PersistentBlockStateContext pv) where
    databaseHandlers = lens pbscAccountMap (\s v -> s{pbscAccountMap = v})

instance HasBlobStore (PersistentBlockStateContext av) where
    blobStore = bscBlobStore . pbscBlobStore
    blobLoadCallback = bscLoadCallback . pbscBlobStore
    blobStoreCallback = bscStoreCallback . pbscBlobStore

instance (AccountVersionFor pv ~ av) => Cache.HasCache (AccountCache av) (PersistentBlockStateContext pv) where
    projectCache = pbscAccountCache

instance Cache.HasCache Modules.ModuleCache (PersistentBlockStateContext pv) where
    projectCache = pbscModuleCache

instance (IsProtocolVersion pv) => MonadProtocolVersion (BlobStoreT (PersistentBlockStateContext pv) m) where
    type MPV (BlobStoreT (PersistentBlockStateContext pv) m) = pv

-- | Create a new account cache of the specified size and a temporary 'LMDBAccountMap' for running the given monadic operation by
--  extending the 'BlobStore' context to a 'PersistentBlockStateContext'.
-- Note. this function should only be used for tests.
withNewAccountCacheAndLMDBAccountMap :: (MonadIO m, MonadCatch.MonadMask m) => Int -> FilePath -> BlobStoreT (PersistentBlockStateContext pv) m a -> BlobStoreT BlobStore m a
withNewAccountCacheAndLMDBAccountMap size lmdbAccountMapDir bsm = MonadCatch.bracket openLmdbAccMap closeLmdbAccMap runAction
  where
    openLmdbAccMap = liftIO $ LMDBAccountMap.openDatabase lmdbAccountMapDir
    closeLmdbAccMap handlers = liftIO $ do
        LMDBAccountMap.closeDatabase handlers
        removeDirectoryRecursive lmdbAccountMapDir `catch` (\(e :: IOException) -> liftIO $ void $ print e)
    runAction lmdbAccMap = do
        ac <- liftIO $ newAccountCache size
        mc <- liftIO $ Modules.newModuleCache 100
        alterBlobStoreT (\bs -> PersistentBlockStateContext bs ac mc lmdbAccMap) bsm

newtype PersistentBlockStateMonad (pv :: ProtocolVersion) (r :: Type) (m :: Type -> Type) (a :: Type) = PersistentBlockStateMonad {runPersistentBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadLogger, TimeMonad, MTL.MonadState s, MonadCatch.MonadCatch, MonadCatch.MonadThrow)

type PersistentState av pv r m =
    ( MonadIO m,
      MonadReader r m,
      HasBlobStore r,
      AccountVersionFor pv ~ av,
      Cache.HasCache (AccountCache av) r,
      Cache.HasCache Modules.ModuleCache r,
      LMDBAccountMap.HasDatabaseHandlers r,
      MonadLogger m
    )

instance MonadTrans (PersistentBlockStateMonad pv r) where
    lift = PersistentBlockStateMonad

instance (PersistentState av pv r m) => MonadBlobStore (PersistentBlockStateMonad pv r m)
instance (PersistentState av pv r m) => MonadBlobStore (PutT (PersistentBlockStateMonad pv r m))
instance (PersistentState av pv r m) => MonadBlobStore (PutH (PersistentBlockStateMonad pv r m))

instance (PersistentState av pv r m) => Cache.MonadCache (AccountCache av) (PersistentBlockStateMonad pv r m)
instance (PersistentState av pv r m) => Cache.MonadCache Modules.ModuleCache (PersistentBlockStateMonad pv r m)

deriving via (LMDBAccountMap.AccountMapStoreMonad m) instance (MonadIO m, MonadLogger m, MonadReader r m, LMDBAccountMap.HasDatabaseHandlers r) => LMDBAccountMap.MonadAccountMapStore (PersistentBlockStateMonad pv r m)
deriving via (LMDBAccountMap.AccountMapStoreMonad m) instance (MonadIO m, MonadLogger m, MonadReader r m, LMDBAccountMap.HasDatabaseHandlers r) => MonadModuleMapStore (PersistentBlockStateMonad pv r m)

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
    getModuleCount = doGetModuleCount . hpbsPointers
    getAccountList = doAccountList . hpbsPointers
    getContractInstanceList = doContractInstanceList . hpbsPointers
    getSeedState = doGetSeedState . hpbsPointers
    getCurrentEpochFinalizationCommitteeParameters = doGetCurrentEpochFinalizationCommitteeParameters . hpbsPointers
    getCurrentEpochBakers = doGetCurrentEpochBakers . hpbsPointers
    getNextEpochBakers = doGetNextEpochBakers . hpbsPointers
    getNextEpochFinalizationCommitteeParameters = doGetNextEpochFinalizationCommitteeParameters . hpbsPointers
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
    getChainParameters = doGetChainParameters . hpbsPointers
    getPaydayEpoch = doGetPaydayEpoch . hpbsPointers
    getPoolStatus = doGetPoolStatus . hpbsPointers
    getPassiveDelegationStatus = doGetPassiveDelegationStatus . hpbsPointers
    getScheduledReleaseAccounts = doGetScheduledReleaseAccounts . hpbsPointers
    getCooldownAccounts = doGetCooldownAccounts . hpbsPointers
    getPreCooldownAccounts = doGetPreCooldownAccounts . hpbsPointers
    getPrePreCooldownAccounts = doGetPrePreCooldownAccounts . hpbsPointers

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

    getAccountTotalStakedAmount = accountTotalStakedAmount

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

    getAccountCooldowns = accountCooldowns

instance (IsProtocolVersion pv, PersistentState av pv r m) => BlockStateOperations (PersistentBlockStateMonad pv r m) where
    bsoGetModule pbs mref = doGetModule pbs mref
    bsoGetAccount bs = doGetAccount bs
    bsoGetAccountIndex = doGetAccountIndex
    bsoGetAccountByIndex = doGetAccountByIndex
    bsoGetInstance = doGetInstance
    bsoAddressWouldClash = doGetAccountExists
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
    bsoAddValidator = case delegationChainParameters @pv of
        DelegationChainParameters -> \bs ai a -> MTL.runExceptT (newAddValidator bs ai a)
    bsoUpdateValidator = case delegationChainParameters @pv of
        DelegationChainParameters -> \bs ts ai u -> MTL.runExceptT (newUpdateValidator bs ts ai u)
    bsoConstrainBakerCommission = doConstrainBakerCommission
    bsoAddDelegator = case delegationChainParameters @pv of
        DelegationChainParameters -> \bs ai a -> MTL.runExceptT (newAddDelegator bs ai a)
    bsoUpdateDelegator = case delegationChainParameters @pv of
        DelegationChainParameters -> \bs ts ai u -> MTL.runExceptT (newUpdateDelegator bs ts ai u)
    bsoUpdateBakerKeys = doUpdateBakerKeys
    bsoUpdateBakerStake = doUpdateBakerStake
    bsoUpdateBakerRestakeEarnings = doUpdateBakerRestakeEarnings
    bsoRemoveBaker = doRemoveBaker
    bsoRewardAccount = doRewardAccount
    bsoGetBakerPoolRewardDetails = doGetBakerPoolRewardDetails
    bsoRewardFoundationAccount = doRewardFoundationAccount
    bsoGetFoundationAccount = doGetFoundationAccount
    bsoMint = doMint
    bsoMintToAccount = doSafeMintToAccount
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
    bsoProcessCooldowns = doProcessCooldowns
    bsoProcessPrePreCooldowns = doProcessPrePreCooldowns
    bsoGetBankStatus = doGetBankStatus
    bsoSetRewardAccounts = doSetRewardAccounts
    bsoIsProtocolUpdateEffective = doIsProtocolUpdateEffective
    bsoUpdateMissedRounds = doUpdateMissedRounds
    bsoPrimeForSuspension = doPrimeForSuspension
    bsoSuspendValidators = doSuspendValidators
    type StateSnapshot (PersistentBlockStateMonad pv r m) = BlockStatePointers pv
    bsoSnapshotState = loadPBS
    bsoRollback = storePBS

instance (IsProtocolVersion pv, PersistentState av pv r m) => BlockStateStorage (PersistentBlockStateMonad pv r m) where
    thawBlockState = doThawBlockState

    freezeBlockState = hashBlockState

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

    saveGlobalMaps HashedPersistentBlockState{..} = do
        -- this load should be cheap as the blockstate is in memory.
        pbs <- loadPBS hpbsPointers
        -- write the accounts that were created in the block and
        -- potentially non-finalized parent blocks.
        -- Note that this also empties the difference map for the
        -- block.
        Accounts.writeAccountsCreated (bspAccounts pbs)
        -- Write the modules that were added in the block and
        -- potentially non-finalized parent blocks.
        -- This also empties the module difference maps.
        Modules.writeModulesAdded =<< refLoad (bspModules pbs)

    reconstructAccountDifferenceMap HashedPersistentBlockState{..} parentDifferenceMap listOfAccounts = do
        accs <- bspAccounts <$> loadPBS hpbsPointers
        Accounts.reconstructDifferenceMap parentDifferenceMap listOfAccounts accs

    reconstructModuleDifferenceMap HashedPersistentBlockState{..} parentInfo = do
        mods <- bspModules <$> loadPBS hpbsPointers
        Modules.reconstructDifferenceMap parentInfo =<< refLoad mods

    loadBlockState hpbsHashM ref = do
        hpbsPointers <- liftIO $ newIORef $ blobRefToBufferedRef ref
        case hpbsHashM of
            Just hpbsHash -> return HashedPersistentBlockState{..}
            Nothing -> hashBlockState hpbsPointers

    blockStateLoadCallback = asks blobLoadCallback
    {-# INLINE blockStateLoadCallback #-}

    collapseCaches = do
        Cache.collapseCache (Proxy :: Proxy (AccountCache av))
        Cache.collapseCache (Proxy :: Proxy Modules.ModuleCache)

    cacheBlockState = cacheState

    cacheBlockStateAndGetTransactionTable = cacheStateAndGetTransactionTable
    tryPopulateGlobalMaps HashedPersistentBlockState{..} = do
        -- load the top level references and write the accounts to the LMDB backed
        -- account map (if this has not already been done).
        BlockStatePointers{..} <- loadPBS hpbsPointers
        LMDBAccountMap.tryPopulateLMDBStore bspAccounts
        -- Write the module map to the LMDB backed module map (if this has not already been done).
        Modules.tryPopulateModuleLMDB =<< refLoad bspModules

-- | Migrate the block state from the representation used by protocol version
--  @oldpv@ to the one used by protocol version @pv@. The migration is done gradually,
--  and that is the reason for the monad @m@ and the transformer @t@. The inner monad @m@ is
--  used to __load__ the state, only the loading part of the 'MonadBlobStore' is used.
--  The outer monad @t m@ is used to __write__ the new state after migration.
--
--  The intention is that the inner @m@ is @BlobStoreT
--  (PersistentBlockStateContext oldpv) IO@, whereas the @t@ is @BlobStoreT
--  (PersistentBlockStateContext pv)@. Since they both implement the
--  'MonadBlobStore' interface some care must be observed to read and write in
--  the correct context in the implementation of this function. Typically,
--  reading from the old state requires @lift@ing the relevant operation.
--
--  The migration function should not do non-trivial changes, i.e., it should
--  only migrate representation, and fill in the defaults as specified by the
--  state migration parameters.
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
      MonadProtocolVersion m,
      MPV m ~ oldpv,
      MonadProtocolVersion (t m),
      MPV (t m) ~ pv,
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
            StateMigrationParametersP5ToP6{} -> RSMNewToNew
            StateMigrationParametersP6ToP7{} -> RSMNewToNew
            StateMigrationParametersP7ToP8{} -> RSMNewToNew
            StateMigrationParametersP8ToP9{} -> RSMNewToNew
    logEvent GlobalState LLTrace "Migrating release schedule"
    newReleaseSchedule <- migrateReleaseSchedule rsMigration bspReleaseSchedule
    pab <- lift . refLoad $ bspBirkParameters ^. birkActiveBakers
    -- When we migrate the accounts, we accumulate state
    initMigrationState :: MigrationState.AccountMigrationState oldpv pv <-
        MigrationState.makeInitialAccountMigrationState bspAccounts pab
    logEvent GlobalState LLTrace "Migrating accounts"
    (newAccounts, migrationState) <-
        MigrationState.runAccountMigrationStateTT
            (Accounts.migrateAccounts migration bspAccounts)
            initMigrationState
    logEvent GlobalState LLTrace "Migrating accounts in cooldown"
    newAccountsInCooldown <-
        migrateAccountsInCooldownForPV
            (MigrationState._migrationPrePreCooldown migrationState)
            bspAccountsInCooldown
    logEvent GlobalState LLTrace "Migrating modules"
    newModules <- migrateHashedBufferedRef (Modules.migrateModules migration) bspModules
    modules <- refLoad newModules
    logEvent GlobalState LLTrace "Migrating contract instances"
    newInstances <- Instances.migrateInstances modules bspInstances
    let newBank = bspBank
    logEvent GlobalState LLTrace "Migrating identity providers"
    newIdentityProviders <- migrateHashedBufferedRefKeepHash bspIdentityProviders
    logEvent GlobalState LLTrace "Migrating anonymity revokers"
    newAnonymityRevokers <- migrateHashedBufferedRefKeepHash bspAnonymityRevokers
    let oldEpoch = bspBirkParameters ^. birkSeedState . epoch
    logEvent GlobalState LLTrace "Migrating Birk parameters"
    newBirkParameters <-
        migratePersistentBirkParameters
            migration
            newAccounts
            (MigrationState._persistentActiveBakers migrationState)
            bspBirkParameters
    logEvent GlobalState LLTrace "Migrating cryptographic parameters"
    newCryptographicParameters <- migrateHashedBufferedRefKeepHash bspCryptographicParameters
    logEvent GlobalState LLTrace "Migrating chain parameters and updates updates"
    newUpdates <- migrateReference (migrateUpdates migration) bspUpdates
    logEvent GlobalState LLTrace "Migrating current epoch bakers"
    curBakers <- extractBakerStakes =<< refLoad (_birkCurrentEpochBakers newBirkParameters)
    logEvent GlobalState LLTrace "Migrating next epoch bakers"
    nextBakers <- extractBakerStakes =<< refLoad (_birkNextEpochBakers newBirkParameters)
    -- clear transaction outcomes.
    let newTransactionOutcomes = emptyTransactionOutcomes (Proxy @pv)
    chainParams <- refLoad . currentParameters =<< refLoad newUpdates
    let timeParams = _cpTimeParameters . unStoreSerialized $ chainParams
    logEvent GlobalState LLTrace "Migrating reward details"
    newRewardDetails <-
        migrateBlockRewardDetails migration curBakers nextBakers timeParams oldEpoch bspRewardDetails
    newProtocolLevelTokens <- migrateProtocolLevelTokensForPV migration bspProtocolLevelTokens
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
              bspAccountsInCooldown = newAccountsInCooldown,
              bspTransactionOutcomes = newTransactionOutcomes,
              bspRewardDetails = newRewardDetails,
              bspProtocolLevelTokens = newProtocolLevelTokens
            }

-- | Thaw the block state, making it ready for modification.
--  This function wraps the underlying 'PersistentBlockState' of the provided 'HashedPersistentBlockState' in a new 'IORef'
--  such that changes to the thawed block state does not propagate into the parent state.
--
--  Further the 'DiffMap.DifferenceMap's of the accounts and modules structures in the provided
--  block state are "bumped" in the sense that new ones are created for the new thawed block with
--  a pointer to the parent difference maps. The parent difference map is empty if the parent is
--  finalized, otherwise it may contain new accounts created in that block.
doThawBlockState ::
    (SupportsPersistentState pv m) =>
    HashedPersistentBlockState pv ->
    m (PersistentBlockState pv)
doThawBlockState HashedPersistentBlockState{..} = do
    bsp@BlockStatePointers{..} <- loadPBS hpbsPointers
    bspAccounts' <- Accounts.mkNewChildDifferenceMap bspAccounts
    -- Since 'Modules.mkNewChild' only affects the difference map, which is not relevant to the
    -- blob store representation or hashing of the 'Modules', we can exploit 'liftCache' to
    -- update the in-memory value without creating a new on-disk reference.
    bspModules' <- liftCache Modules.mkNewChild bspModules
    let bsp' = bsp{bspAccounts = bspAccounts', bspModules = bspModules'}
    liftIO $ newIORef =<< makeBufferedRef bsp'

-- | Cache the block state.
cacheState ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    HashedPersistentBlockState pv ->
    m ()
cacheState hpbs = do
    BlockStatePointers{..} <- loadPBS (hpbsPointers hpbs)
    accts <- liftCache (return @_ @(PersistentAccount (AccountVersionFor pv))) bspAccounts
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
    rels <- cache bspReleaseSchedule
    cdowns <- cache bspAccountsInCooldown
    red <- cache bspRewardDetails
    plts <- cache bspProtocolLevelTokens
    _ <-
        storePBS (hpbsPointers hpbs) $!
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
                  bspAccountsInCooldown = cdowns,
                  bspTransactionOutcomes = bspTransactionOutcomes,
                  bspRewardDetails = red,
                  bspProtocolLevelTokens = plts
                }
    return ()

-- | Cache the block state and get the initial (empty) transaction table with the next
-- update sequence numbers populated.
cacheStateAndGetTransactionTable ::
    forall pv m.
    (SupportsPersistentState pv m) =>
    HashedPersistentBlockState pv ->
    m TransactionTable.TransactionTable
cacheStateAndGetTransactionTable hpbs = do
    BlockStatePointers{..} <- loadPBS (hpbsPointers hpbs)
    -- cache the account table
    accts <- cache bspAccounts
    -- cache the modules
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
                            & TransactionTable.ttNonFinalizedChainUpdates
                                . at' uty
                                ?~ TransactionTable.emptyNFCUWithSequenceNumber sn
                else return tt
    tt <- foldM updInTT TransactionTable.emptyTransactionTable [minBound ..]
    rels <- cache bspReleaseSchedule
    cdowns <- cache bspAccountsInCooldown
    red <- cache bspRewardDetails
    plts <- cache bspProtocolLevelTokens
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
                  bspAccountsInCooldown = cdowns,
                  bspTransactionOutcomes = bspTransactionOutcomes,
                  bspRewardDetails = red,
                  bspProtocolLevelTokens = plts
                }
    return tt
