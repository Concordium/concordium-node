{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
module Concordium.GlobalState.Persistent.BlockState (
    PersistentBlockState,
    BlockStatePointers(..),
    HashedPersistentBlockState(..),
    hashBlockState,
    PersistentBirkParameters(..),
    makePersistentBirkParameters,
    makePersistent,
    initialPersistentState,
    emptyBlockState,
    PersistentBlockStateContext(..),
    PersistentState,
    PersistentBlockStateMonad(..)
) where

import Data.Serialize
import Data.IORef
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Writer.Strict as MTL
import Data.Foldable
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.Execution ( TransactionSummary, DelegationTarget )
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.ID.Types as ID
import qualified Concordium.ID.Parameters as ID
import Concordium.Crypto.EncryptedTransfers (isZeroEncryptedAmount)
import Concordium.Types.Updates
import Concordium.Types.Queries (PoolStatus(..),CurrentPaydayBakerPoolStatus(..), makePoolPendingChange, RewardStatus'(..))
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Types
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import Concordium.GlobalState.Persistent.Bakers
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.Execution as Transactions
import Concordium.GlobalState.Persistent.Instances(PersistentInstance(..), PersistentInstanceV(..), PersistentInstanceParameters(..))
import Concordium.GlobalState.Instance (Instance(..), InstanceV(..), InstanceParameters(..),makeInstanceHash', instanceAddress)
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlockState.Updates
import Concordium.GlobalState.Persistent.PoolRewards
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Basic.BlockState.Account as TransientAccount
import qualified Concordium.Types.UpdateQueues as UQ
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.Types.Accounts as BaseAccounts
import Concordium.Types.SeedState
import Concordium.Logger (MonadLogger)
import Concordium.Types.HashableTo
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.Utils.Serialization.Put
import Concordium.Utils.Serialization
import Concordium.Utils.BinarySearch
import Concordium.Kontrol.Bakers

-- * Birk parameters

data PersistentBirkParameters (av :: AccountVersion) = PersistentBirkParameters {
    -- |The currently-registered bakers.
    _birkActiveBakers :: !(BufferedRef (PersistentActiveBakers av)),
    -- |The bakers that will be used for the next epoch.
    _birkNextEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers av)),
    -- |The bakers for the current epoch.
    _birkCurrentEpochBakers :: !(HashedBufferedRef (PersistentEpochBakers av)),
    -- |The seed state used to derive the leadership election nonce.
    _birkSeedState :: !SeedState
} deriving (Show)

makeLenses ''PersistentBirkParameters

-- |Serialize 'PersistentBirkParameters' in V0 format.
putBirkParametersV0 :: forall m av. (IsAccountVersion av, MonadBlobStore m, MonadPut m) => PersistentBirkParameters av -> m ()
putBirkParametersV0 PersistentBirkParameters{..} = do
        sPut _birkSeedState
        putEpochBakers =<< refLoad _birkNextEpochBakers
        putEpochBakers =<< refLoad _birkCurrentEpochBakers

instance (IsAccountVersion av, MonadBlobStore m) => MHashableTo m H.Hash (PersistentBirkParameters av) where
  getHashM PersistentBirkParameters {..} = do
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
        return (putBSP, bps {
                    _birkActiveBakers = actBakers,
                    _birkNextEpochBakers = nextBakers,
                    _birkCurrentEpochBakers = currentBakers
                })
    store bps = fst <$> storeUpdate bps
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
        return PersistentBirkParameters{
            _birkActiveBakers = activeBaks,
            _birkNextEpochBakers = next,
            _birkCurrentEpochBakers = cur,
            ..
        }

makePersistentBirkParameters ::
    (IsAccountVersion av, MonadBlobStore m) => Basic.BasicBirkParameters av -> m (PersistentBirkParameters av)
makePersistentBirkParameters bbps = do
    _birkActiveBakers <- refMake =<< makePersistentActiveBakers (Basic._birkActiveBakers bbps)
    _birkNextEpochBakers <-
        refMake =<< makePersistentEpochBakers (_unhashed $ Basic._birkNextEpochBakers bbps)
    _birkCurrentEpochBakers <- refMake =<< makePersistentEpochBakers (_unhashed (Basic._birkCurrentEpochBakers bbps))
    let _birkSeedState = Basic._birkSeedState bbps
    return $ PersistentBirkParameters{..}

-- * Epoch baked blocks

type EpochBlocks = Nullable (BufferedRef EpochBlock)

-- |Structure for tracking which bakers have baked blocks
-- in the current epoch.
data EpochBlock = EpochBlock {
    ebBakerId :: !BakerId,
    ebPrevious :: !EpochBlocks
}

instance (MonadBlobStore m) => BlobStorable m EpochBlock where
    storeUpdate eb@EpochBlock{..} = do
        (ppref, ebPrevious') <- storeUpdate ebPrevious
        let putEB = put ebBakerId >> ppref
        let eb' = eb{ebPrevious = ebPrevious'}
        return (putEB, eb')
    store eb = fst <$> storeUpdate eb
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
    getHashM (Some r) = getHashM =<< refLoad r

data HashedEpochBlocks = HashedEpochBlocks {
        hebBlocks :: !EpochBlocks,
        hebHash :: !Rewards.EpochBlocksHash
    }

instance HashableTo Rewards.EpochBlocksHash HashedEpochBlocks where
    getHash = hebHash

instance MonadBlobStore m => BlobStorable m HashedEpochBlocks where
    storeUpdate heb = do
        (pblocks, blocks') <- storeUpdate (hebBlocks heb)
        return (pblocks, heb{hebBlocks = blocks'})
    store = store . hebBlocks
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
emptyHashedEpochBlocks = HashedEpochBlocks {
        hebBlocks = Null,
        hebHash = Rewards.emptyEpochBlocksHash
    }

-- |Add a new 'BakerId' to the start of a 'HashedEpochBlocks'.
consEpochBlock :: (MonadBlobStore m) => BakerId -> HashedEpochBlocks -> m HashedEpochBlocks
consEpochBlock b hebbs = do
        mbr <- refMake EpochBlock{
                ebBakerId = b,
                ebPrevious = hebBlocks hebbs
            }
        return HashedEpochBlocks {
                hebBlocks = Some mbr,
                hebHash = Rewards.epochBlockHash b (hebHash hebbs)
            }

-- |Make a 'HashedEpochBlocks' from a list of 'BakerId's of the blocks (most recent first).
makeHashedEpochBlocks :: (MonadBlobStore m) => [BakerId] -> m HashedEpochBlocks
makeHashedEpochBlocks [] = return emptyHashedEpochBlocks
makeHashedEpochBlocks (b:bs) = do
        hebbs <- makeHashedEpochBlocks bs
        consEpochBlock b hebbs

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
    BlockRewardDetailsV1 :: !(HashedBufferedRef' Rewards.PoolRewardsHash PoolRewards) -> BlockRewardDetails 'AccountV1

instance MonadBlobStore m => MHashableTo m (Rewards.BlockRewardDetailsHash av) (BlockRewardDetails av) where
    getHashM (BlockRewardDetailsV0 heb) = return $ Rewards.BlockRewardDetailsHashV0 (getHash heb)
    getHashM (BlockRewardDetailsV1 pr) = Rewards.BlockRewardDetailsHashV1 <$> getHashM pr

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (BlockRewardDetails av) where
    storeUpdate (BlockRewardDetailsV0 heb) = fmap (fmap BlockRewardDetailsV0) $ storeUpdate heb
    storeUpdate (BlockRewardDetailsV1 hpr) = fmap (fmap BlockRewardDetailsV1) $ storeUpdate hpr
    store bsp = fst <$> storeUpdate bsp
    load = case accountVersion @av of
        SAccountV0 -> fmap (fmap BlockRewardDetailsV0) load
        SAccountV1 -> fmap (fmap BlockRewardDetailsV1) load

instance MonadBlobStore m => Cacheable m (BlockRewardDetails av) where
    cache (BlockRewardDetailsV0 heb) = BlockRewardDetailsV0 <$> cache heb
    cache (BlockRewardDetailsV1 hpr) = BlockRewardDetailsV1 <$> cache hpr

putBlockRewardDetails :: (MonadBlobStore m, MonadPut m) => BlockRewardDetails av -> m ()
putBlockRewardDetails (BlockRewardDetailsV0 heb) = putHashedEpochBlocksV0 heb
putBlockRewardDetails (BlockRewardDetailsV1 hpr) = refLoad hpr >>= putPoolRewards

makeBlockRewardDetails
    :: MonadBlobStore m
    => Basic.BlockRewardDetails av
    -> m (BlockRewardDetails av)
makeBlockRewardDetails (Basic.BlockRewardDetailsV0 heb) =
    BlockRewardDetailsV0 <$> makeHashedEpochBlocks (Basic.hebBlocks heb)
makeBlockRewardDetails (Basic.BlockRewardDetailsV1 pre) =
    BlockRewardDetailsV1 <$> (makePoolRewards (_unhashed pre) >>= refMake)

-- |Extend a 'BlockRewardDetails' ''AccountV0' with an additional baker.
consBlockRewardDetails
    :: MonadBlobStore m
    => BakerId
    -> BlockRewardDetails 'AccountV0
    -> m (BlockRewardDetails 'AccountV0)
consBlockRewardDetails bid (BlockRewardDetailsV0 heb) = do
    BlockRewardDetailsV0 <$> consEpochBlock bid heb

-- |The empty 'BlockRewardDetails'.
emptyBlockRewardDetails
    :: forall av m
     . (MonadBlobStore m, IsAccountVersion av)
    => m (BlockRewardDetails av)
emptyBlockRewardDetails =
    case accountVersion @av of
        SAccountV0 -> return $ BlockRewardDetailsV0 emptyHashedEpochBlocks
        SAccountV1 -> BlockRewardDetailsV1 <$> (emptyPoolRewards >>= refMake)

-- * Block state

-- |Type representing a persistent block state. This is a 'BufferedRef' inside an 'IORef',
-- which supports making changes to the state without them (necessarily) being written to
-- disk.
type PersistentBlockState (pv :: ProtocolVersion) = IORef (BufferedRef (BlockStatePointers pv))


-- |References to the components that make up the block state.
--
-- This type is parametric in the protocol version (as opposed to defined
-- as a data family) on the principle that the structure will be mostly
-- similar across versions. Where component change between versions,
-- those components themselves should be parametrised by the protocol
-- version.
data BlockStatePointers (pv :: ProtocolVersion) = BlockStatePointers {
    bspAccounts :: !(Accounts.Accounts pv),
    bspInstances :: !(Instances.Instances pv),
    bspModules :: !(HashedBufferedRef Modules.Modules),
    bspBank :: !(Hashed Rewards.BankStatus),
    bspIdentityProviders :: !(HashedBufferedRef IPS.IdentityProviders),
    bspAnonymityRevokers :: !(HashedBufferedRef ARS.AnonymityRevokers),
    bspBirkParameters :: !(PersistentBirkParameters (AccountVersionFor pv)),
    bspCryptographicParameters :: !(HashedBufferedRef CryptographicParameters),
    bspUpdates :: !(BufferedRef (Updates pv)),
    bspReleaseSchedule :: !(BufferedRef (Map.Map AccountAddress Timestamp)),
    -- FIXME: Store transaction outcomes in a way that allows for individual indexing.
    bspTransactionOutcomes :: !Transactions.TransactionOutcomes,
    -- |Details of bakers that baked blocks in the current epoch. This is
    -- used for rewarding bakers at the end of epochs.
    bspRewardDetails :: !(BlockRewardDetails (AccountVersionFor pv))
}

-- |A hashed version of 'PersistingBlockState'.  This is used when the block state
-- is not being mutated so that the hash values are not recomputed constantly.
data HashedPersistentBlockState pv = HashedPersistentBlockState {
    hpbsPointers :: !(PersistentBlockState pv),
    hpbsHash :: !StateHash
}

-- |Convert a 'PersistentBlockState' to a 'HashedPersistentBlockState' by computing
-- the state hash.
hashBlockState :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (HashedPersistentBlockState pv)
hashBlockState hpbsPointers = do
        rbsp <- liftIO $ readIORef hpbsPointers
        bsp <- refLoad rbsp
        hpbsHash <- getHashM bsp
        return HashedPersistentBlockState{..}

instance (IsProtocolVersion pv, MonadBlobStore m) => MHashableTo m StateHash (BlockStatePointers pv) where
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

instance (IsProtocolVersion pv, MonadBlobStore m) => BlobStorable m (BlockStatePointers pv) where
    storeUpdate bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate bspAccounts
        (pinsts, bspInstances') <- storeUpdate bspInstances
        (pmods, bspModules') <- storeUpdate bspModules
        (pips, bspIdentityProviders') <- storeUpdate bspIdentityProviders
        (pars, bspAnonymityRevokers') <- storeUpdate bspAnonymityRevokers
        (pbps, bspBirkParameters') <- storeUpdate bspBirkParameters
        (pcryptps, bspCryptographicParameters') <- storeUpdate bspCryptographicParameters
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
                Transactions.putTransactionOutcomes bspTransactionOutcomes
                pupdates
                preleases
                pRewardDetails
        return (putBSP, bsp0 {
                    bspAccounts = bspAccounts',
                    bspInstances = bspInstances',
                    bspModules = bspModules',
                    bspIdentityProviders = bspIdentityProviders',
                    bspAnonymityRevokers = bspAnonymityRevokers',
                    bspBirkParameters = bspBirkParameters',
                    bspCryptographicParameters = bspCryptographicParameters',
                    bspUpdates = bspUpdates',
                    bspReleaseSchedule = bspReleaseSchedule',
                    bspRewardDetails = bspRewardDetails'
                })
    store bsp = fst <$> storeUpdate bsp
    load = do
        maccts <- label "Accounts" load
        minsts <- label "Instances" load
        mmods <- label "Modules" load
        bspBank <- makeHashed <$> label "Bank" get
        mpips <- label "Identity providers" load
        mars <- label "Anonymity revokers" load
        mbps <- label "Birk parameters" load
        mcryptps <- label "Cryptographic parameters" load
        bspTransactionOutcomes <- label "Transaction outcomes" $
            Transactions.getTransactionOutcomes (protocolVersion @pv)
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
            bspUpdates <- mUpdates
            bspReleaseSchedule <- mReleases
            bspRewardDetails <- mRewardDetails
            return $! BlockStatePointers{..}

instance (MonadBlobStore m, IsProtocolVersion pv) => Cacheable m (BlockStatePointers pv) where
    cache BlockStatePointers{..} = do
        accts <- cache bspAccounts
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
        red <- cache bspRewardDetails
        return BlockStatePointers{
            bspAccounts = accts,
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

-- |Accessor for getting the pool rewards when supported by the protocol version.
bspPoolRewards :: AccountVersionFor pv ~ 'AccountV1 => BlockStatePointers pv -> HashedBufferedRef' Rewards.PoolRewardsHash PoolRewards
bspPoolRewards bsp = case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of
    BlockRewardDetailsV1 pr -> pr

-- |Convert an in-memory 'Basic.BlockState' to a disk-backed 'HashedPersistentBlockState'.
makePersistent :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => Basic.BlockState pv -> m (HashedPersistentBlockState pv)
makePersistent Basic.BlockState{..} = do
  persistentBirkParameters <- makePersistentBirkParameters _blockBirkParameters
  persistentMods <- Modules.makePersistentModules _blockModules
  persistentBlockInstances <- Instances.makePersistent persistentMods _blockInstances
  modules <- refMake persistentMods
  identityProviders <- bufferHashed _blockIdentityProviders
  anonymityRevokers <- bufferHashed _blockAnonymityRevokers
  cryptographicParameters <- bufferHashed _blockCryptographicParameters
  blockAccounts <- Accounts.makePersistent _blockAccounts
  updates <- makeBufferedRef =<< makePersistentUpdates _blockUpdates
  rels <- makeBufferedRef _blockReleaseSchedule
  red <- makeBlockRewardDetails _blockRewardDetails
  bsp <-
    makeBufferedRef $
      BlockStatePointers
        { bspAccounts = blockAccounts,
          bspInstances = persistentBlockInstances,
          bspModules = modules,
          bspBank = _blockBank,
          bspIdentityProviders = identityProviders,
          bspAnonymityRevokers = anonymityRevokers,
          bspBirkParameters = persistentBirkParameters,
          bspCryptographicParameters = cryptographicParameters,
          bspTransactionOutcomes = _blockTransactionOutcomes,
          bspUpdates = updates,
          bspReleaseSchedule = rels,
          bspRewardDetails = red
        }
  bps <- liftIO $ newIORef $! bsp
  hashBlockState bps

-- |An initial 'HashedPersistentBlockState', which may be used for testing purposes.
initialPersistentState :: (IsProtocolVersion pv, MonadBlobStore m)
             => SeedState
             -> CryptographicParameters
             -> [TransientAccount.Account (AccountVersionFor pv)]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> UpdateKeysCollection (ChainParametersVersionFor pv)
             -> ChainParameters pv
             -> m (HashedPersistentBlockState pv)
initialPersistentState ss cps accts ips ars keysCollection chainParams = makePersistent $ Basic.initialState ss cps accts ips ars keysCollection chainParams

-- |A mostly empty block state, but with the given birk parameters, 
-- cryptographic parameters, update authorizations and chain parameters.
emptyBlockState
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBirkParameters (AccountVersionFor pv)
    -> CryptographicParameters
    -> UpdateKeysCollection (ChainParametersVersionFor pv)
    -> ChainParameters pv
    -> m (PersistentBlockState pv)
{-# WARNING emptyBlockState "should only be used for testing" #-}
emptyBlockState bspBirkParameters cryptParams keysCollection chainParams = do
  modules <- refMake Modules.emptyModules
  identityProviders <- refMake IPS.emptyIdentityProviders
  anonymityRevokers <- refMake ARS.emptyAnonymityRevokers
  cryptographicParameters <- refMake cryptParams
  bspUpdates <- refMake =<< initialUpdates keysCollection chainParams
  bspReleaseSchedule <- refMake Map.empty
  bspRewardDetails <- emptyBlockRewardDetails
  bsp <- makeBufferedRef $ BlockStatePointers
          { bspAccounts = Accounts.emptyAccounts,
            bspInstances = Instances.emptyInstances,
            bspModules = modules,
            bspBank = makeHashed Rewards.emptyBankStatus,
            bspIdentityProviders = identityProviders,
            bspAnonymityRevokers = anonymityRevokers,
            bspCryptographicParameters = cryptographicParameters,
            bspTransactionOutcomes = Transactions.emptyTransactionOutcomes,
            ..
          }
  liftIO $ newIORef $! bsp

-- |Serialize the block state. The format may depend on the protocol version.
putBlockStateV0 :: (IsProtocolVersion pv, MonadBlobStore m, MonadPut m) => PersistentBlockState pv -> m ()
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

loadPBS :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (BlockStatePointers pv)
loadPBS = loadBufferedRef <=< liftIO . readIORef
{-# INLINE loadPBS #-}

storePBS :: MonadBlobStore m => PersistentBlockState pv -> BlockStatePointers pv -> m (PersistentBlockState pv)
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
    (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) =>
    BlockStatePointers pv ->
    BakerId ->
    m Amount
poolDelegatorCapital bsp bid = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    Trie.lookup bid (pab ^. activeBakers) >>= \case
        Nothing -> return 0
        Just PersistentActiveDelegatorsV1{..} -> return adDelegatorTotalCapital

-- | Get the total passively-delegated capital.
passiveDelegationCapital :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m)
    => BlockStatePointers pv
    -> m Amount
passiveDelegationCapital bsp = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    return $! adDelegatorTotalCapital (pab ^. passiveDelegators)

-- | Get the total capital currently staked by bakers and delegators.
-- Note, this is separate from the stake and capital distribution used for the current payday, as
-- it reflects the current value of accounts.
totalCapital :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => BlockStatePointers pv -> m Amount
totalCapital bsp = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    return $! pab ^. totalActiveCapitalV1

doGetModule :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ModuleRef -> m (Maybe GSWasm.ModuleInterface)
doGetModule s modRef = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    Modules.getInterface modRef mods

doGetModuleList :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [ModuleRef]
doGetModuleList s = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    return $ Modules.moduleRefList mods

doGetModuleSource :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ModuleRef -> m (Maybe Wasm.WasmModule)
doGetModuleSource s modRef = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    Modules.getSource modRef mods

doPutNewModule :: (IsProtocolVersion pv, Wasm.IsWasmVersion v, MonadBlobStore m)
    => PersistentBlockState pv
    -> (GSWasm.ModuleInterfaceV v, Wasm.WasmModuleV v)
    -> m (Bool, PersistentBlockState pv)
doPutNewModule pbs (pmInterface, pmSource) = do
        bsp <- loadPBS pbs
        mods <- refLoad (bspModules bsp)
        mMods' <- Modules.putInterface (pmInterface, pmSource) mods
        case mMods' of
          Nothing -> return (False, pbs)
          Just mods' -> do
            modules <- refMake mods'
            (True,) <$> storePBS pbs (bsp {bspModules = modules})

doGetSeedState :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m SeedState
doGetSeedState pbs = _birkSeedState . bspBirkParameters <$> loadPBS pbs

doSetSeedState :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> SeedState -> m (PersistentBlockState pv)
doSetSeedState pbs ss = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBirkParameters = (bspBirkParameters bsp){_birkSeedState = ss}}

doGetCurrentEpochBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m FullBakers
doGetCurrentEpochBakers pbs = epochToFullBakers =<< refLoad . _birkCurrentEpochBakers . bspBirkParameters =<< loadPBS pbs

doGetCurrentEpochFullBakersEx :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> m FullBakersEx
doGetCurrentEpochFullBakersEx pbs = epochToFullBakersEx =<< refLoad . _birkCurrentEpochBakers . bspBirkParameters =<< loadPBS pbs

doGetCurrentCapitalDistribution :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> m CapitalDistribution
doGetCurrentCapitalDistribution pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of BlockRewardDetailsV1 hp -> hp
    poolRewards <- refLoad hpr
    refLoad $ currentCapital poolRewards

doGetNextEpochBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m FullBakers
doGetNextEpochBakers pbs = do
    bsp <- loadPBS pbs
    epochToFullBakers =<< refLoad (bspBirkParameters bsp ^. birkNextEpochBakers)

doGetSlotBakersP1 :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV0, MonadBlobStore m) => PersistentBlockState pv -> Slot -> m FullBakers
doGetSlotBakersP1 pbs slot = do
        bs <- loadPBS pbs
        let
            bps = bspBirkParameters bs
            SeedState{..} = bps ^. birkSeedState
            slotEpoch = fromIntegral $ slot `quot` epochLength
        case compare slotEpoch (epoch + 1) of
            LT -> epochToFullBakers =<< refLoad (bps ^. birkCurrentEpochBakers)
            EQ -> epochToFullBakers =<< refLoad (bps ^. birkNextEpochBakers)
            GT -> do
                activeBids <- Trie.keysAsc . _activeBakers =<< refLoad (bps ^. birkActiveBakers)
                let resolveBaker (BakerId aid) = Accounts.indexedAccount aid (bspAccounts bs) >>= \case
                        Just acct -> case acct ^. accountBaker of
                            Some bkr -> do
                                pab <- refLoad bkr
                                abi <- refLoad (pab ^. accountBakerInfo)
                                return $ case _bakerPendingChange pab of
                                    BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 remEpoch)
                                        | remEpoch < slotEpoch -> Nothing
                                    BaseAccounts.ReduceStake newAmt (BaseAccounts.PendingChangeEffectiveV0 redEpoch)
                                        | redEpoch < slotEpoch -> Just (FullBakerInfo (abi ^. BaseAccounts.bakerInfo) newAmt)
                                    _ -> Just (FullBakerInfo (abi ^. BaseAccounts.bakerInfo) (pab ^. stakedAmount))
                            Null -> error "Persistent.getSlotBakers invariant violation: active baker account not a baker"
                        Nothing -> error "Persistent.getSlotBakers invariant violation: active baker account not valid"
                futureBakers <- Vec.fromList . catMaybes <$> mapM resolveBaker activeBids
                return FullBakers {
                    fullBakerInfos = futureBakers,
                    bakerTotalStake = sum (_bakerStake <$> futureBakers)
                }

doGetBakerAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
doGetBakerAccount pbs (BakerId ai) = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp)

doTransitionEpochBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Epoch -> m (PersistentBlockState pv)
doTransitionEpochBakers pbs newEpoch = do
        bsp <- loadPBS pbs
        let oldBPs = bspBirkParameters bsp
        curActiveBIDs <- Trie.keysAsc . _activeBakers =<< refLoad (_birkActiveBakers oldBPs)
        -- Retrieve/update the baker info
        let accumBakers (bs0, bkrs0) bkr@(BakerId aid) = Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
                Just PersistentAccount{_accountStake = PersistentAccountStakeBaker acctBkrRef} -> do
                    acctBkr <- refLoad acctBkrRef
                    case _bakerPendingChange acctBkr of
                        BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 remEpoch)
                            -- Removal takes effect next epoch, so exclude it from the list of bakers
                            | remEpoch == newEpoch + 1 -> return (bs0, bkrs0)
                            -- Removal complete, so update the active bakers and account as well
                            | remEpoch <= newEpoch -> do
                                -- Remove the baker from the active bakers
                                curABs <- refLoad (_birkActiveBakers (bspBirkParameters bs0))
                                newAB <- Trie.delete bkr (_activeBakers curABs)
                                abi <- refLoad (_accountBakerInfo acctBkr)
                                newAK <- Trie.delete (BaseAccounts._bakerAggregationVerifyKey abi) (_aggregationKeys curABs)
                                newABs <- refMake $ PersistentActiveBakers {
                                        _activeBakers = newAB,
                                        _aggregationKeys = newAK,
                                        _passiveDelegators = curABs ^. passiveDelegators,
                                        _totalActiveCapital = TotalActiveCapitalV0
                                    }
                                -- Remove the baker from the account
                                let updAcc acc = ((),) <$> setPersistentAccountStake acc PersistentAccountStakeNone
                                (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs0)
                                -- The baker is not included for this epoch
                                return (bs0 {
                                        bspBirkParameters = (bspBirkParameters bs0) {_birkActiveBakers = newABs},
                                        bspAccounts = newAccounts
                                    }, bkrs0)
                        BaseAccounts.ReduceStake newAmt (BaseAccounts.PendingChangeEffectiveV0 redEpoch)
                            -- Reduction takes effect next epoch, so apply it in the generated list
                            | redEpoch == newEpoch + 1 -> return (bs0, (acctBkr ^. accountBakerInfoEx, newAmt) : bkrs0)
                            -- Reduction complete, so update the account as well
                            | redEpoch <= newEpoch -> do
                                -- Reduce the baker's stake on the account
                                newBaker <- refMake acctBkr{_stakedAmount = newAmt, _bakerPendingChange = BaseAccounts.NoChange}
                                let updAcc acc = ((),) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newBaker)
                                (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs0)
                                -- The baker is included with the revised stake
                                return (bs0 {bspAccounts = newAccounts}, (acctBkr ^. accountBakerInfoEx, newAmt) : bkrs0)
                        _ -> return (bs0, (acctBkr ^. accountBakerInfoEx, _stakedAmount acctBkr) : bkrs0)
                _ -> error "Persistent.bsoTransitionEpochBakers invariant violation: active baker account not a valid baker"
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
        storePBS pbs bsp'{bspBirkParameters = (bspBirkParameters bsp') {
            _birkCurrentEpochBakers = newCurrentBakers,
            _birkNextEpochBakers = newNextBakers
          }
        }
    where
        secondIfEqual a b = do
            h1 <- getHashM a
            h2 <- getHashM b
            return $ if (h1 :: H.Hash) == h2 then b else a

doGetActiveBakersAndDelegators
    :: forall pv m
     . (IsProtocolVersion pv,
        MonadBlobStore m,
        AccountVersionFor pv ~ 'AccountV1,
        BakerInfoRef m ~ PersistentBakerInfoEx 'AccountV1)
    => PersistentBlockState pv -> m ([ActiveBakerInfo m], [ActiveDelegatorInfo])
doGetActiveBakersAndDelegators pbs = do
    bsp <- loadPBS pbs
    ab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    abis <- Trie.toAscList (ab ^. activeBakers) >>= mapM (mkActiveBakerInfo bsp)
    let PersistentActiveDelegatorsV1 dset _ = ab ^. passiveDelegators
    lps <- Trie.keys dset >>= mapM (mkActiveDelegatorInfo bsp)
    return (abis, lps)
      where
            mkActiveBakerInfo bsp (BakerId acct, PersistentActiveDelegatorsV1 dlgs _) = do
                theBaker <- Accounts.indexedAccount acct (bspAccounts bsp) >>= \case
                    Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pab} ->
                        refLoad pab
                    _ -> error "Invariant violation: active baker is not a baker account"
                dlglist <- Trie.keysAsc dlgs
                abd <- mapM (mkActiveDelegatorInfo bsp) dlglist
                return ActiveBakerInfo {
                    activeBakerInfoRef = theBaker ^. accountBakerInfoEx,
                    activeBakerEquityCapital = theBaker ^. stakedAmount,
                    activeBakerPendingChange = theBaker ^. bakerPendingChange,
                    activeBakerDelegators = abd
                }
            mkActiveDelegatorInfo :: BlockStatePointers pv -> DelegatorId -> m ActiveDelegatorInfo
            mkActiveDelegatorInfo bsp activeDelegatorId@(DelegatorId acct) = do
                theDelegator@BaseAccounts.AccountDelegationV1{} <-
                    Accounts.indexedAccount acct (bspAccounts bsp) >>= \case
                        Just PersistentAccount{_accountStake = PersistentAccountStakeDelegate pad} ->
                            refLoad pad
                        _ -> error "Invariant violation: active baker is not a baker account"
                return ActiveDelegatorInfo{
                    activeDelegatorStake = theDelegator ^. BaseAccounts.delegationStakedAmount,
                    activeDelegatorPendingChange = theDelegator ^. BaseAccounts.delegationPendingChange,
                    ..
                }

doAddBaker
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0)
    => PersistentBlockState pv
    -> AccountIndex
    -> BakerAdd
    -> m (BakerAddResult, PersistentBlockState pv)
doAddBaker pbs ai BakerAdd{..} = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- Cannot resolve the account
            Nothing -> return (BAInvalidAccount, pbs)
            -- Account is already a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker{}} -> return (BAAlreadyBaker (BakerId ai), pbs)
            Just PersistentAccount{} -> do
                  cp <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> doGetChainParameters pbs
                  if baStake < cp then
                      return (BAStakeUnderThreshold, pbs)
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
                            newpabref <- refMake PersistentActiveBakers{
                                    _aggregationKeys = newAggregationKeys,
                                    _activeBakers = newActiveBakers,
                                    _passiveDelegators = pab ^. passiveDelegators,
                                    _totalActiveCapital = TotalActiveCapitalV0
                                }
                            let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                            let updAcc acc = do
                                    newBakerInfo <- refMake $
                                        bakerKeyUpdateToInfo bid baKeys
                                    newPAB <- refMake PersistentAccountBaker{
                                        _stakedAmount = baStake,
                                        _stakeEarnings = baStakeEarnings,
                                        _accountBakerInfo = newBakerInfo,
                                        _extraBakerInfo = PersistentExtraBakerInfo (),
                                        _bakerPendingChange = BaseAccounts.NoChange
                                    }
                                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                                    return ((), acc')
                            -- This cannot fail to update the account, since we already looked up the account.
                            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                            (BASuccess bid,) <$> storePBS pbs bsp{
                                bspBirkParameters = newBirkParams,
                                bspAccounts = newAccounts
                            }

doConfigureBaker
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1)
    => PersistentBlockState pv
    -> AccountIndex
    -> BakerConfigure
    -> m (BakerConfigureResult, PersistentBlockState pv)
doConfigureBaker pbs ai BakerConfigureAdd{..} = do
        -- It is assumed here that this account is NOT a baker and NOT a delegator.
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- Cannot resolve the account
            Nothing -> return (BCInvalidAccount, pbs)
            Just PersistentAccount{} -> do
                chainParams <- doGetChainParameters pbs
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
                            newpabref <- refMake PersistentActiveBakers{
                                    _aggregationKeys = newAggregationKeys,
                                    _activeBakers = newActiveBakers,
                                    _passiveDelegators = pab ^. passiveDelegators,
                                    _totalActiveCapital = addActiveCapital bcaCapital (_totalActiveCapital pab)
                                }
                            let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                            let updAcc acc = do
                                    let cr = CommissionRates {
                                            _finalizationCommission = bcaFinalizationRewardCommission,
                                            _bakingCommission = bcaBakingRewardCommission,
                                            _transactionCommission = bcaTransactionFeeCommission
                                        }
                                    bpi <- refMake BaseAccounts.BakerPoolInfo {
                                            _poolOpenStatus = bcaOpenForDelegation,
                                            _poolMetadataUrl = bcaMetadataURL,
                                            _poolCommissionRates = cr
                                        }
                                    newBakerInfo <- refMake $ bakerKeyUpdateToInfo bid bcaKeys
                                    newPAB <- refMake PersistentAccountBaker{
                                        _stakedAmount = bcaCapital,
                                        _stakeEarnings = bcaRestakeEarnings,
                                        _accountBakerInfo = newBakerInfo,
                                        _extraBakerInfo = PersistentExtraBakerInfo bpi,
                                        _bakerPendingChange = BaseAccounts.NoChange
                                    }
                                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                                    return ((), acc')
                            -- This cannot fail to update the account, since we already looked up the account.
                            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                            (BCSuccess [] bid,) <$> storePBS pbs bsp{
                                bspBirkParameters = newBirkParams,
                                bspAccounts = newAccounts
                            }
doConfigureBaker pbs ai BakerConfigureUpdate{..} = do
        origBSP <- loadPBS pbs
        cp <- doGetChainParameters pbs
        res <- MTL.runExceptT $ MTL.runWriterT $ flip MTL.execStateT origBSP $ do
                updateKeys
                updateRestakeEarnings
                updateOpenForDelegation
                updateMetadataURL
                updateTransactionFeeCommission cp
                updateBakingRewardCommission cp
                updateFinalizationRewardCommission cp
                updateCapital cp
        case res of
            Left errorRes -> return (errorRes, pbs)
            Right (newBSP, changes) -> (BCSuccess changes bid,) <$> storePBS pbs newBSP
      where
        liftBSO = lift . lift . lift
        bid = BakerId ai
        getAccountOrFail = do
            bsp <- MTL.get
            liftBSO (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
                Nothing -> MTL.throwError BCInvalidAccount
                Just PersistentAccount{_accountStake = PersistentAccountStakeBaker ab} -> liftBSO $ refLoad ab
                Just PersistentAccount{} -> MTL.throwError BCInvalidBaker
        modifyAccount updAcc = do
            bsp <- MTL.get
            (_, newAccounts) <- liftBSO $ Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
            MTL.put bsp{bspAccounts = newAccounts}
        updateKeys = forM_ bcuKeys $ \keys -> do
            acctBkr <- getAccountOrFail
            bsp <- MTL.get
            pab <- liftBSO $ refLoad (_birkActiveBakers (bspBirkParameters bsp))
            bkrInfo <- liftBSO $ refLoad (_accountBakerInfo acctBkr)
            let key = BaseAccounts._bakerAggregationVerifyKey bkrInfo
            -- Try updating the aggregation keys
            (keyOK, newAggregationKeys) <-
                    -- If the aggregation key has not changed, we have nothing to do.
                    if bkuAggregationKey keys == key then
                        return (True, _aggregationKeys pab)
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
            -- Update the account with the new keys
            let updAcc acc = do
                    newBakerInfo <- refMake $ bakerKeyUpdateToInfo (BakerId ai) keys
                    newPAB <- refMake $ acctBkr {_accountBakerInfo = newBakerInfo}
                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                    return ((), acc')
            modifyAccount updAcc
            MTL.modify' $ \s -> s{bspBirkParameters = newBirkParams}
            MTL.tell [BakerConfigureUpdateKeys keys]
        updateRestakeEarnings = forM_ bcuRestakeEarnings $ \restakeEarnings -> do
            acctBkr <- getAccountOrFail
            unless (acctBkr ^. stakeEarnings == restakeEarnings) $ do
                let updAcc acc = do
                        newPAB <- refMake (acctBkr & stakeEarnings .~ restakeEarnings)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureRestakeEarnings restakeEarnings]
        updateOpenForDelegation = forM_ bcuOpenForDelegation $ \openForDelegation -> do
            acctBkr <- getAccountOrFail
            ebi <- liftBSO $ refLoad $ acctBkr ^. bakerPoolInfoRef
            unless (ebi ^. BaseAccounts.poolOpenStatus == openForDelegation) $ do
                let updAcc acc = do
                        newEbi <- refMake (ebi & BaseAccounts.poolOpenStatus .~ openForDelegation)
                        newPAB <- refMake (acctBkr & extraBakerInfo .~ PersistentExtraBakerInfo newEbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
                when (openForDelegation == Transactions.ClosedForAll) $ do
                    -- Transfer all existing delegators to passive delegation.
                    birkParams <- MTL.gets bspBirkParameters
                    activeBkrs <- liftBSO $ refLoad (birkParams ^. birkActiveBakers)
                    -- Update the active bakers
                    (delegators, newActiveBkrs) <- transferDelegatorsToPassive bid activeBkrs
                    newActiveBkrsRef <- refMake newActiveBkrs
                    MTL.modify $ \bsp -> bsp{bspBirkParameters = birkParams & birkActiveBakers .~ newActiveBkrsRef}
                    -- Update each baker account
                    forM_ delegators redelegatePassive
            MTL.tell [BakerConfigureOpenForDelegation openForDelegation]
        updateMetadataURL = forM_ bcuMetadataURL $ \metadataURL -> do
            acctBkr <- getAccountOrFail
            ebi <- liftBSO $ refLoad $ acctBkr ^. bakerPoolInfoRef
            unless (ebi ^. BaseAccounts.poolMetadataUrl == metadataURL) $ do
                let updAcc acc = do
                        newEbi <- refMake (ebi & BaseAccounts.poolMetadataUrl .~ metadataURL)
                        newPAB <- refMake (acctBkr & extraBakerInfo .~ PersistentExtraBakerInfo newEbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureMetadataURL metadataURL]
        updateTransactionFeeCommission cp = forM_ bcuTransactionFeeCommission $ \tfc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
            unless (isInRange tfc range) (MTL.throwError BCTransactionFeeCommissionNotInRange)
            acctBkr <- getAccountOrFail
            ebi <- liftBSO $ refLoad $ acctBkr ^. bakerPoolInfoRef
            unless (ebi ^. BaseAccounts.poolCommissionRates . transactionCommission == tfc) $ do
                let updAcc acc = do
                        newEbi <- refMake (ebi & BaseAccounts.poolCommissionRates . transactionCommission .~ tfc)
                        newPAB <- refMake (acctBkr & extraBakerInfo .~ PersistentExtraBakerInfo newEbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureTransactionFeeCommission tfc]
        updateBakingRewardCommission cp = forM_ bcuBakingRewardCommission $ \brc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . bakingCommissionRange
            unless (isInRange brc range) (MTL.throwError BCBakingRewardCommissionNotInRange)
            acctBkr <- getAccountOrFail
            ebi <- liftBSO $ refLoad $ acctBkr ^. bakerPoolInfoRef
            unless (ebi ^. BaseAccounts.poolCommissionRates . bakingCommission == brc) $ do
                let updAcc acc = do
                        newEbi <- refMake (ebi & BaseAccounts.poolCommissionRates . bakingCommission .~ brc)
                        newPAB <- refMake (acctBkr & extraBakerInfo .~ PersistentExtraBakerInfo newEbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureBakingRewardCommission brc]
        updateFinalizationRewardCommission cp = forM_ bcuFinalizationRewardCommission $ \frc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . finalizationCommissionRange
            unless (isInRange frc range) (MTL.throwError BCFinalizationRewardCommissionNotInRange)
            acctBkr <- getAccountOrFail
            ebi <- liftBSO $ refLoad $ acctBkr ^. bakerPoolInfoRef
            unless (ebi ^. BaseAccounts.poolCommissionRates . finalizationCommission == frc) $ do
                let updAcc acc = do
                        newEbi <- refMake (ebi & BaseAccounts.poolCommissionRates . finalizationCommission .~ frc)
                        newPAB <- refMake (acctBkr & extraBakerInfo .~ PersistentExtraBakerInfo newEbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureFinalizationRewardCommission frc]
        updateCapital cp = forM_ bcuCapital $ \capital -> do
            acctBkr <- getAccountOrFail
            when (_bakerPendingChange acctBkr /= BaseAccounts.NoChange) (MTL.throwError BCChangePending)
            let capitalMin = cp ^. cpPoolParameters . ppMinimumEquityCapital
            let updAcc updateStake acc = do
                    newPAB <- refMake $ updateStake acctBkr
                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                    return ((), acc')
            let cooldownDuration = cp ^. cpCooldownParameters . cpPoolOwnerCooldown
                cooldownElapsed = addDurationSeconds bcuSlotTimestamp cooldownDuration
            if capital == 0 then do
                let bpc = BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                modifyAccount $ updAcc $ bakerPendingChange .~ bpc
                MTL.tell [BakerConfigureStakeReduced capital]
            else do
                when (capital < capitalMin) (MTL.throwError BCStakeUnderThreshold)
                case compare capital (_stakedAmount acctBkr) of
                    LT -> do
                        let bpc = BaseAccounts.ReduceStake capital (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                        modifyAccount $ updAcc $ bakerPendingChange .~ bpc
                        MTL.tell [BakerConfigureStakeReduced capital]
                    EQ ->
                        return ()
                    GT -> do
                        modifyAccount $ updAcc $ stakedAmount .~ capital
                        birkParams <- MTL.gets bspBirkParameters
                        activeBkrs <- liftBSO $ refLoad (birkParams ^. birkActiveBakers)
                        newActiveBkrs <- liftBSO $ refMake $ activeBkrs &
                            totalActiveCapital %~ addActiveCapital (capital - _stakedAmount acctBkr)
                        MTL.modify' $ \bsp -> bsp{bspBirkParameters = birkParams & birkActiveBakers .~ newActiveBkrs}
                        MTL.tell [BakerConfigureStakeIncreased capital]

doConstrainBakerCommission :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1)
    => PersistentBlockState pv -> AccountIndex -> CommissionRanges -> m (PersistentBlockState pv)
doConstrainBakerCommission pbs ai ranges = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            Nothing -> return pbs
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pab} -> do
                ab <- refLoad pab
                ebi <- refLoad $ ab ^. bakerPoolInfoRef
                let oldRates = ebi ^. BaseAccounts.poolCommissionRates
                let newRates = updateRates oldRates
                if oldRates == newRates then
                    return pbs
                else do
                    let updAcc acc = do
                            pebi' <- refMake $ ebi & BaseAccounts.poolCommissionRates .~ newRates
                            pab' <- refMake $ ab & bakerPoolInfoRef .~ pebi'
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker pab')
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    storePBS pbs bsp{bspAccounts = newAccounts}
            _ -> return pbs
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
delegationConfigureDisallowOverdelegation
    :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MTL.MonadError DelegationConfigureResult m, MonadBlobStore m)
    => BlockStatePointers pv
    -> PoolParameters 'ChainParametersV1
    -> DelegationTarget
    -> m ()
delegationConfigureDisallowOverdelegation bsp poolParams target = case target of
  Transactions.DelegatePassive -> return ()
  Transactions.DelegateToBaker bid@(BakerId baid) -> do
    bakerEquityCapital <- Accounts.indexedAccount baid (bspAccounts bsp) >>= \case
      Just PersistentAccount{_accountStake = PersistentAccountStakeBaker abr} ->
          _stakedAmount <$> refLoad abr
      _ ->
          MTL.throwError (DCInvalidDelegationTarget bid)
    capitalTotal <- totalCapital bsp
    bakerDelegatedCapital <- poolDelegatorCapital bsp bid
    let PoolCaps{..} = delegatedCapitalCaps poolParams capitalTotal bakerEquityCapital bakerDelegatedCapital
    when (bakerDelegatedCapital > leverageCap) $ MTL.throwError DCPoolStakeOverThreshold
    when (bakerDelegatedCapital > boundCap) $ MTL.throwError DCPoolOverDelegated

-- |Check that a delegation target is open for delegation.
-- If the target is not a baker, this throws 'DCInvalidDelegationTarget'.
-- If the target is not open for all, this throws 'DCPoolClosed'.
delegationCheckTargetOpen
    :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MTL.MonadError DelegationConfigureResult m, MonadBlobStore m)
    => BlockStatePointers pv
    -> DelegationTarget
    -> m ()
delegationCheckTargetOpen _ Transactions.DelegatePassive = return ()
delegationCheckTargetOpen bsp (Transactions.DelegateToBaker bid@(BakerId baid)) = do
    Accounts.indexedAccount baid (bspAccounts bsp) >>= \case
        Just PersistentAccount{_accountStake = PersistentAccountStakeBaker abr} -> do
            baker <- refLoad abr
            poolInfo <- refLoad (baker ^. bakerPoolInfoRef)
            case poolInfo ^. BaseAccounts.poolOpenStatus of
                Transactions.OpenForAll -> return ()
                _ -> MTL.throwError DCPoolClosed
        _ -> MTL.throwError (DCInvalidDelegationTarget bid)

doConfigureDelegation
    :: forall pv m
     . (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1)
    => PersistentBlockState pv
    -> AccountIndex
    -> DelegationConfigure
    -> m (DelegationConfigureResult, PersistentBlockState pv)
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
          updateBlockState bsp = lift (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
            Nothing -> MTL.throwError DCInvalidAccount
            Just PersistentAccount{} -> do
                delegationCheckTargetOpen bsp dcaDelegationTarget
                newBirkParams <- updateBirk bsp dcaDelegationTarget
                let updAcc acc = do
                        newPAD <- refMake BaseAccounts.AccountDelegationV1{
                            BaseAccounts._delegationIdentity = did,
                            BaseAccounts._delegationStakedAmount = dcaCapital,
                            BaseAccounts._delegationStakeEarnings = dcaRestakeEarnings,
                            BaseAccounts._delegationTarget = dcaDelegationTarget,
                            BaseAccounts._delegationPendingChange = BaseAccounts.NoChange
                        }
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                -- This cannot fail to update the accounts, since we already looked up the accounts:
                (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                return bsp{bspBirkParameters = newBirkParams, bspAccounts = newAccounts}
          updateBirk bsp Transactions.DelegatePassive = lift $ do
            ab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
            let PersistentActiveDelegatorsV1 dset tot = ab ^. passiveDelegators
            newDset <- Trie.insert did () dset
            newAB <- refMake ab{
                    _passiveDelegators = PersistentActiveDelegatorsV1 newDset (tot + dcaCapital),
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
                updateDelegationTarget
                updateRestakeEarnings
                updateCapital cp
                checkOverdelegation cp
        case res of
            Left errorRes -> return (errorRes, pbs)
            Right (newBSP, changes) -> (DCSuccess changes did,) <$> storePBS pbs newBSP
      where
        did = DelegatorId ai
        getAccountOrFail = do
            bsp <- MTL.get
            Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
                Nothing -> MTL.throwError DCInvalidAccount
                Just PersistentAccount{_accountStake = PersistentAccountStakeDelegate ad} -> refLoad ad
                Just PersistentAccount{} -> MTL.throwError DCInvalidDelegator
        modifyAccount updAcc = do
            bsp <- MTL.get
            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
            MTL.put bsp{
                bspAccounts = newAccounts
            }
        updateDelegationTarget = forM_ dcuDelegationTarget $ \target -> do
            acctDlg <- getAccountOrFail
            let oldTarget = acctDlg ^. BaseAccounts.delegationTarget
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
                ab2 <- addDelegator target did stakedAmt ab1 >>= \case
                    Left bid -> MTL.throwError (DCInvalidDelegationTarget bid)
                    Right ab2 -> return ab2
                newActiveBakers <- refMake ab2
                MTL.modify' $ \bsp -> bsp{bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newActiveBakers}
                -- Update the account with the new delegation target.
                let updAcc acc = do
                        newPAD <- refMake (acctDlg & BaseAccounts.delegationTarget .~ target)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                modifyAccount updAcc
            MTL.tell [DelegationConfigureDelegationTarget target]
        updateRestakeEarnings = forM_ dcuRestakeEarnings $ \restakeEarnings -> do
            acctDlg <- getAccountOrFail
            unless (acctDlg ^. BaseAccounts.delegationStakeEarnings == restakeEarnings) $ do
                let updAcc acc = do
                        newPAD <- refMake (acctDlg & BaseAccounts.delegationStakeEarnings .~ restakeEarnings)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                modifyAccount updAcc
            MTL.tell [DelegationConfigureRestakeEarnings restakeEarnings]
        updateCapital cp = forM_ dcuCapital $ \capital -> do
            ad <- getAccountOrFail
            when (BaseAccounts._delegationPendingChange ad /= BaseAccounts.NoChange) (MTL.throwError DCChangePending)
            let updAcc updateStake acc = do
                    newPAD <- refMake $ updateStake ad
                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                    return ((), acc')
            -- Cooldown time, used when the change reduces or removes the stake.
            let cooldownDuration = cp ^. cpCooldownParameters . cpDelegatorCooldown
                cooldownElapsed = addDurationSeconds dcuSlotTimestamp cooldownDuration
            if capital == 0 then do
                let dpc = BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                modifyAccount $ updAcc $ BaseAccounts.delegationPendingChange .~ dpc
                MTL.tell [DelegationConfigureStakeReduced capital]
            else case compare capital (BaseAccounts._delegationStakedAmount ad) of
                LT -> do
                    let dpc = BaseAccounts.ReduceStake capital (BaseAccounts.PendingChangeEffectiveV1 cooldownElapsed)
                    modifyAccount $ updAcc $ BaseAccounts.delegationPendingChange .~ dpc
                    MTL.tell [DelegationConfigureStakeReduced capital]
                EQ ->
                    return ()
                GT -> do
                    bsp1 <- MTL.get
                    ab <- refLoad (bspBirkParameters bsp1 ^. birkActiveBakers)
                    newActiveBakers <- addTotalsInActiveBakers ab ad (capital - BaseAccounts._delegationStakedAmount ad)
                    MTL.modify' $ \bsp -> bsp{bspBirkParameters = bspBirkParameters bsp1 & birkActiveBakers .~ newActiveBakers}
                    modifyAccount $ updAcc $ BaseAccounts.delegationStakedAmount .~ capital
                    MTL.tell [DelegationConfigureStakeIncreased capital]
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
        checkOverdelegation cp = when (isJust dcuCapital || isJust dcuDelegationTarget) $ do
            ad <- getAccountOrFail
            let pp = cp ^. cpPoolParameters
            let target = ad ^. BaseAccounts.delegationTarget
            bsp <- MTL.get
            delegationConfigureDisallowOverdelegation bsp pp target

doUpdateBakerKeys ::(IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV0)
    => PersistentBlockState pv
    -> AccountIndex
    -> BakerKeyUpdate
    -> m (BakerKeyUpdateResult, PersistentBlockState pv)
doUpdateBakerKeys pbs ai bku@BakerKeyUpdate{..} = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pAcctBkr} -> do
                acctBkr <- refLoad pAcctBkr
                pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                bkrInfo <- refLoad (_accountBakerInfo acctBkr)
                -- Try updating the aggregation keys
                (keyOK, newAggregationKeys) <-
                        -- If the aggregation key has not changed, we have nothing to do.
                        if bkuAggregationKey == BaseAccounts._bakerAggregationVerifyKey bkrInfo then
                            return (True, _aggregationKeys pab)
                        else do
                            -- Remove the old key
                            ak1 <- Trie.delete (BaseAccounts._bakerAggregationVerifyKey bkrInfo) (_aggregationKeys pab)
                            -- Add the new key and check that it is not already present
                            let updAgg Nothing = return (True, Trie.Insert ())
                                updAgg (Just ()) = return (False, Trie.NoChange)
                            Trie.adjust updAgg bkuAggregationKey ak1
                if keyOK then do
                    -- The new aggregation key is known to be unique
                    newActiveBakers <- refMake pab{_aggregationKeys = newAggregationKeys}
                    let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newActiveBakers
                    -- Update the account with the new keys
                    let updAcc acc = do
                            newBakerInfo <- refMake $ bakerKeyUpdateToInfo (BakerId ai) bku
                            newPAB <- refMake $ acctBkr {_accountBakerInfo = newBakerInfo}
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BKUSuccess (BakerId ai),) <$> storePBS pbs bsp{
                        bspBirkParameters = newBirkParams,
                        bspAccounts = newAccounts
                    }
                else
                    return (BKUDuplicateAggregationKey, pbs)
            -- Cannot resolve the account, or it is not a baker
            _ -> return (BKUInvalidBaker, pbs)

doUpdateBakerStake
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0)
    => PersistentBlockState pv
    -> AccountIndex
    -> Amount
    -> m (BakerStakeUpdateResult, PersistentBlockState pv)
doUpdateBakerStake pbs ai newStake = do
        bsp <- loadPBS pbs

        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pAcctBkr} -> do
                acctBkr <- refLoad pAcctBkr
                if _bakerPendingChange acctBkr /= BaseAccounts.NoChange
                -- A change is already pending
                then return (BSUChangePending (BakerId ai), pbs)
                -- We can make the change
                else do
                    let curEpoch = epoch $ _birkSeedState (bspBirkParameters bsp)
                    upds <- refLoad (bspUpdates bsp)
                    cooldown <- (2+) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized <$> refLoad (currentParameters upds)

                    bakerStakeThreshold <- (^. cpPoolParameters . ppBakerStakeThreshold) <$> doGetChainParameters pbs
                    let applyUpdate updateStake = do
                           let updAcc acc = do
                                  newPAB <- refMake $ updateStake acctBkr
                                  acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                                  return ((), acc')
                           (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                           storePBS pbs bsp{bspAccounts = newAccounts}
                    case compare newStake (_stakedAmount acctBkr) of
                            LT -> if newStake < bakerStakeThreshold
                                  then return (BSUStakeUnderThreshold, pbs)
                                  else (BSUStakeReduced (BakerId ai) (curEpoch + cooldown),) <$>
                                        applyUpdate (bakerPendingChange .~ BaseAccounts.ReduceStake newStake (BaseAccounts.PendingChangeEffectiveV0 $ curEpoch + cooldown))
                            EQ -> return (BSUStakeUnchanged (BakerId ai), pbs)
                            GT -> (BSUStakeIncreased (BakerId ai),) <$> applyUpdate (stakedAmount .~ newStake)
            _ -> return (BSUInvalidBaker, pbs)

doUpdateBakerRestakeEarnings :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> AccountIndex
    -> Bool
    -> m (BakerRestakeEarningsUpdateResult, PersistentBlockState pv)
doUpdateBakerRestakeEarnings pbs ai newRestakeEarnings = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pAcctBkr} -> do
                acctBkr <- refLoad pAcctBkr
                if newRestakeEarnings == acctBkr ^. stakeEarnings
                then return (BREUUpdated (BakerId ai), pbs)
                else do
                    let updAcc acc = do
                            newPAB <- refMake (acctBkr & stakeEarnings .~ newRestakeEarnings)
                            ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BREUUpdated (BakerId ai),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            _ -> return (BREUInvalidBaker, pbs)


doRemoveBaker
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV0, ChainParametersVersionFor pv ~ 'ChainParametersV0)
    => PersistentBlockState pv
    -> AccountIndex
    -> m (BakerRemoveResult, PersistentBlockState pv)
doRemoveBaker pbs ai = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pab} -> do
                ab <- refLoad pab
                if _bakerPendingChange ab /= BaseAccounts.NoChange then
                    -- A change is already pending
                    return (BRChangePending (BakerId ai), pbs)
                else do
                    -- We can make the change
                    -- Note: this just sets the account to be removed at a future epoch
                    -- transition.
                    let curEpoch = epoch $ _birkSeedState (bspBirkParameters bsp)
                    upds <- refLoad (bspUpdates bsp)
                    cooldown <- (2+) . _cpBakerExtraCooldownEpochs . _cpCooldownParameters . unStoreSerialized <$> refLoad (currentParameters upds)
                    let updAcc acc = do
                            newPAB <- refMake ab{_bakerPendingChange = BaseAccounts.RemoveStake (BaseAccounts.PendingChangeEffectiveV0 $ curEpoch + cooldown)}
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BRRemoved (BakerId ai) (curEpoch + cooldown),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            -- The account is not valid or has no baker
            _ -> return (BRInvalidBaker, pbs)

doRewardAccount :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountIndex -> Amount -> m (Maybe AccountAddress, PersistentBlockState pv)
doRewardAccount pbs ai reward = do
        bsp <- loadPBS pbs
        (mRes, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
        case mRes of
            Nothing -> return (Nothing, pbs)
            Just (addr, updActiveBkrs) -> do
                newActiveBkrs <- updActiveBkrs (bspBirkParameters bsp ^. birkActiveBakers)
                (Just addr,) <$> storePBS pbs bsp{
                        bspAccounts = newAccounts,
                        bspBirkParameters = bspBirkParameters bsp & birkActiveBakers .~ newActiveBkrs
                    }
    where
        updAcc acc = do
            addr <- acc ^^. accountAddress
            (restaked, newAccountStake) <- case acc ^. accountStake of
                pas@(PersistentAccountStakeBaker pbkr) -> do
                    bkr <- refLoad pbkr
                    if bkr ^. stakeEarnings then
                        let upd pActiveBkrs = do
                                activeBkrs <- refLoad pActiveBkrs
                                refMake $! activeBkrs & totalActiveCapital %~ addActiveCapital reward
                        in (upd,) . PersistentAccountStakeBaker <$> refMake (bkr & stakedAmount +~ reward)
                    else
                        return (return, pas)
                pas@(PersistentAccountStakeDelegate pdlg) -> do
                    dlg <- refLoad pdlg
                    case dlg of
                        BaseAccounts.AccountDelegationV1{..} | _delegationStakeEarnings ->
                            let upd pActiveBkrs = do
                                    activeBkrs0 <- refLoad pActiveBkrs
                                    activeBkrs1 <- updateDelegationPoolCapital activeBkrs0 _delegationTarget
                                    refMake $! activeBkrs1 & totalActiveCapital %~ addActiveCapital reward
                            in (upd,) . PersistentAccountStakeDelegate <$> refMake (dlg & BaseAccounts.delegationStakedAmount +~ reward)
                        _ ->
                            return (return, pas)
                pas -> return (return, pas)
            acc' <- rehashAccount $ acc & accountStake .~ newAccountStake & accountAmount +~ reward
            return ((addr, restaked), acc')

        updateDelegationPoolCapital
            :: PersistentActiveBakers 'AccountV1
            -> Transactions.DelegationTarget
            -> m (PersistentActiveBakers 'AccountV1)
        updateDelegationPoolCapital activeBkrs Transactions.DelegatePassive = do
            let tot = adDelegatorTotalCapital $ activeBkrs ^. passiveDelegators
            return $! activeBkrs & passiveDelegators %~ \dlgs ->
                dlgs{adDelegatorTotalCapital = tot + reward}
        updateDelegationPoolCapital activeBkrs (Transactions.DelegateToBaker bid) = do
            let activeBkrsMap = activeBkrs ^. activeBakers
                adj Nothing = error "Invariant violation: active baker account is not in active bakers map"
                adj (Just dlgs) = do
                    let tot = adDelegatorTotalCapital dlgs
                    return ((), Trie.Insert $ dlgs{adDelegatorTotalCapital = tot + reward})
            (_, newActiveBkrsMap) <- Trie.adjust adj bid activeBkrsMap
            return $! activeBkrs & activeBakers .~ newActiveBkrsMap

doGetBakerPoolRewardDetails :: (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> m (Map.Map BakerId BakerPoolRewardDetails)
doGetBakerPoolRewardDetails pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of BlockRewardDetailsV1 hp -> hp
    poolRewards <- refLoad hpr
    rewardsList <- LFMBT.toAscList (bakerPoolRewardDetails poolRewards)
    capitals <- refLoad $ currentCapital poolRewards
    let bakerIdList = bcBakerId <$> Vec.toList (bakerPoolCapital capitals)
    -- The lists must be the same length since the rewards are reset when the current capital
    -- distribution is updated.
    return $! Map.fromList (zip bakerIdList rewardsList)

doGetRewardStatus :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (RewardStatus' Epoch)
doGetRewardStatus pbs = do
        bsp <- loadPBS pbs
        let bankStatus = _unhashed $ bspBank bsp
        let rewardsV0 :: RewardStatus' Epoch
            rewardsV0 = RewardStatusV0 {
                    rsTotalAmount = bankStatus ^. Rewards.totalGTU,
                    rsTotalEncryptedAmount = bankStatus ^. Rewards.totalEncryptedGTU,
                    rsBakingRewardAccount = bankStatus ^. Rewards.bakingRewardAccount,
                    rsFinalizationRewardAccount = bankStatus ^. Rewards.finalizationRewardAccount,
                    rsGasAccount = bankStatus ^. Rewards.gasAccount
                }
            rewardsV1 :: (AccountVersionFor pv ~ 'AccountV1) => m (RewardStatus' Epoch)
            rewardsV1 = do
                poolRewards <- refLoad (bspPoolRewards bsp)
                tc <- totalCapital bsp
                return RewardStatusV1 {
                    rsTotalAmount = bankStatus ^. Rewards.totalGTU,
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

doRewardFoundationAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Amount -> m (PersistentBlockState pv)
doRewardFoundationAccount pbs reward = do
        bsp <- loadPBS pbs
        let updAcc acc = ((),) <$> rehashAccount (acc & accountAmount %~ (+ reward))
        foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
        (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc foundationAccount (bspAccounts bsp)
        storePBS pbs (bsp {bspAccounts = newAccounts})

doGetFoundationAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (PersistentAccount (AccountVersionFor pv))
doGetFoundationAccount pbs = do
        bsp <- loadPBS pbs
        foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
        macc <- Accounts.indexedAccount foundationAccount (bspAccounts bsp)
        case macc of
            Nothing -> error "bsoGetFoundationAccount: invalid foundation account"
            Just acc -> return acc

doMint :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> MintAmounts -> m (PersistentBlockState pv)
doMint pbs mint = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp &
                unhashed %~
                (Rewards.totalGTU +~ mintTotal mint) .
                (Rewards.bakingRewardAccount +~ mintBakingReward mint) .
                (Rewards.finalizationRewardAccount +~ mintFinalizationReward mint)
        let updAcc acc = ((),) <$> rehashAccount (acc & accountAmount %~ (+ mintDevelopmentCharge mint))
        foundationAccount <- (^. cpFoundationAccount) <$> lookupCurrentParameters (bspUpdates bsp)
        (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc foundationAccount (bspAccounts bsp)
        storePBS pbs (bsp {bspBank = newBank, bspAccounts = newAccounts})

doGetAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
doGetAccount pbs addr = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex addr (bspAccounts bsp)

doGetAccountExists :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m Bool
doGetAccountExists pbs aaddr = do
       bsp <- loadPBS pbs
       Accounts.exists aaddr (bspAccounts bsp)

doGetActiveBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [BakerId]
doGetActiveBakers pbs = do
    bsp <- loadPBS pbs
    ab <- refLoad $ bspBirkParameters bsp ^. birkActiveBakers
    Trie.keysAsc (ab ^. activeBakers)

doGetAccountByCredId :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.CredentialRegistrationID -> m (Maybe (AccountIndex, PersistentAccount (AccountVersionFor pv)))
doGetAccountByCredId pbs cid = do
        bsp <- loadPBS pbs
        Accounts.getAccountByCredId cid (bspAccounts bsp)


doGetAccountIndex :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m (Maybe AccountIndex)
doGetAccountIndex pbs addr = do
        bsp <- loadPBS pbs
        Accounts.getAccountIndex addr (bspAccounts bsp)

doGetAccountByIndex :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountIndex -> m (Maybe (PersistentAccount (AccountVersionFor pv)))
doGetAccountByIndex pbs aid = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount aid (bspAccounts bsp)

doAccountList :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Accounts.accountAddresses (bspAccounts bsp)

doAddressWouldClash :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m Bool
doAddressWouldClash pbs addr = do
        bsp <- loadPBS pbs
        Accounts.addressWouldClash addr (bspAccounts bsp)

doRegIdExists :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.CredentialRegistrationID -> m Bool

doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        isJust . fst <$> Accounts.regIdExists regid (bspAccounts bsp)

doCreateAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.GlobalContext -> AccountAddress -> ID.AccountCredential ->  m (Maybe (PersistentAccount (AccountVersionFor pv)), PersistentBlockState pv)
doCreateAccount pbs cryptoParams acctAddr credential = do
        acct <- newAccount cryptoParams acctAddr credential
        bsp <- loadPBS pbs
        -- Add the account
        (res, accts1) <- Accounts.putNewAccount acct (bspAccounts bsp)
        case res of
          Just idx -> do
            -- Record the RegId since we created a new account.
            accts2 <- Accounts.recordRegId (ID.credId credential) idx accts1
            (Just acct,) <$> storePBS pbs (bsp {bspAccounts = accts2})
          Nothing -> -- the account was not created
            return (Nothing, pbs)

doModifyAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountUpdate -> m (PersistentBlockState pv)
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadPBS pbs
        -- Do the update to the account
        (_, accts1) <- Accounts.updateAccountsAtIndex upd _auIndex (bspAccounts bsp)
        storePBS pbs (bsp {bspAccounts = accts1})
    where
        upd oldAccount = ((), ) <$> Accounts.updateAccount aUpd oldAccount

doSetAccountCredentialKeys :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountIndex -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m (PersistentBlockState pv)
doSetAccountCredentialKeys pbs accIndex credIx credKeys = do
        bsp <- loadPBS pbs
        (_, accts1) <- Accounts.updateAccountsAtIndex upd accIndex (bspAccounts bsp)
        storePBS pbs (bsp {bspAccounts = accts1})
    where
        upd oldAccount = ((), ) <$> setPAD (updateCredentialKeys credIx credKeys) oldAccount

doUpdateAccountCredentials :: (IsProtocolVersion pv, MonadBlobStore m) =>
    PersistentBlockState pv
    -> AccountIndex -- ^ Address of the account to update.
    -> [ID.CredentialIndex] -- ^ List of credential indices to remove.
    -> Map.Map ID.CredentialIndex ID.AccountCredential -- ^ New credentials to add.
    -> ID.AccountThreshold -- ^ New account threshold
    -> m (PersistentBlockState pv)
doUpdateAccountCredentials pbs accIndex remove add thrsh = do
        bsp <- loadPBS pbs
        (res, accts1) <- Accounts.updateAccountsAtIndex upd accIndex (bspAccounts bsp)
        case res of
          Just () -> do
            -- If we deploy a credential, record it
            accts2 <- Accounts.recordRegIds ((, accIndex) <$> Map.elems (ID.credId <$> add)) accts1
            storePBS pbs (bsp {bspAccounts = accts2})
          Nothing -> return pbs -- this should not happen, the precondition of this method is that the account exists. But doing nothing is safe.
    where
        upd oldAccount = ((), ) <$> setPAD (updateCredentials remove add thrsh) oldAccount

doGetInstance :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ContractAddress -> m (Maybe Instance)
doGetInstance pbs caddr = do
        bsp <- loadPBS pbs
        minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
        forM minst Instances.fromPersistentInstance

doContractInstanceList :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [Instance]
doContractInstanceList pbs = do
        bsp <- loadPBS pbs
        insts <- Instances.allInstances (bspInstances bsp)
        mapM Instances.fromPersistentInstance insts

doPutNewInstance :: forall m pv. (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> (ContractAddress -> Instance) -> m (ContractAddress, PersistentBlockState pv)
doPutNewInstance pbs fnew = do
        bsp <- loadPBS pbs
        mods <- refLoad (bspModules bsp)
        -- Create the instance
        (inst, insts) <- Instances.newContractInstance (fnew' mods) (bspInstances bsp)
        let ca = instanceAddress inst
        (ca,) <$> storePBS pbs bsp{bspInstances = insts}
    where
        fnew' mods ca =
          case fnew ca of
            inst@(InstanceV0 InstanceV{_instanceVParameters = InstanceParameters{..}, ..}) -> do
                params <- makeBufferedRef $ PersistentInstanceParameters {
                                            pinstanceAddress = _instanceAddress,
                                            pinstanceOwner = instanceOwner,
                                            pinstanceContractModule = GSWasm.miModuleRef instanceModuleInterface,
                                            pinstanceReceiveFuns = instanceReceiveFuns,
                                            pinstanceInitName = instanceInitName,
                                            pinstanceParameterHash = instanceParameterHash
                                        }
                -- We use an irrefutable pattern here. This cannot fail since if it failed it would mean we are trying
                -- to create an instance of a module that does not exist. The Scheduler should not allow this, and the
                -- state implementation relies on this property.
                ~(Just modRef) <- Modules.unsafeGetModuleReferenceV0 (GSWasm.miModuleRef instanceModuleInterface) mods
                return (inst, PersistentInstanceV0 Instances.PersistentInstanceV{
                    pinstanceParameters = params,
                    pinstanceModuleInterface = modRef,
                    pinstanceModel = _instanceVModel,
                    pinstanceAmount = _instanceVAmount,
                    pinstanceHash = _instanceVHash
                })
            inst@(InstanceV1 InstanceV{_instanceVParameters = InstanceParameters{..}, ..}) -> do
              params <- makeBufferedRef $ PersistentInstanceParameters {
                pinstanceAddress = _instanceAddress,
                pinstanceOwner = instanceOwner,
                pinstanceContractModule = GSWasm.miModuleRef instanceModuleInterface,
                pinstanceReceiveFuns = instanceReceiveFuns,
                pinstanceInitName = instanceInitName,
                pinstanceParameterHash = instanceParameterHash
                }
              -- We use an irrefutable pattern here. This cannot fail since if it failed it would mean we are trying
              -- to create an instance of a module that does not exist. The Scheduler should not allow this, and the
              -- state implementation relies on this property.
              ~(Just modRef) <- Modules.unsafeGetModuleReferenceV1 (GSWasm.miModuleRef instanceModuleInterface) mods
              return (inst, PersistentInstanceV1 Instances.PersistentInstanceV{
                  pinstanceParameters = params,
                  pinstanceModuleInterface = modRef,
                  pinstanceModel = _instanceVModel,
                  pinstanceAmount = _instanceVAmount,
                  pinstanceHash = _instanceVHash
                  })

doModifyInstance :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ContractAddress -> AmountDelta -> Maybe Wasm.ContractState -> m (PersistentBlockState pv)
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadPBS pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (_, insts) ->
                storePBS pbs bsp{bspInstances = insts}
    where
        upd (PersistentInstanceV0 oldInst) = do
            (piParams, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0 then
                case val of
                    Nothing -> return ((), PersistentInstanceV0 $ oldInst {pinstanceParameters = newParamsRef})
                    Just newVal -> return ((), PersistentInstanceV0 $ rehash (pinstanceParameterHash piParams) (oldInst {pinstanceParameters = newParamsRef, pinstanceModel = newVal}))
            else
                case val of
                    Nothing -> return ((), PersistentInstanceV0 $ rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst)})
                    Just newVal -> return ((), PersistentInstanceV0 $ rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = newVal})
        upd (PersistentInstanceV1 oldInst) = do
            (piParams, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0 then
                case val of
                    Nothing -> return ((), PersistentInstanceV1 $ oldInst {pinstanceParameters = newParamsRef})
                    Just newVal -> return ((), PersistentInstanceV1 $ rehash (pinstanceParameterHash piParams) (oldInst {pinstanceParameters = newParamsRef, pinstanceModel = newVal}))
            else
                case val of
                    Nothing -> return ((), PersistentInstanceV1 $ rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst)})
                    Just newVal -> return ((), PersistentInstanceV1 $ rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = newVal})
        rehash iph inst@PersistentInstanceV {..} = inst {pinstanceHash = makeInstanceHash' iph pinstanceModel pinstanceAmount}

doGetIdentityProvider :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.IdentityProviderIdentity -> m (Maybe IPS.IpInfo)
doGetIdentityProvider pbs ipId = do
        bsp <- loadPBS pbs
        ips <- refLoad (bspIdentityProviders bsp)
        return $! IPS.idProviders ips ^? ix ipId

doGetAllIdentityProvider :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [IPS.IpInfo]
doGetAllIdentityProvider pbs = do
        bsp <- loadPBS pbs
        ips <- refLoad (bspIdentityProviders bsp)
        return $! Map.elems $ IPS.idProviders ips

doGetAnonymityRevokers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> [ID.ArIdentity] -> m (Maybe [ARS.ArInfo])
doGetAnonymityRevokers pbs arIds = do
        bsp <- loadPBS pbs
        ars <- refLoad (bspAnonymityRevokers bsp)
        return
          $! let arsMap = ARS.arRevokers ars
              in forM arIds (`Map.lookup` arsMap)

doGetAllAnonymityRevokers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [ARS.ArInfo]
doGetAllAnonymityRevokers pbs = do
        bsp <- loadPBS pbs
        ars <- refLoad (bspAnonymityRevokers bsp)
        return $! Map.elems $ ARS.arRevokers ars

doGetCryptoParams :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m CryptographicParameters
doGetCryptoParams pbs = do
        bsp <- loadPBS pbs
        refLoad (bspCryptographicParameters bsp)

doGetPaydayEpoch :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> m Epoch
doGetPaydayEpoch pbs = do
        bsp <- loadPBS pbs
        case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of
            BlockRewardDetailsV1 hpr -> nextPaydayEpoch <$> refLoad hpr

doGetPaydayMintRate :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> m MintRate
doGetPaydayMintRate pbs = do
        bsp <- loadPBS pbs
        case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of
            BlockRewardDetailsV1 hpr -> nextPaydayMintRate <$> refLoad hpr

doSetPaydayEpoch :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> Epoch -> m (PersistentBlockState pv)
doSetPaydayEpoch pbs e = do
        bsp <- loadPBS pbs
        case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of
            BlockRewardDetailsV1 hpr -> do
              pr <- refLoad hpr
              hpr' <- refMake pr{nextPaydayEpoch = e}
              storePBS pbs bsp{bspRewardDetails = BlockRewardDetailsV1 hpr'}

doSetPaydayMintRate :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => PersistentBlockState pv -> MintRate -> m (PersistentBlockState pv)
doSetPaydayMintRate pbs r = do
        bsp <- loadPBS pbs
        case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of
            BlockRewardDetailsV1 hpr -> do
              pr <- refLoad hpr
              hpr' <- refMake pr{nextPaydayMintRate = r}
              storePBS pbs bsp{bspRewardDetails = BlockRewardDetailsV1 hpr'}

doGetPoolStatus ::
    forall pv m.
    ( IsProtocolVersion pv,
      MonadBlobStore m,
      AccountVersionFor pv ~ 'AccountV1,
      ChainParametersVersionFor pv ~ 'ChainParametersV1
    ) =>
    PersistentBlockState pv ->
    Maybe BakerId ->
    m (Maybe PoolStatus)
doGetPoolStatus pbs Nothing = do
        bsp <- loadPBS pbs
        psDelegatedCapital <- passiveDelegationCapital bsp
        psCommissionRates <- _ppPassiveCommissions . _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
        poolRewards <- refLoad (bspPoolRewards bsp)
        let psCurrentPaydayTransactionFeesEarned = passiveDelegationTransactionRewards poolRewards
        psCurrentPaydayDelegatedCapital <- currentPassiveDelegationCapital poolRewards
        psAllPoolTotalCapital <- totalCapital bsp
        return $ Just PassiveDelegationStatus {..}
doGetPoolStatus pbs (Just psBakerId@(BakerId aid)) = do
        bsp <- loadPBS pbs
        Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
            Nothing -> return Nothing
            Just acct -> do
                case acct ^. accountBaker of
                    Null -> return Nothing
                    Some bkrRef -> do
                        baker <- refLoad bkrRef
                        let psBakerEquityCapital = baker ^. stakedAmount
                        psDelegatedCapital <- poolDelegatorCapital bsp psBakerId
                        poolParameters <- _cpPoolParameters <$> lookupCurrentParameters (bspUpdates bsp)
                        psAllPoolTotalCapital <- totalCapital bsp
                        let psDelegatedCapitalCap = delegatedCapitalCap
                                poolParameters
                                psAllPoolTotalCapital
                                psBakerEquityCapital
                                psDelegatedCapital
                        psBakerAddress <- _accountAddress <$> refLoad (acct ^. persistingData)
                        psPoolInfo <- refLoad (baker ^. bakerPoolInfoRef)
                        let psBakerStakePendingChange =
                                makePoolPendingChange (baker ^. bakerPendingChange)
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
                                        return $ Just CurrentPaydayBakerPoolStatus {
                                                bpsBlocksBaked = blockCount,
                                                bpsFinalizationLive = finalizationAwake,
                                                bpsTransactionFeesEarned = transactionFeesAccrued,
                                                bpsEffectiveStake = effectiveStake,
                                                bpsLotteryPower = fromIntegral effectiveStake
                                                    / fromIntegral (_bakerTotalStake epochBakers),
                                                bpsBakerEquityCapital = bcBakerEquityCapital bc,
                                                bpsDelegatedCapital = bcTotalDelegatorCapital bc
                                            }
                        return $ Just BakerPoolStatus{..}

doGetTransactionOutcome :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Transactions.TransactionIndex -> m (Maybe TransactionSummary)
doGetTransactionOutcome pbs transHash = do
        bsp <- loadPBS pbs
        return $! bspTransactionOutcomes bsp ^? ix transHash

doGetTransactionOutcomesHash :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m TransactionOutcomesHash
doGetTransactionOutcomesHash pbs =  do
    bsp <- loadPBS pbs
    return $! getHash (bspTransactionOutcomes bsp)

doSetTransactionOutcomes :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> [TransactionSummary] -> m (PersistentBlockState pv)
doSetTransactionOutcomes pbs transList = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspTransactionOutcomes = Transactions.transactionOutcomesFromList transList}

doNotifyEncryptedBalanceChange :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doNotifyEncryptedBalanceChange pbs amntDiff = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff}

doGetSpecialOutcomes :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (Seq.Seq Transactions.SpecialTransactionOutcome)
doGetSpecialOutcomes pbs = (^. to bspTransactionOutcomes . Transactions.outcomeSpecial) <$> loadPBS pbs

doGetOutcomes :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (Vec.Vector TransactionSummary)
doGetOutcomes pbs = (^. to bspTransactionOutcomes . to Transactions.outcomeValues) <$> loadPBS pbs

doAddSpecialTransactionOutcome :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Transactions.SpecialTransactionOutcome -> m (PersistentBlockState pv)
doAddSpecialTransactionOutcome pbs !o = do
        bsp <- loadPBS pbs
        storePBS pbs $! bsp {bspTransactionOutcomes = bspTransactionOutcomes bsp & Transactions.outcomeSpecial %~ (Seq.|> o)}

doGetElectionDifficulty :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Timestamp -> m ElectionDifficulty
doGetElectionDifficulty pbs ts = do
        bsp <- loadPBS pbs
        futureElectionDifficulty (bspUpdates bsp) ts

doGetNextUpdateSequenceNumber :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> UpdateType -> m UpdateSequenceNumber
doGetNextUpdateSequenceNumber pbs uty = do
        bsp <- loadPBS pbs
        lookupNextUpdateSequenceNumber (bspUpdates bsp) uty

doGetCurrentElectionDifficulty :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m ElectionDifficulty
doGetCurrentElectionDifficulty pbs = do
        bsp <- loadPBS pbs
        upds <- refLoad (bspUpdates bsp)
        _cpElectionDifficulty . unStoreSerialized <$> refLoad (currentParameters upds)

doGetUpdates :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (UQ.Updates pv)
doGetUpdates = makeBasicUpdates <=< refLoad . bspUpdates <=< loadPBS

doGetProtocolUpdateStatus :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m UQ.ProtocolUpdateStatus
doGetProtocolUpdateStatus = protocolUpdateStatus . bspUpdates <=< loadPBS

doProcessUpdateQueues
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> Timestamp
    -> m (Map.Map TransactionTime (UpdateValue (ChainParametersVersionFor pv)), PersistentBlockState pv)
doProcessUpdateQueues pbs ts = do
        bsp <- loadPBS pbs
        let (u, ars, ips) = (bspUpdates bsp, bspAnonymityRevokers bsp, bspIdentityProviders bsp)
        (changes, (u', ars', ips')) <- processUpdateQueues ts (u, ars, ips)
        (changes,) <$> storePBS pbs bsp{bspUpdates = u', bspAnonymityRevokers = ars', bspIdentityProviders = ips'}

doProcessReleaseSchedule :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Timestamp -> m (PersistentBlockState pv)
doProcessReleaseSchedule pbs ts = do
        bsp <- loadPBS pbs
        releaseSchedule <- loadBufferedRef (bspReleaseSchedule bsp)
        if Map.null releaseSchedule
          then return pbs
          else do
          let (accountsToRemove, blockReleaseSchedule') = Map.partition (<= ts) releaseSchedule
              f (ba, readded) addr = do
                let upd acc = do
                      rData <- loadBufferedRef (acc ^. accountReleaseSchedule)
                      (_, nextTs, rData') <- unlockAmountsUntil ts rData
                      rDataRef <- makeBufferedRef rData'
                      acc' <- rehashAccount $ acc & accountReleaseSchedule .~ rDataRef
                      return (nextTs, acc')
                (toRead, ba') <- Accounts.updateAccounts upd addr ba
                return (ba', case snd =<< toRead of
                               Just t -> (addr, t) : readded
                               Nothing -> readded)
          (bspAccounts', accsToReadd) <- foldlM f (bspAccounts bsp, []) (Map.keys accountsToRemove)
          bspReleaseSchedule' <- makeBufferedRef $ foldl' (\b (a, t) -> Map.insert a t b) blockReleaseSchedule' accsToReadd
          storePBS pbs (bsp {bspAccounts = bspAccounts', bspReleaseSchedule = bspReleaseSchedule'})

doGetUpdateKeyCollection
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> m (UpdateKeysCollection (ChainParametersVersionFor pv))
doGetUpdateKeyCollection pbs = do
        bsp <- loadPBS pbs
        u <- refLoad (bspUpdates bsp)
        unStoreSerialized <$> refLoad (currentKeyCollection u)

doEnqueueUpdate
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> TransactionTime
    -> UpdateValue (ChainParametersVersionFor pv)
    -> m (PersistentBlockState pv)
doEnqueueUpdate pbs effectiveTime payload = do
        bsp <- loadPBS pbs
        u' <- enqueueUpdate effectiveTime payload (bspUpdates bsp)
        storePBS pbs bsp{bspUpdates = u'}

doOverwriteElectionDifficulty :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ElectionDifficulty -> m (PersistentBlockState pv)
doOverwriteElectionDifficulty pbs newElectionDifficulty = do
        bsp <- loadPBS pbs
        u' <- overwriteElectionDifficulty newElectionDifficulty (bspUpdates bsp)
        storePBS pbs bsp{bspUpdates = u'}

doClearProtocolUpdate :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (PersistentBlockState pv)
doClearProtocolUpdate pbs = do
        bsp <- loadPBS pbs
        u' <- clearProtocolUpdate (bspUpdates bsp)
        storePBS pbs bsp{bspUpdates = u'}

doSetNextCapitalDistribution
    :: forall pv m
     . (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1)
    => PersistentBlockState pv
    -> CapitalDistribution
    -> m (PersistentBlockState pv)
doSetNextCapitalDistribution pbs cd = do
    bsp <- loadPBS pbs
    capDist <- refMake cd
    newRewardDetails <- case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> do
            pr <- refLoad hpr
            BlockRewardDetailsV1 <$> refMake (pr {nextCapital = capDist})
    storePBS pbs bsp{bspRewardDetails = newRewardDetails}

doRotateCurrentCapitalDistribution
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1)
    => PersistentBlockState pv
    -> m (PersistentBlockState pv)
doRotateCurrentCapitalDistribution pbs = do
    bsp <- loadPBS pbs
    newRewardDetails <- case bspRewardDetails bsp of
        BlockRewardDetailsV1 hpr -> BlockRewardDetailsV1 <$> rotateCapitalDistribution hpr
    storePBS pbs bsp{bspRewardDetails = newRewardDetails}

doAddReleaseSchedule :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> [(AccountAddress, Timestamp)] -> m (PersistentBlockState pv)
doAddReleaseSchedule pbs rel = do
        bsp <- loadPBS pbs
        releaseSchedule <- loadBufferedRef (bspReleaseSchedule bsp)
        let f relSchedule (addr, t) = Map.alter (\case
                                                    Nothing -> Just t
                                                    Just t' -> Just $ min t' t) addr relSchedule
        bspReleaseSchedule' <- makeBufferedRef $ foldl' f releaseSchedule rel
        storePBS pbs bsp {bspReleaseSchedule = bspReleaseSchedule'}

doGetEnergyRate :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m EnergyRate
doGetEnergyRate pbs = do
    bsp <- loadPBS pbs
    lookupEnergyRate (bspUpdates bsp)

doGetChainParameters :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (ChainParameters pv)
doGetChainParameters pbs = do
        bsp <- loadPBS pbs
        lookupCurrentParameters (bspUpdates bsp)

doGetPendingTimeParameters :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [(TransactionTime, TimeParameters (ChainParametersVersionFor pv))]
doGetPendingTimeParameters pbs = do
        bsp <- loadPBS pbs
        lookupPendingTimeParameters (bspUpdates bsp)

doGetPendingPoolParameters :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [(TransactionTime, PoolParameters (ChainParametersVersionFor pv))]
doGetPendingPoolParameters pbs = do
        bsp <- loadPBS pbs
        lookupPendingPoolParameters (bspUpdates bsp)

doGetEpochBlocksBaked :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (Word64, [(BakerId, Word64)])
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
modifyBakerPoolRewardDetailsInPoolRewards :: (MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1) => BlockStatePointers pv -> BakerId -> (BakerPoolRewardDetails -> BakerPoolRewardDetails) -> m (BlockStatePointers pv)
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
            mBPRs <- LFMBT.update (return . (,) () . f) (fromIntegral i) bprs
            case mBPRs of
                Nothing ->
                    error "Invariant violation: unable to find baker in baker pool reward details tree"
                Just ((), newBPRs) ->
                    return newBPRs

doNotifyBlockBaked :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> m (PersistentBlockState pv)
doNotifyBlockBaked pbs bid = do
    bsp <- loadPBS pbs
    case accountVersionFor (protocolVersion @pv) of
        SAccountV0 -> do
            newBlockRewardDetails <- consBlockRewardDetails bid (bspRewardDetails bsp)
            storePBS pbs bsp{bspRewardDetails = newBlockRewardDetails}
        SAccountV1 ->
            let incBPR bpr = bpr{blockCount = blockCount bpr + 1}
            in storePBS pbs =<< modifyBakerPoolRewardDetailsInPoolRewards bsp bid incBPR

doUpdateAccruedTransactionFeesBaker :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesBaker pbs bid delta = do
    bsp <- loadPBS pbs
    let accrueAmountBPR bpr = bpr{transactionFeesAccrued = applyAmountDelta delta (transactionFeesAccrued bpr)}
    storePBS pbs =<< modifyBakerPoolRewardDetailsInPoolRewards bsp bid accrueAmountBPR

doMarkFinalizationAwakeBaker :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> m (PersistentBlockState pv)
doMarkFinalizationAwakeBaker pbs bid = do
    bsp <- loadPBS pbs
    -- TODO: This could be simplified when the baker is already awake, to avoid rewriting the storage.
    let setAwake bpr = bpr{finalizationAwake = True}
    storePBS pbs =<< modifyBakerPoolRewardDetailsInPoolRewards bsp bid setAwake

doUpdateAccruedTransactionFeesPassive :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesPassive pbs delta = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    newBlockRewardDetails <- BlockRewardDetailsV1 <$> refMake pr{
            passiveDelegationTransactionRewards = applyAmountDelta delta (passiveDelegationTransactionRewards pr)
        }
    storePBS pbs $ bsp{bspRewardDetails = newBlockRewardDetails}

doGetAccruedTransactionFeesPassive :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> m Amount
doGetAccruedTransactionFeesPassive pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of BlockRewardDetailsV1 hp -> hp
    passiveDelegationTransactionRewards <$> refLoad hpr

doUpdateAccruedTransactionFeesFoundationAccount :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> AmountDelta -> m (PersistentBlockState pv)
doUpdateAccruedTransactionFeesFoundationAccount pbs delta = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp of BlockRewardDetailsV1 hp -> hp
    pr <- refLoad hpr
    newBlockRewardDetails <- BlockRewardDetailsV1 <$> refMake pr{
            foundationTransactionRewards = applyAmountDelta delta (foundationTransactionRewards pr)
        }
    storePBS pbs $ bsp{bspRewardDetails = newBlockRewardDetails}

doGetAccruedTransactionFeesFoundationAccount :: forall pv m. (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1, MonadBlobStore m) => PersistentBlockState pv -> m Amount
doGetAccruedTransactionFeesFoundationAccount pbs = do
    bsp <- loadPBS pbs
    let hpr = case bspRewardDetails bsp :: BlockRewardDetails 'AccountV1 of BlockRewardDetailsV1 hp -> hp
    foundationTransactionRewards <$> refLoad hpr

doClearEpochBlocksBaked :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (PersistentBlockState pv)
doClearEpochBlocksBaked pbs = do
        bsp <- loadPBS pbs
        rewardDetails <- emptyBlockRewardDetails
        storePBS pbs bsp{bspRewardDetails = rewardDetails}

doRotateCurrentEpochBakers
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> m (PersistentBlockState pv)
doRotateCurrentEpochBakers pbs = do
    bsp <- loadPBS pbs
    let oldBirkParams = bspBirkParameters bsp
        newBirkParams = oldBirkParams & birkCurrentEpochBakers .~ (oldBirkParams ^. birkNextEpochBakers)
    storePBS pbs bsp{bspBirkParameters = newBirkParams}

doSetNextEpochBakers
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> [(PersistentBakerInfoEx (AccountVersionFor pv), Amount)]
    -> m (PersistentBlockState pv)
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

-- |Update an account's delegation to passive delegation. This only updates the account table,
-- and does not update the active baker index, which must be handled separately.
-- The account __must__ be an active delegator.
redelegatePassive :: (MonadBlobStore m, MTL.MonadState (BlockStatePointers pv) m, IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV1) => DelegatorId -> m ()
redelegatePassive (DelegatorId accId) = do
    accounts <- bspAccounts <$> MTL.get
    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc accId accounts
    MTL.modify $ \bsp -> bsp{bspAccounts = newAccounts}
  where
    updAcc pa@PersistentAccount{_accountStake = PersistentAccountStakeDelegate acctDelRef} = do
        acctDel <- refLoad acctDelRef
        let newAcctDel = acctDel{BaseAccounts._delegationTarget = Transactions.DelegatePassive}
        newAcctDelRef <- refMake newAcctDel
        let newPA = pa{_accountStake = PersistentAccountStakeDelegate newAcctDelRef}
        ((),) <$> setPersistentAccountStake newPA PersistentAccountStakeNone
    updAcc _ = error "Invariant violation: active delegator is not a delegation account"

doProcessPendingChanges
    :: forall pv m
     . (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1)
    => PersistentBlockState pv
    -> (BaseAccounts.PendingChangeEffective (AccountVersionFor pv) -> Bool)
    -- ^Guard determining if a change is effective
    -> m (PersistentBlockState pv)
doProcessPendingChanges persistentBS isEffective = do
    bsp <- loadPBS persistentBS
    newBSP <- newBlockState bsp
    storePBS persistentBS newBSP
    where
      newBlockState = MTL.execStateT processPendingChanges

      processPendingChanges = do
        a1 <- modifyPassiveDelegation
        a2 <- modifyBakers
        bsp0 <- MTL.get
        ab <- lift $ refLoad $ bspBirkParameters bsp0 ^. birkActiveBakers
        newAB <- lift $ refMake ab{_totalActiveCapital = TotalActiveCapitalV1 (a1 + a2)}
        MTL.modify $ \bsp ->
            bsp{bspBirkParameters = (bspBirkParameters bsp){_birkActiveBakers = newAB}}

      modifyPassiveDelegation = do
        bsp0 <- MTL.get
        ab <- lift $ refLoad $ bspBirkParameters bsp0 ^. birkActiveBakers
        let oldDelegators = ab ^. passiveDelegators
        newDelegators <- processDelegators oldDelegators
        newAB <- lift $ refMake ab{_passiveDelegators = newDelegators}
        MTL.modify $ \bsp ->
            bsp{bspBirkParameters = (bspBirkParameters bsp){_birkActiveBakers = newAB}}
        return $ adDelegatorTotalCapital newDelegators

      modifyBakers = do
        bsp0 <- MTL.get
        ab <- lift $ refLoad $ bspBirkParameters bsp0 ^. birkActiveBakers
        let oldBakers = ab ^. activeBakers
        (newBakers, total) <- processBakers oldBakers
        newAB <- lift $ refMake ab{_activeBakers = newBakers}
        MTL.modify $ \bsp ->
            bsp{bspBirkParameters = (bspBirkParameters bsp){_birkActiveBakers = newAB}}
        return total

      processDelegators
        :: PersistentActiveDelegators 'AccountV1
        -> MTL.StateT (BlockStatePointers pv) m (PersistentActiveDelegators 'AccountV1)
      processDelegators (PersistentActiveDelegatorsV1 dset _) = do
        (newDlgs, newAmt) <- MTL.runWriterT $ Trie.filterKeysM processDelegator dset
        return (PersistentActiveDelegatorsV1 newDlgs newAmt)

      processDelegator :: DelegatorId -> MTL.WriterT Amount (MTL.StateT (BlockStatePointers pv) m) Bool
      processDelegator (DelegatorId accId) = do
        accounts <- bspAccounts <$> MTL.get
        lift (lift (Accounts.indexedAccount accId accounts)) >>= \case
            Just acct -> updateAccountDelegator accId acct
            Nothing -> error "Invariant violation: active delegator account was not found"

      updateAccountDelegator
        :: AccountIndex
        -> PersistentAccount 'AccountV1 -> MTL.WriterT Amount (MTL.StateT (BlockStatePointers pv) m) Bool
      updateAccountDelegator accId acct = case acct ^. accountDelegator of
        Some acctDelRef -> do
            acctDel@BaseAccounts.AccountDelegationV1{..} <- lift $ lift $ refLoad acctDelRef
            case _delegationPendingChange of
                BaseAccounts.RemoveStake pet | isEffective pet ->
                    lift $ removeDelegatorStake accId
                BaseAccounts.ReduceStake newAmt pet | isEffective pet -> do
                    MTL.tell newAmt
                    lift $ reduceDelegatorStake accId acctDel newAmt
                _ -> do
                    MTL.tell _delegationStakedAmount
                    return True
        Null ->
            error "Invariant violation: active delegator is not a delegation account"

      removeDelegatorStake :: AccountIndex -> MTL.StateT (BlockStatePointers pv) m Bool
      removeDelegatorStake accId = do
        accounts <- bspAccounts <$> MTL.get
        let updAcc acc = ((),) <$> setPersistentAccountStake acc PersistentAccountStakeNone
        (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc accId accounts
        MTL.modify $ \bsp -> bsp{bspAccounts = newAccounts}
        return False

      reduceDelegatorStake
        :: AccountIndex ->
        BaseAccounts.AccountDelegation 'AccountV1
        -> Amount
        -> MTL.StateT (BlockStatePointers pv) m Bool
      reduceDelegatorStake accId acctDel newAmt = do
        accounts <- bspAccounts <$> MTL.get
        newDel <- lift $ refMake acctDel{
            BaseAccounts._delegationStakedAmount = newAmt,
            BaseAccounts._delegationPendingChange = BaseAccounts.NoChange}
        let updAcc acc = ((),) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newDel)
        (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc accId accounts
        MTL.modify $ \bsp -> bsp{bspAccounts = newAccounts}
        return True

      processBakers
        :: BakerIdTrieMap 'AccountV1
        -> MTL.StateT (BlockStatePointers pv) m (BakerIdTrieMap 'AccountV1, Amount)
      processBakers = MTL.runWriterT . Trie.alterMapM processBaker

      processBaker
          :: BakerId
          -> PersistentActiveDelegators 'AccountV1
          -> MTL.WriterT Amount (MTL.StateT (BlockStatePointers pv) m) (Trie.Alteration (PersistentActiveDelegators 'AccountV1))
      processBaker bid@(BakerId accId) oldDelegators = do
          newDelegators <- lift $ processDelegators oldDelegators
          MTL.tell (adDelegatorTotalCapital newDelegators)
          let trieInsert = do
                oldKeys <- Trie.keys (adDelegators oldDelegators)
                let oldTotal = adDelegatorTotalCapital oldDelegators
                newKeys <- Trie.keys (adDelegators newDelegators)
                let newTotal = adDelegatorTotalCapital newDelegators
                if newKeys == oldKeys && newTotal == oldTotal
                then return Trie.NoChange
                else return (Trie.Insert newDelegators)
          accounts <- bspAccounts <$> MTL.get
          lift (lift (Accounts.indexedAccount accId accounts)) >>= \case
            Just acct -> case acct ^. accountBaker of
                Some acctBkrRef -> do
                    acctBkr@PersistentAccountBaker{..} <- lift (lift (refLoad acctBkrRef))
                    case _bakerPendingChange of
                        BaseAccounts.RemoveStake pet | isEffective pet -> do
                            lift $ removeBaker bid acctBkr newDelegators
                            return Trie.Remove
                        BaseAccounts.ReduceStake newAmt pet | isEffective pet -> do
                            MTL.tell newAmt
                            lift $ reduceBakerStake bid newAmt acctBkr
                            lift $ lift trieInsert
                        _ -> do
                            MTL.tell (acctBkr ^. stakedAmount)
                            lift $ lift trieInsert
                Null ->
                    error "Persistent.bsoProcessPendingChanges invariant violation: active baker account not a baker"
            Nothing ->
                error "Persistent.bsoProcessPendingChanges invariant violation: active baker account not valid"

      removeBaker
        :: BakerId
        -> PersistentAccountBaker 'AccountV1
        -> PersistentActiveDelegators 'AccountV1
        -> MTL.StateT (BlockStatePointers pv) m ()
      removeBaker bid@(BakerId accId) acctBkr (PersistentActiveDelegatorsV1 dset dcapital) = do
        accounts <- bspAccounts <$> MTL.get
        let updAcc acc = ((),) <$> setPersistentAccountStake acc PersistentAccountStakeNone
        (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc accId accounts
        MTL.modify $ \bsp -> bsp{bspAccounts = newAccounts}

        dlist <- lift (Trie.keysAsc dset)
        forM_ dlist redelegatePassive

        birkParams <- bspBirkParameters <$> MTL.get
        bab <- lift $ refLoad $ birkParams ^. birkActiveBakers
        newAB <- lift $ Trie.delete bid (bab ^. activeBakers)
        abi <- lift $ refLoad (acctBkr ^. accountBakerInfo)
        newAggKeys <- lift $ Trie.delete (abi ^. BaseAccounts.bakerAggregationVerifyKey) (bab ^. aggregationKeys)
        let PersistentActiveDelegatorsV1 oldDset oldpassiveDelegatorsCapital = bab ^. passiveDelegators
        newDset <- lift $ foldM (\t d -> Trie.insert d () t) oldDset dlist
        newBAB <- lift $ refMake $ PersistentActiveBakers{
                _activeBakers = newAB,
                _aggregationKeys = newAggKeys,
                _passiveDelegators = PersistentActiveDelegatorsV1 newDset (oldpassiveDelegatorsCapital + dcapital),

                _totalActiveCapital = bab ^. totalActiveCapital
            }
        MTL.modify $ \bsp -> bsp{bspBirkParameters = birkParams {_birkActiveBakers = newBAB}}

      reduceBakerStake
        :: BakerId
        -> Amount
        -> PersistentAccountBaker 'AccountV1
        -> MTL.StateT (BlockStatePointers pv) m ()
      reduceBakerStake (BakerId accId) newAmt acctBkr = do
        newBaker <- lift $ refMake acctBkr{_stakedAmount = newAmt, _bakerPendingChange = BaseAccounts.NoChange}
        let updAcc acc = ((),) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newBaker)
        accounts <- bspAccounts <$> MTL.get
        (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc accId accounts
        MTL.modify $ \bsp -> bsp{bspAccounts = newAccounts}

doGetBankStatus :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m Rewards.BankStatus
doGetBankStatus pbs = _unhashed . bspBank <$> loadPBS pbs

doSetRewardAccounts :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Rewards.RewardAccounts -> m (PersistentBlockState pv)
doSetRewardAccounts pbs rewards = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.rewardAccounts .~ rewards}


newtype PersistentBlockStateContext = PersistentBlockStateContext {
    pbscBlobStore :: BlobStore
}

instance HasBlobStore PersistentBlockStateContext where
    blobStore = pbscBlobStore

newtype PersistentBlockStateMonad (pv :: ProtocolVersion) r m a = PersistentBlockStateMonad {runPersistentBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadLogger)

type PersistentState r m = (MonadIO m, MonadReader r m, HasBlobStore r)

instance PersistentState r m => MonadBlobStore (PersistentBlockStateMonad pv r m)
instance PersistentState r m => MonadBlobStore (PutT (PersistentBlockStateMonad pv r m))
instance PersistentState r m => MonadBlobStore (PutH (PersistentBlockStateMonad pv r m))

type instance BlockStatePointer (PersistentBlockState pv) = BlobRef (BlockStatePointers pv)
type instance BlockStatePointer (HashedPersistentBlockState pv) = BlobRef (BlockStatePointers pv)

instance (IsProtocolVersion pv) => MonadProtocolVersion (PersistentBlockStateMonad pv r m) where
    type MPV (PersistentBlockStateMonad pv r m) = pv

instance BlockStateTypes (PersistentBlockStateMonad pv r m) where
    type BlockState (PersistentBlockStateMonad pv r m) = HashedPersistentBlockState pv
    type UpdatableBlockState (PersistentBlockStateMonad pv r m) = PersistentBlockState pv
    type Account (PersistentBlockStateMonad pv r m) = PersistentAccount (AccountVersionFor pv)
    type BakerInfoRef (PersistentBlockStateMonad pv r m) = PersistentBakerInfoEx (AccountVersionFor pv)

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateQuery (PersistentBlockStateMonad pv r m) where
    getModule = doGetModuleSource . hpbsPointers
    getModuleInterface pbs mref = doGetModule (hpbsPointers pbs) mref
    getAccount = doGetAccount . hpbsPointers
    accountExists = doGetAccountExists . hpbsPointers
    getActiveBakers = doGetActiveBakers . hpbsPointers
    getActiveBakersAndDelegators = doGetActiveBakersAndDelegators . hpbsPointers
    getAccountByCredId = doGetAccountByCredId . hpbsPointers
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
    getAnonymityRevokers =  doGetAnonymityRevokers . hpbsPointers
    getUpdateKeysCollection = doGetUpdateKeyCollection . hpbsPointers
    getEnergyRate = doGetEnergyRate . hpbsPointers
    getPaydayEpoch = doGetPaydayEpoch . hpbsPointers
    getPoolStatus = doGetPoolStatus . hpbsPointers

instance (PersistentState r m, IsProtocolVersion pv) => AccountOperations (PersistentBlockStateMonad pv r m) where

  getAccountCanonicalAddress acc = acc ^^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  checkAccountIsAllowed acc AllowedEncryptedTransfers = do
    creds <- getAccountCredentials acc
    return (Map.size creds == 1)
  checkAccountIsAllowed acc AllowedMultipleCredentials = do
    PersistentAccountEncryptedAmount{..} <- loadBufferedRef (acc ^. accountEncryptedAmount)
    if null _incomingEncryptedAmounts && isNothing _aggregatedAmount then do
      isZeroEncryptedAmount <$> loadBufferedRef _selfAmount
    else return False

  getAccountCredentials acc = acc ^^. accountCredentials

  getAccountVerificationKeys acc = acc ^^. accountVerificationKeys

  getAccountEncryptedAmount acc = loadPersistentAccountEncryptedAmount =<< loadBufferedRef (acc ^. accountEncryptedAmount)

  getAccountEncryptionKey acc = acc ^^. accountEncryptionKey

  getAccountReleaseSchedule acc = loadPersistentAccountReleaseSchedule =<< loadBufferedRef (acc ^. accountReleaseSchedule)

  getAccountBaker acc = case acc ^. accountBaker of
        Null -> return Nothing
        Some bref -> do
            pab@PersistentAccountBaker{..} <- refLoad bref
            abi <- refLoad (pab ^. accountBakerInfo)
            case accountVersion @(AccountVersionFor pv) of
                SAccountV0 ->
                    return $ Just BaseAccounts.AccountBaker{
                        _accountBakerInfo = BaseAccounts.BakerInfoExV0 abi, ..}
                SAccountV1 -> do
                    ebi <- refLoad (pab ^. bakerPoolInfoRef)
                    return $ Just BaseAccounts.AccountBaker{
                        _accountBakerInfo = BaseAccounts.BakerInfoExV1 abi ebi, ..}

  getAccountDelegator acc = case acc ^. accountDelegator of
        Null -> return Nothing
        Some dref -> Just <$> refLoad dref

  getAccountStake acc = loadAccountStake (acc ^. accountStake)

  getAccountBakerInfoRef acc = case acc ^. accountBaker of
        Null -> return Nothing
        Some bref -> do
            pab <- refLoad bref
            return (Just (pab ^. accountBakerInfoEx))

  derefBakerInfo = refLoad . bakerInfoRef

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateOperations (PersistentBlockStateMonad pv r m) where
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
    bsoConfigureBaker = doConfigureBaker
    bsoConstrainBakerCommission = doConstrainBakerCommission
    bsoConfigureDelegation = doConfigureDelegation
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
    bsoAddReleaseSchedule = doAddReleaseSchedule
    bsoGetEnergyRate = doGetEnergyRate
    bsoGetChainParameters = doGetChainParameters
    bsoGetEpochBlocksBaked = doGetEpochBlocksBaked
    bsoNotifyBlockBaked = doNotifyBlockBaked
    bsoUpdateAccruedTransactionFeesBaker = doUpdateAccruedTransactionFeesBaker
    bsoMarkFinalizationAwakeBaker = doMarkFinalizationAwakeBaker
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

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateStorage (PersistentBlockStateMonad pv r m) where
    thawBlockState HashedPersistentBlockState{..} =
            liftIO $ newIORef =<< readIORef hpbsPointers

    freezeBlockState pbs = hashBlockState pbs

    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    purgeBlockState _ = return ()
    {-# INLINE purgeBlockState #-}

    archiveBlockState HashedPersistentBlockState{..} = do
        inner <- liftIO $ readIORef hpbsPointers
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef hpbsPointers inner'

    saveBlockState HashedPersistentBlockState{..} = do
        inner <- liftIO $ readIORef hpbsPointers
        (inner', ref) <- flushBufferedRef inner
        liftIO $ writeIORef hpbsPointers inner'
        flushStore
        return ref

    loadBlockState hpbsHash ref = do
        hpbsPointers <- liftIO $ newIORef $ BRBlobbed ref
        return HashedPersistentBlockState{..}

    cacheBlockState pbs@HashedPersistentBlockState{..} = do
        bsp <- liftIO $ readIORef hpbsPointers
        bsp' <- cache bsp
        liftIO $ writeIORef hpbsPointers bsp'
        return pbs

    serializeBlockState hpbs = do
        p <- runPutT (putBlockStateV0 (hpbsPointers hpbs))
        return $ runPut p

    writeBlockState h hpbs =
        runPutH (putBlockStateV0 (hpbsPointers hpbs)) h
