{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
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
import Concordium.GlobalState.Persistent.Instances(PersistentInstance(..), PersistentInstanceParameters(..))
import Concordium.GlobalState.Instance (Instance(..),InstanceParameters(..),makeInstanceHash')
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlockState.Updates
import qualified Concordium.GlobalState.Basic.BlockState.Account as TransientAccount
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.Types.UpdateQueues as UQ
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.Types.Accounts as BaseAccounts
import Concordium.Types.Accounts (StakePendingChange'(..), BakerInfo(_bakerAggregationVerifyKey), PendingChangeEffective(..), HasBakerInfo(..))
import Concordium.Types.SeedState
import Concordium.Logger (MonadLogger)
import Concordium.Types.HashableTo
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.Utils.Serialization.Put
import Concordium.Utils.Serialization

-- * Birk parameters

data PersistentNextEpochBakers (av :: AccountVersion) where
    -- |The 'EpochBakers' for the next epoch.
    PersistentNextEpochBakers :: !(HashedBufferedRef PersistentEpochBakers) -> PersistentNextEpochBakers av
    -- |The next epoch does not require a change in the 'EpochBakers'.
    UnchangedPersistentNextEpochBakers :: PersistentNextEpochBakers 'AccountV1

deriving instance Eq (PersistentNextEpochBakers av)
deriving instance Show (PersistentNextEpochBakers av)

data PersistentBirkParameters (av :: AccountVersion) = PersistentBirkParameters {
    -- |The currently-registered bakers.
    _birkActiveBakers :: !(BufferedRef (PersistentActiveBakers av)),
    -- |The bakers that will be used for the next epoch.
    _birkNextEpochBakers :: !(PersistentNextEpochBakers av),
    -- |The bakers for the current epoch.
    _birkCurrentEpochBakers :: !(HashedBufferedRef PersistentEpochBakers),
    -- |The seed state used to derive the leadership election nonce.
    _birkSeedState :: !SeedState
} deriving (Show)

makeLenses ''PersistentBirkParameters

-- |Serialize 'PersistentBirkParameters' in V0 format.
putBirkParametersV0 :: forall m av. (MonadBlobStore m, MonadPut m, IsAccountVersion av) => PersistentBirkParameters av -> m ()
putBirkParametersV0 PersistentBirkParameters{..} = do
        sPut _birkSeedState
        (_ :: ()) <- case accountVersion @av of
            SAccountV0 -> case _birkNextEpochBakers of
                PersistentNextEpochBakers neb -> putEpochBakers =<< refLoad neb
            SAccountV1 -> case _birkNextEpochBakers of
                PersistentNextEpochBakers neb -> do
                    liftPut $ putWord8 1
                    putEpochBakers =<< refLoad neb
                UnchangedPersistentNextEpochBakers -> liftPut $ putWord8 0
        putEpochBakers =<< refLoad _birkCurrentEpochBakers

instance (MonadBlobStore m, IsAccountVersion av) => MHashableTo m H.Hash (PersistentBirkParameters av) where
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
    MonadBlobStore m => Basic.BasicBirkParameters 'AccountV0 -> m (PersistentBirkParameters 'AccountV0)
makePersistentBirkParameters bbps = do
    _birkActiveBakers <- refMake =<< makePersistentActiveBakers (Basic._birkActiveBakers bbps)
    _birkNextEpochBakers <- case Basic._birkNextEpochBakers bbps of
        Basic.NextEpochBakers neb -> PersistentNextEpochBakers <$> (refMake =<< makePersistentEpochBakers (_unhashed neb))
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
    cache ebs = do
        blocks' <- cache (hebBlocks ebs)
        return $! ebs{hebBlocks = blocks'}

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
    bspInstances :: !Instances.Instances,
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
    -- |Identities of bakers that baked blocks in the current epoch. This is
    -- used for rewarding bakers at the end of epochs.
    bspEpochBlocks :: !HashedEpochBlocks
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
        let bshEpochBlocks = getHash bspEpochBlocks
        return $ makeBlockStateHash BlockStateHashInputs{..}

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
        (pEpochBlocks, bspEpochBlocks') <- storeUpdate bspEpochBlocks
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
                pEpochBlocks
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
                    bspEpochBlocks = bspEpochBlocks'
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
        mEpochBlocks <- label "Epoch blocks" load
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
            bspEpochBlocks <- mEpochBlocks
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
        ebs <- cache bspEpochBlocks
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
            bspEpochBlocks = ebs
        }

-- |Convert an in-memory 'Basic.BlockState' to a disk-backed 'HashedPersistentBlockState'.
makePersistent :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => Basic.BlockState pv -> m (HashedPersistentBlockState pv)
makePersistent Basic.BlockState{..} = do
  persistentBirkParameters <- case accountVersionFor (protocolVersion @pv) of
      SAccountV0 -> makePersistentBirkParameters _blockBirkParameters
      SAccountV1 -> undefined -- FIXME: Handle this
  persistentMods <- Modules.makePersistentModules _blockModules
  persistentBlockInstances <- Instances.makePersistent persistentMods _blockInstances
  modules <- refMake persistentMods
  identityProviders <- bufferHashed _blockIdentityProviders
  anonymityRevokers <- bufferHashed _blockAnonymityRevokers
  cryptographicParameters <- bufferHashed _blockCryptographicParameters
  blockAccounts <- Accounts.makePersistent _blockAccounts
  updates <- makeBufferedRef =<< makePersistentUpdates _blockUpdates
  rels <- makeBufferedRef _blockReleaseSchedule
  ebs <- makeHashedEpochBlocks (Basic.hebBlocks _blockEpochBlocksBaked)
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
          bspEpochBlocks = ebs
        }
  bps <- liftIO $ newIORef $! bsp
  hashBlockState bps

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
  bsp <- makeBufferedRef $ BlockStatePointers
          { bspAccounts = Accounts.emptyAccounts,
            bspInstances = Instances.emptyInstances,
            bspModules = modules,
            bspBank = makeHashed Rewards.emptyBankStatus,
            bspIdentityProviders = identityProviders,
            bspAnonymityRevokers = anonymityRevokers,
            bspCryptographicParameters = cryptographicParameters,
            bspTransactionOutcomes = Transactions.emptyTransactionOutcomes,
            bspEpochBlocks = emptyHashedEpochBlocks,
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
    -- Epoch blocks
    putHashedEpochBlocksV0 bspEpochBlocks

loadPBS :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (BlockStatePointers pv)
loadPBS = loadBufferedRef <=< liftIO . readIORef
{-# INLINE loadPBS #-}

storePBS :: MonadBlobStore m => PersistentBlockState pv -> BlockStatePointers pv -> m (PersistentBlockState pv)
storePBS pbs bsp = liftIO $ do
    pbsp <- makeBufferedRef bsp
    writeIORef pbs pbsp
    return pbs
{-# INLINE storePBS #-}

-- | Fold left over delegators for a given baker
-- 'activeBakerFoldlDelegators' @bsp@ @f@ @a@ @bid@, where
-- * @bsp@ is used to lookup the delegator accounts and active bakers,
-- * @f@ is accumulation function,
-- * @a@ is the initial value,
-- * @bid@ is the baker.
-- If @bid@ is not an active baker in @bsp@, then the initial value @a@ is returned. It is assumed
-- that all delegators to the baker @bid@ are delegator accounts in @bsp@.
activeBakerFoldlDelegators
    :: (IsProtocolVersion pv, MonadBlobStore m)
    => BlockStatePointers pv
    -> (a -> DelegatorId -> BaseAccounts.AccountDelegation (AccountVersionFor pv) -> m a)
    -> a
    -> BakerId
    -> m a
activeBakerFoldlDelegators bsp f a0 bid = do
    pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
    mDset <- Trie.lookup bid (pab ^. activeBakers)
    case mDset of
        Just (PersistentActiveDelegatorsV1 dset) -> foldlM faccount a0 =<< Trie.keys dset
        _ -> return a0
    where
      faccount a did@(DelegatorId aid) = do
        Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
            Just PersistentAccount{_accountStake = PersistentAccountStakeDelegate acctDelRef} ->
                f a did =<< refLoad acctDelRef
            _ ->
                error "Invariant violation: active delegator account not a valid delegator"

-- | Get total pool capital, sum of baker and delegator stakes,
-- 'totalPoolCapital' @bsp@ @bid@, where
-- * @bsp@ is used to lookup accounts and active bakers,
-- * @bid@ is the baker.
-- If @bid@ is not a baker in @bsp@, then @0@ is returned.
-- If @bid@ is not an active baker in @bsp@, then the baker's equity capital (stake) is returned.
-- It is assumed that all delegators to the baker @bid@ are delegator accounts in @bsp@.
totalPoolCapital
    :: forall pv m
     . (IsProtocolVersion pv, MonadBlobStore m)
    => BlockStatePointers pv
    -> BakerId
    -> m Amount
totalPoolCapital bsp bid@(BakerId aid) = do
    Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
        Just PersistentAccount{_accountStake = PersistentAccountStakeBaker acctBkrRef} -> do
            equityCapital <- _stakedAmount <$> refLoad acctBkrRef
            activeBakerFoldlDelegators bsp addDelStake equityCapital bid
        _ ->
            return 0
    where
      addDelStake :: Amount -> DelegatorId -> BaseAccounts.AccountDelegation av -> m Amount
      addDelStake a _ BaseAccounts.AccountDelegationV1{..} = return (a + _delegationStakedAmount)

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

doPutNewModule :: (IsProtocolVersion pv, MonadBlobStore m)
    => PersistentBlockState pv
    -> (GSWasm.ModuleInterface, Wasm.WasmModule)
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

doGetSlotBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Timestamp -> Duration -> Slot -> m FullBakers
doGetSlotBakers pbs genesisTime slotDuration slot = do
        bs <- loadPBS pbs
        let
            bps = bspBirkParameters bs
            SeedState{..} = bps ^. birkSeedState
            slotEpoch = fromIntegral $ slot `quot` epochLength
            slotTime = addDuration genesisTime (fromIntegral slot * slotDuration)
        case compare slotEpoch (epoch + 1) of
            LT -> epochToFullBakers =<< refLoad (bps ^. birkCurrentEpochBakers)
            EQ -> case bps ^. birkNextEpochBakers of
                UnchangedPersistentNextEpochBakers -> epochToFullBakers =<< refLoad (bps ^. birkCurrentEpochBakers)
                PersistentNextEpochBakers neb -> epochToFullBakers =<< refLoad neb
            GT -> do
                activeBids <- Trie.keysAsc . _activeBakers =<< refLoad (bps ^. birkActiveBakers)
                let resolveBaker (BakerId aid) = Accounts.indexedAccount aid (bspAccounts bs) >>= \case
                        Just acct -> case acct ^. accountBaker of
                            Some bkr -> do
                                pab <- refLoad bkr
                                abi <- refLoad (pab ^. accountBakerInfo)
                                return $ case _bakerPendingChange pab of
                                    RemoveStake (PendingChangeEffectiveV0 remEpoch)
                                        | remEpoch < slotEpoch -> Nothing
                                    ReduceStake newAmt (PendingChangeEffectiveV0 redEpoch)
                                        | redEpoch < slotEpoch -> Just (FullBakerInfo (abi ^. bakerInfo) newAmt)
                                    RemoveStake (PendingChangeEffectiveV1 remTime)
                                        | remTime < slotTime -> Nothing
                                    ReduceStake newAmt (PendingChangeEffectiveV1 redTime)
                                        | redTime < slotTime -> Just (FullBakerInfo (abi ^. bakerInfo) newAmt)
                                    _ -> Just (FullBakerInfo (abi ^. bakerInfo) (pab ^. stakedAmount))
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

doTransitionEpochBakers :: forall pv m. (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Timestamp -> Duration -> Epoch -> m (PersistentBlockState pv)
doTransitionEpochBakers pbs genesisTime slotDuration newEpoch = do
        origBSP <- loadPBS pbs
        let oldBPs = bspBirkParameters origBSP
        let epochSlots = fromIntegral (epochLength (oldBPs ^. birkSeedState))
        let newEpochSlot = fromIntegral newEpoch * epochSlots
        let newEpochTime = addDuration genesisTime (newEpochSlot * slotDuration)
        let nextEpochTime = addDuration newEpochTime (epochSlots * slotDuration)
        bakers <- refLoad (_birkActiveBakers oldBPs)
        curActiveBakerDels <- Trie.toAscList (bakers ^. activeBakers)
        -- Retrieve/update the baker info
        let accumBakers (bs0, bkrs0) (bkr@(BakerId aid), pdels) = Accounts.indexedAccount aid (bspAccounts origBSP) >>= \case
                Just PersistentAccount{_accountStake = PersistentAccountStakeBaker acctBkrRef} -> do
                    dels <- case pdels of
                        PersistentActiveDelegatorsV0 -> return []
                        PersistentActiveDelegatorsV1 t -> Trie.keys t
                    bsp <- transitionDelegatorsFromActiveBaker bs0 newEpochTime bkr dels
                    acctBkr <- refLoad acctBkrRef
                    case _bakerPendingChange acctBkr of
                        RemoveStake (PendingChangeEffectiveV0 remEpoch)
                            -- Removal takes effect next epoch, so exclude it from the list of bakers
                            | remEpoch == newEpoch + 1 -> return (bsp, bkrs0)
                            -- Removal complete, so update the active bakers and account as well
                            | remEpoch <= newEpoch -> removeActiveBaker bsp bkrs0 bkr acctBkr
                        ReduceStake newAmt (PendingChangeEffectiveV0 redEpoch)
                            -- Reduction takes effect next epoch, so apply it in the generated list
                            | redEpoch == newEpoch + 1 -> return (bsp, (_accountBakerInfo acctBkr, newAmt) : bkrs0)
                            -- Reduction complete, so update the account as well
                            | redEpoch <= newEpoch -> reduceStakeActiveBaker bsp bkrs0 bkr acctBkr newAmt
                        RemoveStake (PendingChangeEffectiveV1 remTime)
                            -- Removal complete, so update the active bakers and account as well
                            | remTime <= newEpochTime -> do
                                newBsp <- moveDelegatorsToLPool bsp acctBkr
                                removeActiveBaker newBsp bkrs0 bkr acctBkr
                            -- Removal takes effect next epoch, so exclude it from the list of bakers
                            | remTime <= nextEpochTime -> return (bsp, bkrs0)
                        ReduceStake newAmt (PendingChangeEffectiveV1 redTime)
                            -- Reduction complete, so update the account as well
                            | redTime <= newEpochTime -> reduceStakeActiveBaker bsp bkrs0 bkr acctBkr newAmt
                            -- Reduction takes effect next epoch, so apply it in the generated list
                            | redTime <= nextEpochTime -> return (bsp, (_accountBakerInfo acctBkr, newAmt) : bkrs0)
                        _ -> return (bsp, (_accountBakerInfo acctBkr, _stakedAmount acctBkr) : bkrs0)
                _ -> error "Persistent.bsoTransitionEpochBakers invariant violation: active baker account not a valid baker"
        -- Get the baker info. The list of baker ids is reversed in the input so the accumulated list
        -- is in ascending order.
        bsp <- loadPBS pbs
        (bsp', bkrs) <- foldM accumBakers (bsp, []) (reverse curActiveBakerDels)
        newBakerInfos <- refMake . BakerInfos . Vec.fromList $ fst <$> bkrs
        let stakesVec = Vec.fromList $ snd <$> bkrs
        newBakerStakes <- refMake (BakerStakes stakesVec)
        neb <- refLoad (_birkNextEpochBakers oldBPs)
        -- If the baker infos structure has the same hash as the previous one,
        -- use that to avoid duplicate storage.
        _bakerInfos <- secondIfEqual newBakerInfos (_bakerInfos neb)
        -- Also for stakes. This is less likely to be useful, but it's pretty cheap to check,
        -- so why not?
        _bakerStakes <- secondIfEqual newBakerStakes (_bakerStakes neb)
        let _bakerTotalStake = sum stakesVec
        -- TODO: update epoch bakers below to use pool total capital somehow:
        newNextBakers <- refMake PersistentEpochBakers{..}
        storePBS pbs bsp'{bspBirkParameters = (bspBirkParameters bsp') {
            _birkCurrentEpochBakers = _birkNextEpochBakers oldBPs,
            _birkNextEpochBakers = newNextBakers
          }
        }
    where
        transitionDelegatorsFromActiveBaker
            :: BlockStatePointers pv
            -> Timestamp
            -> BakerId
            -> [DelegatorId]
            -> m (BlockStatePointers pv)
        transitionDelegatorsFromActiveBaker bsp newEpochTime bid dels = do
            let ff = updateDelegatorSetAndAccounts bsp newEpochTime
            (newDset, newAccounts) <- foldlM ff (Trie.empty, bspAccounts bsp) dels
            pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
            newBirkBakers <- case accountVersion @(AccountVersionFor pv) of
                                SAccountV0 ->
                                    return (pab ^. activeBakers)
                                SAccountV1 -> do
                                    let ds = PersistentActiveDelegatorsV1 newDset
                                    Trie.insert bid ds (pab ^. activeBakers)
            newpab <- refMake pab{_activeBakers = newBirkBakers}
            let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpab
            return bsp{bspBirkParameters = newBirkParams, bspAccounts = newAccounts}

        updateDelegatorSetAndAccounts
            :: BlockStatePointers pv
            -> Timestamp
            -> (DelegatorIdTrieSet, Accounts.Accounts pv)
            -> DelegatorId
            -> m (DelegatorIdTrieSet, Accounts.Accounts pv)
        updateDelegatorSetAndAccounts bsp newEpochTime (dset, accounts) did@(DelegatorId aid) = do
            Accounts.indexedAccount aid (bspAccounts bsp) >>= \case
                Just acct -> case acct ^. accountDelegator of
                    Some pAcctDel -> do
                        acctDel@BaseAccounts.AccountDelegationV1{..} <- refLoad pAcctDel
                        case _delegationPendingChange of
                            RemoveStake (PendingChangeEffectiveV1 remTime)
                                | remTime <= newEpochTime ->
                                    removeDelegator did dset accounts
                            ReduceStake newAmt (PendingChangeEffectiveV1 redTime)
                                | redTime <= newEpochTime ->
                                    newStakeDelegator did acctDel dset accounts newAmt
                            _ -> return (dset, accounts)
                    Null -> error "Invariant violation: active delegator is not a delegation account"
                _ -> error "Invariant violation: active delegator account was not found"

        removeDelegator :: DelegatorId -> DelegatorIdTrieSet -> Accounts.Accounts pv -> m (DelegatorIdTrieSet, Accounts.Accounts pv)
        removeDelegator (DelegatorId aid) dset accounts = do
            let updAcc acc = ((),) <$> setPersistentAccountStake acc PersistentAccountStakeNone
            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid accounts
            return (dset, newAccounts)

        newStakeDelegator :: DelegatorId -> BaseAccounts.AccountDelegation (AccountVersionFor pv) -> DelegatorIdTrieSet -> Accounts.Accounts pv -> Amount -> m (DelegatorIdTrieSet, Accounts.Accounts pv)
        newStakeDelegator did@(DelegatorId aid) acctDel dset accounts newAmt = do
            newDel <- refMake acctDel{
                BaseAccounts._delegationStakedAmount = newAmt,
                BaseAccounts._delegationPendingChange = NoChange}
            let updAcc acc = ((),) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newDel)
            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid accounts
            newDset <- Trie.insert did () dset
            return (newDset, newAccounts)

        removeActiveBaker bs bkrs bkr@(BakerId aid) acctBkr = do
            curABs <- refLoad (_birkActiveBakers (bspBirkParameters bs))
            newAB <- Trie.delete bkr (_activeBakers curABs)
            abi <- refLoad (_accountBakerInfo acctBkr)
            newAK <- Trie.delete (abi ^. bakerAggregationVerifyKey) (_aggregationKeys curABs)
            newABs <- refMake $ PersistentActiveBakers {
                    _activeBakers = newAB,
                    _aggregationKeys = newAK
                }
            -- Remove the baker from the account
            let updAcc acc = ((),) <$> setPersistentAccountStake acc PersistentAccountStakeNone
            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs)
            -- The baker is not included for this epoch
            return (bs {
                    bspBirkParameters = (bspBirkParameters bs) {_birkActiveBakers = newABs},
                    bspAccounts = newAccounts
                }, bkrs)

        reduceStakeActiveBaker bs bkrs (BakerId aid) acctBkr newAmt = do
            newBaker <- refMake acctBkr{_stakedAmount = newAmt, _bakerPendingChange = NoChange}
            let updAcc acc = ((),) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newBaker)
            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs)
            -- The baker is included with the revised stake
            return (bs {bspAccounts = newAccounts}, (_accountBakerInfo acctBkr, newAmt) : bkrs)

        moveDelegatorsToLPool
            :: BlockStatePointers pv
            -> PersistentAccountBaker 'AccountV1
            -> m (BlockStatePointers pv)
        moveDelegatorsToLPool bsp acctBkr = undefined -- TODO: implement

        secondIfEqual a b = do
            h1 <- getHashM a
            h2 <- getHashM b
            return $ if (h1 :: H.Hash) == h2 then b else a

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
                            newActiveBakers <- Trie.insert bid emptyPersistentAccountDelegators (_activeBakers pab)
                            newpabref <- refMake PersistentActiveBakers{
                                    _aggregationKeys = newAggregationKeys,
                                    _activeBakers = newActiveBakers
                                }
                            let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                            let updAcc acc = do
                                    newBakerInfo <- refMake $
                                        bakerKeyUpdateToInfo bid baKeys
                                    newPAB <- refMake PersistentAccountBaker{
                                        _stakedAmount = baStake,
                                        _stakeEarnings = baStakeEarnings,
                                        _accountBakerInfo = newBakerInfo,
                                        _extraBakerInfo = (),
                                        _bakerPendingChange = NoChange
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
                let epochBakersBR = bspBirkParameters bsp ^. birkCurrentEpochBakers
                epochBakers <- refLoad $ bufferedReference epochBakersBR
                let capitalMax = takeFraction (poolParams ^. ppCapitalBound) (_bakerTotalStake epochBakers)
                let ranges = poolParams ^. ppCommissionBounds
                let keysInRange = isInRange bcaFinalizationRewardCommission (ranges ^. finalizationCommissionRange)
                        && isInRange bcaBakingRewardCommission (ranges ^. bakingCommissionRange)
                        && isInRange bcaTransactionFeeCommission (ranges ^. transactionCommissionRange)
                if bcaCapital < capitalMin then
                      return (BCStakeUnderThreshold, pbs)
                else if bcaCapital > capitalMax then
                    return (BCStakeOverThreshold, pbs)
                  else if not keysInRange then
                      return (BCCommissionNotInRange, pbs)
                  else do
                    let bid = BakerId ai
                    pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                    let updAgg Nothing = return (True, Trie.Insert ())
                        updAgg (Just ()) = return (False, Trie.NoChange)
                    Trie.adjust updAgg (bkuAggregationKey bcaKeys) (_aggregationKeys pab) >>= \case
                        -- Aggregation key is a duplicate
                        (False, _) -> return (BCDuplicateAggregationKey (bkuAggregationKey bcaKeys), pbs)
                        (True, newAggregationKeys) -> do
                            newActiveBakers <- Trie.insert bid emptyPersistentAccountDelegators (_activeBakers pab)
                            newpabref <- refMake PersistentActiveBakers{
                                    _aggregationKeys = newAggregationKeys,
                                    _activeBakers = newActiveBakers
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
                                        _bakerPendingChange = NoChange
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
        let rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
            cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpPoolOwnerCooldown
            msInEpoch = fromIntegral (epochLength $ _birkSeedState . bspBirkParameters $ origBSP) * bcuSlotDuration
            cooldownTimestamp = addDuration bcuTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        res <- MTL.runExceptT $ MTL.runWriterT $ flip MTL.execStateT origBSP $ do
                updateKeys
                updateRestakeEarnings
                updateOpenForDelegation
                updateMetadataURL
                updateTransactionFeeCommission cp
                updateBakingRewardCommission cp
                updateFinalizationRewardCommission cp
                updateCapital cooldownTimestamp cp
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
        requireNoPendingChange = do
            ab <- getAccountOrFail
            when (_bakerPendingChange ab /= NoChange) (MTL.throwError BCChangePending)
        updateKeys = forM_ bcuKeys $ \keys -> do
            acctBkr <- getAccountOrFail
            bsp <- MTL.get
            pab <- liftBSO $ refLoad (_birkActiveBakers (bspBirkParameters bsp))
            (bkrInfo, bkrPoolInfo) <- liftBSO $ refLoad (_accountBakerInfo acctBkr) <&> \case
                BaseAccounts.BakerInfoExV1 bkrInfo bkrPoolInfo -> (bkrInfo, bkrPoolInfo)
            let key = _bakerAggregationVerifyKey bkrInfo
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
                    newBakerInfo <- refMake $ BaseAccounts.BakerInfoExV1
                        (bakerKeyUpdateToInfo (BakerId ai) keys) bkrPoolInfo
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
            abi <- liftBSO $ refLoad $ acctBkr ^. accountBakerInfo
            unless (abi ^. BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolOpenStatus == openForDelegation) $ do
                let updAcc acc = do
                        newAbi <- refMake (abi & BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolOpenStatus .~ openForDelegation)
                        newPAB <- refMake (acctBkr & accountBakerInfo .~ newAbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureOpenForDelegation openForDelegation]
        updateMetadataURL = forM_ bcuMetadataURL $ \metadataURL -> do
            acctBkr <- getAccountOrFail
            abi <- liftBSO $ refLoad $ acctBkr ^. accountBakerInfo
            unless (abi ^. BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolMetadataUrl == metadataURL) $ do
                let updAcc acc = do
                        newAbi <- refMake (abi & BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolMetadataUrl .~ metadataURL)
                        newPAB <- refMake (acctBkr & accountBakerInfo .~ newAbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureMetadataURL metadataURL]
        updateTransactionFeeCommission cp = forM_ bcuTransactionFeeCommission $ \tfc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
            unless (isInRange tfc range) (MTL.throwError BCCommissionNotInRange)
            acctBkr <- getAccountOrFail
            abi <- liftBSO $ refLoad $ acctBkr ^. accountBakerInfo
            unless (abi ^. BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . transactionCommission == tfc) $ do
                let updAcc acc = do
                        newAbi <- refMake (abi & BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . transactionCommission .~ tfc)
                        newPAB <- refMake (acctBkr & accountBakerInfo .~ newAbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureTransactionFeeCommission tfc]
        updateBakingRewardCommission cp = forM_ bcuBakingRewardCommission $ \brc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
            unless (isInRange brc range) (MTL.throwError BCCommissionNotInRange)
            acctBkr <- getAccountOrFail
            abi <- liftBSO $ refLoad $ acctBkr ^. accountBakerInfo
            unless (abi ^. BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . bakingCommission == brc) $ do
                let updAcc acc = do
                        newAbi <- refMake (abi & BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . bakingCommission .~ brc)
                        newPAB <- refMake (acctBkr & accountBakerInfo .~ newAbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureBakingRewardCommission brc]
        updateFinalizationRewardCommission cp = forM_ bcuFinalizationRewardCommission $ \frc -> do
            let range = cp ^. cpPoolParameters . ppCommissionBounds . transactionCommissionRange
            unless (isInRange frc range) (MTL.throwError BCCommissionNotInRange)
            acctBkr <- getAccountOrFail
            abi <- liftBSO $ refLoad $ acctBkr ^. accountBakerInfo
            unless (abi ^. BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . finalizationCommission == frc) $ do
                let updAcc acc = do
                        newAbi <- refMake (abi & BaseAccounts.bieBakerPoolInfo . BaseAccounts.poolCommissionRates . finalizationCommission .~ frc)
                        newPAB <- refMake (acctBkr & accountBakerInfo .~ newAbi)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                modifyAccount updAcc
            MTL.tell [BakerConfigureFinalizationRewardCommission frc]
        updateCapital cooldownTimestamp cp = forM_ bcuCapital $ \capital -> do
            requireNoPendingChange
            ab <- getAccountOrFail
            let capitalMin = cp ^. cpPoolParameters . ppMinimumEquityCapital
            when (capital < capitalMin) (MTL.throwError BCStakeUnderThreshold)
            bsp <- MTL.get
            let epochBakersBR = bspBirkParameters bsp ^. birkCurrentEpochBakers
            epochBakers <- liftBSO $ refLoad $ bufferedReference epochBakersBR
            let pp = cp ^. cpPoolParameters
            let capitalMax = takeFraction (pp ^. ppCapitalBound) (_bakerTotalStake epochBakers)
            when (capital > capitalMax) (MTL.throwError BCStakeOverThreshold)
            let updAcc updateStake acc = do
                    newPAB <- refMake $ updateStake ab
                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                    return ((), acc')
            case compare capital (_stakedAmount ab) of
                LT -> do
                    let bpc = ReduceStake capital (PendingChangeEffectiveV1 cooldownTimestamp)
                    modifyAccount $ updAcc $ bakerPendingChange .~ bpc
                    MTL.tell [BakerConfigureStakeReduced capital]
                EQ ->
                    return ()
                GT -> do
                    modifyAccount $ updAcc $ stakedAmount .~ capital
                    MTL.tell [BakerConfigureStakeIncreased capital]
doConfigureBaker pbs ai BakerConfigureRemove{..} = do
        bsp <- loadPBS pbs
        cp <- doGetChainParameters pbs
        let rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
            cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpPoolOwnerCooldown
            msInEpoch = fromIntegral (epochLength $ _birkSeedState . bspBirkParameters $ bsp) * bcrSlotDuration
            cooldownTimestamp = addDuration bcrTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just PersistentAccount{_accountStake = PersistentAccountStakeBaker pab} -> do
                ab <- refLoad pab
                if _bakerPendingChange ab /= NoChange then
                    -- A change is already pending
                    return (BCChangePending, pbs)
                else do
                    let updAcc acc = do
                            newPAB <- refMake ab{_bakerPendingChange = RemoveStake (PendingChangeEffectiveV1 cooldownTimestamp)}
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BCSuccess [] (BakerId ai),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            -- The account is not valid or has no baker
            _ -> return (BCInvalidBaker, pbs)

delegationConfigureDisallowOverdelegation
    :: (IsProtocolVersion pv, MTL.MonadError DelegationConfigureResult m, MonadBlobStore n)
    => (forall a. n a -> m a)
    -> BlockStatePointers pv
    -> PoolParameters 'ChainParametersV1
    -> DelegationTarget
    -> m ()
delegationConfigureDisallowOverdelegation liftMBS bsp poolParams target = case target of
  Transactions.DelegateToLPool -> return ()
  Transactions.DelegateToBaker bid@(BakerId baid) -> do
    let capitalBound = poolParams ^. ppCapitalBound
        leverageBound = poolParams ^. ppLeverageBound
    poolCapital <- liftMBS $ totalPoolCapital bsp bid
    bakerEquityCapital <- liftMBS (Accounts.indexedAccount baid (bspAccounts bsp)) >>= \case
      Just PersistentAccount{_accountStake = PersistentAccountStakeBaker abr} ->
          liftMBS $ _stakedAmount <$> refLoad abr
      _ ->
          MTL.throwError (DCInvalidDelegationTarget bid)
    when (fromIntegral poolCapital > fromIntegral bakerEquityCapital * leverageBound) $
      MTL.throwError DCPoolStakeOverThreshold
    let epochBakersBR = bspBirkParameters bsp ^. birkCurrentEpochBakers
    epochBakers <- liftMBS $ refLoad $ bufferedReference epochBakersBR
    let allCCD = _bakerTotalStake epochBakers
    when (poolCapital > takeFraction capitalBound allCCD) $
      MTL.throwError DCPoolOverDelegated

doConfigureDelegation
    :: (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ 'AccountV1, ChainParametersVersionFor pv ~ 'ChainParametersV1)
    => PersistentBlockState pv
    -> AccountIndex
    -> DelegationConfigure
    -> m (DelegationConfigureResult, PersistentBlockState pv)
doConfigureDelegation pbs ai DelegationConfigureAdd{..} = do
        -- It is assumed here that this account is NOT a baker and NOT a delegator.
        bsp <- loadPBS pbs
        poolParams <- _cpPoolParameters <$> doGetChainParameters pbs
        result <- MTL.runExceptT $ do
            newBSP <- updateBlockState bsp
            delegationConfigureDisallowOverdelegation lift newBSP poolParams dcaDelegationTarget
            return newBSP
        case result of
            Left e -> return (e, pbs)
            Right newBirkParams -> (DCSuccess [] did,) <$> storePBS pbs newBirkParams
        where
          did = DelegatorId ai
          updateBlockState bsp = lift (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
            Nothing -> MTL.throwError DCInvalidAccount
            Just PersistentAccount{} -> do
                newBirkParams <- updateBirk bsp dcaDelegationTarget
                let updAcc acc = do
                        newPAD <- refMake BaseAccounts.AccountDelegationV1{
                            BaseAccounts._delegationIdentity = did,
                            BaseAccounts._delegationStakedAmount = dcaCapital,
                            BaseAccounts._delegationStakeEarnings = dcaRestakeEarnings,
                            BaseAccounts._delegationTarget = dcaDelegationTarget,
                            BaseAccounts._delegationPendingChange = NoChange
                        }
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                -- This cannot fail to update the accounts, since we already looked up the accounts:
                (_, newAccounts) <- lift $ Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                return bsp{bspBirkParameters = newBirkParams, bspAccounts = newAccounts}
          updateBirk bsp Transactions.DelegateToLPool = undefined -- TODO: implement delegate to L-pool here.
          updateBirk bsp (Transactions.DelegateToBaker bid) = do
            pab <- lift $ refLoad (bspBirkParameters bsp ^. birkActiveBakers)
            mDels <- lift $ Trie.lookup bid (pab ^. activeBakers)
            case mDels of
                Nothing -> MTL.throwError (DCInvalidDelegationTarget bid)
                Just (PersistentActiveDelegatorsV1 dels) -> do
                    newDels <- lift $ PersistentActiveDelegatorsV1 <$> Trie.insert did () dels
                    newActiveBakers <- lift $ Trie.insert bid newDels (pab ^. activeBakers)
                    newpabref <- lift $ refMake pab{_activeBakers = newActiveBakers}
                    return $ bspBirkParameters bsp & birkActiveBakers .~ newpabref
doConfigureDelegation pbs ai DelegationConfigureUpdate{..} = do
        origBSP <- loadPBS pbs
        cp <- doGetChainParameters pbs
        let rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
            cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpDelegatorCooldown
            msInEpoch = fromIntegral (epochLength $ _birkSeedState . bspBirkParameters $ origBSP) * dcuSlotDuration
            cooldownTimestamp = addDuration dcuTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        res <- MTL.runExceptT $ MTL.runWriterT $ flip MTL.execStateT origBSP $ do
                updateDelegationTarget
                updateRestakeEarnings
                updateCapital cooldownTimestamp cp
        case res of
            Left errorRes -> return (errorRes, pbs)
            Right (newBSP, changes) -> (DCSuccess changes did,) <$> storePBS pbs newBSP
      where
        liftBSO = lift . lift . lift
        did = DelegatorId ai
        getAccountOrFail = do
            bsp <- MTL.get
            liftBSO (Accounts.indexedAccount ai (bspAccounts bsp)) >>= \case
                Nothing -> MTL.throwError DCInvalidAccount
                Just PersistentAccount{_accountStake = PersistentAccountStakeDelegate ad} -> liftBSO $ refLoad ad
                Just PersistentAccount{} -> MTL.throwError DCInvalidDelegator
        modifyAccount updAcc = do
            bsp <- MTL.get
            (_, newAccounts) <- liftBSO $ Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
            MTL.put bsp{
                bspAccounts = newAccounts
            }
        requireNoPendingChange = do
            ad <- getAccountOrFail
            when (BaseAccounts._delegationPendingChange ad /= NoChange) (MTL.throwError DCChangePending)
        updateDelegationTarget = forM_ dcuDelegationTarget $ \target -> do
            acctBkr <- getAccountOrFail
            unless (acctBkr ^. BaseAccounts.delegationTarget == target) $ do
                let updAcc acc = do
                        newPAD <- refMake (acctBkr & BaseAccounts.delegationTarget .~ target)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                modifyAccount updAcc
            MTL.tell [DelegationConfigureDelegationTarget target]
        updateRestakeEarnings = forM_ dcuRestakeEarnings $ \restakeEarnings -> do
            acctBkr <- getAccountOrFail
            unless (acctBkr ^. BaseAccounts.delegationStakeEarnings == restakeEarnings) $ do
                let updAcc acc = do
                        newPAD <- refMake (acctBkr & BaseAccounts.delegationStakeEarnings .~ restakeEarnings)
                        ((), ) <$> setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                modifyAccount updAcc
            MTL.tell [DelegationConfigureRestakeEarnings restakeEarnings]
        updateCapital cooldownTimestamp cp = forM_ dcuCapital $ \capital -> do
            requireNoPendingChange
            ad <- getAccountOrFail
            let updAcc updateStake acc = do
                    newPAD <- refMake $ updateStake ad
                    acc' <- setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                    return ((), acc')
            case compare capital (BaseAccounts._delegationStakedAmount ad) of
                LT -> do
                    let dpc = ReduceStake capital (PendingChangeEffectiveV1 cooldownTimestamp)
                    modifyAccount $ updAcc $ BaseAccounts.delegationPendingChange .~ dpc
                    MTL.tell [DelegationConfigureStakeReduced capital]
                EQ ->
                    return ()
                GT -> do
                    modifyAccount $ updAcc $ BaseAccounts.delegationStakedAmount .~ capital
                    bsp <- MTL.get
                    let pp = cp ^. cpPoolParameters
                    let target = ad ^. BaseAccounts.delegationTarget
                    -- The delegation target may be updated by 'updateDelegationTarget', hence it
                    -- is important that 'updateDelegationTarget' is invoked before 'updateCapital'.
                    delegationConfigureDisallowOverdelegation liftBSO bsp pp target
                    MTL.tell [DelegationConfigureStakeIncreased capital]
doConfigureDelegation pbs ai DelegationConfigureRemove{..} = do
        bsp <- loadPBS pbs
        cp <- doGetChainParameters pbs
        let rewardPeriodLength = fromIntegral $ cp ^. cpTimeParameters . tpRewardPeriodLength
            cooldown = fromIntegral $ cp ^. cpCooldownParameters . cpDelegatorCooldown
            msInEpoch = fromIntegral (epochLength $ _birkSeedState . bspBirkParameters $ bsp) * dcrSlotDuration
            cooldownTimestamp = addDuration dcrTimestamp (cooldown * rewardPeriodLength * msInEpoch)
        Accounts.indexedAccount ai (bspAccounts bsp) >>= \case
            Just PersistentAccount{_accountStake = PersistentAccountStakeDelegate pad} -> do
                ad <- refLoad pad
                if BaseAccounts._delegationPendingChange ad /= NoChange then
                    return (DCChangePending, pbs)
                else do
                    newBirkParams <- updateBirk bsp (BaseAccounts._delegationTarget ad)
                    let updAcc acc = do
                            let rs = RemoveStake (PendingChangeEffectiveV1 cooldownTimestamp)
                            newPAD <- refMake ad{BaseAccounts._delegationPendingChange = rs}
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeDelegate newPAD)
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (DCSuccess [] (DelegatorId ai),) <$> storePBS pbs bsp{
                        bspAccounts = newAccounts,
                        bspBirkParameters = newBirkParams}
            _ -> return (DCInvalidDelegator, pbs)
        where
          updateBirk bsp Transactions.DelegateToLPool = undefined -- TODO: implement delegate to L-pool here.
          updateBirk bsp (Transactions.DelegateToBaker bid) = do
            pab <- refLoad (bspBirkParameters bsp ^. birkActiveBakers)
            newBakers <- Trie.delete bid (pab ^. activeBakers)
            newpabref <- refMake pab{_activeBakers = newBakers}
            return $ bspBirkParameters bsp & birkActiveBakers .~ newpabref

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
                bkrInfo <- refLoad (_accountBakerInfo acctBkr) <&> \case
                    BaseAccounts.BakerInfoExV0 bkrInfo -> bkrInfo
                -- Try updating the aggregation keys
                (keyOK, newAggregationKeys) <-
                        -- If the aggregation key has not changed, we have nothing to do.
                        if bkuAggregationKey == _bakerAggregationVerifyKey bkrInfo then
                            return (True, _aggregationKeys pab)
                        else do
                            -- Remove the old key
                            ak1 <- Trie.delete (_bakerAggregationVerifyKey bkrInfo) (_aggregationKeys pab)
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
                            newBakerInfo <- refMake $ BaseAccounts.BakerInfoExV0 $
                                bakerKeyUpdateToInfo (BakerId ai) bku
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
                if _bakerPendingChange acctBkr /= NoChange
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
                                        applyUpdate (bakerPendingChange .~ ReduceStake newStake (PendingChangeEffectiveV0 $ curEpoch + cooldown))
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
                if _bakerPendingChange ab /= NoChange then
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
                            newPAB <- refMake ab{_bakerPendingChange = RemoveStake (PendingChangeEffectiveV0 $ curEpoch + cooldown)}
                            acc' <- setPersistentAccountStake acc (PersistentAccountStakeBaker newPAB)
                            return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BRRemoved (BakerId ai) (curEpoch + cooldown),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            -- The account is not valid or has no baker
            _ -> return (BRInvalidBaker, pbs)


doRewardBaker :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> Amount -> m (Maybe AccountAddress, PersistentBlockState pv)
doRewardBaker pbs (BakerId ai) reward = do
        bsp <- loadPBS pbs
        (maddr, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
        (maddr,) <$> storePBS pbs bsp{bspAccounts = newAccounts}
    where
        updAcc acc = do
            addr <- acc ^^. accountAddress
            newAccountBaker <- case acc ^. accountStake of
                pas@(PersistentAccountStakeBaker pbkr) -> do
                    bkr <- refLoad pbkr
                    if bkr ^. stakeEarnings then
                        PersistentAccountStakeBaker <$> refMake (bkr & stakedAmount +~ reward)
                    else
                        return pas
                pas -> return pas
            acc' <- rehashAccount $ acc & accountStake .~ newAccountBaker & accountAmount +~ reward
            return (addr, acc')

doGetRewardStatus :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m Rewards.BankStatus
doGetRewardStatus pbs = _unhashed . bspBank <$> loadPBS pbs

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

doAccountList :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Accounts.accountAddresses (bspAccounts bsp)

doAddressWouldClash :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m Bool
doAddressWouldClash pbs addr = do
        bsp <- loadPBS pbs
        Accounts.addressWouldClash addr (bspAccounts bsp)

doRegIdExists :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.CredentialRegistrationID -> m (Maybe AccountIndex)
doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        fst <$> Accounts.regIdExists regid (bspAccounts bsp)

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
        let ca = instanceAddress (instanceParameters inst)
        (ca,) <$> storePBS pbs bsp{bspInstances = insts}
        
    where
        fnew' mods ca = let inst@Instance{instanceParameters = InstanceParameters{..}, ..} = fnew ca in do
            params <- makeBufferedRef $ PersistentInstanceParameters {
                                            pinstanceAddress = instanceAddress,
                                            pinstanceOwner = instanceOwner,
                                            pinstanceContractModule = GSWasm.miModuleRef instanceModuleInterface,
                                            pinstanceReceiveFuns = instanceReceiveFuns,
                                            pinstanceInitName = instanceInitName,
                                            pinstanceParameterHash = instanceParameterHash
                                        }
            -- This in an irrefutable pattern because otherwise it would have failed in previous stages
            -- as it would be trying to create an instance of a module that doesn't exist.
            ~(Just modRef) <- Modules.getModuleReference (GSWasm.miModuleRef instanceModuleInterface) mods
            return (inst, PersistentInstance{
                pinstanceParameters = params,
                pinstanceModuleInterface = modRef,
                pinstanceModel = instanceModel,
                pinstanceAmount = instanceAmount,
                pinstanceHash = instanceHash
            })

doModifyInstance :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ContractAddress -> AmountDelta -> Wasm.ContractState -> m (PersistentBlockState pv)
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadPBS pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (_, insts) ->
                storePBS pbs bsp{bspInstances = insts}
    where
        upd oldInst = do
            (piParams, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0 then
                return ((), rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceModel = val})
            else
                return ((), rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = val})
        rehash iph inst@PersistentInstance {..} = inst {pinstanceHash = makeInstanceHash' iph pinstanceModel pinstanceAmount}

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

doGetEpochBlocksBaked :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (Word64, [(BakerId, Word64)])
doGetEpochBlocksBaked pbs = do
        bsp <- loadPBS pbs
        accumBakers (hebBlocks (bspEpochBlocks bsp)) 0 Map.empty
    where
        accumBakers Null t m = return (t, Map.toList m)
        accumBakers (Some ref) t m = do
            EpochBlock{..} <- refLoad ref
            let !t' = t + 1
                !m' = m & at ebBakerId . non 0 +~ 1
            accumBakers ebPrevious t' m'

doNotifyBlockBaked :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> m (PersistentBlockState pv)
doNotifyBlockBaked pbs bid = do
        bsp <- loadPBS pbs
        newEpochBlocks <- consEpochBlock bid (bspEpochBlocks bsp)
        storePBS pbs bsp{bspEpochBlocks = newEpochBlocks}

doClearEpochBlocksBaked :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (PersistentBlockState pv)
doClearEpochBlocksBaked pbs = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspEpochBlocks = emptyHashedEpochBlocks}

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
    type BakerInfoRef (PersistentBlockStateMonad pv r m) = BufferedRef BakerInfo

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateQuery (PersistentBlockStateMonad pv r m) where
    getModule = doGetModuleSource . hpbsPointers
    getAccount = doGetAccount . hpbsPointers
    getActiveBakers = doGetActiveBakers . hpbsPointers
    getAccountByCredId = doGetAccountByCredId . hpbsPointers
    getContractInstance = doGetInstance . hpbsPointers
    getModuleList = doGetModuleList . hpbsPointers
    getAccountList = doAccountList . hpbsPointers
    getContractInstanceList = doContractInstanceList . hpbsPointers
    getSeedState = doGetSeedState . hpbsPointers
    getCurrentEpochBakers = doGetCurrentEpochBakers . hpbsPointers
    getSlotBakers = doGetSlotBakers . hpbsPointers
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
    getProtocolUpdateStatus = doGetProtocolUpdateStatus . hpbsPointers
    getCryptographicParameters = doGetCryptoParams . hpbsPointers

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
            PersistentAccountBaker{..} <- refLoad bref
            abi <- refLoad _accountBakerInfo
            return $ Just BaseAccounts.AccountBaker{_accountBakerInfo = abi, ..}

  getAccountDelegator acc = case acc ^. accountDelegator of
        Null -> return Nothing
        Some dref -> Just <$> refLoad dref

  getAccountStake acc = loadAccountStake (acc ^. accountStake)

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateOperations (PersistentBlockStateMonad pv r m) where
    bsoGetModule pbs mref = doGetModule pbs mref
    bsoGetAccount bs = doGetAccount bs
    bsoGetAccountIndex = doGetAccountIndex
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
    bsoAddBaker = doAddBaker
    bsoConfigureBaker = doConfigureBaker
    bsoConfigureDelegation = doConfigureDelegation
    bsoUpdateBakerKeys = doUpdateBakerKeys
    bsoUpdateBakerStake = doUpdateBakerStake
    bsoUpdateBakerRestakeEarnings = doUpdateBakerRestakeEarnings
    bsoRemoveBaker = doRemoveBaker
    bsoRewardBaker = doRewardBaker
    bsoRewardFoundationAccount = doRewardFoundationAccount
    bsoGetFoundationAccount = doGetFoundationAccount
    bsoMint = doMint
    bsoGetIdentityProvider = doGetIdentityProvider
    bsoGetAnonymityRevokers = doGetAnonymityRevokers
    bsoGetCryptoParams = doGetCryptoParams
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoProcessUpdateQueues = doProcessUpdateQueues
    bsoProcessReleaseSchedule = doProcessReleaseSchedule
    bsoGetUpdateKeyCollection = doGetUpdateKeyCollection
    bsoGetNextUpdateSequenceNumber = doGetNextUpdateSequenceNumber
    bsoEnqueueUpdate = doEnqueueUpdate
    bsoOverwriteElectionDifficulty = doOverwriteElectionDifficulty
    bsoClearProtocolUpdate = doClearProtocolUpdate
    bsoAddReleaseSchedule = doAddReleaseSchedule
    bsoGetEnergyRate = doGetEnergyRate
    bsoGetChainParameters = doGetChainParameters
    bsoGetEpochBlocksBaked = doGetEpochBlocksBaked
    bsoNotifyBlockBaked = doNotifyBlockBaked
    bsoClearEpochBlocksBaked = doClearEpochBlocksBaked
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
