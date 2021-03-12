{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.Execution ( TransactionSummary )
import qualified Concordium.Wasm as Wasm
import qualified Concordium.ID.Types as ID
import qualified Concordium.ID.Parameters as ID
import Concordium.Types.Updates
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Types
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount, _stakedAmount, _stakeEarnings, _accountBakerInfo, _bakerPendingChange, stakedAmount, stakeEarnings, accountBakerInfo, bakerPendingChange)
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
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import Concordium.Types.SeedState
import Concordium.Logger (MonadLogger)
import Concordium.Types.HashableTo
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.Utils.Serialization.Put
import Concordium.Utils.Serialization

-- * Birk parameters

data PersistentBirkParameters = PersistentBirkParameters {
    -- |The currently-registered bakers.
    _birkActiveBakers :: !(BufferedRef PersistentActiveBakers),
    -- |The bakers that will be used for the next epoch.
    _birkNextEpochBakers :: !(HashedBufferedRef PersistentEpochBakers),
    -- |The bakers for the current epoch.
    _birkCurrentEpochBakers :: !(HashedBufferedRef PersistentEpochBakers),
    -- |The seed state used to derive the leadership election nonce.
    _birkSeedState :: !SeedState
} deriving (Show)

makeLenses ''PersistentBirkParameters

-- |Serialize 'PersistentBirkParameters' in V0 format.
putBirkParametersV0 :: (MonadBlobStore m, MonadPut m) => PersistentBirkParameters -> m ()
putBirkParametersV0 PersistentBirkParameters{..} = do
        sPut _birkSeedState
        putEpochBakersV0 =<< refLoad _birkNextEpochBakers
        putEpochBakersV0 =<< refLoad _birkCurrentEpochBakers

instance MonadBlobStore m => MHashableTo m H.Hash PersistentBirkParameters where
  getHashM PersistentBirkParameters {..} = do
    nextHash <- getHashM _birkNextEpochBakers
    currentHash <- getHashM _birkCurrentEpochBakers
    let bpH0 = H.hash $ "SeedState" <> encode _birkSeedState
        bpH1 = H.hashOfHashes nextHash currentHash
    return $ H.hashOfHashes bpH0 bpH1

instance MonadBlobStore m => BlobStorable m PersistentBirkParameters where
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

instance MonadBlobStore m => Cacheable m PersistentBirkParameters where
    cache PersistentBirkParameters{..} = do
        active <- cache _birkActiveBakers
        next <- cache _birkNextEpochBakers
        cur <- cache _birkCurrentEpochBakers
        return PersistentBirkParameters{
            _birkActiveBakers = active,
            _birkNextEpochBakers = next,
            _birkCurrentEpochBakers = cur,
            ..
        }

makePersistentBirkParameters :: MonadBlobStore m => Basic.BasicBirkParameters -> m PersistentBirkParameters
makePersistentBirkParameters bbps = do
    _birkActiveBakers <- refMake =<< makePersistentActiveBakers (Basic._birkActiveBakers bbps)
    _birkNextEpochBakers <- refMake =<< makePersistentEpochBakers (_unhashed (Basic._birkNextEpochBakers bbps))
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

-- |Add a new 'BakerId' to the start of a 'HashedEpcohBlocks'.
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


-- |References to the componenets that make up the block state.
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
    bspBirkParameters :: !PersistentBirkParameters,
    bspCryptographicParameters :: !(HashedBufferedRef CryptographicParameters),
    bspUpdates :: !(BufferedRef Updates),
    bspReleaseSchedule :: !(BufferedRef (Map.Map AccountAddress Timestamp)),
    -- FIXME: Store transaction outcomes in a way that allows for individual indexing.
    bspTransactionOutcomes :: !Transactions.TransactionOutcomes,
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
                put bspTransactionOutcomes
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
        bspTransactionOutcomes <- label "Transaction outcomes" get
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
        insts <- cache bspInstances
        mods <- cache bspModules
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
makePersistent :: (IsProtocolVersion pv, MonadBlobStore m) => Basic.BlockState pv -> m (HashedPersistentBlockState pv)
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

-- |An initial 'HashedPersistentBlockState', which may be used for testing purposes.
initialPersistentState :: (IsProtocolVersion pv, MonadBlobStore m)
             => SeedState
             -> CryptographicParameters
             -> [TransientAccount.Account pv]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> Authorizations
             -> ChainParameters
             -> m (HashedPersistentBlockState pv)
initialPersistentState ss cps accts ips ars auths chainParams = makePersistent $ Basic.initialState ss cps accts ips ars auths chainParams

-- |A mostly empty block state, but with the given birk parameters, 
-- cryptographic parameters, update authorizations and chain parameters.
emptyBlockState :: (MonadBlobStore m) => PersistentBirkParameters -> CryptographicParameters -> Authorizations -> ChainParameters -> m (PersistentBlockState pv)
emptyBlockState bspBirkParameters cryptParams auths chainParams = do
  modules <- refMake Modules.emptyModules
  identityProviders <- refMake IPS.emptyIdentityProviders
  anonymityRevokers <- refMake ARS.emptyAnonymityRevokers
  cryptographicParameters <- refMake cryptParams
  bspUpdates <- refMake =<< initialUpdates auths chainParams
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

doGetModule :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ModuleRef -> m (Maybe Wasm.ModuleInterface)
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
    -> (Wasm.ModuleInterface, Wasm.WasmModule)
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

doGetSlotBakers :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Slot -> m FullBakers
doGetSlotBakers pbs slot = do
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
                                    RemoveBaker remEpoch
                                        | remEpoch < slotEpoch -> Nothing
                                    ReduceStake newAmt redEpoch
                                        | redEpoch < slotEpoch -> Just (FullBakerInfo abi newAmt)
                                    _ -> Just (FullBakerInfo abi (pab ^. stakedAmount))
                            Null -> error "Persistent.getSlotBakers invariant violation: active baker account not a baker"
                        Nothing -> error "Persistent.getSlotBakers invariant violation: active baker account not valid"
                futureBakers <- Vec.fromList . catMaybes <$> mapM resolveBaker activeBids
                return FullBakers {
                    fullBakerInfos = futureBakers,
                    bakerTotalStake = sum (_bakerStake <$> futureBakers)
                }

doGetBakerAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> BakerId -> m (Maybe (PersistentAccount pv))
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
                Just PersistentAccount{_accountBaker = Some acctBkrRef} -> do
                    acctBkr <- refLoad acctBkrRef
                    case _bakerPendingChange acctBkr of
                        RemoveBaker remEpoch
                            -- Removal takes effect next epoch, so exclude it from the list of bakers
                            | remEpoch == newEpoch + 1 -> return (bs0, bkrs0)
                            -- Removal complete, so update the active bakers and account as well
                            | remEpoch <= newEpoch -> do
                                -- Remove the baker from the active bakers
                                curABs <- refLoad (_birkActiveBakers (bspBirkParameters bs0))
                                newAB <- Trie.delete bkr (_activeBakers curABs)
                                abi <- refLoad (_accountBakerInfo acctBkr)
                                newAK <- Trie.delete (_bakerAggregationVerifyKey abi) (_aggregationKeys curABs)
                                newABs <- refMake $ PersistentActiveBakers {
                                        _activeBakers = newAB,
                                        _aggregationKeys = newAK
                                    }
                                -- Remove the baker from the account
                                let updAcc acc = ((),) <$> setPersistentAccountBaker acc Null
                                (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs0)
                                -- The baker is not included for this epoch
                                return (bs0 {
                                        bspBirkParameters = (bspBirkParameters bs0) {_birkActiveBakers = newABs},
                                        bspAccounts = newAccounts
                                    }, bkrs0)
                        ReduceStake newAmt redEpoch
                            -- Reduction takes effect next epoch, so apply it in the generated list
                            | redEpoch == newEpoch + 1 -> return (bs0, (_accountBakerInfo acctBkr, newAmt) : bkrs0)
                            -- Reduction complete, so update the account as well
                            | redEpoch <= newEpoch -> do
                                -- Reduce the baker's stake on the account
                                let newBaker = acctBkr{_stakedAmount = newAmt, _bakerPendingChange = NoChange}
                                let updAcc acc = ((),) <$> setPersistentAccountBaker acc (Some newBaker)
                                (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc aid (bspAccounts bs0)
                                -- The baker is included with the revised stake
                                return (bs0 {bspAccounts = newAccounts}, (_accountBakerInfo acctBkr, newAmt) : bkrs0)
                        _ -> return (bs0, (_accountBakerInfo acctBkr, _stakedAmount acctBkr) : bkrs0)
                _ -> error "Persistent.bsoTransitionEpochBakers invariant violation: active baker account not a valid baker"
        -- Get the baker info. The list of baker ids is reversed in the input so the accumulated list
        -- is in ascending order.
        (bsp', bkrs) <- foldM accumBakers (bsp, []) (reverse curActiveBIDs)
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
        newNextBakers <- refMake PersistentEpochBakers{..}
        storePBS pbs bsp'{bspBirkParameters = (bspBirkParameters bsp') {
            _birkCurrentEpochBakers = _birkNextEpochBakers oldBPs,
            _birkNextEpochBakers = newNextBakers
          }
        }
    where
        secondIfEqual a b = do
            h1 <- getHashM a
            h2 <- getHashM b
            return $ if (h1 :: H.Hash) == h2 then b else a

doAddBaker :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> BakerAdd -> m (BakerAddResult, PersistentBlockState pv)
doAddBaker pbs aaddr BakerAdd{..} = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex aaddr (bspAccounts bsp) >>= \case
            -- Cannot resolve the account
            Nothing -> return (BAInvalidAccount, pbs)
            -- Account is already a baker
            Just (ai, PersistentAccount{_accountBaker = Some _}) -> return (BAAlreadyBaker (BakerId ai), pbs)
            Just (ai, PersistentAccount{}) -> do
                    let bid = BakerId ai
                    pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                    let updAgg Nothing = return (True, Trie.Insert ())
                        updAgg (Just ()) = return (False, Trie.NoChange)
                    Trie.adjust updAgg (bkuAggregationKey baKeys) (_aggregationKeys pab) >>= \case
                        -- Aggregation key is a duplicate
                        (False, _) -> return (BADuplicateAggregationKey, pbs)
                        (True, newAggregationKeys) -> do
                            newActiveBakers <- Trie.insert bid () (_activeBakers pab)
                            newpabref <- refMake PersistentActiveBakers{
                                    _aggregationKeys = newAggregationKeys,
                                    _activeBakers = newActiveBakers
                                }
                            let newBirkParams = bspBirkParameters bsp & birkActiveBakers .~ newpabref
                            let updAcc acc = do
                                    newBakerInfo <- refMake (bakerKeyUpdateToInfo bid baKeys)
                                    acc' <- setPersistentAccountBaker acc (Some PersistentAccountBaker{
                                        _stakedAmount = baStake,
                                        _stakeEarnings = baStakeEarnings,
                                        _accountBakerInfo = newBakerInfo,
                                        _bakerPendingChange = NoChange
                                    })
                                    return ((), acc')
                            -- This cannot fail to update the account, since we already looked up the account.
                            (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                            (BASuccess bid,) <$> storePBS pbs bsp{
                                bspBirkParameters = newBirkParams,
                                bspAccounts = newAccounts
                            }

doUpdateBakerKeys ::(IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> BakerKeyUpdate -> m (BakerKeyUpdateResult, PersistentBlockState pv)
doUpdateBakerKeys pbs aaddr bku@BakerKeyUpdate{..} = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex aaddr (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just (ai, PersistentAccount{_accountBaker = Some pAcctBkr}) -> do
                acctBkr <- refLoad pAcctBkr
                pab <- refLoad (_birkActiveBakers (bspBirkParameters bsp))
                bkrInfo <- refLoad (_accountBakerInfo acctBkr)
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
                            newBakerInfo <- refMake (bakerKeyUpdateToInfo (BakerId ai) bku)
                            acc' <- setPersistentAccountBaker acc (Some acctBkr {_accountBakerInfo = newBakerInfo})
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

doUpdateBakerStake :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> Amount -> m (BakerStakeUpdateResult, PersistentBlockState pv)
doUpdateBakerStake pbs aaddr newStake = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex aaddr (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just (ai, PersistentAccount{_accountBaker = Some pAcctBkr}) -> do
                acctBkr <- refLoad pAcctBkr
                if _bakerPendingChange acctBkr /= NoChange
                -- A change is already pending
                then return (BSUChangePending (BakerId ai), pbs)
                -- We can make the change
                else do
                    let curEpoch = epoch $ _birkSeedState (bspBirkParameters bsp)
                    upds <- refLoad (bspUpdates bsp)
                    cooldown <- (2+) . _cpBakerExtraCooldownEpochs . unStoreSerialized <$> refLoad (currentParameters upds)
                    let (res, updateStake) = case compare newStake (_stakedAmount acctBkr) of
                                LT -> (BSUStakeReduced (BakerId ai) (curEpoch + cooldown), bakerPendingChange .~ ReduceStake newStake (curEpoch + cooldown))
                                EQ -> (BSUStakeUnchanged (BakerId ai), id)
                                GT -> (BSUStakeIncreased (BakerId ai), stakedAmount .~ newStake)
                        updAcc acc = do
                                acc' <- setPersistentAccountBaker acc (Some (updateStake acctBkr))
                                return ((), acc')
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (res,) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            _ -> return (BSUInvalidBaker, pbs)

doUpdateBakerRestakeEarnings :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> Bool -> m (BakerRestakeEarningsUpdateResult, PersistentBlockState pv)
doUpdateBakerRestakeEarnings pbs aaddr newRestakeEarnings = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex aaddr (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just (ai, PersistentAccount{_accountBaker = Some pAcctBkr}) -> do
                acctBkr <- refLoad pAcctBkr
                if newRestakeEarnings == acctBkr ^. stakeEarnings
                then return (BREUUpdated (BakerId ai), pbs)
                else do
                    let updAcc acc = ((), ) <$> setPersistentAccountBaker acc (Some (acctBkr & stakeEarnings .~ newRestakeEarnings))
                    (_, newAccounts) <- Accounts.updateAccountsAtIndex updAcc ai (bspAccounts bsp)
                    (BREUUpdated (BakerId ai),) <$> storePBS pbs bsp{bspAccounts = newAccounts}
            _ -> return (BREUInvalidBaker, pbs)


doRemoveBaker :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m (BakerRemoveResult, PersistentBlockState pv)
doRemoveBaker pbs aaddr = do
        bsp <- loadPBS pbs
        Accounts.getAccountWithIndex aaddr (bspAccounts bsp) >>= \case
            -- The account is valid and has a baker
            Just (ai, PersistentAccount{_accountBaker = Some pab}) -> do
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
                    cooldown <- (2+) . _cpBakerExtraCooldownEpochs . unStoreSerialized <$> refLoad (currentParameters upds)
                    let updAcc acc = do
                            acc' <- setPersistentAccountBaker acc (Some ab{_bakerPendingChange = RemoveBaker (curEpoch + cooldown)})
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
            newAccountBaker <- forM (acc ^. accountBaker) $ \pbkr -> do
                bkr <- refLoad pbkr
                if bkr ^. stakeEarnings then
                    refMake $ bkr & stakedAmount +~ reward
                else
                    return pbkr
            acc' <- rehashAccount $ acc & accountBaker .~ newAccountBaker & accountAmount +~ reward
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

doGetFoundationAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (PersistentAccount pv)
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

doGetAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m (Maybe (PersistentAccount pv))
doGetAccount pbs addr = do
        bsp <- loadPBS pbs
        Accounts.getAccount addr (bspAccounts bsp)

doGetAccountIndex :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> m (Maybe AccountIndex)
doGetAccountIndex pbs addr = do
        bsp <- loadPBS pbs
        Accounts.getAccountIndex addr (bspAccounts bsp)

doAccountList :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Accounts.accountAddresses (bspAccounts bsp)

doRegIdExists :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        fst <$> Accounts.regIdExists regid (bspAccounts bsp)

doCreateAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> ID.GlobalContext -> AccountAddress -> ID.AccountCredential ->  m (Maybe (PersistentAccount pv), PersistentBlockState pv)
doCreateAccount pbs cryptoParams acctAddr credential = do
        acct <- newAccount cryptoParams acctAddr credential
        bsp <- loadPBS pbs
        -- Add the account
        (res, accts1) <- Accounts.putNewAccount acct (bspAccounts bsp)
        if res then do
            -- Record the RegId
            accts2 <- Accounts.recordRegId (ID.credId credential) accts1
            (Just acct,) <$> storePBS pbs (bsp {bspAccounts = accts2})
        else
            return (Nothing, pbs)

doModifyAccount :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountUpdate -> m (PersistentBlockState pv)
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadPBS pbs
        -- Do the update to the account
        (_, accts1) <- Accounts.updateAccounts upd _auAddress (bspAccounts bsp)
        storePBS pbs (bsp {bspAccounts = accts1})
    where
        upd oldAccount = ((), ) <$> Accounts.updateAccount aUpd oldAccount

doSetAccountCredentialKeys :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m (PersistentBlockState pv)
doSetAccountCredentialKeys pbs accAddress credIx credKeys = do
        bsp <- loadPBS pbs
        (_, accts1) <- Accounts.updateAccounts upd accAddress (bspAccounts bsp)
        storePBS pbs (bsp {bspAccounts = accts1})
    where
        upd oldAccount = ((), ) <$> setPAD (updateCredentialKeys credIx credKeys) oldAccount

doUpdateAccountCredentials :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> AccountAddress -> [ID.CredentialIndex] -> Map.Map ID.CredentialIndex ID.AccountCredential -> ID.AccountThreshold -> m (PersistentBlockState pv)
doUpdateAccountCredentials pbs accAddress remove add thrsh = do
        bsp <- loadPBS pbs
        (_, accts1) <- Accounts.updateAccounts upd accAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- Accounts.recordRegIds (Map.elems $ ID.credId <$> add) accts1
        storePBS pbs (bsp {bspAccounts = accts2})
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
                                            pinstanceContractModule = instanceContractModule,
                                            pinstanceReceiveFuns = instanceReceiveFuns,
                                            pinstanceInitName = instanceInitName,
                                            pinstanceParameterHash = instanceParameterHash
                                        }
            -- This in an irrefutable pattern because otherwise it would have failed in previous stages
            -- as it would be trying to create an instance of a module that doesn't exist.
            ~(Just modRef) <- Modules.getModuleReference (Wasm.miModuleRef instanceModuleInterface) mods
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

doGetUpdates :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m Basic.Updates
doGetUpdates = makeBasicUpdates <=< refLoad . bspUpdates <=< loadPBS

doGetProtocolUpdateStatus :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m (Either ProtocolUpdate [(TransactionTime, ProtocolUpdate)])
doGetProtocolUpdateStatus = protocolUpdateStatus . bspUpdates <=< loadPBS

doProcessUpdateQueues :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> Timestamp -> m (Map.Map TransactionTime UpdateValue, PersistentBlockState pv)
doProcessUpdateQueues pbs ts = do
        bsp <- loadPBS pbs
        (changes, u') <- processUpdateQueues ts (bspUpdates bsp)
        (changes,) <$> storePBS pbs bsp{bspUpdates = u'}

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
                return (ba', case join toRead of
                               Just t -> (addr, t) : readded
                               Nothing -> readded)
          (bspAccounts', accsToReadd) <- foldlM f (bspAccounts bsp, []) (Map.keys accountsToRemove)
          bspReleaseSchedule' <- makeBufferedRef $ foldl' (\b (a, t) -> Map.insert a t b) blockReleaseSchedule' accsToReadd
          storePBS pbs (bsp {bspAccounts = bspAccounts', bspReleaseSchedule = bspReleaseSchedule'})

doGetCurrentAuthorizations :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m Authorizations
doGetCurrentAuthorizations pbs = do
        bsp <- loadPBS pbs
        u <- refLoad (bspUpdates bsp)
        unStoreSerialized <$> refLoad (currentAuthorizations u)

doEnqueueUpdate :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> TransactionTime -> UpdateValue -> m (PersistentBlockState pv)
doEnqueueUpdate pbs effectiveTime payload = do
        bsp <- loadPBS pbs
        u' <- enqueueUpdate effectiveTime payload (bspUpdates bsp)
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

doGetChainParameters :: (IsProtocolVersion pv, MonadBlobStore m) => PersistentBlockState pv -> m ChainParameters
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

instance BlockStateTypes (PersistentBlockStateMonad pv r m) where
    type BlockState (PersistentBlockStateMonad pv r m) = HashedPersistentBlockState pv
    type UpdatableBlockState (PersistentBlockStateMonad pv r m) = PersistentBlockState pv
    type Account (PersistentBlockStateMonad pv r m) = PersistentAccount pv

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateQuery (PersistentBlockStateMonad pv r m) where
    getModule = doGetModuleSource . hpbsPointers
    getAccount = doGetAccount . hpbsPointers
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

  getAccountAddress acc = acc ^^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  getAccountCredentials acc = acc ^^. accountCredentials

  getAccountMaxCredentialValidTo acc = acc ^^. accountMaxCredentialValidTo

  getAccountVerificationKeys acc = acc ^^. accountVerificationKeys

  getAccountEncryptedAmount acc = loadPersistentAccountEncryptedAmount =<< loadBufferedRef (acc ^. accountEncryptedAmount)

  getAccountEncryptionKey acc = acc ^^. accountEncryptionKey

  getAccountReleaseSchedule acc = loadPersistentAccountReleaseSchedule =<< loadBufferedRef (acc ^. accountReleaseSchedule)

  getAccountBaker acc = case acc ^. accountBaker of
        Null -> return Nothing
        Some bref -> do
            PersistentAccountBaker{..} <- refLoad bref
            abi <- refLoad _accountBakerInfo
            return $ Just AccountBaker{_accountBakerInfo = abi, ..}

instance (IsProtocolVersion pv, PersistentState r m) => BlockStateOperations (PersistentBlockStateMonad pv r m) where
    bsoGetModule pbs mref = doGetModule pbs mref
    bsoGetAccount = doGetAccount
    bsoGetAccountIndex = doGetAccountIndex
    bsoGetInstance = doGetInstance
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
    bsoGetCurrentAuthorizations = doGetCurrentAuthorizations
    bsoGetNextUpdateSequenceNumber = doGetNextUpdateSequenceNumber
    bsoEnqueueUpdate = doEnqueueUpdate
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

    purgeBlockState pbs = liftIO $ writeIORef (hpbsPointers pbs) (error "Block state purged")

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
        return $ runPutLazy p

    writeBlockState h hpbs =
        runPutH (putBlockStateV0 (hpbsPointers hpbs)) h