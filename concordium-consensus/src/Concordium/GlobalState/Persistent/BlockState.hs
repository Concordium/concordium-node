{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Persistent.BlockState where

import Data.Serialize
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Lens.Micro.Platform
import Concordium.Utils
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Map.Strict as Map hiding (null)

import GHC.Generics (Generic)

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as BB
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Types
import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.AnonymityRevokers as ARS
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.Persistent.Accounts as Accounts
import Concordium.GlobalState.Persistent.Bakers
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.Execution as Transactions
import Concordium.GlobalState.Persistent.Instances(PersistentInstance(..), PersistentInstanceParameters(..), CacheableInstanceParameters(..))
import Concordium.GlobalState.Instance (Instance(..),InstanceParameters(..),makeInstanceHash')
import Concordium.GlobalState.Persistent.Account
import qualified Concordium.GlobalState.Basic.BlockState.Account as TransientAccount
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Modules as TransientMods
import Concordium.GlobalState.SeedState
import Concordium.Logger (MonadLogger)
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Persistent.LFMBTree as L

type PersistentBlockState = IORef (BufferedRef BlockStatePointers)

data BlockStatePointers = BlockStatePointers {
    bspAccounts :: !Accounts.Accounts,
    bspInstances :: !Instances.Instances,
    bspModules :: !(HashedBufferedRef Modules),
    bspBank :: !(Hashed Rewards.BankStatus),
    bspIdentityProviders :: !(HashedBufferedRef IPS.IdentityProviders),
    bspAnonymityRevokers :: !(HashedBufferedRef ARS.AnonymityRevokers),
    bspBirkParameters :: !PersistentBirkParameters,
    bspCryptographicParameters :: !(HashedBufferedRef CryptographicParameters),
    -- FIXME: Store transaction outcomes in a way that allows for individual indexing.
    bspTransactionOutcomes :: !Transactions.TransactionOutcomes,
    bspHashes :: !(Maybe Basic.BlockStateHashes)
}

makeBlockHashesM :: (MonadIO m, MonadReader r m, HasBlobStore r) => BlockStatePointers -> m Basic.BlockStateHashes
makeBlockHashesM BlockStatePointers {..} = do
  birkHash <- getHashM bspBirkParameters
  cryptoHash <- getHashM bspCryptographicParameters
  ipsHash <- getHashM bspIdentityProviders
  arsHash <- getHashM bspAnonymityRevokers
  modulesHash <- getHashM bspModules
  accountsHash <- getHashM bspAccounts
  instancesHash <- getHashM bspInstances
  let hashOfBirkParamsAndCryptoParams = H.hashOfHashes birkHash cryptoHash
      hashOfIPsAndARs = H.hashOfHashes ipsHash arsHash
      hashOfModulesAndBank = H.hashOfHashes modulesHash (getHash bspBank)
      hashOfAccountsAndInstances = H.hashOfHashes accountsHash instancesHash
      hashOfBirkCryptoIPsARs = H.hashOfHashes hashOfBirkParamsAndCryptoParams hashOfIPsAndARs
      hashOfModulesBankAccountsIntances = H.hashOfHashes hashOfModulesAndBank hashOfAccountsAndInstances
      blockStateHash = H.hashOfHashes hashOfBirkCryptoIPsARs hashOfModulesBankAccountsIntances
  return Basic.BlockStateHashes {..}

instance (MonadIO m, MonadReader r m, HasBlobStore r) => MHashableTo m H.Hash BlockStatePointers where
  getHashM bps = maybe (fmap Basic.blockStateHash (makeBlockHashesM bps)) (return . Basic.blockStateHash) (bspHashes bps)

instance (MonadIO m, MonadReader r m, HasBlobStore r, BlobStorable r m (Nullable (BlobRef Accounts.RegIdHistory))) => BlobStorable r m BlockStatePointers where
    storeUpdate bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate bspAccounts
        (pinsts, bspInstances') <- storeUpdate bspInstances
        (pmods, bspModules') <- storeUpdate bspModules
        (pips, bspIdentityProviders') <- storeUpdate bspIdentityProviders
        (pars, bspAnonymityRevokers') <- storeUpdate bspAnonymityRevokers
        (pbps, bspBirkParameters') <- storeUpdate bspBirkParameters
        (pcryptps, bspCryptographicParameters') <- storeUpdate bspCryptographicParameters
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
        return (putBSP, bsp0 {
                    bspAccounts = bspAccounts',
                    bspInstances = bspInstances',
                    bspModules = bspModules',
                    bspIdentityProviders = bspIdentityProviders',
                    bspAnonymityRevokers = bspAnonymityRevokers',
                    bspBirkParameters = bspBirkParameters',
                    bspCryptographicParameters = bspCryptographicParameters'
                })
    store bsp = fst <$> storeUpdate bsp
    load = do
        maccts <- label "Accounts" $ load
        minsts <- label "Instances" $ load
        mmods <- label "Modules" $ load
        bspBank <- makeHashed <$> label "Bank" get
        mpips <- label "Identity providers" $ load
        mars <- label "Anonymity revokers" $ load
        mbps <- label "Birk parameters" $ load
        mcryptps <- label "Cryptographic parameters" $ load
        bspTransactionOutcomes <- label "Transaction outcomes" $ get
        return $! do
            bspAccounts <- maccts
            bspInstances <- minsts
            bspModules <- mmods
            bspIdentityProviders <- mpips
            bspAnonymityRevokers <- mars
            bspBirkParameters <- mbps
            bspCryptographicParameters <- mcryptps
            let bspHashes = Nothing
            return $! BlockStatePointers{..}

data PersistentModule = PersistentModule {
    pmInterface :: !Wasm.ModuleInterface,
    pmIndex :: !ModuleIndex
}

persistentModuleToModule :: PersistentModule -> Module
persistentModuleToModule PersistentModule{..} = Module {
    moduleInterface = pmInterface,
    moduleIndex = pmIndex
}

instance Serialize PersistentModule where
    put PersistentModule{..} = put pmInterface <> put pmIndex
    get = PersistentModule <$> get <*> get

instance (MonadIO m, MonadReader r m, HasBlobStore r) => BlobStorable r m PersistentModule

data Modules = Modules {
    modules :: Trie.TrieN (BufferedBlobbed BlobRef) ModuleRef PersistentModule,
    nextModuleIndex :: !ModuleIndex,
    runningHash :: !H.Hash
}

emptyModules :: Modules
emptyModules = Modules {
        modules = Trie.empty,
        nextModuleIndex = 0,
        runningHash = H.hash ""
    }

instance (HasBlobStore r, MonadReader r m, MonadIO m) => BlobStorable r m Modules where
    storeUpdate ms@Modules{..} = do
        (pm, modules') <- storeUpdate modules
        return (pm >> put nextModuleIndex >> put runningHash, ms {modules = modules'})
    store m = fst <$> storeUpdate m
    load = do
        mmodules <- load
        nextModuleIndex <- get
        runningHash <- get
        return $ do
            modules <- mmodules
            return Modules{..}

instance HashableTo H.Hash Modules where
  getHash Modules {..} = runningHash

instance Monad m => MHashableTo m H.Hash Modules

makePersistentModules :: MonadIO m => TransientMods.Modules -> m Modules
makePersistentModules TransientMods.Modules{..} = do
    m' <- Trie.fromList $ upd <$> HM.toList _modules
    return $ Modules m' _nextModuleIndex _runningHash
    where
        upd (mref, Module{..}) = (mref, PersistentModule{
                pmInterface = moduleInterface,
                pmIndex = moduleIndex
            })

data PersistentBirkParameters = PersistentBirkParameters {
    _birkElectionDifficulty :: ElectionDifficulty,
    -- |The current stake of bakers. All updates should be to this state.
    _birkCurrentBakers :: !PersistentBakers,
    -- |The state of bakers at the end of the previous epoch,
    -- will be used as lottery bakers in next epoch.
    _birkPrevEpochBakers :: !(BufferedRef PersistentBakers),
    -- |The state of the bakers fixed before previous epoch,
    -- the lottery power and reward account is used in leader election.
    _birkLotteryBakers :: !(BufferedRef PersistentBakers),
    _birkSeedState :: !SeedState
} deriving (Generic, Show)

makeLenses ''PersistentBirkParameters

instance (MonadIO m, MonadReader r m, HasBlobStore r) => MHashableTo m H.Hash PersistentBirkParameters where
  getHashM PersistentBirkParameters {..} = do
    currentHash <- getHashM _birkCurrentBakers
    prevHash <- getHashM _birkPrevEpochBakers
    lotteryHash <- getHashM _birkLotteryBakers
    let bpH0 = H.hash $ "ElectDiff" <> encode _birkElectionDifficulty <> "SeedState" <> encode _birkSeedState
        bpH1 = H.hashOfHashes prevHash lotteryHash
        bpH2 = H.hashOfHashes currentHash bpH1
    return $ H.hashOfHashes bpH0 bpH2

instance (MonadIO m, MonadReader r m, HasBlobStore r) => BlobStorable r m PersistentBirkParameters where
    storeUpdate bps@PersistentBirkParameters{..} = do
        (ppebs, prevEpochBakers) <- storeUpdate _birkPrevEpochBakers
        (plbs, lotteryBakers) <- storeUpdate _birkLotteryBakers
        (pcbs, currBakers) <- storeUpdate _birkCurrentBakers
        let putBSP = do
                put _birkElectionDifficulty
                pcbs
                ppebs
                plbs
                put _birkSeedState
        return (putBSP, bps {
                    _birkCurrentBakers = currBakers,
                    _birkPrevEpochBakers = prevEpochBakers,
                    _birkLotteryBakers = lotteryBakers
                })
    store bps = fst <$> storeUpdate bps
    load = do
        _birkElectionDifficulty <- label "Election difficulty" get
        mcbs <- label "Current bakers" $ load
        mpebs <- label "Previous-epoch bakers" $ load
        mlbs <- label "Lottery bakers" $ load
        _birkSeedState <- label "Seed state" get
        return $! do
            _birkCurrentBakers <- mcbs
            _birkPrevEpochBakers <- mpebs
            _birkLotteryBakers <- mlbs
            return PersistentBirkParameters{..}

makePersistentBirkParameters :: (MonadIO m, MonadReader r m, HasBlobStore r) => Basic.BasicBirkParameters -> m PersistentBirkParameters
makePersistentBirkParameters Basic.BasicBirkParameters{..} = do
    prevEpochBakers <- refMake =<< makePersistentBakers ( _unhashed _birkPrevEpochBakers)
    lotteryBakers <- refMake =<< makePersistentBakers (_unhashed _birkLotteryBakers)
    currBakers <- makePersistentBakers _birkCurrentBakers
    return $ PersistentBirkParameters
        _birkElectionDifficulty
        currBakers
        prevEpochBakers
        lotteryBakers
        _birkSeedState

makePersistent :: (MonadIO m, MonadReader r m, HasBlobStore r)  => Basic.BlockState -> m PersistentBlockState
makePersistent Basic.BlockState{..} = do
  persistentBlockInstances <- Instances.makePersistent _blockInstances
  persistentBirkParameters <- makePersistentBirkParameters _blockBirkParameters
  persistentMods <- makePersistentModules _blockModules
  modules <- refMake persistentMods
  identityProviders <- bufferHashed _blockIdentityProviders
  anonymityRevokers <- bufferHashed _blockAnonymityRevokers
  cryptographicParameters <- bufferHashed _blockCryptographicParameters
  blockAccounts <- Accounts.makePersistent _blockAccounts
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
          bspHashes = Just _blockHashes
        }
  liftIO $ newIORef $! bsp

initialPersistentState :: (MonadIO m, MonadReader r m, HasBlobStore r) =>Basic.BasicBirkParameters
             -> CryptographicParameters
             -> [TransientAccount.Account]
             -> IPS.IdentityProviders
             -> ARS.AnonymityRevokers
             -> Amount
             -> m PersistentBlockState
initialPersistentState bps cps accts ips ars amt = makePersistent $ Basic.initialState bps cps accts ips ars amt

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: (MonadIO m, MonadReader r m, HasBlobStore r) => PersistentBirkParameters -> CryptographicParameters -> m PersistentBlockState
emptyBlockState bspBirkParameters cryptParams = do
  modules <- refMake emptyModules
  identityProviders <- refMake IPS.emptyIdentityProviders
  anonymityRevokers <- refMake ARS.emptyAnonymityRevokers
  cryptographicParameters <- refMake cryptParams
  -- calculate hashes
  birkHash <- getHashM bspBirkParameters
  cryptoHash <- getHashM cryptographicParameters
  ipsHash <- getHashM identityProviders
  arsHash <- getHashM anonymityRevokers
  modulesHash <- getHashM modules
  accountsHash <- getHashM Accounts.emptyAccounts
  instancesHash <- getHashM Instances.emptyInstances
  let hashOfBirkParamsAndCryptoParams = H.hashOfHashes birkHash cryptoHash
      hashOfIPsAndARs = H.hashOfHashes ipsHash arsHash
      hashOfModulesAndBank = H.hashOfHashes modulesHash (getHash Rewards.emptyBankStatus)
      hashOfAccountsAndInstances = H.hashOfHashes accountsHash instancesHash
      hashOfBirkCryptoIPsARs = H.hashOfHashes hashOfBirkParamsAndCryptoParams hashOfIPsAndARs
      hashOfModulesBankAccountsIntances = H.hashOfHashes hashOfModulesAndBank hashOfAccountsAndInstances
      blockStateHash = H.hashOfHashes hashOfBirkCryptoIPsARs hashOfModulesBankAccountsIntances
  let bspHashes = Just Basic.BlockStateHashes {..}

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

fromPersistentInstance ::  MonadPersistentBlockState r m =>
    PersistentBlockState -> Instances.PersistentInstance -> m Instance
fromPersistentInstance _ Instances.PersistentInstance{pinstanceCachedParameters = (Some CacheableInstanceParameters{..}), ..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    let instanceParameters = InstanceParameters {
            instanceAddress = pinstanceAddress,
            instanceOwner = pinstanceOwner,
            instanceContractModule = pinstanceContractModule,
            instanceInitName = pinstanceInitName,
            instanceReceiveFuns = pinstanceReceiveFuns,
            instanceModuleInterface = pinstanceModuleInterface,
            instanceParameterHash = pinstanceParameterHash
        }
    return Instance{ instanceModel = pinstanceModel,
            instanceAmount = pinstanceAmount,
            instanceHash = pinstanceHash,
            ..
         }
fromPersistentInstance pbs Instances.PersistentInstance{..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    doGetModule pbs pinstanceContractModule >>= \case
        Nothing -> error "fromPersistentInstance: unresolvable module" -- TODO: Possibly don't error here
        Just m -> do
            let instanceParameters = InstanceParameters {
                    instanceAddress = pinstanceAddress,
                    instanceOwner = pinstanceOwner,
                    instanceContractModule = pinstanceContractModule,
                    instanceInitName = pinstanceInitName,
                    instanceReceiveFuns = pinstanceReceiveFuns,
                    instanceModuleInterface = moduleInterface m,
                    instanceParameterHash = pinstanceParameterHash
                }
            return Instance{
                    instanceModel = pinstanceModel,
                    instanceAmount = pinstanceAmount,
                    instanceHash = pinstanceHash,
                    ..
                }

type MonadPersistentBlockState r m =
  ( MHashableTo m H.Hash IPS.IdentityProviders,
    MHashableTo m H.Hash ARS.AnonymityRevokers,
    MHashableTo m H.Hash Modules,
    MHashableTo m H.Hash PersistentAccount,
    MHashableTo m H.Hash CryptographicParameters,
    MonadIO m,
    MonadReader r m,
    HasBlobStore r,
    BlobStorable r m (Nullable (BufferedRef BakerInfo, Amount)),
    MHashableTo (PersistentBlockStateMonad r m) H.Hash PersistentBakers
  )

loadPBS :: MonadPersistentBlockState r m => PersistentBlockState -> m BlockStatePointers
loadPBS = loadBufferedRef <=< liftIO . readIORef

storePBS :: MonadPersistentBlockState r m => PersistentBlockState -> BlockStatePointers -> m PersistentBlockState
storePBS pbs bsp = liftIO $ do
    pbsp <- makeBufferedRef bsp
    writeIORef pbs pbsp
    return pbs

doGetModule :: MonadPersistentBlockState r m => PersistentBlockState -> ModuleRef -> m (Maybe Module)
doGetModule s modRef = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    fmap persistentModuleToModule <$> Trie.lookup modRef (modules mods)

doGetModuleList :: MonadPersistentBlockState r m => PersistentBlockState -> m [ModuleRef]
doGetModuleList s = do
    bsp <- loadPBS s
    mods <- refLoad (bspModules bsp)
    Trie.keys (modules mods)

doPutNewModule :: MonadPersistentBlockState r m =>PersistentBlockState
    -> Wasm.ModuleInterface
    -> m (Bool, PersistentBlockState)
doPutNewModule pbs pmInterface = do
        let mref = Wasm.miModuleRef pmInterface
        bsp <- loadPBS pbs
        mods <- refLoad (bspModules bsp)
        let
            newMod = PersistentModule{pmIndex = nextModuleIndex mods, ..}
            tryIns Nothing = return (True, Trie.Insert newMod)
            tryIns (Just _) = return (False, Trie.NoChange)
        (b, modules') <- Trie.adjust tryIns mref (modules mods)
        if b then do
            let
                newMods = mods {modules = modules', nextModuleIndex = nextModuleIndex mods + 1}
            modules <- refMake newMods
            (True,) <$> storePBS pbs (bsp {bspModules = modules})
        else
            return (False, pbs)

doGetBlockBirkParameters :: MonadPersistentBlockState r m => PersistentBlockState -> m PersistentBirkParameters
doGetBlockBirkParameters pbs = bspBirkParameters <$> loadPBS pbs

doAddBaker :: MonadPersistentBlockState r m => PersistentBlockState -> BakerInfo -> m (Either BakerError BakerId, PersistentBlockState)
doAddBaker pbs binfo = do
        bsp <- loadPBS pbs
        createBaker binfo (bspBirkParameters bsp ^. birkCurrentBakers) >>= \case
            Left err -> return (Left err, pbs)
            Right (bid, newBakers) -> (Right bid,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doUpdateBaker :: MonadPersistentBlockState r m => PersistentBlockState -> BB.BakerUpdate -> PersistentBlockStateMonad r m (Bool, PersistentBlockState)
doUpdateBaker pbs bupdate = do
        bsp <- loadPBS pbs
        PersistentBlockStateMonad (updateBaker bupdate (bspBirkParameters bsp ^. birkCurrentBakers)) >>= \case
            Nothing -> return (False, pbs)
            Just newBakers ->
              (True,) <$!> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doRemoveBaker :: MonadPersistentBlockState r m => PersistentBlockState -> BakerId -> PersistentBlockStateMonad r m (Bool, PersistentBlockState)
doRemoveBaker pbs bid = do
        bsp <- loadPBS pbs
        (rv, newBakers) <- PersistentBlockStateMonad $ removeBaker bid (bspBirkParameters bsp ^. birkCurrentBakers)
        (rv,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doGetRewardStatus :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m Rewards.BankStatus
doGetRewardStatus pbs = _unhashed . bspBank <$> loadPBS pbs

doSetInflation :: MonadPersistentBlockState r m => PersistentBlockState -> Amount -> PersistentBlockStateMonad r m PersistentBlockState
doSetInflation pbs amount = do
        bsp <- loadPBS pbs
        storePBS pbs (bsp {bspBank = bspBank bsp & unhashed . Rewards.mintedGTUPerSlot .~ amount})

doMint :: MonadPersistentBlockState r m => PersistentBlockState -> Amount -> PersistentBlockStateMonad r m (Amount, PersistentBlockState)
doMint pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & (unhashed . Rewards.totalGTU +~ amount) . (unhashed . Rewards.centralBankGTU +~ amount)
        (newBank ^. unhashed . Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doDecrementCentralBankGTU :: MonadPersistentBlockState r m => PersistentBlockState -> Amount -> PersistentBlockStateMonad r m (Amount, PersistentBlockState)
doDecrementCentralBankGTU pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & unhashed . Rewards.centralBankGTU -~ amount
        (newBank ^. unhashed . Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doGetAccount :: MonadPersistentBlockState r m => PersistentBlockState -> AccountAddress -> m (Maybe PersistentAccount)
doGetAccount pbs addr = do
        bsp <- loadPBS pbs
        Accounts.getAccount addr (bspAccounts bsp)

doAccountList :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Accounts.accountAddresses (bspAccounts bsp)

doRegIdExists :: MonadPersistentBlockState r m => PersistentBlockState -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        fst <$> Accounts.regIdExists regid (bspAccounts bsp)

doPutNewAccount :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentAccount -> m (Bool, PersistentBlockState)
doPutNewAccount pbs acct = do
        bsp <- loadPBS pbs
        -- Add the account
        (res, accts1) <- Accounts.putNewAccount acct (bspAccounts bsp)
        if res then (True,) <$> do
            PersistingAccountData{..} <- acct ^^. id
            -- Record the RegIds of any credentials
            accts2 <- foldM (flip Accounts.recordRegId) accts1 (ID.cdvRegId <$> _accountCredentials)
            -- Update the delegation if necessary
            case _accountStakeDelegate of
                Nothing -> storePBS pbs (bsp {bspAccounts = accts2})
                target@(Just _) -> assert (null _accountInstances) $ do
                    newCurrBakers <- addStake target (acct ^. accountAmount) (bspBirkParameters bsp ^. birkCurrentBakers)
                    storePBS pbs (bsp {
                            bspAccounts = accts2,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                        })
        else
            return (False, pbs)

doModifyAccount :: MonadPersistentBlockState r m => PersistentBlockState -> AccountUpdate -> m PersistentBlockState
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadPBS pbs
        -- Do the update to the account
        (mbalinfo, accts1) <- Accounts.updateAccounts upd _auAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- case _auCredential of
            Just cdi -> Accounts.recordRegId (ID.cdvRegId cdi) accts1
            Nothing -> return accts1
        -- If the amount is changed update the delegate stake
        birkParams1 <- case (_auAmount, mbalinfo) of
                (Just deltaAmnt, Just delegate) -> do
                    newCurrBakers <- modifyStake delegate deltaAmnt (bspBirkParameters bsp ^. birkCurrentBakers)
                    return $ bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                _ -> return $ bspBirkParameters bsp
        storePBS pbs (bsp {bspAccounts = accts2, bspBirkParameters = birkParams1})
    where
        upd oldAccount = do
          delegate <- oldAccount ^^. accountStakeDelegate
          newAcc <- Accounts.updateAccount aUpd oldAccount
          return (delegate, newAcc)

doGetInstance :: MonadPersistentBlockState r m => PersistentBlockState -> ContractAddress -> m (Maybe Instance)
doGetInstance pbs caddr = do
        bsp <- loadPBS pbs
        minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
        forM minst $ fromPersistentInstance pbs

doContractInstanceList :: MonadPersistentBlockState r m => PersistentBlockState -> m [Instance]
doContractInstanceList pbs = do
        bsp <- loadPBS pbs
        insts <- Instances.allInstances (bspInstances bsp)
        mapM (fromPersistentInstance pbs) insts

doPutNewInstance :: MonadPersistentBlockState r m => PersistentBlockState -> (ContractAddress -> Instance) -> PersistentBlockStateMonad r m (ContractAddress, PersistentBlockState)
doPutNewInstance pbs fnew = do
        bsp <- loadPBS pbs
        -- Create the instance
        (inst, insts) <- Instances.newContractInstance fnew' (bspInstances bsp)
        let ca = instanceAddress (instanceParameters inst)
        -- Update the owner account's set of instances
        let updAcct oldAccount = do
              delegate <- oldAccount ^^. accountStakeDelegate
              newAccount <- oldAccount & accountInstances %~~ Set.insert ca
              return (delegate, newAccount)
        (mdelegate, accts) <- Accounts.updateAccounts updAcct (instanceOwner (instanceParameters inst)) (bspAccounts bsp)
        -- Update the stake delegate
        case mdelegate of
            Nothing -> error "Invalid contract owner"
            Just delegate -> do
                newCurrBakers <- modifyStake delegate (amountToDelta (instanceAmount inst)) (bspBirkParameters bsp ^. birkCurrentBakers)
                (ca,) <$> storePBS pbs bsp{
                                    bspInstances = insts,
                                    bspAccounts = accts,
                                    bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                                }
    where
        fnew' ca = let inst@Instance{instanceParameters = InstanceParameters{..}, ..} = fnew ca in do
            params <- makeBufferedRef $ PersistentInstanceParameters {
                                            pinstanceAddress = instanceAddress,
                                            pinstanceOwner = instanceOwner,
                                            pinstanceContractModule = instanceContractModule,
                                            pinstanceReceiveFuns = instanceReceiveFuns,
                                            pinstanceInitName = instanceInitName,
                                            pinstanceParameterHash = instanceParameterHash
                                        }
            return (inst, PersistentInstance{
                pinstanceParameters = params,
                pinstanceCachedParameters = Some (CacheableInstanceParameters{
                        pinstanceModuleInterface = instanceModuleInterface
                    }),
                pinstanceModel = instanceModel,
                pinstanceAmount = instanceAmount,
                pinstanceHash = instanceHash
            })

doModifyInstance :: MonadPersistentBlockState r m => PersistentBlockState -> ContractAddress -> AmountDelta -> Wasm.ContractState -> m PersistentBlockState
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadPBS pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (Nothing, insts) -> -- no change to staking
                storePBS pbs bsp{bspInstances = insts}
            Just (Just owner, insts) ->
                -- Lookup the owner account and update its stake delegate
                Accounts.getAccount owner (bspAccounts bsp) >>= \case
                    Nothing -> error "Invalid contract owner"
                    Just acct -> do
                        delegate <- acct ^^. accountStakeDelegate
                        newCurrBakers <- modifyStake delegate deltaAmnt (bspBirkParameters bsp ^. birkCurrentBakers)
                        storePBS pbs bsp{
                            bspInstances = insts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                        }
    where
        upd oldInst = do
            (piParams, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0 then
                return (Nothing, rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceModel = val})
            else do
                acct <- pinstanceOwner <$> loadBufferedRef (pinstanceParameters oldInst)
                return (Just acct, rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = val})
        rehash iph inst@(PersistentInstance {..}) = inst {pinstanceHash = makeInstanceHash' iph pinstanceModel pinstanceAmount}

doDelegateStake :: MonadPersistentBlockState r m => PersistentBlockState -> AccountAddress -> Maybe BakerId -> m (Bool, PersistentBlockState)
doDelegateStake pbs aaddr target = do
        bsp <- loadPBS pbs
        targetValid <- case target of
                Nothing -> return True
                Just bid -> do
                    mbInfo <- L.lookup bid $ bspBirkParameters bsp ^. birkCurrentBakers . bakerMap
                    case mbInfo of
                      Just (Some _) -> return True
                      _ -> return False
        if targetValid then do
            let updAcc acct = do
                  delegate <- acct ^^. accountStakeDelegate
                  newAccount <- acct & accountStakeDelegate .~~ target
                  instances <- acct ^^. accountInstances
                  return ((delegate, acct ^. accountAmount, Set.toList instances), newAccount)
            Accounts.updateAccounts updAcc aaddr (bspAccounts bsp) >>= \case
                (Nothing, _) -> error "Invalid account address"
                (Just (acctOldTarget, acctBal, acctInsts), accts) -> do
                    instBals <- forM acctInsts $ \caddr -> maybe (error "Invalid contract instance") pinstanceAmount <$> Instances.lookupContractInstance caddr (bspInstances bsp)
                    let stake = acctBal + sum instBals
                    newCurrBakers <- removeStake acctOldTarget stake =<< addStake target stake (bspBirkParameters bsp ^. birkCurrentBakers)
                    pbs' <- storePBS pbs bsp{
                            bspAccounts = accts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                        }
                    return (True, pbs')
        else return (False, pbs)

doGetIdentityProvider :: MonadPersistentBlockState r m => PersistentBlockState -> ID.IdentityProviderIdentity -> m (Maybe IPS.IpInfo)
doGetIdentityProvider pbs ipId = do
        bsp <- loadPBS pbs
        ips <- refLoad (bspIdentityProviders bsp)
        return $! IPS.idProviders ips ^? ix ipId

doGetAllIdentityProvider :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m [IPS.IpInfo]
doGetAllIdentityProvider pbs = do
        bsp <- loadPBS pbs
        ips <- refLoad (bspIdentityProviders bsp)
        return $! Map.elems $ IPS.idProviders ips

doGetAnonymityRevokers :: MonadPersistentBlockState r m => PersistentBlockState -> [ID.ArIdentity] -> PersistentBlockStateMonad r m (Maybe [ARS.ArInfo])
doGetAnonymityRevokers pbs arIds = do
        bsp <- loadPBS pbs
        ars <- refLoad (bspAnonymityRevokers bsp)
        return
          $! let arsMap = ARS.arRevokers ars
              in forM arIds (`Map.lookup` arsMap)

doGetAllAnonymityRevokers :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m [ARS.ArInfo]
doGetAllAnonymityRevokers pbs = do
        bsp <- loadPBS pbs
        ars <- refLoad (bspAnonymityRevokers bsp)
        return $! Map.elems $ ARS.arRevokers ars

doGetCryptoParams :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m CryptographicParameters
doGetCryptoParams pbs = do
        bsp <- loadPBS pbs
        refLoad (bspCryptographicParameters bsp)

doGetTransactionOutcome :: MonadPersistentBlockState r m => PersistentBlockState -> Transactions.TransactionIndex -> PersistentBlockStateMonad r m (Maybe TransactionSummary)
doGetTransactionOutcome pbs transHash = do
        bsp <- loadPBS pbs
        return $! bspTransactionOutcomes bsp ^? ix transHash

doSetTransactionOutcomes :: MonadPersistentBlockState r m => PersistentBlockState -> [TransactionSummary] -> PersistentBlockStateMonad r m PersistentBlockState
doSetTransactionOutcomes pbs transList = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspTransactionOutcomes = Transactions.transactionOutcomesFromList transList}

doNotifyExecutionCost :: MonadPersistentBlockState r m => PersistentBlockState -> Amount -> PersistentBlockStateMonad r m PersistentBlockState
doNotifyExecutionCost pbs amnt = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspBank = bspBank bsp & unhashed . Rewards.executionCost +~ amnt}

doNotifyEncryptedBalanceChange :: MonadPersistentBlockState r m => PersistentBlockState -> AmountDelta -> m PersistentBlockState
doNotifyEncryptedBalanceChange pbs amntDiff = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & unhashed . Rewards.totalEncryptedGTU %~ applyAmountDelta amntDiff}


doNotifyIdentityIssuerCredential :: MonadPersistentBlockState r m => PersistentBlockState -> ID.IdentityProviderIdentity -> PersistentBlockStateMonad r m PersistentBlockState
doNotifyIdentityIssuerCredential pbs idk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspBank = bspBank bsp & (unhashed . Rewards.identityIssuersRewards . at' idk . non 0) +~ 1}

doGetExecutionCost :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m Amount
doGetExecutionCost pbs = (^. unhashed . Rewards.executionCost) . bspBank <$> loadPBS pbs

doGetSpecialOutcomes :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m [Transactions.SpecialTransactionOutcome]
doGetSpecialOutcomes pbs = (^. to bspTransactionOutcomes . Transactions.outcomeSpecial) <$> loadPBS pbs

doGetOutcomes :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBlockStateMonad r m (Vec.Vector TransactionSummary)
doGetOutcomes pbs = (^. to bspTransactionOutcomes . to Transactions.outcomeValues) <$> loadPBS pbs

doAddSpecialTransactionOutcome :: MonadPersistentBlockState r m => PersistentBlockState -> Transactions.SpecialTransactionOutcome -> PersistentBlockStateMonad r m PersistentBlockState
doAddSpecialTransactionOutcome pbs !o = do
        bsp <- loadPBS pbs
        storePBS pbs $! bsp {bspTransactionOutcomes = bspTransactionOutcomes bsp & Transactions.outcomeSpecial %~ (o :)}

doUpdateBirkParameters :: MonadPersistentBlockState r m => PersistentBlockState -> PersistentBirkParameters -> PersistentBlockStateMonad r m PersistentBlockState
doUpdateBirkParameters pbs newBirk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspBirkParameters = newBirk}

doSetElectionDifficulty :: MonadPersistentBlockState r m => PersistentBlockState -> ElectionDifficulty -> PersistentBlockStateMonad r m PersistentBlockState
doSetElectionDifficulty pbs d = do
        bsp <- loadPBS pbs
        storePBS pbs bsp {bspBirkParameters = bspBirkParameters bsp & birkElectionDifficulty .~ d}

data PersistentBlockStateContext = PersistentBlockStateContext {
    pbscBlobStore :: BlobStore
}

instance HasBlobStore PersistentBlockStateContext where
    blobStore = pbscBlobStore

newtype PersistentBlockStateMonad r m a = PersistentBlockStateMonad {runPersistentBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadLogger)

type instance BlockStatePointer PersistentBlockState = BlobRef BlockStatePointers

instance BlockStateTypes (PersistentBlockStateMonad r m) where
    type BlockState (PersistentBlockStateMonad r m) = PersistentBlockState
    type UpdatableBlockState (PersistentBlockStateMonad r m) = PersistentBlockState
    type BirkParameters (PersistentBlockStateMonad r m) = PersistentBirkParameters
    type Bakers (PersistentBlockStateMonad r m) = PersistentBakers
    type Account (PersistentBlockStateMonad r m) = PersistentAccount

instance (MonadIO m, MonadReader r m, HasBlobStore r) => BirkParametersOperations (PersistentBlockStateMonad r m) where

    getSeedState bps = return $ _birkSeedState bps

    updateBirkParametersForNewEpoch seedState bps = do
        currentBakers <- makeBufferedRef $ bps ^. birkCurrentBakers
        return $ bps &
            birkSeedState .~ seedState &
            -- use stake distribution saved from the former epoch for leader election
            birkLotteryBakers .~ (bps ^. birkPrevEpochBakers) &
            -- save the stake distribution from the end of the epoch
            birkPrevEpochBakers .~ currentBakers

    getElectionDifficulty = return . _birkElectionDifficulty

    getCurrentBakers = return . _birkCurrentBakers

    getLotteryBakers = loadBufferedRef . _birkLotteryBakers

    updateSeedState f bps = return $ bps & birkSeedState %~ f

instance MonadPersistentBlockState r m => BlockStateQuery (PersistentBlockStateMonad r m) where
    getModule = doGetModule
    getAccount = doGetAccount
    getContractInstance = doGetInstance
    getModuleList = doGetModuleList
    getAccountList = doAccountList
    getContractInstanceList = doContractInstanceList
    getBlockBirkParameters = doGetBlockBirkParameters
    getRewardStatus = doGetRewardStatus
    getTransactionOutcome = doGetTransactionOutcome
    getSpecialOutcomes = doGetSpecialOutcomes
    getOutcomes = doGetOutcomes
    getAllIdentityProviders = doGetAllIdentityProvider
    getAllAnonymityRevokers = doGetAllAnonymityRevokers
    {-# INLINE getModule #-}
    {-# INLINE getAccount #-}
    {-# INLINE getContractInstance #-}
    {-# INLINE getModuleList #-}
    {-# INLINE getAccountList #-}
    {-# INLINE getContractInstanceList #-}
    {-# INLINE getBlockBirkParameters #-}
    {-# INLINE getRewardStatus #-}
    {-# INLINE getTransactionOutcome #-}
    {-# INLINE getOutcomes #-}
    {-# INLINE getSpecialOutcomes #-}
    {-# INLINE getAllIdentityProviders #-}
    {-# INLINE getAllAnonymityRevokers #-}

doGetBakerStake :: MonadPersistentBlockState r m => PersistentBakers -> BakerId -> PersistentBlockStateMonad r m (Maybe Amount)
doGetBakerStake bs bid =
  PersistentBlockStateMonad $
    L.lookup bid (bs ^. bakerMap) >>= \case
      Just (Some (_, s)) -> return (Just s)
      _ -> return Nothing

instance MonadPersistentBlockState r m => BakerQuery (PersistentBlockStateMonad r m) where

  getBakerStake = doGetBakerStake

  getBakerFromKey bs k = return $ bs ^. bakersByKey . at' k

  getTotalBakerStake bs = return $ bs ^. bakerTotalStake

  getBakerInfo bs bid = L.lookup bid (bs ^. bakerMap) >>= \case
    Just (Some (bInfoRef, _)) -> Just <$> loadBufferedRef bInfoRef
    _ -> return Nothing

  getFullBakerInfos PersistentBakers {..} = do
    l <- L.toAscPairList _bakerMap
    Map.fromAscList <$> mapM getFullInfo [(i, x) | (i, Some x) <- l]
    where
      getFullInfo (i, (binfoRef, stake)) = do
        binfo <- loadBufferedRef binfoRef
        return $ (i, FullBakerInfo binfo stake)

instance MonadPersistentBlockState r m => AccountOperations (PersistentBlockStateMonad r m) where

  getAccountAddress acc = acc ^^. accountAddress

  getAccountAmount acc = return $ acc ^. accountAmount

  getAccountNonce acc = return $ acc ^. accountNonce

  getAccountCredentials acc = acc ^^. accountCredentials

  getAccountMaxCredentialValidTo acc = acc ^^. accountMaxCredentialValidTo

  getAccountVerificationKeys acc = acc ^^. accountVerificationKeys

  getAccountEncryptedAmount acc = return $ acc ^. accountEncryptedAmount
  
  getAccountEncryptionKey acc = acc ^^. accountEncryptionKey

  getAccountStakeDelegate acc = acc ^^. accountStakeDelegate

  getAccountInstances acc = acc ^^. accountInstances

  createNewAccount cryptoParams _accountVerificationKeys _accountAddress cdv = do
      let pData = PersistingAccountData {
                    _accountEncryptionKey = ID.makeEncryptionKey cryptoParams (ID.cdvRegId cdv),
                    _accountCredentials = [cdv],
                    _accountMaxCredentialValidTo = ID.pValidTo (ID.cdvPolicy cdv),
                    _accountStakeDelegate = Nothing,
                    _accountInstances = Set.empty,
                    ..
                  }
          _accountNonce = minNonce
          _accountAmount = 0
          _accountEncryptedAmount = initialAccountEncryptedAmount
      _persistingData <- makeBufferedRef pData
      let _accountHash = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount pData
      return $ PersistentAccount {..}

  updateAccountAmount acc amnt = do
    let newAcc@PersistentAccount{..} = acc & accountAmount .~ amnt
    pData <- loadBufferedRef _persistingData
    return $ newAcc & accountHash .~ makeAccountHash _accountNonce amnt _accountEncryptedAmount pData

instance MonadPersistentBlockState r m => BlockStateOperations (PersistentBlockStateMonad r m) where
    bsoGetModule pbs mref = fmap moduleInterface <$> doGetModule pbs mref
    bsoGetAccount = doGetAccount
    bsoGetInstance = doGetInstance
    bsoRegIdExists = doRegIdExists
    bsoPutNewAccount = doPutNewAccount
    bsoPutNewInstance = doPutNewInstance
    bsoPutNewModule = doPutNewModule
    bsoModifyAccount = doModifyAccount
    bsoModifyInstance = doModifyInstance
    bsoNotifyExecutionCost = doNotifyExecutionCost
    bsoNotifyEncryptedBalanceChange = doNotifyEncryptedBalanceChange
    bsoNotifyIdentityIssuerCredential = doNotifyIdentityIssuerCredential
    bsoGetExecutionCost = doGetExecutionCost
    bsoGetBlockBirkParameters = doGetBlockBirkParameters
    bsoAddBaker = doAddBaker
    bsoUpdateBaker = doUpdateBaker
    bsoRemoveBaker = doRemoveBaker
    bsoSetInflation = doSetInflation
    bsoMint = doMint
    bsoDecrementCentralBankGTU = doDecrementCentralBankGTU
    bsoDelegateStake = doDelegateStake
    bsoGetIdentityProvider = doGetIdentityProvider
    bsoGetAnonymityRevokers = doGetAnonymityRevokers
    bsoGetCryptoParams = doGetCryptoParams
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoUpdateBirkParameters = doUpdateBirkParameters
    bsoSetElectionDifficulty = doSetElectionDifficulty
    {-# INLINE bsoGetModule #-}
    {-# INLINE bsoGetAccount #-}
    {-# INLINE bsoGetInstance #-}
    {-# INLINE bsoRegIdExists #-}
    {-# INLINE bsoPutNewAccount #-}
    {-# INLINE bsoPutNewInstance #-}
    {-# INLINE bsoPutNewModule #-}
    {-# INLINE bsoModifyAccount #-}
    {-# INLINE bsoModifyInstance #-}
    {-# INLINE bsoNotifyExecutionCost #-}
    {-# INLINE bsoNotifyIdentityIssuerCredential #-}
    {-# INLINE bsoGetExecutionCost #-}
    {-# INLINE bsoNotifyEncryptedBalanceChange #-}
    {-# INLINE bsoGetBlockBirkParameters #-}
    {-# INLINE bsoAddBaker #-}
    {-# INLINE bsoUpdateBaker #-}
    {-# INLINE bsoRemoveBaker #-}
    {-# INLINE bsoSetInflation #-}
    {-# INLINE bsoMint #-}
    {-# INLINE bsoDecrementCentralBankGTU #-}
    {-# INLINE bsoDelegateStake #-}
    {-# INLINE bsoGetIdentityProvider #-}
    {-# INLINE bsoGetAnonymityRevokers #-}
    {-# INLINE bsoGetCryptoParams #-}
    {-# INLINE bsoSetTransactionOutcomes #-}
    {-# INLINE bsoAddSpecialTransactionOutcome #-}
    {-# INLINE bsoUpdateBirkParameters #-}
    {-# INLINE bsoSetElectionDifficulty #-}

instance MonadPersistentBlockState r m => BlockStateStorage (PersistentBlockStateMonad r m) where
    {-# INLINE thawBlockState #-}
    thawBlockState pbs = do
            bsp <- loadPBS pbs
            pbsp <- makeBufferedRef bsp {
                        bspBank = bspBank bsp & unhashed . Rewards.executionCost .~ 0 & unhashed . Rewards.identityIssuersRewards .~ HM.empty
                    }
            liftIO $ newIORef $! pbsp

    {-# INLINE freezeBlockState #-}
    freezeBlockState pbs = do
      bs <- loadPBS pbs
      hashes <- makeBlockHashesM bs
      _ <- storePBS pbs (bs {bspHashes = Just hashes})
      return pbs

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    {-# INLINE purgeBlockState #-}
    purgeBlockState pbs = liftIO $ writeIORef pbs (error "Block state purged")

    {-# INLINE archiveBlockState #-}
    archiveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef pbs inner'

    saveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        (inner', ref) <- flushBufferedRef inner
        liftIO $ writeIORef pbs inner'
        bs <- blobStore <$> ask
        liftIO $ flushBlobStore bs
        return ref

    loadBlockState = liftIO . newIORef . BRBlobbed
