{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving,
        TypeFamilies, BangPatterns, TemplateHaskell, LambdaCase, OverloadedStrings, TupleSections, StandaloneDeriving
 #-}

module Concordium.GlobalState.Persistent.BlockState where

import Data.Void
import Data.Serialize
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans
import Control.Monad
import Lens.Micro.Platform
import qualified Data.Set as Set
import Data.Maybe
import Data.Functor.Identity

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.Types
import qualified Concordium.ID.Types as ID
import Acorn.Types (linkWithMaxSize, ValidResult)

import Concordium.GlobalState.Classes
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.GlobalState.Transactions as Transactions
import Concordium.GlobalState.Persistent.Instances(PersistentInstance(..), PersistentInstanceParameters(..), CacheableInstanceParameters(..))
import Concordium.GlobalState.Instance (Instance(..),InstanceParameters(..))
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Modules as TransientMods

type PersistentBlockState = IORef (BufferedRef BlockStatePointers)

data BlockStatePointers = BlockStatePointers {
    bspAccounts :: !Account.Accounts,
    bspInstances ::  !Instances.Instances,
    bspModules :: BufferedRef Modules,
    bspBank :: !Rewards.BankStatus,
    bspIdentityProviders :: BufferedRef IPS.IdentityProviders,
    bspBirkParameters :: !BirkParameters, -- TODO: Possibly store BirkParameters allowing for sharing
    bspCryptographicParameters :: BufferedRef CryptographicParameters,
    bspTransactionOutcomes :: !Transactions.TransactionOutcomes
}

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef BlockStatePointers where
    storeUpdate p bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate p bspAccounts
        (pinsts, bspInstances') <- storeUpdate p bspInstances
        (pmods, bspModules') <- storeUpdate p bspModules
        (pips, bspIdentityProviders') <- storeUpdate p bspIdentityProviders
        (pcryptps, bspCryptographicParameters') <- storeUpdate p bspCryptographicParameters
        let putBSP = do
                paccts
                pinsts
                pmods
                put bspBank
                pips
                put bspBirkParameters
                pcryptps
                put bspTransactionOutcomes
        return (putBSP, bsp0 {
                    bspAccounts = bspAccounts',
                    bspInstances = bspInstances',
                    bspModules = bspModules',
                    bspIdentityProviders = bspIdentityProviders',
                    bspCryptographicParameters = bspCryptographicParameters'
                })
    store p bsp = fst <$> storeUpdate p bsp
    load p = do
        maccts <- label "Accounts" $ load p
        minsts <- label "Instances" $ load p
        mmods <- label "Modules" $ load p
        bspBank <- label "Bank" $ get
        mpips <- label "Identity providers" $ load p
        bspBirkParameters <- label "Birk parameters" $ get
        mcryptps <- label "Cryptographic parameters" $ load p
        bspTransactionOutcomes <- label "Transaction outcomes" $ get
        return $! do
            bspAccounts <- maccts
            bspInstances <- minsts
            bspModules <- mmods
            bspIdentityProviders <- mpips
            bspCryptographicParameters <- mcryptps
            return $! BlockStatePointers{..}

data PersistentModule = PersistentModule {
    pmInterface :: !(Interface Core.UA),
    pmValueInterface :: !(UnlinkedValueInterface Void),
    pmIndex :: !ModuleIndex,
    pmSource :: !(Core.Module Core.UA)
}

persistentModuleToModule :: PersistentModule -> Module
persistentModuleToModule PersistentModule{..} = Module {
    moduleInterface = pmInterface,
    moduleValueInterface = pmValueInterface,
    moduleIndex = pmIndex,
    moduleSource = pmSource
}


instance Serialize PersistentModule where
    put PersistentModule{..} = put pmInterface >> put pmValueInterface >> put pmIndex >> put pmSource
    get = PersistentModule <$> get <*> get <*> get <*> get

instance (MonadBlobStore m ref) => BlobStorable m ref PersistentModule

data Modules = Modules {
    modules :: Trie.TrieN (BufferedBlobbed BlobRef) Core.ModuleRef PersistentModule,
    nextModuleIndex :: !ModuleIndex,
    runningHash :: !H.Hash
}

emptyModules :: Modules
emptyModules = Modules {
        modules = Trie.empty,
        nextModuleIndex = 0,
        runningHash = H.hash ""
    }

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef Modules where
    storeUpdate p ms@Modules{..} = do
        (pm, modules') <- storeUpdate p modules
        return (pm >> put nextModuleIndex >> put runningHash, ms {modules = modules'})
    store p m = fst <$> storeUpdate p m
    load p = do
        mmodules <- load p
        nextModuleIndex <- get
        runningHash <- get
        return $ do
            modules <- mmodules
            return Modules{..}

makePersistentModules :: TransientMods.Modules -> Modules
makePersistentModules (TransientMods.Modules m nmi rh) = Modules m' nmi rh
    where
        m' = runIdentity $ Trie.fromList $ fmap upd $ HM.toList m
        upd (mref, TransientMods.MemModule{..}) = (mref, PersistentModule{
                pmInterface = mmoduleInterface,
                pmValueInterface = mmoduleValueInterface,
                pmIndex = mmoduleIndex,
                pmSource = mmoduleSource
            })

data ModuleCache = ModuleCache {
    _cachedLinkedDefs :: HM.HashMap (Core.ModuleRef, Core.Name) (LinkedExprWithDeps Void),
    _cachedLinkedContracts :: HM.HashMap (Core.ModuleRef, Core.TyName) (LinkedContractValue Void)
}
makeLenses ''ModuleCache

emptyModuleCache :: ModuleCache
emptyModuleCache = ModuleCache HM.empty HM.empty

class HasModuleCache a where
    moduleCache :: a -> IORef ModuleCache

makePersistent :: MonadIO m => Basic.BlockState -> m PersistentBlockState
makePersistent Basic.BlockState{..} = liftIO $ newIORef $! BRMemory BlockStatePointers {
        bspAccounts = Account.makePersistent _blockAccounts
        , bspInstances = Instances.makePersistent _blockInstances
        , bspModules = BRMemory $! makePersistentModules _blockModules
        , bspBank = _blockBank
        , bspIdentityProviders = BRMemory $! _blockIdentityProviders
        , bspBirkParameters = _blockBirkParameters
        , bspCryptographicParameters = BRMemory $! _blockCryptographicParameters
        , bspTransactionOutcomes = _blockTransactionOutcomes
        }
    
initialPersistentState :: MonadIO m => BirkParameters
             -> CryptographicParameters
             -> [Account]
             -> [IPS.IpInfo]
             -> Amount
             -> m PersistentBlockState
initialPersistentState bps cps accts ips amt = makePersistent $ Basic.initialState bps cps accts ips amt


newtype LinkerWrapper r m a = LinkerWrapper { runLinkerWrapper :: ReaderT PersistentBlockState m a }
    deriving (Functor, Applicative, Monad, MonadReader PersistentBlockState, MonadTrans)

instance (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => LinkerMonad Void (LinkerWrapper r m) where
    getExprInModule modRef name = do
        blockState <- ask
        mmod <- lift $ doGetModule blockState modRef
        case mmod of
            Nothing -> return Nothing
            Just Module{..} -> return (HM.lookup name (viDefs moduleValueInterface))
    tryGetLinkedExpr modref name = do
        blockState <- ask
        lift $ doTryGetLinkedExpr blockState modref name
    putLinkedExpr modref name linked = do
        blockState <- ask
        void $ lift $ doPutLinkedExpr blockState modref name linked

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: MonadIO m => BirkParameters -> CryptographicParameters -> m PersistentBlockState
emptyBlockState bspBirkParameters cryptParams = liftIO $ newIORef $! BRMemory BlockStatePointers {
  bspAccounts = Account.emptyAccounts
  , bspInstances = Instances.emptyInstances
  , bspModules = BRMemory $! emptyModules
  , bspBank = Rewards.emptyBankStatus
  , bspIdentityProviders = BRMemory $! IPS.emptyIdentityProviders
  , bspCryptographicParameters = BRMemory $! cryptParams
  , bspTransactionOutcomes = Transactions.emptyTransactionOutcomes
  ,..
  }




doLinkContract :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) =>
    PersistentBlockState -> Core.ModuleRef -> Module -> Core.TyName -> m (LinkedContractValue Void)
doLinkContract pbs mref m cname = do
    mcontract <- doTryGetLinkedContract pbs mref cname
    case mcontract of
        Just contract -> return contract
        Nothing -> do
            let unlinked = viContracts (moduleValueInterface m) HM.! cname
            linked <- flip runReaderT pbs $ runLinkerWrapper $ do
                cvInitMethod <- myLink (cvInitMethod unlinked)
                cvReceiveMethod <- myLink (cvReceiveMethod unlinked)
                cvImplements <- mapM (\iv -> do
                                        ivSenders <- mapM myLink (ivSenders iv)
                                        ivGetters <- mapM myLink (ivGetters iv)
                                        return ImplementsValue{..}
                                    ) (cvImplements unlinked)
                return ContractValue{..}
            _ <- doPutLinkedContract pbs mref cname linked
            return linked
    where
        myLink ule = (_1 %~ leExpr) . fromJust <$> linkWithMaxSize mref ule maxBound

fromPersistentInstance :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) =>
    PersistentBlockState -> Instances.PersistentInstance -> m Instance
fromPersistentInstance _ Instances.PersistentInstance{pinstanceCachedParameters = (Some CacheableInstanceParameters{..}), ..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    let instanceParameters = InstanceParameters {
            instanceAddress = pinstanceAddress,
            instanceOwner = pinstanceOwner,
            instanceContractModule = pinstanceContractModule,
            instanceContract = pinstanceContract,
            instanceReceiveFun = pinstanceReceiveFun,
            instanceModuleInterface = pinstanceModuleInterface,
            instanceModuleValueInterface = pinstanceModuleValueInterface,
            instanceMessageType = pinstanceMessageType,
            instanceImplements = pinstanceImplements,
            instanceParameterHash = pinstanceParameterHash
        }
    return Instance{
            instanceModel = pinstanceModel,
            instanceAmount = pinstanceAmount,
            instanceHash = pinstanceHash,
            ..
        }
fromPersistentInstance pbs Instances.PersistentInstance{..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    doGetModule pbs pinstanceContractModule >>= \case
        Nothing -> error "fromPersistentInstance: unresolvable module" -- TODO: Possibly don't error here
        Just m -> do
            conVal <- doLinkContract pbs pinstanceContractModule m pinstanceContract
            let instanceParameters = InstanceParameters {
                    instanceAddress = pinstanceAddress,
                    instanceOwner = pinstanceOwner,
                    instanceContractModule = pinstanceContractModule,
                    instanceContract = pinstanceContract,
                    instanceReceiveFun = fst (cvReceiveMethod conVal),
                    instanceModuleInterface = moduleInterface m,
                    instanceModuleValueInterface = moduleValueInterface m,
                    instanceMessageType = msgTy (exportedContracts (moduleInterface m) HM.! pinstanceContract),
                    instanceImplements = cvImplements conVal,
                    instanceParameterHash = pinstanceParameterHash
                }
            return Instance{
                    instanceModel = pinstanceModel,
                    instanceAmount = pinstanceAmount,
                    instanceHash = pinstanceHash,
                    ..
                }

loadPBS :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m BlockStatePointers
loadPBS = loadBufferedRef <=< liftIO . readIORef

storePBS :: (MonadIO m) => PersistentBlockState -> BlockStatePointers -> m PersistentBlockState
storePBS pbs bsp = liftIO (writeIORef pbs (BRMemory bsp)) >> return pbs

doGetModule :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Core.ModuleRef -> m (Maybe Module)
doGetModule s modRef = do
        bsp <- loadPBS s
        mods <- loadBufferedRef (bspModules bsp)
        fmap persistentModuleToModule <$> Trie.lookup modRef (modules mods)

doGetModuleList :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [Core.ModuleRef]
doGetModuleList s = do
        bsp <- loadPBS s
        mods <- loadBufferedRef (bspModules bsp)
        Trie.keys (modules mods)

doPutNewModule :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState
    -> Core.ModuleRef
    -> Interface Core.UA
    -> UnlinkedValueInterface Void
    -> Core.Module Core.UA
    -> m (Bool, PersistentBlockState)
doPutNewModule pbs mref pmInterface pmValueInterface pmSource = do
        bsp <- loadPBS pbs
        mods <- loadBufferedRef (bspModules bsp)
        let
            newMod = PersistentModule{pmIndex = nextModuleIndex mods, ..}
            tryIns Nothing = return (True, Trie.Insert newMod)
            tryIns (Just _) = return (False, Trie.NoChange)
        (b, modules') <- Trie.adjust tryIns mref (modules mods)
        if b then do
            let
                newMods = mods {modules = modules', nextModuleIndex = nextModuleIndex mods + 1}
            (True,) <$> storePBS pbs (bsp {bspModules = BRMemory newMods})
        else
            return (False, pbs)

doTryGetLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExprWithDeps Void))
doTryGetLinkedExpr _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedDefs cache)

doPutLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> LinkedExprWithDeps Void -> m PersistentBlockState
doPutLinkedExpr pbs modRef n !le = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedDefs %~ HM.insert (modRef, n) le)
        return pbs

doTryGetLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> m (Maybe (LinkedContractValue Void))
doTryGetLinkedContract _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedContracts cache)

doPutLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> LinkedContractValue Void -> m PersistentBlockState
doPutLinkedContract pbs modRef n !lc = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedContracts %~ HM.insert (modRef, n) lc)
        return pbs

doGetBlockBirkParameters :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m BirkParameters
doGetBlockBirkParameters pbs = bspBirkParameters <$> loadPBS pbs

doAddBaker :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BakerCreationInfo -> m (Maybe BakerId, PersistentBlockState)
doAddBaker pbs binfo = do
        bsp <- loadPBS pbs
        case createBaker binfo (bspBirkParameters bsp ^. birkCurrentBakers) of
            Nothing -> return $! (Nothing, pbs)
            Just (bid, newBakers) -> (Just bid,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doUpdateBaker :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BakerUpdate -> m (Bool, PersistentBlockState)
doUpdateBaker pbs bupdate = do
        bsp <- loadPBS pbs
        case updateBaker bupdate (bspBirkParameters bsp ^. birkCurrentBakers) of
            Nothing -> return $! (False, pbs)
            Just newBakers -> (True, ) <$!> storePBS pbs (bsp {bspBirkParameters =  bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doRemoveBaker :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BakerId -> m (Bool, PersistentBlockState)
doRemoveBaker pbs bid = do
        bsp <- loadPBS pbs
        let (rv, newBakers) = removeBaker bid (bspBirkParameters bsp ^. birkCurrentBakers)
        (rv,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doGetRewardStatus :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m Rewards.BankStatus
doGetRewardStatus pbs = bspBank <$> loadPBS pbs

doSetInflation :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doSetInflation pbs amount = do
        bsp <- loadPBS pbs
        storePBS pbs (bsp {bspBank = bspBank bsp & Rewards.mintedGTUPerSlot .~ amount})

doMint :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doMint pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & (Rewards.totalGTU +~ amount) . (Rewards.centralBankGTU +~ amount)
        (newBank ^. Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doDecrementCentralBankGTU :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doDecrementCentralBankGTU pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & Rewards.centralBankGTU -~ amount
        (newBank ^. Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doGetAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> m (Maybe Account)
doGetAccount pbs addr = do
        bsp <- loadPBS pbs
        Account.getAccount addr (bspAccounts bsp)

doAccountList :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Account.accountAddresses (bspAccounts bsp)

doRegIdExists :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        fst <$> Account.regIdExists regid (bspAccounts bsp)

doPutNewAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Account -> m (Bool, PersistentBlockState)
doPutNewAccount pbs acct = do 
        bsp <- loadPBS pbs
        (res, accts') <- Account.putNewAccount acct (bspAccounts bsp)
        (res,) <$> storePBS pbs (bsp {bspAccounts = accts'})
    
doModifyAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountUpdate -> m PersistentBlockState
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadPBS pbs
        -- Do the update to the account
        (mbalinfo, accts1) <- Account.updateAccount upd _auAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- case _auCredential of
            Just cdi -> Account.recordRegId (ID.cdvRegId cdi) accts1
            Nothing -> return accts1
        -- If the amount is changed update the delegate stake
        let birkParams1 = case (_auAmount, mbalinfo) of
                (Just deltaAmnt, Just delegate) ->
                    bspBirkParameters bsp & birkCurrentBakers %~ modifyStake delegate deltaAmnt
                _ -> bspBirkParameters bsp
        storePBS pbs (bsp {bspAccounts = accts2, bspBirkParameters = birkParams1})
    where
        upd oldAccount = return ((oldAccount ^. accountStakeDelegate), updateAccount aUpd oldAccount)

doGetInstance :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> ContractAddress -> m (Maybe Instance)
doGetInstance pbs caddr = do
        bsp <- loadPBS pbs
        minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
        forM minst $ fromPersistentInstance pbs

doContractInstanceList :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> m [Instance]
doContractInstanceList pbs = do
        bsp <- loadPBS pbs
        insts <- Instances.allInstances (bspInstances bsp)
        mapM (fromPersistentInstance pbs) insts

doPutNewInstance :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> (ContractAddress -> Instance) -> m (ContractAddress, PersistentBlockState)
doPutNewInstance pbs fnew = do
        bsp <- loadPBS pbs
        -- Create the instance
        (inst, insts) <- Instances.newContractInstance fnew' (bspInstances bsp)
        let ca = instanceAddress (instanceParameters inst)
        -- Update the owner account's set of instances
        let updAcct oldAccount = return (oldAccount ^. accountStakeDelegate, oldAccount & accountInstances %~ Set.insert ca)
        (mdelegate, accts) <- Account.updateAccount updAcct (instanceOwner (instanceParameters inst)) (bspAccounts bsp)
        -- Update the stake delegate
        case mdelegate of
            Nothing -> error "Invalid contract owner"
            Just delegate -> (ca,) <$> storePBS pbs bsp{
                                    bspInstances = insts,
                                    bspAccounts = accts,
                                    bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers %~ modifyStake delegate (amountToDelta (instanceAmount inst))
                                }
    where
        fnew' ca = let inst@Instance{instanceParameters = InstanceParameters{..}, ..} = fnew ca in
            return (inst, PersistentInstance{
                pinstanceParameters = BRMemory (PersistentInstanceParameters{
                        pinstanceAddress = instanceAddress,
                        pinstanceOwner = instanceOwner,
                        pinstanceContractModule = instanceContractModule,
                        pinstanceContract = instanceContract,
                        pinstanceParameterHash = instanceParameterHash
                    }),
                pinstanceCachedParameters = Some (CacheableInstanceParameters{
                        pinstanceReceiveFun = instanceReceiveFun,
                        pinstanceModuleInterface = instanceModuleInterface,
                        pinstanceModuleValueInterface = instanceModuleValueInterface,
                        pinstanceMessageType = instanceMessageType,
                        pinstanceImplements = instanceImplements
                    }),
                pinstanceModel = instanceModel,
                pinstanceAmount = instanceAmount,
                pinstanceHash = instanceHash
            })

doModifyInstance :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ContractAddress -> AmountDelta -> Value Void -> m PersistentBlockState
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadPBS pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (Nothing, insts) -> -- no change to staking
                storePBS pbs bsp{bspInstances = insts}
            Just (Just owner, insts) ->
                -- Lookup the owner account and update its stake delegate
                Account.getAccount owner (bspAccounts bsp) >>= \case
                    Nothing -> error "Invalid contract owner"
                    Just acct -> storePBS pbs bsp{
                            bspInstances = insts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers %~ modifyStake (_accountStakeDelegate acct) deltaAmnt
                        }
    where
        upd oldInst = do
            if deltaAmnt == 0 then
                return (Nothing, oldInst {pinstanceModel = val})
            else do
                acct <- pinstanceOwner <$> loadBufferedRef (pinstanceParameters oldInst)
                return (Just acct, oldInst {pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = val})

doDelegateStake :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> Maybe BakerId -> m (Bool, PersistentBlockState)
doDelegateStake pbs aaddr target = do
        bsp <- loadPBS pbs
        let targetValid = case target of
                Nothing -> True
                Just bid -> isJust $ bspBirkParameters bsp ^. birkCurrentBakers . bakerMap . at bid
        if targetValid then do
            let updAcc acct = return ((acct ^. accountStakeDelegate, acct ^. accountAmount, Set.toList $ acct ^. accountInstances),
                                acct & accountStakeDelegate .~ target)
            Account.updateAccount updAcc aaddr (bspAccounts bsp) >>= \case
                (Nothing, _) -> error "Invalid account address"
                (Just (acctOldTarget, acctBal, acctInsts), accts) -> do
                    instBals <- forM acctInsts $ \caddr -> maybe (error "Invalid contract instance") pinstanceAmount <$> Instances.lookupContractInstance caddr (bspInstances bsp)
                    let stake = acctBal + sum instBals
                    (True,) <$> storePBS pbs bsp{
                            bspAccounts = accts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers %~
                                removeStake acctOldTarget stake . addStake target stake
                        }
        else
            return (False, pbs)

doGetIdentityProvider :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m (Maybe IPS.IpInfo)
doGetIdentityProvider pbs ipId = do
        bsp <- loadPBS pbs
        ips <- loadBufferedRef (bspIdentityProviders bsp)
        return $! IPS.idProviders ips ^? ix ipId

doGetCryptoParams :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m CryptographicParameters
doGetCryptoParams pbs = do
        bsp <- loadPBS pbs
        loadBufferedRef (bspCryptographicParameters bsp)

doGetTransactionOutcome :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.TransactionHash -> m (Maybe ValidResult)
doGetTransactionOutcome pbs transHash = do
        bsp <- loadPBS pbs
        return $! (bspTransactionOutcomes bsp) ^? ix transHash

doSetTransactionOutcomes :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> [(Transactions.TransactionHash, ValidResult)] -> m PersistentBlockState
doSetTransactionOutcomes pbs transList = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspTransactionOutcomes = Transactions.transactionOutcomesFromList transList}

doNotifyExecutionCost :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doNotifyExecutionCost pbs amnt = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & Rewards.executionCost +~ amnt}

doNotifyIdentityIssuerCredential :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m PersistentBlockState
doNotifyIdentityIssuerCredential pbs idk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & (Rewards.identityIssuersRewards . at idk . non 0) +~ 1}

doGetExecutionCost :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m Amount
doGetExecutionCost pbs = (^. Rewards.executionCost) . bspBank <$> loadPBS pbs

doGetSpecialOutcomes :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [Transactions.SpecialTransactionOutcome]
doGetSpecialOutcomes pbs = (^. to bspTransactionOutcomes . Transactions.outcomeSpecial) <$> loadPBS pbs

doAddSpecialTransactionOutcome :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.SpecialTransactionOutcome -> m PersistentBlockState
doAddSpecialTransactionOutcome pbs o = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspTransactionOutcomes = bspTransactionOutcomes bsp & Transactions.outcomeSpecial %~ (o:)}

doUpdateBirkParameters :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BirkParameters -> m PersistentBlockState
doUpdateBirkParameters pbs newBirk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBirkParameters = newBirk}

data PersistentBlockStateContext = PersistentBlockStateContext {
    pbscModuleCache :: IORef ModuleCache,
    pbscBlobStore :: BlobStore
}

instance HasModuleCache PersistentBlockStateContext where
    moduleCache = pbscModuleCache

instance HasBlobStore PersistentBlockStateContext where
    blobStore = pbscBlobStore

newtype PersistentBlockStateMonad r m a = PersistentBlockStateMonad (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance BlockStateTypes (PersistentBlockStateMonad r m) where
    type BlockState (PersistentBlockStateMonad r m) = PersistentBlockState
    type UpdatableBlockState (PersistentBlockStateMonad r m) = PersistentBlockState

instance (MonadIO m, HasModuleCache r, HasBlobStore r, MonadReader r m) => BlockStateQuery (PersistentBlockStateMonad r m) where
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
    {-# INLINE getModule #-}
    {-# INLINE getAccount #-}
    {-# INLINE getContractInstance #-}
    {-# INLINE getModuleList #-}
    {-# INLINE getAccountList #-}
    {-# INLINE getContractInstanceList #-}
    {-# INLINE getBlockBirkParameters #-}
    {-# INLINE getRewardStatus #-}
    {-# INLINE getTransactionOutcome #-}
    {-# INLINE getSpecialOutcomes #-}

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BlockStateOperations (PersistentBlockStateMonad r m) where
    bsoGetModule = doGetModule
    bsoGetAccount = doGetAccount
    bsoGetInstance = doGetInstance
    bsoRegIdExists = doRegIdExists
    bsoPutNewAccount = doPutNewAccount
    bsoPutNewInstance = doPutNewInstance
    bsoPutNewModule = doPutNewModule
    bsoTryGetLinkedExpr = doTryGetLinkedExpr
    bsoPutLinkedExpr = doPutLinkedExpr
    bsoTryGetLinkedContract = doTryGetLinkedContract
    bsoPutLinkedContract = doPutLinkedContract
    bsoModifyAccount = doModifyAccount
    bsoModifyInstance = doModifyInstance
    bsoNotifyExecutionCost = doNotifyExecutionCost
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
    bsoGetCryptoParams = doGetCryptoParams
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoUpdateBirkParameters = doUpdateBirkParameters
    {-# INLINE bsoGetModule #-}
    {-# INLINE bsoGetAccount #-}
    {-# INLINE bsoGetInstance #-}
    {-# INLINE bsoRegIdExists #-}
    {-# INLINE bsoPutNewAccount #-}
    {-# INLINE bsoPutNewInstance #-}
    {-# INLINE bsoPutNewModule #-}
    {-# INLINE bsoTryGetLinkedExpr #-}
    {-# INLINE bsoPutLinkedExpr #-}
    {-# INLINE bsoTryGetLinkedContract #-}
    {-# INLINE bsoPutLinkedContract #-}
    {-# INLINE bsoModifyAccount #-}
    {-# INLINE bsoModifyInstance #-}
    {-# INLINE bsoNotifyExecutionCost #-}
    {-# INLINE bsoNotifyIdentityIssuerCredential #-}
    {-# INLINE bsoGetExecutionCost #-}
    {-# INLINE bsoGetBlockBirkParameters #-}
    {-# INLINE bsoAddBaker #-}
    {-# INLINE bsoUpdateBaker #-}
    {-# INLINE bsoRemoveBaker #-}
    {-# INLINE bsoSetInflation #-}
    {-# INLINE bsoMint #-}
    {-# INLINE bsoDecrementCentralBankGTU #-}
    {-# INLINE bsoDelegateStake #-}
    {-# INLINE bsoGetIdentityProvider #-}
    {-# INLINE bsoGetCryptoParams #-}
    {-# INLINE bsoSetTransactionOutcomes #-}
    {-# INLINE bsoAddSpecialTransactionOutcome #-}
    {-# INLINE bsoUpdateBirkParameters #-}

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BlockStateStorage (PersistentBlockStateMonad r m) where
    {-# INLINE thawBlockState #-}
    thawBlockState pbs = do
            bsp <- loadPBS pbs
            liftIO $ newIORef $! BRMemory bsp {
                    bspBank = bspBank bsp & Rewards.executionCost .~ 0 & Rewards.identityIssuersRewards .~ HM.empty
                }

    {-# INLINE freezeBlockState #-}
    freezeBlockState pbs = return pbs

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    {-# INLINE purgeBlockState #-}
    purgeBlockState pbs = liftIO $ writeIORef pbs (error "Block state purged")

    {-# INLINE archiveBlockState #-}
    archiveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef pbs inner'
