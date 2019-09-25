{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving,
        TypeFamilies, BangPatterns, TemplateHaskell, LambdaCase, OverloadedStrings
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
import Concordium.GlobalState.Instances (Instance(..),InstanceParameters(..))
import Concordium.GlobalState.SeedState (SeedState)
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Modules as TransientMods


-- FIXME: relocate orphans
instance (MonadBlobStore m ref) => BlobStorable m ref IPS.IdentityProviders

instance (MonadBlobStore m ref) => BlobStorable m ref CryptographicParameters

type PersistentBlockState = BufferedRef BlockStatePointers

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
        maccts <- load p
        minsts <- load p
        mmods <- load p
        bspBank <- get
        mpips <- load p
        bspBirkParameters <- get
        mcryptps <- load p
        bspTransactionOutcomes <- get
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

makePersistent :: Basic.BlockState -> PersistentBlockState
makePersistent Basic.BlockState{..} = BRMemory BlockStatePointers {
        bspAccounts = Account.makePersistent _blockAccounts
        , bspInstances = Instances.makePersistent _blockInstances
        , bspModules = BRMemory $! makePersistentModules _blockModules
        , bspBank = _blockBank
        , bspIdentityProviders = BRMemory $! _blockIdentityProviders
        , bspBirkParameters = _blockBirkParameters
        , bspCryptographicParameters = BRMemory $! _blockCryptographicParameters
        , bspTransactionOutcomes = _blockTransactionOutcomes
        }
    


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
emptyBlockState :: BirkParameters -> CryptographicParameters -> PersistentBlockState
emptyBlockState bspBirkParameters cryptParams = BRMemory BlockStatePointers {
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

doGetModule :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Core.ModuleRef -> m (Maybe Module)
doGetModule s modRef = do
        bsp <- loadBufferedRef s
        mods <- loadBufferedRef (bspModules bsp)
        fmap persistentModuleToModule <$> Trie.lookup modRef (modules mods)

doGetModuleList :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m [Core.ModuleRef]
doGetModuleList s = do
        bsp <- loadBufferedRef s
        mods <- loadBufferedRef (bspModules bsp)
        Trie.keys (modules mods)

doPutNewModule :: (MonadBlobStore m BlobRef) => PersistentBlockState
    -> Core.ModuleRef
    -> Interface Core.UA
    -> UnlinkedValueInterface Void
    -> Core.Module Core.UA
    -> m (Bool, PersistentBlockState)
doPutNewModule pbs mref pmInterface pmValueInterface pmSource = do
        bsp <- loadBufferedRef pbs
        mods <- loadBufferedRef (bspModules bsp)
        let
            newMod = PersistentModule{pmIndex = nextModuleIndex mods, ..}
            tryIns Nothing = return (True, Trie.Insert newMod)
            tryIns (Just _) = return (False, Trie.NoChange)
        (b, modules') <- Trie.adjust tryIns mref (modules mods)
        if b then do
            let
                newMods = mods {modules = modules', nextModuleIndex = nextModuleIndex mods + 1}
            return $! (True, BRMemory (bsp {bspModules = BRMemory newMods}))
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

doGetBlockBirkParameters :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m BirkParameters
doGetBlockBirkParameters pbs = bspBirkParameters <$> loadBufferedRef pbs

doAddBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerCreationInfo -> m (BakerId, PersistentBlockState)
doAddBaker pbs binfo = do
        bsp <- loadBufferedRef pbs
        let (bid, newBakers) = createBaker binfo (bspBirkParameters bsp ^. birkBakers)
        return (bid, BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers .~ newBakers}))

doUpdateBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerUpdate -> m PersistentBlockState
doUpdateBaker pbs bupdate = do
        bsp <- loadBufferedRef pbs
        return $ BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers %~ updateBaker bupdate})

doRemoveBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerId -> m (Bool, PersistentBlockState)
doRemoveBaker pbs bid = do
        bsp <- loadBufferedRef pbs
        let (rv, newBakers) = removeBaker bid (bspBirkParameters bsp ^. birkBakers)
        return (rv, BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers .~ newBakers}))

doGetRewardStatus :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m Rewards.BankStatus
doGetRewardStatus pbs = bspBank <$> loadBufferedRef pbs

doSetInflation :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doSetInflation pbs amount = do
        bsp <- loadBufferedRef pbs
        return $ BRMemory (bsp {bspBank = bspBank bsp & Rewards.mintedGTUPerSlot .~ amount})

doMint :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doMint pbs amount = do
        bsp <- loadBufferedRef pbs
        let newBank = bspBank bsp & (Rewards.totalGTU +~ amount) . (Rewards.centralBankGTU +~ amount)
        return (newBank ^. Rewards.centralBankGTU, BRMemory (bsp {bspBank = newBank}))

doDecrementCentralBankGTU :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doDecrementCentralBankGTU pbs amount = do
        bsp <- loadBufferedRef pbs
        let newBank = bspBank bsp & Rewards.centralBankGTU -~ amount
        return (newBank ^. Rewards.centralBankGTU, BRMemory (bsp {bspBank = newBank}))

doGetAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> m (Maybe Account)
doGetAccount pbs addr = do
        bsp <- loadBufferedRef pbs
        Account.getAccount addr (bspAccounts bsp)

doAccountList :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadBufferedRef pbs
        Account.accountAddresses (bspAccounts bsp)

doRegIdExists :: (MonadBlobStore m BlobRef) => PersistentBlockState -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadBufferedRef pbs
        fst <$> Account.regIdExists regid (bspAccounts bsp)

doPutNewAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Account -> m (Bool, PersistentBlockState)
doPutNewAccount pbs acct = do 
        bsp <- loadBufferedRef pbs
        (res, accts') <- Account.putNewAccount acct (bspAccounts bsp)
        return (res, BRMemory (bsp {bspAccounts = accts'}))
    
doModifyAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountUpdate -> m PersistentBlockState
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadBufferedRef pbs
        -- Do the update to the account
        (mbalinfo, accts1) <- Account.updateAccount upd _auAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- case _auCredential of
            Just cdi -> Account.recordRegId (ID.cdvRegId cdi) accts1
            Nothing -> return accts1
        -- If the amount is changed update the delegate stake
        let birkParams1 = case (_auAmount, mbalinfo) of
                (Just deltaAmnt, Just delegate) ->
                    bspBirkParameters bsp & birkBakers %~ modifyStake delegate deltaAmnt
                _ -> bspBirkParameters bsp
        return $! BRMemory (bsp {bspAccounts = accts2, bspBirkParameters = birkParams1})
    where
        upd oldAccount = return ((oldAccount ^. accountStakeDelegate), updateAccount aUpd oldAccount)

doGetInstance :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> ContractAddress -> m (Maybe Instance)
doGetInstance pbs caddr = do
        bsp <- loadBufferedRef pbs
        minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
        forM minst $ fromPersistentInstance pbs

doContractInstanceList :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> m [Instance]
doContractInstanceList pbs = do
        bsp <- loadBufferedRef pbs
        insts <- Instances.allInstances (bspInstances bsp)
        mapM (fromPersistentInstance pbs) insts

doPutNewInstance :: (MonadBlobStore m BlobRef) => PersistentBlockState -> (ContractAddress -> Instance) -> m (ContractAddress, PersistentBlockState)
doPutNewInstance pbs fnew = do
        bsp <- loadBufferedRef pbs
        -- Create the instance
        (inst, insts) <- Instances.newContractInstance fnew' (bspInstances bsp)
        let ca = instanceAddress (instanceParameters inst)
        -- Update the owner account's set of instances
        let updAcct oldAccount = return (oldAccount ^. accountStakeDelegate, oldAccount & accountInstances %~ Set.insert ca)
        (mdelegate, accts) <- Account.updateAccount updAcct (instanceOwner (instanceParameters inst)) (bspAccounts bsp)
        -- Update the stake delegate
        case mdelegate of
            Nothing -> error "Invalid contract owner"
            Just delegate -> return (ca, BRMemory bsp{
                                    bspInstances = insts,
                                    bspAccounts = accts,
                                    bspBirkParameters = bspBirkParameters bsp & birkBakers %~ modifyStake delegate (amountToDelta (instanceAmount inst))
                                })
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

doModifyInstance :: (MonadBlobStore m BlobRef) => PersistentBlockState -> ContractAddress -> AmountDelta -> Value Void -> m PersistentBlockState
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadBufferedRef pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (Nothing, insts) -> -- no change to staking
                return (BRMemory bsp{bspInstances = insts})
            Just (Just owner, insts) ->
                -- Lookup the owner account and update its stake delegate
                Account.getAccount owner (bspAccounts bsp) >>= \case
                    Nothing -> error "Invalid contract owner"
                    Just acct -> return $! BRMemory bsp{
                            bspInstances = insts,
                            bspBirkParameters = bspBirkParameters bsp & birkBakers %~ modifyStake (_accountStakeDelegate acct) deltaAmnt
                        }
    where
        upd oldInst = do
            if deltaAmnt == 0 then
                return (Nothing, oldInst {pinstanceModel = val})
            else do
                acct <- pinstanceOwner <$> loadBufferedRef (pinstanceParameters oldInst)
                return (Just acct, oldInst {pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = val})

doDelegateStake :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> Maybe BakerId -> m (Bool, PersistentBlockState)
doDelegateStake pbs aaddr target = do
        bsp <- loadBufferedRef pbs
        let targetValid = case target of
                Nothing -> True
                Just bid -> isJust $ bspBirkParameters bsp ^. birkBakers . bakerMap . at bid
        if targetValid then do
            let updAcc acct = return ((acct ^. accountStakeDelegate, acct ^. accountAmount, Set.toList $ acct ^. accountInstances),
                                acct & accountStakeDelegate .~ target)
            Account.updateAccount updAcc aaddr (bspAccounts bsp) >>= \case
                (Nothing, _) -> error "Invalid account address"
                (Just (acctOldTarget, acctBal, acctInsts), accts) -> do
                    instBals <- forM acctInsts $ \caddr -> maybe (error "Invalid contract instance") pinstanceAmount <$> Instances.lookupContractInstance caddr (bspInstances bsp)
                    let stake = acctBal + sum instBals
                    return $! (True, BRMemory bsp{
                            bspAccounts = accts,
                            bspBirkParameters = bspBirkParameters bsp & birkBakers %~
                                removeStake acctOldTarget stake . addStake target stake
                        })
        else
            return (False, pbs)

doGetIdentityProvider :: (MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m (Maybe IPS.IdentityProviderData)
doGetIdentityProvider pbs ipId = do
        bsp <- loadBufferedRef pbs
        ips <- loadBufferedRef (bspIdentityProviders bsp)
        return $! IPS.idProviders ips ^? ix ipId

doGetCryptoParams :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m CryptographicParameters
doGetCryptoParams pbs = do
        bsp <- loadBufferedRef pbs
        loadBufferedRef (bspCryptographicParameters bsp)

doGetTransactionOutcome :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.TransactionHash -> m (Maybe ValidResult)
doGetTransactionOutcome pbs transHash = do
        bsp <- loadBufferedRef pbs
        return $! (bspTransactionOutcomes bsp) ^? ix transHash

doSetTransactionOutcomes :: (MonadBlobStore m BlobRef) => PersistentBlockState -> [(Transactions.TransactionHash, ValidResult)] -> m PersistentBlockState
doSetTransactionOutcomes pbs transList = do
        bsp <- loadBufferedRef pbs
        return $! BRMemory bsp{bspTransactionOutcomes = Transactions.transactionOutcomesFromList transList}

doNotifyExecutionCost :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doNotifyExecutionCost pbs amnt = do
        bsp <- loadBufferedRef pbs
        return $! BRMemory bsp{bspBank = bspBank bsp & Rewards.executionCost +~ amnt}

doNotifyIdentityIssuerCredential :: (MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m PersistentBlockState
doNotifyIdentityIssuerCredential pbs idk = do
        bsp <- loadBufferedRef pbs
        return $! BRMemory bsp{bspBank = bspBank bsp & (Rewards.identityIssuersRewards . at idk . non 0) +~ 1}

doGetExecutionCost :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m Amount
doGetExecutionCost pbs = (^. Rewards.executionCost) . bspBank <$> loadBufferedRef pbs

doGetSpecialOutcomes :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m [Transactions.SpecialTransactionOutcome]
doGetSpecialOutcomes pbs = (^. to bspTransactionOutcomes . Transactions.outcomeSpecial) <$> loadBufferedRef pbs

doAddSpecialTransactionOutcome :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.SpecialTransactionOutcome -> m PersistentBlockState
doAddSpecialTransactionOutcome pbs o = do
        bsp <- loadBufferedRef pbs
        return $! BRMemory bsp{bspTransactionOutcomes = bspTransactionOutcomes bsp & Transactions.outcomeSpecial %~ (o:)}

doUpdateSeedState :: (MonadBlobStore m BlobRef) => PersistentBlockState -> SeedState -> m PersistentBlockState
doUpdateSeedState pbs ss = do
        bsp <- loadBufferedRef pbs
        return $! BRMemory bsp{bspBirkParameters = bspBirkParameters bsp & seedState .~ ss}