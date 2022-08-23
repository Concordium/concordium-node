{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Persistent.Instances where

import Data.Word
import Data.Functor.Foldable hiding (Nil)
import Control.Monad
import Control.Monad.Reader
import Data.Serialize
import Data.Bits
import Data.Bifunctor (second)
import qualified Data.Set as Set
import Control.Exception

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm
import Concordium.Utils.Serialization.Put
import Concordium.Utils.Serialization (putByteStringLen)

import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Instance as Transient
import qualified Concordium.GlobalState.Basic.BlockState.InstanceTable as Transient
import Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.BlockState (InstanceInfoTypeV(..), InstanceInfoType(..))
import Concordium.GlobalState.Persistent.CachedRef
import qualified Concordium.GlobalState.Persistent.Cache as Cache
----------------------------------------------------------------------------------------------------

-- |State of a smart contract parametrized by the contract version. This is the
-- persistent version which supports storing and loading the state from a blob
-- store.
data InstanceStateV (v :: Wasm.WasmVersion) where
  InstanceStateV0 :: Wasm.ContractState -> InstanceStateV GSWasm.V0
  InstanceStateV1 :: StateV1.PersistentState -> InstanceStateV GSWasm.V1

-- |The fixed parameters associated with a smart contract instance
data PersistentInstanceParameters = PersistentInstanceParameters {
    -- |Address of the instance
    pinstanceAddress :: !ContractAddress,
    -- |Address of this contract instance owner, i.e., the creator account.
    pinstanceOwner :: !AccountAddress,
    -- |The module that the contract is defined in
    pinstanceContractModule :: !ModuleRef,
    -- |The name of the contract
    pinstanceInitName :: !Wasm.InitName,
    -- |The contract's allowed receive functions.
    pinstanceReceiveFuns :: !(Set.Set Wasm.ReceiveName),
    -- |Hash of the fixed parameters
    pinstanceParameterHash :: !H.Hash
}

instance Show PersistentInstanceParameters where
    show PersistentInstanceParameters{..} = show pinstanceAddress ++ " :: " ++ show pinstanceContractModule ++ "." ++ show pinstanceInitName

instance HashableTo H.Hash PersistentInstanceParameters where
    getHash = pinstanceParameterHash

instance Serialize PersistentInstanceParameters where
    put PersistentInstanceParameters{..} = do
        put pinstanceAddress
        put pinstanceOwner
        put pinstanceContractModule
        put pinstanceInitName
        put pinstanceReceiveFuns
    get = do
        pinstanceAddress <- get
        pinstanceOwner <- get
        pinstanceContractModule <- get
        pinstanceInitName <- get
        pinstanceReceiveFuns <- get
        let pinstanceParameterHash = makeInstanceParameterHash pinstanceAddress pinstanceOwner pinstanceContractModule pinstanceInitName
        return PersistentInstanceParameters{..}

instance MonadBlobStore m => BlobStorable m PersistentInstanceParameters
instance Applicative m => Cacheable m PersistentInstanceParameters

----------------------------------------------------------------------------------------------------

-- |An instance of a smart contract. This is parametrized by the Wasm version
-- `v` that is used to tie the instance version to the module version. At
-- present the version only appears in the module, but with the state changes it
-- will also appear in the contract state.
data PersistentInstanceV (v :: Wasm.WasmVersion) = PersistentInstanceV {
    -- |The fixed parameters of the instance.
    pinstanceParameters :: !(BufferedRef PersistentInstanceParameters),
    -- |The interface of 'pinstanceContractModule'. Note this is a 'HashedCachedRef' to a Module as this
    -- is how the data is stored in the Modules table. A 'Module' carries a BlobRef to the source
    -- but that reference should never be consulted in the scope of Instance operations.
    -- The module will always be of version @v@.
    pinstanceModuleInterface :: !(HashedCachedRef Modules.ModuleCache Modules.Module),
    -- |The current local state of the instance
    pinstanceModel :: !(InstanceStateV v),
    -- |The current amount of GTU owned by the instance
    pinstanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    pinstanceHash :: H.Hash
}

-- |Either a V0 or V1 instance. V1 instance is only allowed in protocol versions
-- P4 and up, however this is not explicit here since it needlessly complicates
-- code. The Scheduler ensures that no V1 instances can be constructed prior to
-- P4 version. This type exists since we need to store all versions of instances
-- in the instance table, as opposed to having multiple instance tables for
-- different instance versions. This is necessary because there is a single
-- address space for all contract instances.
data PersistentInstance (pv :: ProtocolVersion) where
  PersistentInstanceV0 :: PersistentInstanceV GSWasm.V0 -> PersistentInstance pv
  PersistentInstanceV1 :: PersistentInstanceV GSWasm.V1 -> PersistentInstance pv

instance Show (PersistentInstance pv) where
    show (PersistentInstanceV0 PersistentInstanceV {pinstanceModel = InstanceStateV0 model,..}) = show pinstanceParameters ++ " {balance=" ++ show pinstanceAmount ++ ", model=" ++ show model ++ "}"
    show (PersistentInstanceV1 PersistentInstanceV {..}) = show pinstanceParameters ++ " {balance=" ++ show pinstanceAmount ++ "}"

loadInstanceParameters :: MonadBlobStore m => PersistentInstance pv -> m PersistentInstanceParameters
loadInstanceParameters (PersistentInstanceV0 PersistentInstanceV {..}) = loadBufferedRef pinstanceParameters
loadInstanceParameters (PersistentInstanceV1 PersistentInstanceV {..}) = loadBufferedRef pinstanceParameters

cacheInstanceParameters :: MonadBlobStore m => PersistentInstance pv -> m (PersistentInstanceParameters, BufferedRef PersistentInstanceParameters)
cacheInstanceParameters (PersistentInstanceV0 PersistentInstanceV {..}) = cacheBufferedRef pinstanceParameters
cacheInstanceParameters (PersistentInstanceV1 PersistentInstanceV {..}) = cacheBufferedRef pinstanceParameters

loadInstanceModule :: SupportsPersistentModules m => PersistentInstance pv -> m Module
loadInstanceModule (PersistentInstanceV0 PersistentInstanceV {..}) = refLoad pinstanceModuleInterface
loadInstanceModule (PersistentInstanceV1 PersistentInstanceV {..}) = refLoad pinstanceModuleInterface

instance HashableTo H.Hash (PersistentInstance pv) where
    getHash (PersistentInstanceV0 PersistentInstanceV{..})= pinstanceHash
    getHash (PersistentInstanceV1 PersistentInstanceV{..})= pinstanceHash

-- In protocol versions 1, 2, 3 there was only a single instance type. Since
-- there is no accessible versioning when loading the persistent instance, to
-- decide whether we are loading instance V0 or instance V1, we essentially have
-- two implementations of BlobStorable. One for protocol versions <= P3, and
-- another one for later protocol versions. The latter ones add versioning information.
instance (IsProtocolVersion pv, SupportsPersistentModules m) => BlobStorable m (PersistentInstance pv) where
    storeUpdate inst = do
        if demoteProtocolVersion (protocolVersion @pv) <= P3 then
          case inst of
            PersistentInstanceV0 i -> second PersistentInstanceV0 <$> storeUnversionedV0 i
            PersistentInstanceV1 _i -> error "Precondition violation. V1 instances do not exist in protocol versions <= 3."
        else case inst of
            PersistentInstanceV0 i -> addVersion <$> storeUnversionedV0 i
            PersistentInstanceV1 i -> storeV1 i
     where storeUnversionedV0 :: PersistentInstanceV GSWasm.V0 -> m (Put, PersistentInstanceV GSWasm.V0)
           storeUnversionedV0 PersistentInstanceV{pinstanceModel=InstanceStateV0 model,..} = do
             (pparams, newParameters) <- storeUpdate pinstanceParameters
             (pinterface, newpInterface) <- storeUpdate pinstanceModuleInterface
             let putInst = do
                   pparams
                   pinterface
                   put model
                   put pinstanceAmount
             return (putInst, PersistentInstanceV{pinstanceParameters = newParameters, pinstanceModuleInterface = newpInterface,
                              pinstanceModel=InstanceStateV0 model,..})
           storeV1 :: PersistentInstanceV GSWasm.V1 -> m (Put, PersistentInstance pv)
           storeV1 PersistentInstanceV{pinstanceModel=InstanceStateV1 model,..} = do
             (pparams, newParameters) <- storeUpdate pinstanceParameters
             (pinterface, newpInterface) <- storeUpdate pinstanceModuleInterface
             (pstate, newpstate) <- storeUpdate model
             let putInst = do
                   putWord8 1 -- version tag
                   pparams
                   pinterface
                   pstate
                   put pinstanceAmount
             return (putInst, PersistentInstanceV1 PersistentInstanceV{pinstanceParameters = newParameters,
                                                  pinstanceModuleInterface = newpInterface,
                                                  pinstanceModel = InstanceStateV1 newpstate,..})

           addVersion (s, inst') = (putWord8 0 <> s,  PersistentInstanceV0 inst')

    store pinst = fst <$> storeUpdate pinst
    load = do
      if demoteProtocolVersion (protocolVersion @pv) <= P3 then do
        loadIV0
      else
        getWord8 >>= \case
          0 -> loadIV0
          1 -> loadIV1
          n -> error $ "Unsupported persistent instance version " ++ show n
      where 
        loadIV0 = do
          rparams <- load
          rInterface <- load
          model <- get
          pinstanceAmount <- get
          return $ do
              pinstanceParameters <- rparams
              pinstanceModuleInterface <- rInterface
              pip <- loadBufferedRef pinstanceParameters
              let pinstanceModel = InstanceStateV0 model
              let pinstanceHash = makeInstanceHashV0State (pinstanceParameterHash pip) pinstanceModel pinstanceAmount
              return $! PersistentInstanceV0 (PersistentInstanceV {..})
        loadIV1 = do
          rparams <- load
          rInterface <- load
          model <- load
          pinstanceAmount <- get
          return $ do
              pinstanceParameters <- rparams
              pinstanceModuleInterface <- rInterface
              pinstanceModel <- InstanceStateV1 <$> model
              pip <- loadBufferedRef pinstanceParameters
              pinstanceHash <- makeInstanceHashV1State (pinstanceParameterHash pip) pinstanceModel pinstanceAmount
              return $! PersistentInstanceV1 (PersistentInstanceV {..})

instance MonadBlobStore m => Cacheable m (InstanceStateV GSWasm.V1) where
  cache (InstanceStateV1 model) = InstanceStateV1 <$> cache model

-- This cacheable instance is a bit unusual. Caching instances requires us to have access
-- to the modules so that we can share the module interfaces from different instances.
instance SupportsPersistentModules m => Cacheable (ReaderT Modules m) (PersistentInstance pv) where
    cache (PersistentInstanceV0 p@PersistentInstanceV{..}) = do
        modules <- ask
        lift $! do
            -- we only cache parameters and get the interface from the modules
            -- table. The rest is already in memory at this point since the
            -- fields are flat, i.e., without indirection via BufferedRef or
            -- similar reference wrappers.
            ips <- cache pinstanceParameters
            params <- loadBufferedRef ips
            let modref = pinstanceContractModule params
            miface <- Modules.getModuleReference modref modules
            case miface of
              Nothing -> return (PersistentInstanceV0 p{pinstanceParameters = ips}) -- this case should never happen, but it is safe to do this.
              Just iface -> return (PersistentInstanceV0 p{pinstanceModuleInterface = iface, pinstanceParameters = ips})
    cache (PersistentInstanceV1 p@PersistentInstanceV{..}) = do
        modules <- ask
        lift $! do
            -- we only cache parameters and get the interface from the modules
            -- table. The rest is already in memory at this point since the
            -- fields are flat, i.e., without indirection via BufferedRef or
            -- similar reference wrappers.
            ips <- cache pinstanceParameters
            params <- loadBufferedRef ips
            imodel <- cache pinstanceModel
            let modref = pinstanceContractModule params
            miface <- Modules.getModuleReference modref modules
            case miface of
              Nothing -> return (PersistentInstanceV1 p{pinstanceParameters = ips, pinstanceModel = imodel}) -- this case should never happen, but it is safe to do this.
              Just iface -> return (PersistentInstanceV1 p{pinstanceModuleInterface = iface, pinstanceParameters = ips, pinstanceModel = imodel})

-- |Construct instance information from a persistent instance, loading as much
-- data as necessary from persistent storage.
mkInstanceInfo :: SupportsPersistentModules m => PersistentInstance pv -> m (InstanceInfoType InstanceStateV)
mkInstanceInfo (PersistentInstanceV0 inst) = InstanceInfoV0 <$> mkInstanceInfoV inst
mkInstanceInfo (PersistentInstanceV1 inst) = InstanceInfoV1 <$> mkInstanceInfoV inst
mkInstanceInfoV :: (SupportsPersistentModules m, Wasm.IsWasmVersion v) => PersistentInstanceV v -> m (InstanceInfoTypeV InstanceStateV v)
mkInstanceInfoV PersistentInstanceV{..} = do
  PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
  instanceModuleInterface <- moduleVInterface . unsafeToModuleV <$> refLoad pinstanceModuleInterface
  let iiParameters = Transient.InstanceParameters {
        _instanceAddress = pinstanceAddress,
        instanceOwner = pinstanceOwner,
        instanceInitName = pinstanceInitName,
        instanceReceiveFuns = pinstanceReceiveFuns,
        instanceModuleInterface = instanceModuleInterface,
        instanceParameterHash = pinstanceParameterHash
        }
  return InstanceInfoV{
    iiState = pinstanceModel,
    iiBalance = pinstanceAmount,
    ..
  }

-- |Serialize a V0 smart contract instance in V0 format.
putV0InstanceV0 :: (MonadBlobStore m, MonadPut m) => PersistentInstanceV GSWasm.V0 -> m ()
putV0InstanceV0 PersistentInstanceV {pinstanceModel = InstanceStateV0 model,..} = do
        -- Instance parameters
        PersistentInstanceParameters{..} <- refLoad pinstanceParameters
        liftPut $ do
            -- only put the subindex part of the address
            put (contractSubindex pinstanceAddress)
            put pinstanceOwner
            put pinstanceContractModule
            put pinstanceInitName
            put model
            put pinstanceAmount

-- |Serialize a V1 smart contract instance in V0 format.
putV1InstanceV0 :: (MonadBlobStore m, MonadPut m) => PersistentInstanceV GSWasm.V1 -> m ()
putV1InstanceV0 PersistentInstanceV {pinstanceModel = InstanceStateV1 model,..} = do
        -- Instance parameters
        PersistentInstanceParameters{..} <- refLoad pinstanceParameters
        stateString <- StateV1.toByteString model
        liftPut $ do
            -- only put the subindex part of the address
            put (contractSubindex pinstanceAddress)
            put pinstanceOwner
            put pinstanceContractModule
            put pinstanceInitName
            putByteStringLen stateString -- serialize with explicit length to enable serialization via FFI.
            put pinstanceAmount

----------------------------------------------------------------------------------------------------

-- |An alias to document when a hash is intended to be the parameter hash.
type InstanceParametersHash = H.Hash
-- |An alias to document when a hash is intended to be the instance state hash.
type InstanceStateHash = H.Hash
-- |An alias to document when a hash is intended to be the instance hash.
type InstanceHash = H.Hash

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> ModuleRef -> Wasm.InitName -> InstanceParametersHash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHashV0State :: InstanceParametersHash -> InstanceStateV GSWasm.V0 -> Amount -> InstanceHash
makeInstanceHashV0State paramsHash (InstanceStateV0 conState) = makeInstanceHashV0 paramsHash (getHash conState)

makeInstanceHashV0 :: InstanceParametersHash -> InstanceStateHash -> Amount -> InstanceHash
makeInstanceHashV0 paramsHash csHash a = H.hash $ runPut $ do
        put paramsHash
        put csHash
        put a

makeInstanceHashV1State :: MonadBlobStore m => InstanceParametersHash -> InstanceStateV GSWasm.V1 -> Amount -> m InstanceHash
makeInstanceHashV1State paramsHash (InstanceStateV1 conState) a = do
    csHash <- getHashM conState
    return $! makeInstanceHashV1 paramsHash csHash a

makeInstanceHashV1 :: InstanceParametersHash -> H.Hash -> Amount -> InstanceHash
makeInstanceHashV1 paramsHash csHash a =
    H.hash $ runPut $ do
        put paramsHash
        put csHash
        put a

makeBranchHash :: H.Hash -> H.Hash -> H.Hash
makeBranchHash h1 h2 = H.hashShort $! (H.hashToShortByteString h1 <> H.hashToShortByteString h2)

-- |Internal tree nodes of an 'InstanceTable'.
-- Branches satisfy the following invariant properties:
-- * The left branch is always a full sub-tree with height 1 less than the parent (or a leaf if the parent height is 0)
-- * The right branch has height less than the parent
-- * The hash is @computeBranchHash l r@ where @l@ and @r@ are the left and right subtrees
-- * The first @Bool@ is @True@ if the tree is full, i.e. the right sub-tree is full with height 1 less than the parent
-- * The second @Bool@ is @True@ if the either subtree has vacant leaves
data IT pv r
    -- |A branch has the following fields:
    -- * the height of the branch (0 if all children are leaves)
    -- * whether the branch is a full binary tree
    -- * whether the tree has vacant leaves
    -- * the Merkle hash (lazy)
    -- * the left and right subtrees
    = Branch {
        branchHeight :: !Word8,
        branchFull :: !Bool,
        branchHasVacancies :: !Bool,
        branchHash :: H.Hash,
        branchLeft :: r,
        branchRight :: r
    }
    -- |A leaf holds a contract instance
    | Leaf !(PersistentInstance pv)
    -- |A vacant leaf records the 'ContractSubindex' of the last instance
    -- with this 'ContractIndex'.
    | VacantLeaf !ContractSubindex
    deriving (Show, Functor, Foldable, Traversable)

showITString :: IT pv String -> String
showITString (Branch h _ _ _ l r) = show h ++ ":(" ++ l ++ ", " ++ r ++ ")"
showITString (Leaf i) = show i
showITString (VacantLeaf si) = "[Vacant " ++ show si ++ "]"

hasVacancies :: IT pv r -> Bool
hasVacancies Branch{..} = branchHasVacancies
hasVacancies Leaf{} = False
hasVacancies VacantLeaf{} = True

isFull :: IT pv r -> Bool
isFull Branch{..} = branchFull
isFull _ = True

nextHeight :: IT pv r -> Word8
nextHeight Branch{..} = branchHeight + 1
nextHeight _ = 0

instance HashableTo H.Hash (IT pv r) where
    getHash (Branch {..}) = branchHash
    getHash (Leaf i) = getHash i
    getHash (VacantLeaf si) = H.hash $ runPut $ put si

conditionalSetBit :: (Bits a) => Int -> Bool -> a -> a
conditionalSetBit _ False x = x
conditionalSetBit b True x = setBit x b

instance (IsProtocolVersion pv, BlobStorable m r, MonadIO m, Cache.MonadCache ModuleCache m) => BlobStorable m (IT pv r) where
    storeUpdate (Branch {..}) = do
        (pl, l') <- storeUpdate branchLeft
        (pr, r') <- storeUpdate branchRight
        let putBranch = do
                putWord8 $ conditionalSetBit 0 branchFull $
                        conditionalSetBit 1 branchHasVacancies $ 0
                putWord8 branchHeight
                put branchHash
                pl
                pr
        return (putBranch, Branch{branchLeft = l', branchRight = r', ..})
    storeUpdate (Leaf i) = do
        (pinst, i') <- storeUpdate i
        return (putWord8 8 >> pinst, Leaf i')
    storeUpdate v@(VacantLeaf si) = return (putWord8 9 >> put si, v)
    store v = fst <$> storeUpdate v
    load = do
        tag <- getWord8
        if tag < 8 then do
            hgt <- getWord8
            hsh <- get
            ml <- load
            mr <- load
            return $ Branch hgt (testBit tag 0) (testBit tag 1) hsh <$> ml <*> mr
        else if tag == 8 then
            fmap Leaf <$> load
        else -- tag == 9
            return . VacantLeaf <$> get

-- |Fold the instance table using the provided accumulator function. The tree is
-- traversed in-order from left to right and the accumulator is called on each
-- leaf. The accumulator is called with @Left addr@ when the leaf is vacant,
-- i.e. the instance on that address was deleted. It is called with @Right inst@
-- when there is an instance in the spot.
mapReduceIT :: forall a m pv t. (Monoid a, MRecursive m t, Base t ~ IT pv) => (Either ContractAddress (PersistentInstance pv) -> m a) -> t -> m a
mapReduceIT mfun = mr 0 <=< mproject
    where
        mr :: ContractIndex -> IT pv t -> m a
        mr lowIndex (Branch hgt _ _ _ l r) = liftM2 (<>) (mr lowIndex =<< mproject l) (mr (setBit lowIndex (fromIntegral hgt)) =<< mproject r)
        mr _ (Leaf i) = mfun (Right i)
        mr lowIndex (VacantLeaf si) = mfun (Left (ContractAddress lowIndex si))

makeBranch :: Word8 -> Bool -> IT pv t -> IT pv t -> t -> t -> IT pv t
makeBranch branchHeight branchFull l r branchLeft branchRight = Branch{..}
    where
        branchHasVacancies = hasVacancies l || hasVacancies r
        branchHash = makeBranchHash (getHash l) (getHash r)

newContractInstanceIT :: forall m pv t a. (MRecursive m t, MCorecursive m t, Base t ~ IT pv) => (ContractAddress -> m (a, PersistentInstance pv)) -> t -> m (a, t)
newContractInstanceIT mk t0 = (\(res, v) -> (res,) <$> membed v) =<< nci 0 t0 =<< mproject t0
    where
        nci :: ContractIndex -> t -> IT pv t -> m (a, IT pv t)
        -- Insert into a tree with vacancies: insert in left if it has vacancies, otherwise right
        nci offset _ (Branch h f True _ l r) = do
            projl <- mproject l
            projr <- mproject r
            if branchHasVacancies projl then do
                (res, projl') <- nci offset l projl
                l' <- membed projl'
                return (res, makeBranch h f projl' projr l' r)
            else assert (branchHasVacancies projr) $ do
                (res, projr') <- nci (setBit offset (fromIntegral h)) r projr
                r' <- membed projr'
                return (res, makeBranch h f projl projr' l r')
        -- Insert into full tree with no vacancies: create new branch at top level
        nci offset l projl@(Branch h True False _ _ _) = do
            (res, projr) <- leaf (setBit offset (fromIntegral $ h+1)) 0
            r <- membed projr
            return (res, makeBranch (h+1) False projl projr l r)
        -- Insert into non-full tree with no vacancies: insert on right subtree (invariant implies left is full, but can add to right)
        nci offset _ (Branch h False False _ l r) = do
            projl <- mproject l
            projr <- mproject r
            (res, projr') <- nci (setBit offset (fromIntegral h)) r projr
            r' <- membed projr'
            return (res, makeBranch h (isFull projr' && nextHeight projr' == h) projl projr' l r')
        -- Insert at leaf: create a new branch
        nci offset l projl@Leaf{} = do
            (res, projr) <- leaf (setBit offset 0) 0
            r <- membed projr
            return (res, makeBranch 0 True projl projr l r)
        -- Insert at a vacant leaf: create leaf with next subindex
        nci offset _ (VacantLeaf si) = leaf offset (succ si)
        leaf ind subind = do
            (res, c) <- mk (ContractAddress ind subind)
            return (res, Leaf c)


data Instances pv
    -- |The empty instance table
    = InstancesEmpty
    -- |A non-empty instance table (recording the size)
    | InstancesTree !Word64 !(BufferedFix (IT pv))

instance Show (Instances pv) where
    show InstancesEmpty = "Empty"
    show (InstancesTree _ t) = showFix showITString t

instance (IsProtocolVersion pv, SupportsPersistentModules m) => MHashableTo m H.Hash (Instances pv) where
  getHashM InstancesEmpty = return $ H.hash "EmptyInstances"
  getHashM (InstancesTree _ t) = getHash <$> mproject t

instance (IsProtocolVersion pv, SupportsPersistentModules m) => BlobStorable m (Instances pv) where
    storeUpdate i@InstancesEmpty = return (putWord8 0, i)
    storeUpdate (InstancesTree s t) = do
        (pt, t') <- storeUpdate t
        return (putWord8 1 >> put s >> pt, InstancesTree s t')
    store i = fst <$> storeUpdate i
    load = do
        tag <- getWord8
        if tag == 0 then
            return (return InstancesEmpty)
        else do
            s <- get
            fmap (InstancesTree s) <$> load

instance (MonadBlobStore m, Cacheable m r, Cacheable m (PersistentInstance pv)) => Cacheable m (IT pv r) where
    cache Branch{..} = do
        branchLeft' <- cache branchLeft
        branchRight' <- cache branchRight
        return Branch {branchLeft = branchLeft', branchRight = branchRight', ..}
    cache (Leaf l) = Leaf <$> cache l
    cache vacant = return vacant

instance (IsProtocolVersion pv, SupportsPersistentModules m) => Cacheable (ReaderT Modules m) (Instances pv) where
    cache i@InstancesEmpty = return i
    cache (InstancesTree s r) = InstancesTree s <$> cache r
            

emptyInstances :: Instances pv
emptyInstances = InstancesEmpty

newContractInstance :: forall m pv a. (IsProtocolVersion pv, SupportsPersistentModules m) => (ContractAddress -> m (a, PersistentInstance pv)) -> Instances pv -> m (a, Instances pv)
newContractInstance fnew InstancesEmpty = do
        let ca = ContractAddress 0 0
        (res, newInst) <- fnew ca
        (res,) . InstancesTree 1 <$> membed (Leaf newInst)
newContractInstance fnew (InstancesTree s it) = (\(res, it') -> (res, InstancesTree (s+1) it')) <$> newContractInstanceIT fnew it

deleteContractInstance :: forall m pv. (IsProtocolVersion pv, SupportsPersistentModules m) => ContractAddress -> Instances pv -> m (Instances pv)
deleteContractInstance _ InstancesEmpty = return InstancesEmpty
deleteContractInstance addr t0@(InstancesTree s it0) = dci (fmap (InstancesTree (s-1)) . membed) (contractIndex addr) =<< mproject it0
    where
        dci succCont i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadInstanceParameters inst
                if addr == aaddr then
                    succCont (VacantLeaf $ contractSubindex aaddr)
                else
                    return t0
            | otherwise = return t0
        dci _ _ VacantLeaf{} = return t0
        dci succCont i (Branch h f _ _ l r)
            | i < 2^h =
                let newCont projl' = do
                        projr <- mproject r
                        l' <- membed projl'
                        succCont $! makeBranch h f projl' projr l' r
                in dci newCont i =<< mproject l
            | i < 2^(h+1) =
                let newCont projr' = do
                        projl <- mproject l
                        r' <- membed projr'
                        succCont $! makeBranch h f projl projr' l r'
                in dci newCont (i - 2^h) =<< mproject r
            | otherwise = return t0

lookupContractInstance :: forall m pv. (IsProtocolVersion pv, SupportsPersistentModules m) => ContractAddress -> Instances pv -> m (Maybe (PersistentInstance pv))
lookupContractInstance _ InstancesEmpty = return Nothing
lookupContractInstance addr (InstancesTree _ it0) = lu (contractIndex addr) =<< mproject it0
    where
        lu i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadInstanceParameters inst
                return $! if addr == aaddr then Just inst else Nothing
            | otherwise = return Nothing
        lu _ VacantLeaf{} = return Nothing
        lu i (Branch h _ _ _ l r)
            | i < 2^h = lu i =<< mproject l
            | i < 2^(h+1) = lu (i - 2^h) =<< mproject r
            | otherwise = return Nothing

updateContractInstance :: forall m pv a. (IsProtocolVersion pv, SupportsPersistentModules m) => (PersistentInstance pv -> m (a, PersistentInstance pv)) -> ContractAddress -> Instances pv -> m (Maybe (a, Instances pv))
updateContractInstance _ _ InstancesEmpty = return Nothing
updateContractInstance fupd addr (InstancesTree s it0) = upd baseSuccess (contractIndex addr) =<< mproject it0
    where
        baseSuccess res itproj = do
            it <- membed itproj
            return $ Just (res, InstancesTree s it)
        upd succCont i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadInstanceParameters inst
                if addr == aaddr then do
                    (res, inst') <- fupd inst
                    succCont res (Leaf inst')
                else
                    return Nothing
            | otherwise = return Nothing
        upd _ _ VacantLeaf{} = return Nothing
        upd succCont i (Branch h f _ _ l r)
            | i < 2^h =
                let newCont res projl' = do
                        projr <- mproject r
                        l' <- membed projl'
                        succCont res $! makeBranch h f projl' projr l' r
                in upd newCont i =<< mproject l
            | i < 2^(h+1) =
                let newCont res projr' = do
                        projl <- mproject l
                        r' <- membed projr'
                        succCont res $! makeBranch h f projl projr' l r'
                in upd newCont (i - 2^h) =<< mproject r
            | otherwise = return Nothing

-- |Retrieve the list of all instance addresses. The addresses are returned in increasing order.
allInstances :: forall m pv. (IsProtocolVersion pv, SupportsPersistentModules m) => Instances pv -> m [ContractAddress]
allInstances InstancesEmpty = return []
allInstances (InstancesTree _ it) = mapReduceIT mfun it
    where
        mfun (Left _) = return mempty
        mfun (Right inst) = (:[]) . pinstanceAddress <$> loadInstanceParameters inst

makePersistent :: forall m pv. SupportsPersistentModules m => Modules.Modules -> Transient.Instances -> m (Instances pv)
makePersistent _ (Transient.Instances Transient.Empty) = return InstancesEmpty
makePersistent mods (Transient.Instances (Transient.Tree s t)) = InstancesTree s <$> conv t
    where
        conv :: Transient.IT -> m (BufferedFix (IT pv))
        conv (Transient.Branch lvl fll vac hsh l r) = do
            l' <- conv l
            r' <- conv r
            membed (Branch lvl fll vac hsh l' r')
        conv (Transient.Leaf i) = convInst i >>= membed . Leaf
        conv (Transient.VacantLeaf si) = membed (VacantLeaf si)
        convInst (Transient.InstanceV0 Transient.InstanceV {_instanceVParameters=Transient.InstanceParameters{..},
                                                            _instanceVModel=Transient.InstanceStateV0 transientModel,..}) = do
            pIParams <- makeBufferedRef $ PersistentInstanceParameters{
                pinstanceAddress = _instanceAddress,
                pinstanceOwner = instanceOwner,
                pinstanceContractModule = GSWasm.miModuleRef instanceModuleInterface,
                pinstanceInitName = instanceInitName,
                pinstanceParameterHash = instanceParameterHash,
                pinstanceReceiveFuns = instanceReceiveFuns
            }
            -- This pattern is irrefutable because if the instance exists in the Basic version,
            -- then the module must be present in the persistent implementation.
            ~(Just pIModuleInterface) <- Modules.getModuleReference (GSWasm.miModuleRef instanceModuleInterface) mods
            return $ PersistentInstanceV0 PersistentInstanceV {
                pinstanceParameters = pIParams,
                pinstanceModuleInterface = pIModuleInterface,
                pinstanceModel = InstanceStateV0 transientModel,
                pinstanceAmount = _instanceVAmount,
                pinstanceHash = _instanceVHash
            }
        convInst (Transient.InstanceV1 Transient.InstanceV {_instanceVParameters=Transient.InstanceParameters{..}, 
                                                            _instanceVModel=Transient.InstanceStateV1 transientModel,..}) = do
            pIParams <- makeBufferedRef $ PersistentInstanceParameters{
                pinstanceAddress = _instanceAddress,
                pinstanceOwner = instanceOwner,
                pinstanceContractModule = GSWasm.miModuleRef instanceModuleInterface,
                pinstanceInitName = instanceInitName,
                pinstanceParameterHash = instanceParameterHash,
                pinstanceReceiveFuns = instanceReceiveFuns
            }
            -- This pattern is irrefutable because if the instance exists in the Basic version,
            -- then the module must be present in the persistent implementation.
            ~(Just pIModuleInterface) <- Modules.getModuleReference (GSWasm.miModuleRef instanceModuleInterface) mods
            return $ PersistentInstanceV1 PersistentInstanceV {
                pinstanceParameters = pIParams,
                pinstanceModuleInterface = pIModuleInterface,
                pinstanceModel = InstanceStateV1 (StateV1.makePersistent transientModel),
                pinstanceAmount = _instanceVAmount,
                pinstanceHash = _instanceVHash
            }

-- |Serialize instances in V0 format.
putInstancesV0 :: (IsProtocolVersion pv, SupportsPersistentModules m, MonadPut m) => Instances pv -> m ()
putInstancesV0 InstancesEmpty = liftPut $ putWord8 0
putInstancesV0 (InstancesTree _ it) = do
        mapReduceIT putOptInstance it
        liftPut $ putWord8 0
    where
        putOptInstance (Left ca) = liftPut $ do
            putWord8 1
            put (contractSubindex ca)
        putOptInstance (Right (PersistentInstanceV0 inst)) = do
            liftPut $ putWord8 2
            putV0InstanceV0 inst
        putOptInstance (Right (PersistentInstanceV1 inst)) = do
            liftPut $ putWord8 3
            putV1InstanceV0 inst
