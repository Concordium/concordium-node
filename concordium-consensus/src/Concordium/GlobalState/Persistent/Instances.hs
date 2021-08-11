{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Persistent.Instances where

import Data.Word
import Data.Functor.Foldable hiding (Nil)
import Control.Monad
import Control.Monad.IO.Class
import Data.Serialize
import Data.Bits
import qualified Data.Set as Set
import Control.Exception

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm
import Concordium.Utils.Serialization.Put

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.Types.Instance as Transient
import qualified Concordium.GlobalState.Basic.BlockState.InstanceTable as Transient
import Concordium.GlobalState.Persistent.BlockState.Modules
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules

----------------------------------------------------------------------------------------------------

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

-- |An instance of a smart contract.
data PersistentInstance = PersistentInstance {
    -- |The fixed parameters of the instance
    pinstanceParameters :: !(BufferedRef PersistentInstanceParameters),
    -- |The interface of 'pinstanceContractModule'. Note this is a BufferedRef to a Module as this
    -- is how the data is stored in the Modules table. A 'Module' carries a BlobRef to the source
    -- but that reference should never be consulted in the scope of Instance operations.
    pinstanceModuleInterface :: !(BufferedRef Module),
    -- |The current local state of the instance
    pinstanceModel :: !Wasm.ContractState,
    -- |The current amount of GTU owned by the instance
    pinstanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    pinstanceHash :: H.Hash
}

instance Show PersistentInstance where
    show PersistentInstance{..} = show pinstanceParameters ++ " {balance=" ++ show pinstanceAmount ++ ", model=" ++ show pinstanceModel ++ "}"

instance HashableTo H.Hash PersistentInstance where
    getHash = pinstanceHash

instance MonadBlobStore m => BlobStorable m PersistentInstance where
    storeUpdate PersistentInstance{..} = do
        (pparams, newParameters) <- storeUpdate pinstanceParameters
        (pinterface, newpInterface) <- storeUpdate pinstanceModuleInterface
        let putInst = do
                pparams
                pinterface
                put pinstanceModel
                put pinstanceAmount
        return (putInst, PersistentInstance{pinstanceParameters = newParameters, pinstanceModuleInterface = newpInterface, ..})
    store pinst = fst <$> storeUpdate pinst
    load = do
        rparams <- load
        rInterface <- load
        pinstanceModel <- get
        pinstanceAmount <- get
        return $ do
            pinstanceParameters <- rparams
            pinstanceModuleInterface <- rInterface
            pip <- loadBufferedRef pinstanceParameters
            let pinstanceHash = makeInstanceHash pip pinstanceModel pinstanceAmount
            return PersistentInstance{..}

instance MonadBlobStore m => Cacheable m PersistentInstance where
    cache p@PersistentInstance{..} = do
        -- TODO: We do not currently cache the pinstanceCachedParameters.
        -- This behaviour is probably fine.
        ips <- cache pinstanceParameters
        return p{pinstanceParameters = ips}

fromPersistentInstance ::  MonadBlobStore m => PersistentInstance -> m Transient.Instance
fromPersistentInstance PersistentInstance{..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    instanceModuleInterface <- interface <$> loadBufferedRef pinstanceModuleInterface
    let instanceParameters = Transient.InstanceParameters {
            instanceAddress = pinstanceAddress,
            instanceOwner = pinstanceOwner,
            instanceContractModule = pinstanceContractModule,
            instanceInitName = pinstanceInitName,
            instanceReceiveFuns = pinstanceReceiveFuns,
            instanceModuleInterface = instanceModuleInterface,
            instanceParameterHash = pinstanceParameterHash
        }
    return Transient.Instance{ instanceModel = pinstanceModel,
            instanceAmount = pinstanceAmount,
            instanceHash = pinstanceHash,
            ..
         }

-- |Serialize a smart contract instance in V0 format.
putInstanceV0 :: (MonadBlobStore m, MonadPut m) => PersistentInstance -> m ()
putInstanceV0 PersistentInstance{..} = do
        -- Instance parameters
        PersistentInstanceParameters{..} <- refLoad pinstanceParameters
        liftPut $ do
            -- only put the subindex part of the address
            put (contractSubindex pinstanceAddress)
            put pinstanceOwner
            put pinstanceContractModule
            put pinstanceInitName
            put pinstanceModel
            put pinstanceAmount

----------------------------------------------------------------------------------------------------

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> ModuleRef -> Wasm.InitName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHash :: PersistentInstanceParameters -> Wasm.ContractState -> Amount -> H.Hash
makeInstanceHash params conState a = H.hashLazy $ runPutLazy $ do
        put (pinstanceParameterHash params)
        putByteString (H.hashToByteString (getHash conState))
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
data IT r
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
    | Leaf !PersistentInstance
    -- |A vacant leaf records the 'ContractSubindex' of the last instance
    -- with this 'ContractIndex'.
    | VacantLeaf !ContractSubindex
    deriving (Show, Functor, Foldable, Traversable)

showITString :: IT String -> String
showITString (Branch h _ _ _ l r) = show h ++ ":(" ++ l ++ ", " ++ r ++ ")"
showITString (Leaf i) = show i
showITString (VacantLeaf si) = "[Vacant " ++ show si ++ "]"

hasVacancies :: IT r -> Bool
hasVacancies Branch{..} = branchHasVacancies
hasVacancies Leaf{} = False
hasVacancies VacantLeaf{} = True

isFull :: IT r -> Bool
isFull Branch{..} = branchFull
isFull _ = True

nextHeight :: IT r -> Word8
nextHeight Branch{..} = branchHeight + 1
nextHeight _ = 0

instance HashableTo H.Hash (IT r) where
    getHash (Branch {..}) = branchHash
    getHash (Leaf i) = getHash i
    getHash (VacantLeaf si) = H.hash $ runPut $ put si

conditionalSetBit :: (Bits a) => Int -> Bool -> a -> a
conditionalSetBit _ False x = x
conditionalSetBit b True x = setBit x b

instance (BlobStorable m r, MonadIO m) => BlobStorable m (IT r) where
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

mapReduceIT :: forall a m t. (Monoid a, MRecursive m t, Base t ~ IT) => (Either ContractAddress PersistentInstance -> m a) -> t -> m a
mapReduceIT mfun = mr 0 <=< mproject
    where
        mr :: ContractIndex -> IT t -> m a
        mr lowIndex (Branch hgt _ _ _ l r) = liftM2 (<>) (mr lowIndex =<< mproject l) (mr (setBit lowIndex (fromIntegral hgt)) =<< mproject r)
        mr _ (Leaf i) = mfun (Right i)
        mr lowIndex (VacantLeaf si) = mfun (Left (ContractAddress lowIndex si))

makeBranch :: Word8 -> Bool -> IT t -> IT t -> t -> t -> IT t
makeBranch branchHeight branchFull l r branchLeft branchRight = Branch{..}
    where
        branchHasVacancies = hasVacancies l || hasVacancies r
        branchHash = makeBranchHash (getHash l) (getHash r)

newContractInstanceIT :: forall m t a. (MRecursive m t, MCorecursive m t, Base t ~ IT) => (ContractAddress -> m (a, PersistentInstance)) -> t -> m (a, t)
newContractInstanceIT mk t0 = (\(res, v) -> (res,) <$> membed v) =<< nci 0 t0 =<< mproject t0
    where
        nci :: ContractIndex -> t -> IT t -> m (a, IT t)
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


data Instances
    -- |The empty instance table
    = InstancesEmpty
    -- |A non-empty instance table (recording the size)
    | InstancesTree !Word64 !(BufferedBlobbed BlobRef IT)

instance Show Instances where
    show InstancesEmpty = "Empty"
    show (InstancesTree _ t) = showFix showITString t

instance MonadBlobStore m => MHashableTo m H.Hash Instances where
  getHashM InstancesEmpty = return $ H.hash "EmptyInstances"
  getHashM (InstancesTree _ t) = getHash <$> mproject t

instance (MonadBlobStore m) => BlobStorable m Instances where
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

instance (MonadBlobStore m) => Cacheable m Instances where
    cache i@InstancesEmpty = return i
    cache (InstancesTree s r) = InstancesTree s <$> cacheBufferedBlobbed cacheIT r
        where
            cacheIT (Leaf l) = Leaf <$> cache l
            cacheIT it = return it

emptyInstances :: Instances
emptyInstances = InstancesEmpty

newContractInstance :: forall m a. MonadBlobStore m => (ContractAddress -> m (a, PersistentInstance)) -> Instances -> m (a, Instances)
newContractInstance fnew InstancesEmpty = do
        let ca = ContractAddress 0 0
        (res, newInst) <- fnew ca
        (res,) . InstancesTree 1 <$> membed (Leaf newInst)
newContractInstance fnew (InstancesTree s it) = (\(res, it') -> (res, InstancesTree (s+1) it')) <$> newContractInstanceIT fnew it

deleteContractInstance :: forall m. MonadBlobStore m => ContractAddress -> Instances -> m Instances
deleteContractInstance _ InstancesEmpty = return InstancesEmpty
deleteContractInstance addr t0@(InstancesTree s it0) = dci (fmap (InstancesTree (s-1)) . membed) (contractIndex addr) =<< mproject it0
    where
        dci succCont i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadBufferedRef (pinstanceParameters inst)
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

lookupContractInstance :: forall m. MonadBlobStore m => ContractAddress -> Instances -> m (Maybe PersistentInstance)
lookupContractInstance _ InstancesEmpty = return Nothing
lookupContractInstance addr (InstancesTree _ it0) = lu (contractIndex addr) =<< mproject it0
    where
        lu i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadBufferedRef (pinstanceParameters inst)
                return $! if addr == aaddr then Just inst else Nothing
            | otherwise = return Nothing
        lu _ VacantLeaf{} = return Nothing
        lu i (Branch h _ _ _ l r)
            | i < 2^h = lu i =<< mproject l
            | i < 2^(h+1) = lu (i - 2^h) =<< mproject r
            | otherwise = return Nothing

updateContractInstance :: forall m a. MonadBlobStore m => (PersistentInstance -> m (a, PersistentInstance)) -> ContractAddress -> Instances -> m (Maybe (a, Instances))
updateContractInstance _ _ InstancesEmpty = return Nothing
updateContractInstance fupd addr (InstancesTree s it0) = upd baseSuccess (contractIndex addr) =<< mproject it0
    where
        baseSuccess res itproj = do
            it <- membed itproj
            return $ Just (res, InstancesTree s it)
        upd succCont i (Leaf inst)
            | i == 0 = do
                aaddr <- pinstanceAddress <$> loadBufferedRef (pinstanceParameters inst)
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

allInstances :: forall m. MonadBlobStore m => Instances -> m [PersistentInstance]
allInstances InstancesEmpty = return []
allInstances (InstancesTree _ it) = mapReduceIT mfun it
    where
        mfun (Left _) = return mempty
        mfun (Right inst) = return [inst]

makePersistent :: forall m. MonadBlobStore m => Modules.Modules -> Transient.Instances -> m Instances
makePersistent _ (Transient.Instances Transient.Empty) = return InstancesEmpty
makePersistent mods (Transient.Instances (Transient.Tree s t)) = InstancesTree s <$> conv t
    where
        conv :: Transient.IT -> m (BufferedBlobbed BlobRef IT)
        conv (Transient.Branch lvl fll vac hsh l r) = do
            l' <- conv l
            r' <- conv r
            makeBufferedBlobbed (Branch lvl fll vac hsh l' r')
        conv (Transient.Leaf i) = convInst i >>= makeBufferedBlobbed . Leaf
        conv (Transient.VacantLeaf si) = makeBufferedBlobbed (VacantLeaf si)
        convInst Transient.Instance{instanceParameters=Transient.InstanceParameters{..}, ..} = do
            pIParams <- makeBufferedRef $ PersistentInstanceParameters{
                pinstanceAddress = instanceAddress,
                pinstanceOwner = instanceOwner,
                pinstanceContractModule = instanceContractModule,
                pinstanceInitName = instanceInitName,
                pinstanceParameterHash = instanceParameterHash,
                pinstanceReceiveFuns = instanceReceiveFuns
            }
            -- This pattern is irrefutable because if the instance exists in the Basic version,
            -- then the module must be present in the persistent implementation.
            ~(Just pIModuleInterface) <- Modules.getModuleReference (Wasm.miModuleRef instanceModuleInterface) mods
            return $ PersistentInstance {
                pinstanceParameters = pIParams,
                pinstanceModuleInterface = pIModuleInterface,
                pinstanceModel = instanceModel,
                pinstanceAmount = instanceAmount,
                pinstanceHash = instanceHash
            }

-- |Serialize instances in V0 format.
putInstancesV0 :: (MonadBlobStore m, MonadPut m) => Instances -> m ()
putInstancesV0 InstancesEmpty = liftPut $ putWord8 0
putInstancesV0 (InstancesTree _ it) = do
        mapReduceIT putOptInstance it
        liftPut $ putWord8 0
    where
        putOptInstance (Left ca) = liftPut $ do
            putWord8 1
            put (contractSubindex ca)
        putOptInstance (Right inst) = do
            liftPut $ putWord8 2
            putInstanceV0 inst
