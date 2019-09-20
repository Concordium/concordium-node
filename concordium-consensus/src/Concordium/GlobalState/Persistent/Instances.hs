{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, DerivingVia, DerivingStrategies, MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, LambdaCase, TupleSections, FlexibleContexts, DefaultSignatures, DeriveFoldable, DeriveTraversable, FlexibleInstances, QuantifiedConstraints, UndecidableInstances, StandaloneDeriving, RecordWildCards #-}
module Concordium.GlobalState.Persistent.Instances where

import qualified Data.Vector as V
import Data.List
import Data.Word
import Data.Functor.Foldable hiding (Nil)
import Data.Bifunctor
import Control.Monad
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Void
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict(HashMap)
import Data.Bits

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces


data CachableInstanceParameters = CachableInstanceParameters {
    -- |The contract's receive function
    pinstanceReceiveFun :: !(LinkedExpr Void),
    -- |The interface of 'instanceContractModule'
    pinstanceModuleInterface :: !(Interface Core.UA),
    -- |The value interface of 'instanceContractModule'
    pinstanceModuleValueInterface :: !(UnlinkedValueInterface Void),
    -- |The type of messages the contract receive function supports
    pinstanceMessageType :: !(Core.Type Core.UA Core.ModuleRef),
    -- |Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it here
    -- simplifies things.
    pinstanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) (LinkedImplementsValue Void))
}

-- |The fixed parameters associated with a smart contract instance
data PersistentInstanceParameters = PersistentInstanceParameters {
    -- |Address of the instance
    pinstanceAddress :: !ContractAddress,
    -- |Address of this contract instance owner, i.e., the creator account.
    pinstanceOwner :: !AccountAddress,
    -- |The module that the contract is defined in
    pinstanceContractModule :: !Core.ModuleRef,
    -- |The name of the contract
    pinstanceContract :: !Core.TyName,
    -- |Hash of the fixed parameters
    pinstanceParameterHash :: !H.Hash
}

instance Show PersistentInstanceParameters where
    show PersistentInstanceParameters{..} = show pinstanceAddress ++ " :: " ++ show pinstanceContractModule ++ "." ++ show pinstanceContract

instance HashableTo H.Hash PersistentInstanceParameters where
    getHash = pinstanceParameterHash

instance Serialize PersistentInstanceParameters where
    put PersistentInstanceParameters{..} = do
        put pinstanceAddress
        put pinstanceOwner
        put pinstanceContractModule
        put pinstanceContract
    get = do
        pinstanceAddress <- get
        pinstanceOwner <- get
        pinstanceContractModule <- get
        pinstanceContract <- get
        let pinstanceParameterHash = makeInstanceParameterHash pinstanceAddress pinstanceOwner pinstanceContractModule pinstanceContract
        return PersistentInstanceParameters{..}

instance (MonadBlobStore m ref) => BlobStorable m ref PersistentInstanceParameters


-- |An instance of a smart contract.
data PersistentInstance = PersistentInstance {
    -- |The fixed parameters of the instance
    pinstanceParameters :: !(BufferedRef PersistentInstanceParameters),
    -- |The fixed parameters that might be cached
    pinstanceCachedParameters :: !(Nullable CachableInstanceParameters),
    -- |The current local state of the instance
    pinstanceModel :: !(Value Void),
    -- |The current amount of GTU owned by the instance
    pinstanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    pinstanceHash :: H.Hash
}

instance Show PersistentInstance where
    show PersistentInstance{..} = show pinstanceParameters ++ " {balance=" ++ show pinstanceAmount ++ ", model=" ++ show pinstanceModel ++ "}"

instance HashableTo H.Hash PersistentInstance where
    getHash = pinstanceHash

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef PersistentInstance where
    storeUpdate p PersistentInstance{..} = do
        (pparams, newParameters) <- storeUpdate p pinstanceParameters
        let putInst = do
            pparams
            putStorable pinstanceModel
            put pinstanceAmount
        return (putInst, PersistentInstance{pinstanceParameters = newParameters, ..})
    store p pinst = fst <$> storeUpdate p pinst
    load p = do
        rparams <- load p
        pinstanceModel <- getStorable
        pinstanceAmount <- get
        return $ do
            pinstanceParameters <- rparams
            pip <- loadBufferedRef pinstanceParameters
            let pinstanceCachedParameters = Null
            let pinstanceHash = makeInstanceHash pip pinstanceModel pinstanceAmount
            return PersistentInstance{..}



makeInstanceParameterHash :: ContractAddress -> AccountAddress -> Core.ModuleRef -> Core.TyName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHash :: PersistentInstanceParameters -> Value Void -> Amount -> H.Hash
makeInstanceHash params v a = H.hashLazy $ runPutLazy $ do
        put (pinstanceParameterHash params)
        putStorable v
        put a

-- |Internal tree nodes of an 'InstanceTable'.
-- Branches satisfy the following invariant properties:
-- * The left branch is always a full sub-tree with height 1 less than the parent (or a leaf if the parent height is 0)
-- * The right branch has height less than the parent
-- * The hash is @computeBranchHash l r@ where @l@ and @r@ are the left and right subtrees
-- * The first @Bool@ is @True@ if the tree is full, i.e. the right sub-tree is full with height 1 less than the parent
-- * The second @Bool@ is @True@ if the tree has vacant leaves: either @hasVacancies l@ or @hasVacancies r@ is @True@
data IT r
    -- |A branch has the following fields:
    -- * the height of the branch (0 if all children are leaves)
    -- * whether the branch is a full binary tree
    -- * whether the tree has vacant leaves
    -- * the Merkle hash (lazy)
    -- * the left and right subtrees
    = Branch !Word8 !Bool !Bool H.Hash r r
    -- |A leaf holds a contract instance
    | Leaf !PersistentInstance
    -- |A vacant leaf records the 'ContractSubindex' of the last instance
    -- with this 'ContractIndex'.
    | VacantLeaf !ContractSubindex
    deriving (Show, Functor, Foldable, Traversable)

instance HashableTo H.Hash (IT r) where
    getHash (Branch _ _ _ h _ _) = h
    getHash (Leaf i) = getHash i
    getHash (VacantLeaf si) = H.hash $ runPut $ put si

instance (BlobStorable m BlobRef r) => BlobStorable m BlobRef (IT r) where
    storeUpdate p (Branch hgt fll vac hsh l r) = do
        (pl, l') <- storeUpdate p l
        (pr, r') <- storeUpdate p r
        let putBranch = do
            putWord8 $ case (fll, vac) of
                (False, False) -> 0
                (True, False) -> 1
                (False, True) -> 2
                (True, True) -> 3
            putWord8 hgt
            put hsh
            pl
            pr
        return (putBranch, Branch hgt fll vac hsh l' r')
    storeUpdate p (Leaf i) = do
        (pi, i') <- storeUpdate p i
        return (putWord8 4 >> pi, Leaf i')
    storeUpdate p v@(VacantLeaf si) = return (putWord8 5 >> put si, v)
    store p v = fst <$> storeUpdate p v
    load p = do
        tag <- getWord8
        if tag < 4 then do
            hgt <- getWord8
            hsh <- get
            ml <- load p
            mr <- load p
            return $ Branch hgt (testBit tag 0) (testBit tag 1) hsh <$> ml <*> mr
        else if tag == 4 then
            fmap Leaf <$> load p
        else -- tag == 5
            return . VacantLeaf <$> get
            


        