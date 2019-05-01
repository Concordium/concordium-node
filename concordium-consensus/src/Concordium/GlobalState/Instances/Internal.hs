{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Instances.Internal where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces

import Data.Word
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import Data.Serialize
import Data.HashMap.Strict(HashMap)

-- |The fixed parameters associated with a smart contract instance
data InstanceParameters = InstanceParameters {
    -- |Address of the instance
    instanceAddress :: !ContractAddress,
    -- |The module that the contract is defined in
    instanceContractModule :: !Core.ModuleRef,
    -- |The name of the contract
    instanceContract :: !Core.TyName,
    -- |The contract's receive function
    instanceReceiveFun :: !Expr,
    -- |The interface of 'instanceContractModule'
    instanceModuleInterface :: !Interface,
    -- |The value interface of 'instanceContractModule'
    instanceModuleValueInterface :: !ValueInterface,
    -- |The type of messages the contract receive function supports
    instanceMessageType :: !(Core.Type Core.ModuleRef),
    -- |Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it here
    -- simplifies things.
    instanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) ImplementsValue),
    -- |Hash of the fixed parameters
    instanceParameterHash :: !H.Hash
}

instance Show InstanceParameters where
    show InstanceParameters{..} = show instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceContract

instance HashableTo H.Hash InstanceParameters where
    getHash = instanceParameterHash

-- |An instance of a smart contract.
data Instance = Instance {
    -- |The fixed parameters of the instance
    instanceParameters :: !InstanceParameters,
    -- |The current local state of the instance
    instanceModel :: !Value,
    -- |The current amount of GTU owned by the instance
    instanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    instanceHash :: H.Hash
}

instance Show Instance where
    show Instance{..} = show instanceParameters ++ " {balance=" ++ show instanceAmount ++ ", model=" ++ show instanceModel ++ "}"

instance HashableTo H.Hash Instance where
    getHash = instanceHash

makeInstanceParameterHash :: ContractAddress -> Core.ModuleRef -> Core.TyName -> H.Hash
makeInstanceParameterHash ca modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put modRef
        put conName

makeInstanceHash :: InstanceParameters -> Value -> Amount -> H.Hash
makeInstanceHash params v a = H.hashLazy $ runPutLazy $ do
        put (instanceParameterHash params)
        putStorable v
        put a

data InstanceTable 
    -- |The empty instance table
    = Empty
    -- |A non-empty instance table
    | Tree !IT
    deriving (Show)

computeBranchHash :: IT -> IT -> H.Hash
computeBranchHash t1 t2 = H.hash $ runPut $ put (getHash t1 :: H.Hash) <> put (getHash t2 :: H.Hash)

-- |Internal tree nodes of an 'InstanceTable'.
-- Branches satisfy the following invariant properties:
-- * The left branch is always a full sub-tree with height 1 less than the parent (or a leaf if the parent height is 0)
-- * The right branch has height less than the parent
-- * The hash is @computeBranchHash l r@ where @l@ and @r@ are the left and right subtrees
-- * The first @Bool@ is @True@ if the tree is full, i.e. the right sub-tree is full with height 1 less than the parent
-- * The second @Bool@ is @True@ if the tree has vacant leaves: either @hasVacancies l@ or @hasVacancies r@ is @True@
data IT
    -- |A branch has the following fields:
    -- * the height of the branch (0 if all children are leaves)
    -- * whether the branch is a full binary tree
    -- * whether the tree has vacant leaves
    -- * the Merkle hash (lazy)
    -- * the left and right subtrees
    = Branch !Word8 !Bool !Bool H.Hash IT IT
    -- |A leaf holds a contract instance
    | Leaf !Instance
    -- |A vacant leaf records the 'ContractSubindex' of the last instance
    -- with this 'ContractIndex'.
    | VacantLeaf !ContractSubindex
    deriving (Show)

instance HashableTo H.Hash IT where
    getHash (Branch _ _ _ h _ _) = h
    getHash (Leaf i) = getHash i
    getHash (VacantLeaf si) = H.hash $ runPut $ put si

instance HashableTo H.Hash InstanceTable where
    -- The hash of the empty tree is defined arbitrarily to be the hash of the empty string
    getHash Empty = H.hash ""
    getHash (Tree t) = getHash t

-- |A fold over the leaves of an 'IT'
foldIT :: SimpleFold IT (Either ContractSubindex Instance)
foldIT up (Branch _ _ _ _ l r) = foldIT up l <> foldIT up r
foldIT up t@(Leaf i) = t <$ up (Right i)
foldIT up t@(VacantLeaf si) = t <$ up (Left si)

type instance Index IT = ContractIndex
type instance IxValue IT = Instance

instance Ixed IT where
    ix i upd br@(Branch b f v _ t1 t2)
        | i < 2^b = mkBranch <$> ix i upd t1 <*> pure t2
        | i < 2^(b+1) = mkBranch t1 <$> (ix (i - 2^b) upd t2)
        | otherwise = pure br
        where
            mkBranch t1' t2' = Branch b f v (computeBranchHash t1' t2') t1' t2'
    ix i upd l@(Leaf inst)
        | i == 0    = Leaf <$> upd inst
        | otherwise = pure l
    ix _ _ v@(VacantLeaf _) = pure v

type instance Index InstanceTable = ContractAddress
type instance IxValue InstanceTable = Instance

instance Ixed InstanceTable where
    ix _ _ t@Empty = pure t
    ix i upd (Tree t) = Tree <$> (ix (contractIndex i) . filtered ((== i) . instanceAddress . instanceParameters)) upd t

-- |Determine if an 'IT' is a full binary tree.
isFull :: IT -> Bool
isFull (Branch _ f _ _ _ _) = f
isFull _ = True

-- |The height for the level above.
nextHeight :: IT -> Word8
nextHeight (Branch h _ _ _ _ _) = h + 1
nextHeight _ = 0

hasVacancies :: IT -> Bool
hasVacancies (Branch _ _ v _ _ _) = v
hasVacancies (Leaf _) = False
hasVacancies (VacantLeaf _) = True

newContractInstance :: Lens InstanceTable InstanceTable ContractAddress Instance
newContractInstance mk Empty = Tree . Leaf <$> mk (ContractAddress 0 0)
newContractInstance mk (Tree t0) = Tree <$> nci 0 t0
    where
        -- Insert into a tree with vacancies: insert in left if it has vacancies, otherwise right
        nci offset (Branch h f True _ l r)
            | hasVacancies l = let newBranch l' = mkBranch h f (hasVacancies l' || hasVacancies r) l' r in newBranch <$> nci offset l 
            | hasVacancies r = let newBranch r' = mkBranch h f (hasVacancies r') l r' in newBranch <$> nci (offset + 2^h) r
            | otherwise = error "newContractInstance: branch has vacancies, but children do not"
        -- Insert into full tree with no vacancies: create new branch at top level
        nci offset b@(Branch h True False _ _ _) = mkBranch (h+1) False False b <$> (leaf (offset + 2^(h+1)) 0)
        -- Insert into non-full tree with no vacancies: insert on right subtree (invariant implies left is full, but can add to right)
        nci offset (Branch h False False _ l r) = newBranch <$> nci (offset + 2^h) r
            where
                newBranch r' = mkBranch h (isFull r' && nextHeight r' == h) False l r'
        -- Insert at leaf: create a new branch
        nci offset b@(Leaf _) = mkBranch 0 True False b <$> (leaf (offset + 1) 0)
        -- Insert at vacant leaf: create leaf with next subindex
        nci offset (VacantLeaf si) = leaf offset (succ si)
        mkBranch h f v t1' t2' = Branch h f v (computeBranchHash t1' t2') t1' t2'
        leaf ind subind = Leaf <$> mk (ContractAddress ind subind)

deleteContractInstance :: ContractIndex -> InstanceTable -> InstanceTable
deleteContractInstance _ Empty = Empty
deleteContractInstance i0 (Tree t0) = Tree $ dci i0 t0
    where
        dci i l@(Leaf inst)
            | i == 0 = VacantLeaf $ contractSubindex $ instanceAddress $ instanceParameters inst
            | otherwise = l
        dci _ vl@(VacantLeaf _) = vl
        dci i b@(Branch h f _ _ l r)
            | i < 2^h = mkBranch (dci i l) r
            | i < 2^(h+1) = mkBranch l (dci (i - 2^h) r)
            | otherwise = b
            where
                mkBranch t1' t2' = Branch h f (hasVacancies t1' || hasVacancies t2') (computeBranchHash t1' t2') t1' t2'

deleteContractInstanceExact :: ContractAddress -> InstanceTable -> InstanceTable
deleteContractInstanceExact _ Empty = Empty
deleteContractInstanceExact addr (Tree t0) = Tree $ dci (contractIndex addr) t0
    where
        dci i l@(Leaf inst)
            | i == 0 && addr == instanceAddress (instanceParameters inst)
                        = VacantLeaf $ contractSubindex $ instanceAddress $ instanceParameters inst
            | otherwise = l
        dci _ vl@(VacantLeaf _) = vl
        dci i b@(Branch h f _ _ l r)
            | i < 2^h = mkBranch (dci i l) r
            | i < 2^(h+1) = mkBranch l (dci (i - 2^h) r)
            | otherwise = b
            where
                mkBranch t1' t2' = Branch h f (hasVacancies t1' || hasVacancies t2') (computeBranchHash t1' t2') t1' t2'

-- |A collection of smart contract instances.
newtype Instances = Instances {
  _instances :: InstanceTable
  }

instance HashableTo H.Hash Instances where
    getHash = getHash . _instances
