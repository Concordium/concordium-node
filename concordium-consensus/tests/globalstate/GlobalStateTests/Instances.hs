{-# LANGUAGE OverloadedStrings #-}
module GlobalStateTests.Instances where

import Data.Word
import Control.Monad
import Data.Serialize
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Acorn.Interfaces
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Instances.Internal
import Concordium.GlobalState.Instances

import Test.QuickCheck
import Test.Hspec

checkBinary :: Show a => (a -> a -> Bool) -> a -> a -> String -> String -> String -> Either String ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

invariantIT :: ContractIndex -> IT -> Either String (Word8, Bool, Bool, ContractIndex, H.Hash)
invariantIT offset (Leaf inst) = do
        checkBinary (==) (contractIndex $ iaddress inst) offset "==" "account index" "expected value"
        return (0, True, False, succ offset, getHash inst)
invariantIT offset (VacantLeaf si) = return (0, True, True, succ offset, H.hash $ runPut $ put si)
invariantIT offset (Branch h f v hsh l r) = do
        (hl, fl, vl, offset', hshl) <- invariantIT offset l
        checkBinary (==) hl h "==" "sucessor level of left child" "node level"
        unless fl $ Left "tree is not left-full"
        (hr, fr, vr, offset'', hshr) <- invariantIT offset' r
        checkBinary (==) hsh (H.hash $ runPut $ put hshl <> put hshr) "==" "branch hash" "hash(leftHash <> rightHash)"
        checkBinary (<=) hr h "<=" "successor level of right child" "node level"
        checkBinary (==) f (fr && hr == h) "<->" "branch marked full" "right child is full at next lower level"
        checkBinary (==) v (vl || vr) "<->" "branch has vacancies" "at least one child has vacancies"
        return (succ h, f, v, offset'', hsh)

invariantInstanceTable :: InstanceTable -> Either String ()
invariantInstanceTable Empty = Right ()
invariantInstanceTable (Tree t) = () <$ invariantIT 0 t

invariantInstances :: Instances -> Either String ()
invariantInstances = invariantInstanceTable . _instances

makeArbitraryInstance :: Gen (ContractAddress -> Instance)
makeArbitraryInstance = do
        let
            modRef = Core.ModuleRef (H.hash "module")
            tyname = 0
            dummyExpr = UnCast
            contract = ContractValue dummyExpr dummyExpr HM.empty
            messageType = Core.TBase Core.TInt32
        model <- VLiteral . Core.Int32 <$> arbitrary
        amount <- Amount <$> arbitrary
        return $ makeInstance modRef tyname contract messageType emptyInterface emptyValueInterface model amount

makeDummyInstance :: InstanceData -> ContractAddress -> Instance
makeDummyInstance (InstanceData model amount) =
        makeInstance modRef tyname contract messageType emptyInterface emptyValueInterface model amount
    where
        modRef = Core.ModuleRef (H.hash "module")
        tyname = 0
        dummyExpr = UnCast
        contract = ContractValue dummyExpr dummyExpr HM.empty
        messageType = Core.TBase Core.TInt32

data InstanceData = InstanceData Value Amount
    deriving (Eq, Show)

instance Arbitrary InstanceData where
    arbitrary = do
        model <- VLiteral . Core.Int32 <$> arbitrary
        amount <- Amount <$> arbitrary
        return $ InstanceData model amount

instanceData :: Instance -> InstanceData
instanceData inst = InstanceData (instanceModel inst) (instanceAmount inst)

data Model = Model {
    -- Data of instances
    modelInstances :: HM.HashMap ContractIndex (ContractSubindex, InstanceData),
    -- The next free subindex for free indexes
    modelFree :: Map.Map ContractIndex ContractSubindex,
    -- The lowest index that has never been assigned
    modelBound :: ContractIndex
} deriving (Eq, Show)

emptyModel :: Model
emptyModel = Model HM.empty Map.empty 0

modelGetInstanceData :: ContractAddress -> Model -> Maybe InstanceData
modelGetInstanceData (ContractAddress ci csi) m = do
        (csi', idata) <- HM.lookup ci (modelInstances m)
        guard $ csi == csi'
        return idata
    
modelUpdateInstanceAt :: ContractAddress -> Amount -> Value -> Model -> Model
modelUpdateInstanceAt (ContractAddress ci csi) amt val m = m {modelInstances = HM.adjust upd ci (modelInstances m)}
    where
        upd o@(csi', _)
            | csi == csi' = (csi, InstanceData val amt)
            | otherwise = o

modelCreateInstance :: (ContractAddress -> Instance) -> Model -> (ContractAddress, Model)
modelCreateInstance mk m
    | null (modelFree m) =
        let ca = ContractAddress (modelBound m) 0 in
            (ca, m {
                        modelInstances = HM.insert (modelBound m) (0, instanceData (mk ca)) (modelInstances m),
                        modelBound = succ $ modelBound m
                    })
    | otherwise =
        let
            ((ci, csi), free') = Map.deleteFindMin (modelFree m)
            ca = ContractAddress ci csi
        in
            (ca, m {
                modelInstances = HM.insert ci (csi, instanceData (mk ca)) (modelInstances m),
                modelFree = free'
            })

modelDeleteInstance :: ContractAddress -> Model -> Model
modelDeleteInstance (ContractAddress ci csi) m = case HM.lookup ci (modelInstances m) of
        Nothing -> m
        Just (csi', _) -> if csi /= csi' then m else
                            m {
                                modelInstances = HM.delete ci (modelInstances m),
                                modelFree = Map.insert ci (succ csi) (modelFree m)
                            }

instanceTableToModel :: InstanceTable -> Model
instanceTableToModel Empty = emptyModel
instanceTableToModel (Tree t0) = ttm 0 emptyModel t0
    where
        ttm offset m (Branch h _ _ _ l r) =
            let m' = ttm offset m l in
                ttm (offset + 2^h) m' r
        ttm offset m (Leaf inst) = m {
                                        modelInstances = HM.insert offset (contractSubindex $ instanceAddress $ instanceParameters inst, instanceData inst) (modelInstances m),
                                        modelBound = modelBound m + 1
                                    }
        ttm offset m (VacantLeaf si) = m {
                                        modelFree = Map.insert offset (succ si) (modelFree m),
                                        modelBound = modelBound m + 1
                                    }

modelCheck :: Instances -> Model -> Property
modelCheck (Instances t) m = m === instanceTableToModel t

checkEqualThen :: (Monad m, Eq a, Show a) => a -> a -> m Property -> m Property
checkEqualThen a b r = if a /= b then return (a === b) else r

checkBoolThen :: (Monad m) => String -> Bool -> m Property -> m Property
checkBoolThen _ True r = r
checkBoolThen ex False _ = return $ counterexample ex False

checkEitherThen_ :: (Monad m) => Either String a -> m Property -> m Property
checkEitherThen_ (Left ex) _ = return $ counterexample ex False
checkEitherThen_ (Right _) r = r

checkInvariantThen :: (Monad m) => Instances -> m Property -> m Property
checkInvariantThen insts r = case invariantInstances insts of
        Right _ -> r
        Left ex -> return $ counterexample (ex ++ "\n" ++ show (_instances insts)) False

generateFromUpdates :: Int -> Gen (Instances, Model)
generateFromUpdates n0 = gen n0 emptyInstances emptyModel
    where
        gen 0 insts model = return (insts, model)
        gen n insts model = oneof $ [create,create,create] ++ if null (modelInstances model) then [] else [deleteExisting]
            where
                create = do
                    instData <- arbitrary
                    let (_, insts') = createInstance (makeDummyInstance instData) insts
                    let (_, model') = modelCreateInstance (makeDummyInstance instData) model
                    gen (n-1) insts' model'
                deleteExisting = do
                    (ci, (csi, _)) <- elements $ HM.toList (modelInstances model)
                    let
                        ca = ContractAddress ci csi
                        insts' = deleteInstance ca insts
                        model' = modelDeleteInstance ca model
                    gen (n-1) insts' model'


testUpdates :: Int -> Gen Property
testUpdates n0 = if n0 <= 0 then return (property True) else tu n0 emptyInstances emptyModel
    where
        tu 0 insts model = checkInvariantThen insts $ return $ modelCheck insts model
        tu n insts model = checkInvariantThen insts $
                checkEqualThen model (instanceTableToModel $ _instances insts) $
                oneof $ [create, deleteAbsent, updateAbsent] ++
                            (if null (modelInstances model) then [] else [updateExisting, deleteExisting]) ++
                            (if null (modelFree model) then [] else [deleteFree, updateFree])
            where
                create = do
                    instData <- arbitrary
                    let (ca, insts') = createInstance (makeDummyInstance instData) insts
                    let (cam, model') = modelCreateInstance (makeDummyInstance instData) model
                    checkEqualThen ca cam $
                        tu (n-1) insts' model'
                deleteAbsent = do
                    ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
                    csi <- ContractSubindex <$> arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = deleteInstance ca insts
                        model' = modelDeleteInstance ca model
                    tu (n-1) insts' model'
                updateAbsent = do
                    ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
                    csi <- ContractSubindex <$> arbitrary
                    InstanceData v a <- arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt ca a v insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n-1) insts' model'
                updateExisting = do
                    (ci, (csi0, _)) <- elements $ HM.toList (modelInstances model)
                    csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
                    InstanceData v a <- arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt ca a v insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n-1) insts' model'
                deleteExisting = do
                    (ci, (csi0, _)) <- elements $ HM.toList (modelInstances model)
                    csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
                    let
                        ca = ContractAddress ci csi
                        insts' = deleteInstance ca insts
                        model' = modelDeleteInstance ca model
                    tu (n-1) insts' model'
                updateFree = do
                    (ci, csi0) <- elements $ Map.toList (modelFree model)
                    csi <- ContractSubindex <$> oneof [choose (0, fromIntegral csi0 - 1), choose (fromIntegral csi0, maxBound)]
                    InstanceData v a <- arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt ca a v insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n-1) insts' model'
                deleteFree = do
                    (ci, csi0) <- elements $ Map.toList (modelFree model)
                    csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
                    let
                        ca = ContractAddress ci csi
                        insts' = deleteInstance ca insts
                        model' = modelDeleteInstance ca model
                    tu (n-1) insts' model'

testCreateDelete :: Int -> Gen Property
testCreateDelete n = do
    (insts, model) <- generateFromUpdates n
    checkInvariantThen insts $ return $ modelCheck insts model

{-
testGetInstance :: Instances -> Model -> Gen Property
testGetInstance insts model = oneof [present, deleted, absent]
    where
        present = do
            (ci, (csi0, d)) <- elements $ HM.toList (modelInstances model)
-}



tests :: Spec
tests = parallel $ describe "GlobalStateTests.Instances" $ do
    it "50000 create/delete - check at end" $ withMaxSuccess 10 $ testCreateDelete 50000
    it "500 instance updates - check every step" $ withMaxSuccess 10000 $ testUpdates 500
    