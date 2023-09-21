{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.Instances where

import Control.Monad
import Data.FileEmbed
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Instance
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm

import qualified Data.ByteString as BS
import qualified Data.FixedByteString as FBS

import Basic.InstanceTable
import Basic.Instances

import Test.Hspec
import Test.QuickCheck

contractSourcesV0 :: [(FilePath, BS.ByteString)]
contractSourcesV0 = $(makeRelativeToProject "../concordium-base/smart-contracts/testdata/contracts/" >>= embedDir)

-- Read all the files in smart-contracts/testdata/contracts in base and get any valid contract interfaces.
-- This assumes there is at least one, otherwise the tests will fail.
validContractArtifactsV0 :: [(Wasm.ModuleSource GSWasm.V0, GSWasm.ModuleInterfaceV GSWasm.V0)]
validContractArtifactsV0 = mapMaybe packModule contractSourcesV0
  where
    packModule (_, sourceBytes) =
        let source = Wasm.ModuleSource sourceBytes
        in  (source,) <$> WasmV0.processModule (Wasm.WasmModuleV source)

contractSourcesV1 :: [(FilePath, BS.ByteString)]
contractSourcesV1 = $(makeRelativeToProject "../concordium-base/smart-contracts/testdata/contracts/v1" >>= embedDir)

-- Read all the files in smart-contracts/testdata/contracts/v1 in base and get any valid contract interfaces.
-- This assumes there is at least one, otherwise the tests will fail.
validContractArtifactsV1 :: [(Wasm.ModuleSource GSWasm.V1, GSWasm.ModuleInterfaceV GSWasm.V1)]
validContractArtifactsV1 = mapMaybe packModule contractSourcesV1
  where
    packModule (_, sourceBytes) =
        let source = Wasm.ModuleSource sourceBytes
        in  (source,) <$> WasmV1.processModule SP5 (Wasm.WasmModuleV source)

checkBinary :: (Show a) => (a -> a -> Bool) -> a -> a -> String -> String -> String -> Either String ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

invariantIT :: ContractIndex -> IT -> Either String (Word8, Bool, Bool, ContractIndex, H.Hash, Word64)
invariantIT offset (Leaf inst) = do
    checkBinary (==) (contractIndex $ instanceAddress inst) offset "==" "account index" "expected value"
    return (0, True, False, succ offset, getHash inst, 1)
invariantIT offset (VacantLeaf si) = return (0, True, True, succ offset, H.hash $ runPut $ put si, 0)
invariantIT offset (Branch h f v hsh l r) = do
    (hl, fl, vl, offset', hshl, cl) <- invariantIT offset l
    checkBinary (==) hl h "==" "sucessor level of left child" "node level"
    unless fl $ Left "tree is not left-full"
    (hr, fr, vr, offset'', hshr, cr) <- invariantIT offset' r
    checkBinary (==) hsh (H.hash $ runPut $ put hshl <> put hshr) "==" "branch hash" "hash(leftHash <> rightHash)"
    checkBinary (<=) hr h "<=" "successor level of right child" "node level"
    checkBinary (==) f (fr && hr == h) "<->" "branch marked full" "right child is full at next lower level"
    checkBinary (==) v (vl || vr) "<->" "branch has vacancies" "at least one child has vacancies"
    return (succ h, f, v, offset'', hsh, cl + cr)

invariantInstanceTable :: InstanceTable -> Either String ()
invariantInstanceTable Empty = Right ()
invariantInstanceTable (Tree c0 t) = do
    (_, _, _, _, _, c) <- invariantIT 0 t
    checkBinary (==) c0 c "==" "reported number of instances" "actual number"

invariantInstances :: Instances -> Either String ()
invariantInstances = invariantInstanceTable . _instances

-- These generators name contracts as numbers to make sure the names are valid.
genInitName :: Gen Wasm.InitName
genInitName =
    Wasm.InitName . Text.pack . ("init_" ++) . show <$> (arbitrary :: Gen Word)

genReceiveName :: Gen Wasm.ReceiveName
genReceiveName = do
    contract <- show <$> (arbitrary :: Gen Word)
    receive <- show <$> (arbitrary :: Gen Word)
    return . Wasm.ReceiveName . Text.pack $ receive ++ "." ++ contract

genReceiveNames :: Gen (Map.Map Wasm.InitName (Set.Set Wasm.ReceiveName))
genReceiveNames = do
    n <- choose (1, 10)
    ns <- replicateM n $ do
        i <- genInitName
        m <- choose (0, 10)
        receives <- replicateM m genReceiveName
        return (i, Set.fromList receives)
    return $ Map.fromList ns

genV0ContractState :: Gen Wasm.ContractState
genV0ContractState = do
    n <- choose (1, 1000)
    Wasm.ContractState . BS.pack <$> vector n

-- This currently always generates the empty state.
genV1ContractState :: Gen StateV1.InMemoryPersistentState
genV1ContractState = do
    seed <- arbitrary
    len <- choose (0, 10)
    return $ StateV1.generatePersistentTree seed len

makeDummyInstance :: InstanceData -> Gen (ContractAddress -> Instance GSWasm.InstrumentedModuleV)
makeDummyInstance (InstanceDataV0 model amount) = do
    let owner = AccountAddress . FBS.pack . replicate 32 $ 0
    (_, mInterface@GSWasm.ModuleInterface{..}) <- elements validContractArtifactsV0
    initName <- if Set.null miExposedInit then return (Wasm.InitName "init_") else elements (Set.toList miExposedInit)
    let receiveNames = fromMaybe Set.empty $ Map.lookup initName miExposedReceive
    return $ makeInstance initName receiveNames mInterface model amount owner
makeDummyInstance (InstanceDataV1 model amount) = do
    let owner = AccountAddress . FBS.pack . replicate 32 $ 1
    (_, mInterface@GSWasm.ModuleInterface{..}) <- elements validContractArtifactsV1
    initName <- if Set.null miExposedInit then return (Wasm.InitName "init_") else elements (Set.toList miExposedInit)
    let receiveNames = fromMaybe Set.empty $ Map.lookup initName miExposedReceive
    return $ makeInstance initName receiveNames mInterface model amount owner

data InstanceData
    = InstanceDataV0 (InstanceStateV Wasm.V0) Amount
    | InstanceDataV1 (InstanceStateV Wasm.V1) Amount

instance Eq InstanceData where
    (InstanceDataV0 (InstanceStateV0 v1) a1) == (InstanceDataV0 (InstanceStateV0 v2) a2) = v1 == v2 && a1 == a2
    (InstanceDataV1 v1 a1) == (InstanceDataV1 v2 a2) = encode v1 == encode v2 && a1 == a2
    _ == _ = False

instance Show InstanceData where
    show (InstanceDataV0 (InstanceStateV0 v) a) = "V0: " ++ show v ++ ", " ++ show a
    show (InstanceDataV1 s a) = "V1: " ++ show (encode s) ++ ", " ++ show a

instance Arbitrary InstanceData where
    arbitrary = oneof [InstanceDataV0 <$> v0 <*> arbitrary, InstanceDataV1 <$> v1 <*> arbitrary]
      where
        v0 = InstanceStateV0 <$> genV0ContractState
        v1 = InstanceStateV1 <$> genV1ContractState

instanceData :: Instance GSWasm.InstrumentedModuleV -> InstanceData
instanceData (InstanceV0 InstanceV{..}) = InstanceDataV0 _instanceVModel _instanceVAmount
instanceData (InstanceV1 InstanceV{..}) = InstanceDataV1 _instanceVModel _instanceVAmount

data Model = Model
    { -- Data of instances
      modelInstances :: Map.Map ContractIndex (ContractSubindex, InstanceData),
      -- The next free subindex for free indexes
      modelFree :: Map.Map ContractIndex ContractSubindex,
      -- The lowest index that has never been assigned
      modelBound :: ContractIndex
    }
    deriving (Eq, Show)

emptyModel :: Model
emptyModel = Model Map.empty Map.empty 0

modelGetInstanceData :: ContractAddress -> Model -> Maybe InstanceData
modelGetInstanceData (ContractAddress ci csi) m = do
    (csi', idata) <- Map.lookup ci (modelInstances m)
    guard $ csi == csi'
    return idata

modelUpdateInstanceAt :: forall v. (Wasm.IsWasmVersion v) => ContractAddress -> Amount -> InstanceStateV v -> Model -> Model
modelUpdateInstanceAt (ContractAddress ci csi) amt val m = m{modelInstances = Map.adjust upd ci (modelInstances m)}
  where
    upd o@(csi', ex)
        | csi == csi' = case Wasm.getWasmVersion @v of
            Wasm.SV0 -> case ex of
                InstanceDataV0 _ _ -> (csi, InstanceDataV0 val amt)
                _ -> error "Contract version mismatch."
            Wasm.SV1 -> case ex of
                InstanceDataV1 _ _ -> (csi, InstanceDataV1 val amt)
                _ -> error "Contract version mismatch."
        | otherwise = o

modelCreateInstance :: (ContractAddress -> Instance GSWasm.InstrumentedModuleV) -> Model -> (ContractAddress, Model)
modelCreateInstance mk m
    | null (modelFree m) =
        let ca = ContractAddress (modelBound m) 0
        in  ( ca,
              m
                { modelInstances = Map.insert (modelBound m) (0, instanceData (mk ca)) (modelInstances m),
                  modelBound = succ $ modelBound m
                }
            )
    | otherwise =
        let
            ((ci, csi), free') = Map.deleteFindMin (modelFree m)
            ca = ContractAddress ci csi
        in
            ( ca,
              m
                { modelInstances = Map.insert ci (csi, instanceData (mk ca)) (modelInstances m),
                  modelFree = free'
                }
            )

modelDeleteInstance :: ContractAddress -> Model -> Model
modelDeleteInstance (ContractAddress ci csi) m = case Map.lookup ci (modelInstances m) of
    Nothing -> m
    Just (csi', _) ->
        if csi /= csi'
            then m
            else
                m
                    { modelInstances = Map.delete ci (modelInstances m),
                      modelFree = Map.insert ci (succ csi) (modelFree m)
                    }

instanceTableToModel :: InstanceTable -> Model
instanceTableToModel Empty = emptyModel
instanceTableToModel (Tree _ t0) = ttm 0 emptyModel t0
  where
    ttm offset m (Branch h _ _ _ l r) =
        let m' = ttm offset m l
        in  ttm (offset + 2 ^ h) m' r
    ttm offset m (Leaf inst) =
        m
            { modelInstances = Map.insert offset (contractSubindex $ instanceAddress inst, instanceData inst) (modelInstances m),
              modelBound = modelBound m + 1
            }
    ttm offset m (VacantLeaf si) =
        m
            { modelFree = Map.insert offset (succ si) (modelFree m),
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

arbitraryMapElement :: Map.Map k v -> Gen (k, v)
arbitraryMapElement m = do
    ind <- choose (0, Map.size m - 1)
    return (Map.elemAt ind m)

generateFromUpdates :: Int -> Gen (Instances, Model)
generateFromUpdates n0 = gen n0 emptyInstances emptyModel
  where
    gen 0 insts model = return (insts, model)
    gen n insts model = oneof $ [create, create, create] ++ if null (modelInstances model) then [] else [deleteExisting]
      where
        create = do
            instData <- arbitrary
            dummyInstance <- makeDummyInstance instData
            let (_, insts') = createInstance dummyInstance insts
            let (_, model') = modelCreateInstance dummyInstance model
            gen (n - 1) insts' model'
        deleteExisting = do
            (ci, (csi, _)) <- arbitraryMapElement (modelInstances model)
            let
                ca = ContractAddress ci csi
                insts' = deleteInstance ca insts
                model' = modelDeleteInstance ca model
            gen (n - 1) insts' model'

testUpdates :: Int -> Gen Property
testUpdates n0 = if n0 <= 0 then return (property True) else tu n0 emptyInstances emptyModel
  where
    tu 0 insts model = checkInvariantThen insts $ return $ modelCheck insts model
    tu n insts model =
        checkInvariantThen insts $
            checkEqualThen model (instanceTableToModel $ _instances insts) $
                oneof $
                    [create, deleteAbsent, updateAbsent]
                        ++ (if null (modelInstances model) then [] else [updateExisting, deleteExisting])
                        ++ (if null (modelFree model) then [] else [deleteFree, updateFree])
      where
        create = do
            instData <- arbitrary
            dummyInstance <- makeDummyInstance instData
            let (ca, insts') = createInstance dummyInstance insts
            let (cam, model') = modelCreateInstance dummyInstance model
            checkEqualThen (instanceAddress ca) cam $
                tu (n - 1) insts' model'
        deleteAbsent = do
            ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
            csi <- ContractSubindex <$> arbitrary
            let
                ca = ContractAddress ci csi
                insts' = deleteInstance ca insts
                model' = modelDeleteInstance ca model
            tu (n - 1) insts' model'
        updateAbsent = do
            ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
            csi <- ContractSubindex <$> arbitrary
            arbitrary >>= \case
                InstanceDataV0 v a -> do
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
                InstanceDataV1 v a -> do
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
        updateExisting = do
            (ci, (csi0, curVer)) <- arbitraryMapElement (modelInstances model)
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            case curVer of
                InstanceDataV0 _ _ -> do
                    v <- InstanceStateV0 <$> genV0ContractState
                    a <- arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
                InstanceDataV1 _ _ -> do
                    v <- InstanceStateV1 <$> genV1ContractState
                    a <- arbitrary
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
        deleteExisting = do
            (ci, (csi0, _)) <- arbitraryMapElement (modelInstances model)
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            let
                ca = ContractAddress ci csi
                insts' = deleteInstance ca insts
                model' = modelDeleteInstance ca model
            tu (n - 1) insts' model'
        updateFree = do
            (ci, csi0) <- arbitraryMapElement (modelFree model)
            csi <- ContractSubindex <$> oneof [choose (0, fromIntegral csi0 - 1), choose (fromIntegral csi0, maxBound)]
            arbitrary >>= \case
                InstanceDataV0 v a -> do
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
                InstanceDataV1 v a -> do
                    let
                        ca = ContractAddress ci csi
                        insts' = updateInstanceAt' ca a (Just v) Nothing insts
                        model' = modelUpdateInstanceAt ca a v model
                    tu (n - 1) insts' model'
        deleteFree = do
            (ci, csi0) <- arbitraryMapElement (modelFree model)
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            let
                ca = ContractAddress ci csi
                insts' = deleteInstance ca insts
                model' = modelDeleteInstance ca model
            tu (n - 1) insts' model'

testCreateDelete :: Int -> Gen Property
testCreateDelete n = do
    (insts, model) <- generateFromUpdates n
    checkInvariantThen insts $ return $ modelCheck insts model

testGetInstance :: Instances -> Model -> Gen Property
testGetInstance insts model =
    oneof $
        [present | not (null $ modelInstances model)]
            ++ [deleted | not (null $ modelFree model)]
            ++ [absent]
  where
    present = do
        (ci, (csi, d)) <- arbitraryMapElement (modelInstances model)
        return $ fmap instanceData (getInstance (ContractAddress ci csi) insts) === Just d
    deleted = do
        (ci, csi0) <- arbitraryMapElement (modelFree model)
        csi <- ContractSubindex <$> oneof [choose (0, fromIntegral csi0 - 1), choose (fromIntegral csi0, maxBound)]
        return $ fmap instanceData (getInstance (ContractAddress ci csi) insts) === Nothing
    absent = do
        ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
        csi <- ContractSubindex <$> arbitrary
        return $ fmap instanceData (getInstance (ContractAddress ci csi) insts) === Nothing

testFoldInstances :: Instances -> Model -> Property
testFoldInstances insts model = allInsts === modInsts
  where
    allInsts = (\i -> (instanceAddress i, instanceData i)) <$> (insts ^.. foldInstances)
    modInsts = (\(ci, (csi, d)) -> (ContractAddress ci csi, d)) <$> Map.toAscList (modelInstances model)

tests :: Word -> Spec
tests lvl = describe "GlobalStateTests.Instances" $ do
    it "getInstance" $
        withMaxSuccess (100 * fromIntegral lvl) $
            forAllBlind (generateFromUpdates 5000) $
                \(i, m) -> withMaxSuccess 100 $ testGetInstance i m
    it "foldInstances" $ withMaxSuccess 100 $ forAllBlind (generateFromUpdates 5000) $ uncurry testFoldInstances
    it "10000 create/delete - check at end" $ withMaxSuccess 10 $ testCreateDelete 10000
    it "500 instance updates - check every step" $ withMaxSuccess (100 * fromIntegral lvl) $ testUpdates 500
