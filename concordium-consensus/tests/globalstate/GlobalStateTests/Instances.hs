{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GlobalStateTests.Instances where

import Control.Monad
import Data.FileEmbed
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm

import qualified Data.ByteString as BS
import qualified Data.FixedByteString as FBS

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBTree
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.Modules
import Concordium.GlobalState.Persistent.Cache
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.ID.Types (accountAddressSize)
import Control.Exception
import Control.Monad.Reader
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

-- | Assert that a binary predicate holds.
checkBinary :: (Show a, MonadFail m) => (a -> a -> Bool) -> a -> a -> String -> String -> String -> m ()
checkBinary bop x y sbop sx sy =
    unless (bop x y) $
        fail $
            "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

-- | Check an invariant on 'Instances.IT' (see 'invariantInstances'). The return value is a tuple
--  consisting of:
--   * the height of the branch (0 for leaves)
--   * whether the branch is full (in the tree sense - not in the sense of no vacancies)
--   * whether the branch has vacancies
--   * the index of the next leaf
--   * the Merkle hash of the branch
invariantIT ::
    (IsProtocolVersion pv) =>
    ContractIndex ->
    Instances.IT pv (BufferedFix (Instances.IT pv)) ->
    TestMonad pv (Word8, Bool, Bool, ContractIndex, H.Hash)
invariantIT offset (Instances.Leaf inst) = do
    params <- Instances.loadInstanceParameters inst
    checkBinary (==) (contractIndex $ Instances.pinstanceAddress params) offset "==" "account index" "expected value"
    return (0, True, False, succ offset, getHash inst)
invariantIT offset (Instances.VacantLeaf si) = do
    return (0, True, True, succ offset, H.hash (encode si))
invariantIT offset (Instances.Branch h f v hsh l r) = do
    (hl, fl, vl, offset', hshl) <- invariantIT offset =<< mproject l
    checkBinary (==) hl h "==" "sucessor level of left child" "node level"
    unless fl $ fail "tree is not left-full"
    (hr, fr, vr, offset'', hshr) <- invariantIT offset' =<< mproject r
    checkBinary (==) hsh (H.hash $ runPut $ put hshl <> put hshr) "==" "branch hash" "hash(leftHash <> rightHash)"
    checkBinary (<=) hr h "<=" "successor level of right child" "node level"
    checkBinary (==) f (fr && hr == h) "<->" "branch marked full" "right child is full at next lower level"
    checkBinary (==) v (vl || vr) "<->" "branch has vacancies" "at least one child has vacancies"
    return (succ h, f, v, offset'', hsh)

-- | Check the following invariant on 'Instances.Instances':
--   * At each leaf node, the account index matches the index in the table.
--   * At each branch node:
--      * The level of the node is 1+ the level of the left child.
--      * The left child is a full subtree.
--      * The recorded hash is the hash of the combined hashes of the left and right subtrees.
--      * The level of the node is at least 1+ the level of the right child.
--      * The branch is marked full if and only if the right subtree is full.
--      * The branch has vacancies exactly when at least one subtree has vacancies.
--   * The root records the correct size of the table.
invariantInstances :: (IsProtocolVersion pv) => Instances.Instances pv -> TestMonad pv ()
invariantInstances Instances.InstancesEmpty = return ()
invariantInstances (Instances.InstancesTree size bf) = do
    (_, _, _, ContractIndex recSize, _) <- invariantIT 0 =<< mproject bf
    checkBinary (==) recSize size "==" "measured size" "recorded size"

-- | A model for an individual instance.
data ModelInstance = forall v.
      (Wasm.IsWasmVersion v) =>
    ModelInstance
    { mInstanceParameters :: !Instances.PersistentInstanceParameters,
      mInstanceModule :: !(Wasm.ModuleSource v),
      mInstanceInterface :: !(GSWasm.ModuleInterfaceV v),
      mInstanceState :: !(Instances.InstanceStateV v),
      mInstanceStateHash :: !Instances.InstanceStateHash,
      mInstanceAmount :: !Amount
    }

instance Show ModelInstance where
    show ModelInstance{..} =
        "ModelInstance{"
            ++ "mInstanceParameters = "
            ++ show mInstanceParameters
            ++ ", "
            ++ "mInstanceModule = "
            ++ show mInstanceModule
            ++ ", "
            ++ "mInstanceInterface = "
            ++ show mInstanceInterface
            ++ ", "
            ++ "mInstanceStateHash = "
            ++ show mInstanceStateHash
            ++ ", "
            ++ "mInstanceAmount = "
            ++ show mInstanceAmount
            ++ "}"

instance HashableTo H.Hash ModelInstance where
    getHash (ModelInstance params _ _ (Instances.InstanceStateV0 _) isHash modAmt) =
        Instances.makeInstanceHashV0 (getHash params) isHash modAmt
    getHash (ModelInstance params _ _ (Instances.InstanceStateV1 _) isHash modAmt) =
        Instances.makeInstanceHashV0 (getHash params) isHash modAmt

-- | The address associated with a model of a smart contract instance.
mInstanceAddr :: ModelInstance -> ContractAddress
mInstanceAddr = Instances.pinstanceAddress . mInstanceParameters

-- | Construct a 'Instances.PersistentInstance' from a 'ModelInstance'.
toPersistentInstance :: (IsProtocolVersion pv) => ModelInstance -> TestMonad pv (Instances.PersistentInstance pv)
toPersistentInstance (ModelInstance @v params modSrc iface pinstanceModel _ pinstanceAmount) = do
    pinstanceParameters <- refMake @_ @BufferedRef params
    moduleVSource <- storeRef (Wasm.WasmModuleV modSrc)
    case Wasm.getWasmVersion @v of
        Wasm.SV0 -> do
            pinstanceModuleInterface <-
                refMake
                    (ModuleV0 ModuleV{moduleVInterface = makePersistentInstrumentedModuleV <$> iface, ..})
            let pinstanceHash =
                    Instances.makeInstanceHashV0State
                        (Instances.pinstanceParameterHash params)
                        pinstanceModel
                        pinstanceAmount
            return $ Instances.PersistentInstanceV0 Instances.PersistentInstanceV{..}
        Wasm.SV1 -> do
            pinstanceModuleInterface <-
                refMake
                    (ModuleV1 ModuleV{moduleVInterface = makePersistentInstrumentedModuleV <$> iface, ..})
            pinstanceHash <-
                Instances.makeInstanceHashV1State
                    (Instances.pinstanceParameterHash params)
                    pinstanceModel
                    pinstanceAmount
            return $ Instances.PersistentInstanceV1 Instances.PersistentInstanceV{..}

-- | Assert that a 'ModelInstance' matches a 'Instances.PersistentInstance'.
modelsPersistentInstance :: forall pv. (IsProtocolVersion pv) => ModelInstance -> Instances.PersistentInstance pv -> TestMonad pv Property
modelsPersistentInstance modInst perInst = do
    case (modInst, perInst) of
        (ModelInstance modParams modSrc modIFace modModel@Instances.InstanceStateV0{} _ modAmt, Instances.PersistentInstanceV0 Instances.PersistentInstanceV{..}) -> do
            perParams <- refLoad pinstanceParameters
            ModuleV{..} <- unsafeToModuleV <$> refLoad pinstanceModuleInterface
            perSrc <- loadRef moduleVSource
            perInstrMod <- mapM loadInstrumentedModuleV moduleVInterface
            statesProp <- compareStates pinstanceModel modModel
            return $
                counterexample "instance parameters" (perParams === modParams)
                    .&&. counterexample "module source" (Wasm.wmvSource perSrc === modSrc)
                    .&&. counterexample "module interface" (perInstrMod == modIFace)
                    .&&. counterexample "instance state" statesProp
                    .&&. counterexample "amount" (pinstanceAmount === modAmt)
        (ModelInstance modParams modSrc modIFace modModel@Instances.InstanceStateV1{} _ modAmt, Instances.PersistentInstanceV1 Instances.PersistentInstanceV{..}) -> do
            perParams <- refLoad pinstanceParameters
            ModuleV{..} <- unsafeToModuleV <$> refLoad pinstanceModuleInterface
            perSrc <- loadRef moduleVSource
            perInstrMod <- mapM loadInstrumentedModuleV moduleVInterface
            statesProp <- compareStates pinstanceModel modModel
            return $
                counterexample "instance parameters" (perParams === modParams)
                    .&&. counterexample "module source" (Wasm.wmvSource perSrc === modSrc)
                    .&&. counterexample "module interface" (perInstrMod == modIFace)
                    .&&. counterexample "instance state" statesProp
                    .&&. counterexample "amount" (pinstanceAmount === modAmt)
        _ -> return $ counterexample "instance version mismatch" False
  where
    compareStates :: Instances.InstanceStateV v -> Instances.InstanceStateV v -> TestMonad pv Property
    compareStates (Instances.InstanceStateV0 cs0) (Instances.InstanceStateV0 cs1) =
        return (cs0 === cs1)
    compareStates (Instances.InstanceStateV1 ps0) (Instances.InstanceStateV1 ps1) = do
        bs0 <- StateV1.toByteString ps0
        bs1 <- StateV1.toByteString ps1
        return (bs0 === bs1)

-- | Generate an arbitrary account address.
genAccountAddress :: Gen AccountAddress
genAccountAddress = AccountAddress . FBS.pack <$> vector accountAddressSize

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

-- | Generate a V0 contract state.
genV0ContractState :: Gen Wasm.ContractState
genV0ContractState = do
    n <- choose (1, 1000)
    Wasm.ContractState . BS.pack <$> vector n

-- | Generate a V1 contract state.
genV1ContractState :: Gen StateV1.InMemoryPersistentState
genV1ContractState = do
    seed <- arbitrary
    len <- choose (0, 10)
    return $ StateV1.generatePersistentTree seed len

-- | Generate a model of a contract instance. This produces a function that takes the address and
--  returns the model instance so that the address can be determined after the non-deterministic
--  generation of the instance.
genModelInstance :: Gen (ContractAddress -> ModelInstance)
genModelInstance = oneof [genV0, genV1]
  where
    genV0 = do
        pinstanceOwner <- genAccountAddress
        (mInstanceModule, mInstanceInterface@GSWasm.ModuleInterface{..}) <- elements validContractArtifactsV0
        let pinstanceContractModule = Wasm.getModuleRef (Wasm.WasmModuleV mInstanceModule)
        pinstanceInitName <- if Set.null miExposedInit then return (Wasm.InitName "init_") else elements (Set.toList miExposedInit)
        let pinstanceReceiveFuns = fromMaybe Set.empty $ Map.lookup pinstanceInitName miExposedReceive
        memState <- genV0ContractState
        let mInstanceStateHash = getHash memState
        let mInstanceState = Instances.InstanceStateV0 memState
        mInstanceAmount <- arbitrary
        return $ \pinstanceAddress ->
            ModelInstance
                { mInstanceParameters =
                    Instances.PersistentInstanceParameters
                        { pinstanceParameterHash =
                            Instances.makeInstanceParameterHash
                                pinstanceAddress
                                pinstanceOwner
                                pinstanceContractModule
                                pinstanceInitName,
                          ..
                        },
                  ..
                }
    genV1 = do
        pinstanceOwner <- genAccountAddress
        (mInstanceModule, mInstanceInterface@GSWasm.ModuleInterface{..}) <- elements validContractArtifactsV1
        let pinstanceContractModule = Wasm.getModuleRef (Wasm.WasmModuleV mInstanceModule)
        pinstanceInitName <- if Set.null miExposedInit then return (Wasm.InitName "init_") else elements (Set.toList miExposedInit)
        let pinstanceReceiveFuns = fromMaybe Set.empty $ Map.lookup pinstanceInitName miExposedReceive
        memState <- genV1ContractState
        let mInstanceStateHash = getHash memState
            mInstanceState = Instances.InstanceStateV1 . StateV1.makePersistent $ memState
        mInstanceAmount <- arbitrary
        return $ \pinstanceAddress ->
            ModelInstance
                { mInstanceParameters =
                    Instances.PersistentInstanceParameters
                        { pinstanceParameterHash =
                            Instances.makeInstanceParameterHash
                                pinstanceAddress
                                pinstanceOwner
                                pinstanceContractModule
                                pinstanceInitName,
                          ..
                        },
                  ..
                }

-- | Convert an instance table to a list of the hashes of the leaves.
instancesToHashList :: (IsProtocolVersion pv) => Instances.Instances pv -> TestMonad pv [H.Hash]
instancesToHashList Instances.InstancesEmpty = return []
instancesToHashList (Instances.InstancesTree _ instTab) = go [] =<< mproject instTab
  where
    go accum Instances.Branch{..} = do
        accum' <- go accum =<< mproject branchRight
        go accum' =<< mproject branchLeft
    go accum (Instances.Leaf inst) =
        return $ getHash inst : accum
    go accum (Instances.VacantLeaf si) =
        return $ H.hash (encode si) : accum

-- | A model for the instance table.
data Model = Model
    { -- | Data of instances
      modelInstances :: Map.Map ContractIndex ModelInstance,
      -- | The next free subindex for free indexes
      modelFree :: Map.Map ContractIndex ContractSubindex,
      -- | The lowest index that has never been assigned
      modelBound :: ContractIndex
    }
    deriving (Show)

-- | Convert a model instance table to a list of the hashes of the leaves.
modelToHashList :: Model -> [H.Hash]
modelToHashList Model{..} = l 0
  where
    l i
        | i == modelBound = []
        | Just mi <- Map.lookup i modelInstances = getHash mi : l (succ i)
        | Just si <- Map.lookup i modelFree = H.hash (encode (si - 1)) : l (succ i)
        | otherwise = error "Missing leaf in model"

instance (IsProtocolVersion pv) => HashableTo (InstancesHash pv) Model where
    getHash m@Model{..} =
        Instances.makeInstancesHash (_contractIndex modelBound) $
            LFMBTree.hashAsLFMBTV0 emptyHash (modelToHashList m)
      where
        emptyHash = H.hash "EmptyInstances"

-- | The initial empty instance table model.
emptyModel :: Model
emptyModel = Model Map.empty Map.empty 0

-- | Get the model instance at a particular address in the model instance table.
modelGetInstanceData :: ContractAddress -> Model -> Maybe ModelInstance
modelGetInstanceData ca@(ContractAddress ci _) m = do
    inst <- Map.lookup ci (modelInstances m)
    guard $ ca == mInstanceAddr inst
    return inst

-- | Update a model instance as a particular address in the model instance table.
--  This does nothing if the instance does not exist.
--  If the instance does exist, then it must be of the same version as the supplied state.
modelUpdateInstanceAt ::
    forall v.
    (Wasm.IsWasmVersion v) =>
    ContractAddress ->
    Amount ->
    Instances.InstanceStateV v ->
    Instances.InstanceStateHash ->
    Model ->
    Model
modelUpdateInstanceAt addr@(ContractAddress ci _) amt val hsh m =
    m{modelInstances = Map.adjust upd ci (modelInstances m)}
  where
    upd inst@ModelInstance{..}
        | addr == mInstanceAddr inst = case (mInstanceState, val) of
            (Instances.InstanceStateV0 _, Instances.InstanceStateV0 _) ->
                ModelInstance
                    { mInstanceState = val,
                      mInstanceAmount = amt,
                      mInstanceStateHash = hsh,
                      ..
                    }
            (Instances.InstanceStateV1 _, Instances.InstanceStateV1 _) ->
                ModelInstance
                    { mInstanceState = val,
                      mInstanceAmount = amt,
                      mInstanceStateHash = hsh,
                      ..
                    }
            _ -> error "Contract version mismatch."
        | otherwise = inst

-- | Update a model instance table by creating a new instance. Returns the address of the new
--  instances as well as the updated model.
modelCreateInstance :: (ContractAddress -> ModelInstance) -> Model -> (ContractAddress, Model)
modelCreateInstance mk m
    | null (modelFree m) =
        let ca = ContractAddress (modelBound m) 0
        in  ( ca,
              m
                { modelInstances = Map.insert (modelBound m) (mk ca) (modelInstances m),
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
                { modelInstances = Map.insert ci (mk ca) (modelInstances m),
                  modelFree = free'
                }
            )

-- | Update a model instance table by deleting the instance at a given contract address.
-- This does nothing to the model if there is no instance at the given address.
modelDeleteInstance :: ContractAddress -> Model -> Model
modelDeleteInstance ca@(ContractAddress ci csi) m = case Map.lookup ci (modelInstances m) of
    Nothing -> m
    Just inst ->
        if ca /= mInstanceAddr inst
            then m
            else
                m
                    { modelInstances = Map.delete ci (modelInstances m),
                      modelFree = Map.insert ci (succ csi) (modelFree m)
                    }

-- | Choose an arbitrary key-value pair from a map.
arbitraryMapElement :: Map.Map k v -> Gen (k, v)
arbitraryMapElement m = do
    ind <- choose (0, Map.size m - 1)
    return (Map.elemAt ind m)

-- | A test monad that can be used for performing operations on an instance table.
--  This uses the in-memory blob store.
newtype TestMonad (pv :: ProtocolVersion) a = TestMonad {runTestMonad :: ModuleCache -> MemBlobStore -> IO a}
    deriving
        (Functor, Applicative, Monad, MonadIO, MonadFail)
        via (ReaderT ModuleCache (ReaderT MemBlobStore IO))
    deriving
        (MonadBlobStore)
        via (ReaderT ModuleCache (MemBlobStoreT IO))

instance MonadCache ModuleCache (TestMonad pv) where
    getCache = TestMonad $ \c _ -> return c

instance (IsProtocolVersion pv) => MonadProtocolVersion (TestMonad pv) where
    type MPV (TestMonad pv) = pv

-- | Run a 'TestMonad' with a fresh in-memory blob store and an empty 0-sized module cache.
runTestMonadFresh :: TestMonad pv a -> IO a
runTestMonadFresh a = bracket newMemBlobStore destroyMemBlobStore $ \mbs -> do
    c <- newModuleCache 0
    runTestMonad a c mbs

-- | Generate a 'TestMonad' action for generating an instance table (by repeated creation and
--  deletion of instances), and a corresponding model.
generateFromUpdates :: (IsProtocolVersion pv) => Int -> Gen (TestMonad pv (Instances.Instances pv), Model)
generateFromUpdates n0 = gen n0 (return Instances.emptyInstances) emptyModel
  where
    gen 0 insts model = return (insts, model)
    gen n insts model = oneof $ [create, create, create] ++ [deleteExisting | not (null (modelInstances model))]
      where
        create = do
            dummyInstance <- genModelInstance

            let insts' = fmap snd . Instances.newContractInstance (\ca -> ((),) <$> toPersistentInstance (dummyInstance ca)) =<< insts
            let (_, model') = modelCreateInstance dummyInstance model
            gen (n - 1) insts' model'
        deleteExisting = do
            (_, mi) <- arbitraryMapElement (modelInstances model)
            let
                ca = mInstanceAddr mi
                insts' = Instances.deleteContractInstance ca =<< insts
                model' = modelDeleteInstance ca model
            gen (n - 1) insts' model'

-- | Create and delete instances, then check invariants hold.
testCreateDelete :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Int -> Property
testCreateDelete _ n = forAllShow (generateFromUpdates @pv n) (show . snd) $
    \(insts, model) -> idempotentIOProperty $ runTestMonadFresh $ do
        insts' <- insts
        invariantInstances insts'
        hlActual <- instancesToHashList insts'
        let hlModel = modelToHashList model

        hActual <- getHashM @_ @(InstancesHash pv) insts'
        let hModel = getHash model
        return $ hlActual === hlModel .&&. hActual === hModel

-- | Check the structural invariants of the instances table and check that the hashes match the
--  model.
checkInvariants ::
    forall pv.
    (IsProtocolVersion pv) =>
    Model ->
    Instances.Instances pv ->
    TestMonad pv ()
checkInvariants model insts = do
    invariantInstances insts
    hlActual <- instancesToHashList insts
    let hlModel = modelToHashList model
    checkBinary (==) hlActual hlModel "==" "actual hash list" "model hash list"
    hActual <- getHashM @_ @(InstancesHash pv) insts
    let hModel = getHash model
    checkBinary (==) hActual hModel "==" "actual root hash" "model root hash"

-- | An abstracted representation of an update to the instance table that can be useful when
-- reporting failures.
data Update = Create ContractAddress | Delete ContractAddress | Update ContractAddress
    deriving (Show)

-- | Test the various operations on the instance table for the given number of iterations.
--  After each operation, the invariants are asserted.
testUpdates :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Int -> Gen Property
testUpdates _ n0 = do
    (events, prop) <- tu n0 [] (return Instances.emptyInstances) emptyModel
    return $ counterexample (show events) $ idempotentIOProperty $ runTestMonadFresh prop
  where
    tu 0 evts insts model = return (reverse evts, checkInvariants @pv model =<< insts)
    tu n evts insts0 model =
        oneof $
            [create, deleteAbsent, updateAbsent]
                ++ (if null (modelInstances model) then [] else [updateExisting, deleteExisting])
                ++ (if null (modelFree model) then [] else [deleteFree, updateFree])
      where
        insts = do
            i <- insts0
            checkInvariants model i
            return i
        create = do
            dummyInstance <- genModelInstance
            let (cam, model') = modelCreateInstance dummyInstance model
            let insts' = do
                    (ca, i) <- Instances.newContractInstance (\ca -> (ca,) <$> toPersistentInstance (dummyInstance ca)) =<< insts
                    checkBinary (==) ca cam "==" "new instance address" "model new instance address"
                    return i
            tu (n - 1) (Create cam : evts) insts' model'
        deleteAbsent = do
            ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
            csi <- ContractSubindex <$> arbitrary
            let ca = ContractAddress ci csi
                insts' = Instances.deleteContractInstance ca =<< insts
                model' = modelDeleteInstance ca model
            tu (n - 1) (Delete ca : evts) insts' model'

        updateAbsent = do
            -- Pick a never-used contract index.
            ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
            csi <- ContractSubindex <$> arbitrary
            let ca = ContractAddress ci csi
            let insts' = do
                    i <- insts
                    res <-
                        Instances.updateContractInstance
                            (\_ -> fail "Update called on instance that should not exist.")
                            ca
                            i
                    unless (isNothing res) $
                        fail "Expected Nothing result when updating missing contract instance."
                    return i
            -- We do not update the model, as updating a non-existing instance will have no
            -- effect.
            tu (n - 1) (Update ca : evts) insts' model

        updateExisting = do
            (ci, mi@ModelInstance{..}) <- arbitraryMapElement (modelInstances model)
            let csi0 = contractSubindex (mInstanceAddr mi)
            -- We use a valid index, but possibly invalid subindex.
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            let ca = ContractAddress ci csi
            (newAmt :: Amount) <- arbitrary
            case mInstanceState of
                Instances.InstanceStateV0{} -> do
                    cs <- genV0ContractState
                    let newState = Instances.InstanceStateV0 cs
                    let newStateHash = getHash cs
                    let insts' = do
                            i <- insts
                            let upd (Instances.PersistentInstanceV0 inst) = do
                                    Instances.PersistentInstanceParameters{..} <-
                                        refLoad
                                            (Instances.pinstanceParameters inst)
                                    return
                                        ( (),
                                          Instances.PersistentInstanceV0
                                            inst
                                                { Instances.pinstanceAmount = newAmt,
                                                  Instances.pinstanceModel = newState,
                                                  Instances.pinstanceHash =
                                                    Instances.makeInstanceHashV0
                                                        pinstanceParameterHash
                                                        newStateHash
                                                        newAmt
                                                }
                                        )
                                upd _ = error "Instance version does not match expected value."
                            res <- Instances.updateContractInstance upd ca i
                            return $ maybe i snd res
                    let model' = modelUpdateInstanceAt ca newAmt newState newStateHash model
                    tu (n - 1) (Update ca : evts) insts' model'
                Instances.InstanceStateV1{} -> do
                    imps <- genV1ContractState
                    let newState = Instances.InstanceStateV1 (StateV1.makePersistent imps)
                    let newStateHash = getHash imps
                    let insts' = do
                            i <- insts
                            let upd (Instances.PersistentInstanceV1 inst) = do
                                    Instances.PersistentInstanceParameters{..} <-
                                        refLoad
                                            (Instances.pinstanceParameters inst)
                                    return
                                        ( (),
                                          Instances.PersistentInstanceV1
                                            inst
                                                { Instances.pinstanceAmount = newAmt,
                                                  Instances.pinstanceModel = newState,
                                                  Instances.pinstanceHash =
                                                    Instances.makeInstanceHashV1
                                                        pinstanceParameterHash
                                                        newStateHash
                                                        newAmt
                                                }
                                        )
                                upd _ = error "Instance version does not match expected value."
                            res <- Instances.updateContractInstance upd ca i
                            return $ maybe i snd res
                    let model' = modelUpdateInstanceAt ca newAmt newState newStateHash model
                    tu (n - 1) (Update ca : evts) insts' model'

        deleteExisting = do
            (ci, mi) <- arbitraryMapElement (modelInstances model)
            let csi0 = contractSubindex (mInstanceAddr mi)
            -- We use a valid index, but possibly invalid subindex.
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            let ca = ContractAddress ci csi
                insts' = Instances.deleteContractInstance ca =<< insts
                model' = modelDeleteInstance ca model
            tu (n - 1) (Delete ca : evts) insts' model'

        updateFree = do
            (ci, csi0) <- arbitraryMapElement (modelFree model)
            csi <- ContractSubindex <$> oneof [choose (0, fromIntegral csi0 - 1), choose (fromIntegral csi0, maxBound)]
            let ca = ContractAddress ci csi
            let insts' = do
                    i <- insts
                    res <-
                        Instances.updateContractInstance
                            (\_ -> fail "Update called on instance that should not exist.")
                            ca
                            i
                    unless (isNothing res) $
                        fail "Expected Nothing result when updating missing contract instance."
                    return i
            -- We do not update the model, as updating a non-existing instance will have no
            -- effect.
            tu (n - 1) (Update ca : evts) insts' model

        deleteFree = do
            (ci, csi0) <- arbitraryMapElement (modelFree model)
            csi <- oneof [return csi0, ContractSubindex <$> arbitrary]
            let
                ca = ContractAddress ci csi
                insts' = Instances.deleteContractInstance ca =<< insts
                model' = modelDeleteInstance ca model
            tu (n - 1) (Delete ca : evts) insts' model'

-- | Given a 'TestMonad' that generates an instance table and a corresponding model, test that
--  getting arbitrary contract addresses returns the same result in the instance table and model.
testGetInstance :: (IsProtocolVersion pv) => TestMonad pv (Instances.Instances pv) -> Model -> Gen Property
testGetInstance insts model =
    oneof $
        [present | not (null $ modelInstances model)]
            ++ [deleted | not (null $ modelFree model)]
            ++ [absent]
  where
    present = do
        (_, mi) <- arbitraryMapElement (modelInstances model)
        return $ idempotentIOProperty $ runTestMonadFresh $ do
            i <- Instances.lookupContractInstance (mInstanceAddr mi) =<< insts
            case i of
                Nothing -> return $ counterexample ("Missing instance @" ++ show (mInstanceAddr mi)) False
                Just ai -> modelsPersistentInstance mi ai
    deleted = do
        (ci, csi0) <- arbitraryMapElement (modelFree model)
        csi <- ContractSubindex <$> oneof [choose (0, fromIntegral csi0 - 1), choose (fromIntegral csi0, maxBound)]
        let ca = ContractAddress ci csi
        return $ idempotentIOProperty $ runTestMonadFresh $ do
            i <- Instances.lookupContractInstance ca =<< insts
            return $ counterexample ("Instance should be deleted @" ++ show ca) (isNothing i)
    absent = do
        ci <- ContractIndex <$> choose (fromIntegral $ modelBound model, maxBound)
        csi <- ContractSubindex <$> arbitrary
        let ca = ContractAddress ci csi
        return $ idempotentIOProperty $ runTestMonadFresh $ do
            i <- Instances.lookupContractInstance ca =<< insts
            return $ counterexample ("Instance should be absent @" ++ show ca) (isNothing i)

tests :: Word -> Spec
tests lvl = describe "GlobalStateTests.Instances" $ parallel $ do
    it "getInstance (P7)" $
        withMaxSuccess (100 * fromIntegral lvl) $
            forAllBlind (generateFromUpdates @'P7 5000) $
                \(i, m) -> withMaxSuccess 100 $ testGetInstance i m
    -- The hashing scheme for P1-P6 should be the same, but distinct from P7 onwards.
    it "5 create/delete - check at end (P5)" $ withMaxSuccess 5000 $ testCreateDelete SP5 5
    it "5 create/delete - check at end (P7)" $ withMaxSuccess 5000 $ testCreateDelete SP7 5
    it "10000 create/delete - check at end (P7)" $ withMaxSuccess 10 $ testCreateDelete SP7 10000
    it "500 instance updates - check every step (P5)" $ withMaxSuccess (100 * fromIntegral lvl) $ testUpdates SP5 500
    it "500 instance updates - check every step (P7)" $ withMaxSuccess (100 * fromIntegral lvl) $ testUpdates SP7 500
