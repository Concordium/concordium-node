{-# LANGUAGE OverloadedStrings #-}

module Concordium.Scheduler.WasmIntegration (applyInitFun, applyReceiveFun, processModule) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Concordium.Crypto.FFIHelpers (rs_free_array_len)
import Concordium.GlobalState.Wasm
import Concordium.Types
import Concordium.Utils.Serialization
import Concordium.Wasm

foreign import ccall "validate_and_process_v0"
    validate_and_process ::
        -- |Pointer to the Wasm module source.
        Ptr Word8 ->
        -- |Length of the module source.
        CSize ->
        -- |Total length of the output.
        Ptr CSize ->
        -- |Length of the artifact.
        Ptr CSize ->
        -- |Processed module artifact.
        Ptr (Ptr Word8) ->
        -- |Null, or exports.
        IO (Ptr Word8)

foreign import ccall "call_init_v0"
    call_init ::
        -- |Pointer to the Wasm artifact.
        Ptr Word8 ->
        -- |Length of the artifact.
        CSize ->
        -- |Pointer to the serialized chain meta + init ctx.
        Ptr Word8 ->
        -- |Length of the preceding data.
        CSize ->
        -- |Amount
        Word64 ->
        -- |Pointer to the name of function to invoke.
        Ptr Word8 ->
        -- |Length of the name.
        CSize ->
        -- |Pointer to the parameter.
        Ptr Word8 ->
        -- |Length of the parameter bytes.
        CSize ->
        -- |Limit the number of logs and size of return values.
        Word8 ->
        -- |Available energy.
        Word64 ->
        -- |Length of the output byte array, if non-null.
        Ptr CSize ->
        -- |New state and logs, if applicable, or null, signalling out-of-energy.
        IO (Ptr Word8)

foreign import ccall "call_receive_v0"
    call_receive ::
        -- |Pointer to the Wasm artifact.
        Ptr Word8 ->
        -- |Length of the artifact.
        CSize ->
        -- |Pointer to the serialized receive context.
        Ptr Word8 ->
        -- |Length of the preceding data.
        CSize ->
        -- |Amount
        Word64 ->
        -- |Pointer to the name of the function to invoke.
        Ptr Word8 ->
        -- |Length of the name.
        CSize ->
        -- |Pointer to the current state of the smart contracts. This will not be modified.
        Ptr Word8 ->
        -- |Length of the state.
        CSize ->
        -- |Pointer to the parameter.
        Ptr Word8 ->
        -- |Length of the parameter bytes.
        CSize ->
        -- |Max parameter size.
        CSize ->
        -- |Limit the number of logs and size of return values.
        Word8 ->
        -- |Available energy.
        Word64 ->
        -- |Length of the output byte array, if non-null.
        Ptr CSize ->
        -- |New state, logs, and actions, if applicable, or null, signalling out-of-energy.
        IO (Ptr Word8)

-- |Apply an init function which is assumed to be a part of the module.
applyInitFun ::
    InstrumentedModuleV V0 ->
    -- |Chain information available to the contracts.
    ChainMetadata ->
    -- |Additional parameters supplied by the chain and
    -- available to the init method.
    InitContext ->
    -- |Which method to invoke.
    InitName ->
    -- |User-provided parameter to the init method.
    Parameter ->
    -- |Limit the number of logs and size of return values.
    Bool ->
    -- |Amount the contract is going to be initialized with.
    Amount ->
    -- |Maximum amount of energy that can be used by the interpreter.
    InterpreterEnergy ->
    -- |Nothing if execution ran out of energy.
    -- Just (result, remainingEnergy) otherwise, where @remainingEnergy@ is the amount of energy that is left from the amount given.
    Maybe (Either ContractExecutionFailure (SuccessfulResultData ()), InterpreterEnergy)
applyInitFun miface cm initCtx iName param limitLogsAndRvs amnt iEnergy = processInterpreterResult (get :: Get ()) result
  where
    result = unsafePerformIO $ do
        BSU.unsafeUseAsCStringLen wasmArtifact $ \(wasmArtifactPtr, wasmArtifactLen) ->
            BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                        alloca $ \outputLenPtr -> do
                            outPtr <-
                                call_init
                                    (castPtr wasmArtifactPtr)
                                    (fromIntegral wasmArtifactLen)
                                    (castPtr initCtxBytesPtr)
                                    (fromIntegral initCtxBytesLen)
                                    amountWord
                                    (castPtr nameBytesPtr)
                                    (fromIntegral nameBytesLen)
                                    (castPtr paramBytesPtr)
                                    (fromIntegral paramBytesLen)
                                    (if limitLogsAndRvs then 1 else 0)
                                    energy
                                    outputLenPtr
                            if outPtr == nullPtr
                                then return Nothing
                                else do
                                    len <- peek outputLenPtr
                                    bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                                    return (Just bs)
    wasmArtifact = imWasmArtifactBytes miface
    initCtxBytes = encodeChainMeta cm <> encodeInitContext initCtx
    paramBytes = BSS.fromShort (parameter param)
    energy = fromIntegral iEnergy
    amountWord = _amount amnt
    nameBytes = Text.encodeUtf8 (initName iName)

processInterpreterResult ::
    -- |How to decode the output messages.
    Get a ->
    -- |Nothing if runtime failure, serialized output otherwise.
    Maybe BS.ByteString ->
    -- |Result, and remaining energy.
    Maybe (Either ContractExecutionFailure (SuccessfulResultData a), InterpreterEnergy)
processInterpreterResult aDecoder result = case result of
    Nothing -> Just (Left RuntimeFailure, 0)
    Just bs ->
        let decoder = do
                tag <- getWord8
                case tag of
                    0 -> return Nothing
                    1 -> do
                        rejectReason <- getInt32be
                        remainingEnergy <- getWord64be
                        return (Just (Left (ContractReject rejectReason), fromIntegral remainingEnergy))
                    2 -> do
                        rData <- getSuccessfulResultData aDecoder
                        remainingEnergy <- getWord64be
                        return (Just (Right rData, fromIntegral remainingEnergy))
                    _ -> fail $ "Invalid tag: " ++ show tag
        in  case runGet decoder bs of
                Right x -> x
                Left err -> error $ "Invariant violation. Could not interpret output from interpreter: " ++ err

-- |Apply a receive function which is assumed to be part of the given module.
applyReceiveFun ::
    InstrumentedModuleV V0 ->
    -- |Metadata available to the contract.
    ChainMetadata ->
    -- |Additional parameter supplied by the chain and
    -- available to the receive method.
    ReceiveContext ->
    -- |Which method to invoke.
    ReceiveName ->
    -- |Parameters available to the method.
    Parameter ->
    -- |Max parameter size.
    Word16 ->
    -- |Limit the number of logs and size of return values.
    Bool ->
    -- |Amount the contract is initialized with.
    Amount ->
    -- |State of the contract to start in.
    ContractState ->
    -- |Amount of energy available for execution.
    InterpreterEnergy ->
    -- |Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
    Maybe (Either ContractExecutionFailure (SuccessfulResultData ActionsTree), InterpreterEnergy)
applyReceiveFun miface cm receiveCtx rName param maxParamLen limitLogsAndRvs amnt cs initialEnergy = processInterpreterResult getActionsTree result
  where
    result = unsafePerformIO $ do
        BSU.unsafeUseAsCStringLen wasmArtifact $ \(wasmArtifactPtr, wasmArtifactLen) ->
            BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen stateBytes $ \(stateBytesPtr, stateBytesLen) ->
                        BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                            alloca $ \outputLenPtr -> do
                                outPtr <-
                                    call_receive
                                        (castPtr wasmArtifactPtr)
                                        (fromIntegral wasmArtifactLen)
                                        (castPtr initCtxBytesPtr)
                                        (fromIntegral initCtxBytesLen)
                                        amountWord
                                        (castPtr nameBytesPtr)
                                        (fromIntegral nameBytesLen)
                                        (castPtr stateBytesPtr)
                                        (fromIntegral stateBytesLen)
                                        (castPtr paramBytesPtr)
                                        (fromIntegral paramBytesLen)
                                        (fromIntegral maxParamLen)
                                        (if limitLogsAndRvs then 1 else 0)
                                        energy
                                        outputLenPtr
                                if outPtr == nullPtr
                                    then return Nothing
                                    else do
                                        len <- peek outputLenPtr
                                        bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                                        return (Just bs)
    wasmArtifact = imWasmArtifactBytes miface
    initCtxBytes = encodeChainMeta cm <> encodeReceiveContext receiveCtx
    amountWord = _amount amnt
    stateBytes = contractState cs
    energy = fromIntegral initialEnergy
    paramBytes = BSS.fromShort (parameter param)
    nameBytes = Text.encodeUtf8 (receiveName rName)

-- |Process a module as received and make a module interface. This should
-- check the module is well-formed, and has the right imports and exports. It
-- should also do any pre-processing of the module (such as partial
-- compilation or instrumentation) that is needed to apply the exported
-- functions from it in an efficient way.
{-# NOINLINE processModule #-}
processModule :: WasmModuleV V0 -> Maybe (ModuleInterfaceV V0)
processModule modl = do
    (bs, miModule) <- ffiResult
    case getExports bs of
        Left _ -> Nothing
        Right (miExposedInit, miExposedReceive) ->
            let miModuleRef = getModuleRef modl
            in  Just ModuleInterface{miModuleSize = moduleSourceLength (wmvSource modl), ..}
  where
    ffiResult = unsafePerformIO $ do
        unsafeUseModuleSourceAsCStringLen (wmvSource modl) $ \(wasmBytesPtr, wasmBytesLen) ->
            alloca $ \outputLenPtr ->
                alloca $ \artifactLenPtr ->
                    alloca $ \outputModuleArtifactPtr -> do
                        outPtr <- validate_and_process (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen) outputLenPtr artifactLenPtr outputModuleArtifactPtr
                        if outPtr == nullPtr
                            then return Nothing
                            else do
                                len <- peek outputLenPtr
                                bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                                artifactLen <- peek artifactLenPtr
                                artifactPtr <- peek outputModuleArtifactPtr
                                moduleArtifact <-
                                    BSU.unsafePackCStringFinalizer
                                        artifactPtr
                                        (fromIntegral artifactLen)
                                        (rs_free_array_len artifactPtr (fromIntegral artifactLen))
                                return (Just (bs, instrumentedModuleFromBytes SV0 moduleArtifact))

    getExports bs =
        flip runGet bs $ do
            len <- fromIntegral <$> getWord16be
            namesByteStrings <- replicateM len getByteStringWord16
            let names =
                    foldM
                        ( \(inits, receives) name -> do
                            case Text.decodeUtf8' name of
                                Left _ -> Nothing
                                Right nameText
                                    | isValidInitName nameText -> return (Set.insert (InitName nameText) inits, receives)
                                    | isValidReceiveName nameText ->
                                        let cname = "init_" <> Text.takeWhile (/= '.') nameText
                                        in  return (inits, Map.insertWith Set.union (InitName cname) (Set.singleton (ReceiveName nameText)) receives)
                                    | otherwise -> Nothing
                        )
                        (Set.empty, Map.empty)
                        namesByteStrings
            case names of
                Nothing -> fail "Incorrect response from FFI call."
                Just x@(exposedInits, exposedReceives) ->
                    if Map.keysSet exposedReceives `Set.isSubsetOf` exposedInits then return x else fail "Receive functions that do not correspond to any contract."
