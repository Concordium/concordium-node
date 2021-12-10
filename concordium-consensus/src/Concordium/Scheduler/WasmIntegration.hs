{-# LANGUAGE OverloadedStrings #-}
module Concordium.Scheduler.WasmIntegration(applyInitFun, applyReceiveFun, processModule) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Serialize
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import System.IO.Unsafe
import Control.Monad

import Concordium.Crypto.FFIHelpers(rs_free_array_len)
import Concordium.Types
import Concordium.Wasm
import Concordium.GlobalState.Wasm
import Concordium.Utils.Serialization

foreign import ccall "validate_and_process_v0"
   validate_and_process :: Ptr Word8 -- ^Pointer to the Wasm module source.
                        -> CSize -- ^Length of the module source.
                        -> Ptr CSize -- ^Total length of the output.
                        -> Ptr (Ptr ModuleArtifactV0) -- ^Null, or the processed module artifact. This is null if and only if the return value is null.
                        -> IO (Ptr Word8) -- ^Null, or exports.

foreign import ccall "call_init_v0"
   call_init :: Ptr ModuleArtifactV0 -- ^Pointer to the Wasm artifact.
             -> Ptr Word8 -- ^Pointer to the serialized chain meta + init ctx.
             -> CSize -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Word64 -- ^Available energy.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state and logs, if applicable, or null, signaling out-of-energy.


foreign import ccall "call_receive_v0"
   call_receive :: Ptr ModuleArtifactV0 -- ^Pointer to the Wasm artifact.
             -> Ptr Word8 -- ^Pointer to the serialized receive context.
             -> CSize  -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of the function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the current state of the smart contracts. This will not be modified.
             -> CSize -- ^Length of the state.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Word64 -- ^Available energy.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state, logs, and actions, if applicable, or null, signaling out-of-energy.

-- |Apply an init function which is assumed to be a part of the module.
applyInitFun
    :: ModuleInterfaceV V0
    -> ChainMetadata -- ^Chain information available to the contracts.
    -> InitContext -- ^Additional parameters supplied by the chain and
                  -- available to the init method.
    -> InitName -- ^Which method to invoke.
    -> Parameter -- ^User-provided parameter to the init method.
    -> Amount -- ^Amount the contract is going to be initialized with.
    -> InterpreterEnergy -- ^Maximum amount of energy that can be used by the interpreter.
    -> Maybe (Either ContractExecutionFailure (SuccessfulResultData ()), InterpreterEnergy)
    -- ^Nothing if execution ran out of energy.
    -- Just (result, remainingEnergy) otherwise, where @remainingEnergy@ is the amount of energy that is left from the amount given.
applyInitFun miface cm initCtx iName param amnt iEnergy = processInterpreterResult (get :: Get ()) result
  where result = unsafePerformIO $ do
              withModuleArtifact wasmArtifact $ \wasmArtifactPtr ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                      alloca $ \outputLenPtr -> do
                        outPtr <- call_init wasmArtifactPtr
                                           (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                           amountWord
                                           (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                           (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                           energy
                                           outputLenPtr
                        if outPtr == nullPtr then return Nothing
                        else do
                          len <- peek outputLenPtr
                          bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                          return (Just bs)
        wasmArtifact = imWasmArtifact miface
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
      in
      case runGet decoder bs of
        Right x -> x
        Left err -> error $ "Invariant violation. Could not interpret output from interpreter: " ++ err


-- |Apply a receive function which is assumed to be part of the given module.
applyReceiveFun
    :: ModuleInterfaceV V0
    -> ChainMetadata -- ^Metadata available to the contract.
    -> ReceiveContext -- ^Additional parameter supplied by the chain and
                     -- available to the receive method.
    -> ReceiveName  -- ^Which method to invoke.
    -> Parameter -- ^Parameters available to the method.
    -> Amount  -- ^Amount the contract is initialized with.
    -> ContractState -- ^State of the contract to start in.
    -> InterpreterEnergy  -- ^Amount of energy available for execution.
    -> Maybe (Either ContractExecutionFailure (SuccessfulResultData ActionsTree), InterpreterEnergy)
    -- ^Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
applyReceiveFun miface cm receiveCtx rName param amnt cs initialEnergy = processInterpreterResult getActionsTree result
  where result = unsafePerformIO $ do
              withModuleArtifact wasmArtifact $ \wasmArtifactPtr ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen stateBytes $ \(stateBytesPtr, stateBytesLen) ->
                      BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                        alloca $ \outputLenPtr -> do
                          outPtr <- call_receive wasmArtifactPtr
                                                 (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                                 amountWord
                                                 (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                                 (castPtr stateBytesPtr) (fromIntegral stateBytesLen)
                                                 (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                                 energy
                                                 outputLenPtr
                          if outPtr == nullPtr then return Nothing
                          else do
                            len <- peek outputLenPtr
                            bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                            return (Just bs)
        wasmArtifact = imWasmArtifact miface
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
processModule :: WasmModule -> Maybe (ModuleInterfaceV V0)
processModule modl = do
  guard (wasmVersion modl == 0)
  (bs, imWasmArtifactV0) <- ffiResult
  case getExports bs of
    Left _ -> Nothing
    Right (miExposedInit, miExposedReceive) ->
      let miModuleRef = getModuleRef modl
          miModule = InstrumentedWasmModuleV0{..}
      in Just ModuleInterface{miModuleSize = moduleSourceLength $ wasmSource modl,..}

  where ffiResult = unsafePerformIO $ do
          unsafeUseModuleSourceAsCStringLen (wasmSource modl) $ \(wasmBytesPtr, wasmBytesLen) ->
              alloca $ \outputLenPtr -> 
                alloca $ \outputModuleArtifactPtr -> do
                  outPtr <- validate_and_process (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen) outputLenPtr outputModuleArtifactPtr
                  if outPtr == nullPtr then return Nothing
                  else do
                    len <- peek outputLenPtr
                    bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                    moduleArtifact <- newModuleArtifactV0 =<< peek outputModuleArtifactPtr
                    return (Just (bs, moduleArtifact))

        getExports bs =
          flip runGet bs $ do
            len <- fromIntegral <$> getWord16be
            namesByteStrings <- replicateM len getByteStringWord16
            let names = foldM (\(inits, receives) name -> do
                          case Text.decodeUtf8' name of
                            Left _ -> Nothing
                            Right nameText | isValidInitName nameText -> return (Set.insert (InitName nameText) inits, receives)
                                           | isValidReceiveName nameText ->
                                               let cname = "init_" <> Text.takeWhile (/= '.') nameText
                                               in return (inits, Map.insertWith Set.union (InitName cname) (Set.singleton (ReceiveName nameText)) receives)
                                           | otherwise -> Nothing
                          ) (Set.empty, Map.empty) namesByteStrings
            case names of
              Nothing -> fail "Incorrect response from FFI call."
              Just x@(exposedInits, exposedReceives) ->
                if Map.keysSet exposedReceives `Set.isSubsetOf` exposedInits then return x else fail "Receive functions that do not correspond to any contract."
