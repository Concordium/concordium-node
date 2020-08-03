{-# LANGUAGE OverloadedStrings #-}
module Concordium.Scheduler.WasmIntegration(applyInitFun, applyReceiveFun, processModule) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Encoding as Text
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import System.IO.Unsafe
import Control.Monad

import Concordium.Crypto.FFIHelpers(rs_free_array_len)
import Concordium.Types
import Concordium.Wasm

import qualified Language.Wasm as LW
import qualified Language.Wasm.Structure as LW

foreign import ccall unsafe "call_init"
   call_init :: Ptr Word8 -- ^Pointer to the Wasm module.
             -> CSize -- ^Length of the Wasm module.
             -> Ptr Word8 -- ^Pointer to the serialized chain meta + init ctx.
             -> CSize -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state and logs, if applicable, or null, signaling out-of-energy.


foreign import ccall unsafe "call_receive"
   call_receive :: Ptr Word8 -- ^Pointer to the Wasm module.
             -> CSize -- ^Length of the Wasm module.
             -> Ptr Word8 -- ^Pointer to the serialized receive context.
             -> CSize  -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of the function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the current state of the smart contracts. This will not be modified.
             -> CSize -- ^Length of the state.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state, logs, and actions, if applicable, or null, signaling out-of-energy.

-- |Apply an init function which is assumed to be a part of the module.
applyInitFun
    :: ModuleInterface
    -> ChainMetadata -- ^Chain information available to the contracts.
    -> InitContext -- ^Additional parameters supplied by the chain and
                  -- available to the init method.
    -> InitName -- ^Which method to invoke.
    -> Parameter -- ^User-provided parameter to the init method.
    -> Amount -- ^Amount the contract is going to be initialized with.
    -> InterpreterEnergy -- ^Maximum amount of energy that can be used by the interpreter.
    -> Maybe (Either ContractExecutionFailure (SuccessfulResultData ()), InterpreterEnergy)
    -- ^Nothing if execution ran out of energy.
    -- Just (result, usedEnergy) otherwise, where @usedEnergy@ is the amount of energy that was used.
applyInitFun miface cm initCtx iName param amnt energy = processInterpreterResult result energy
  where result = unsafeDupablePerformIO $ do
              BSU.unsafeUseAsCStringLen wasmBytes $ \(wasmBytesPtr, wasmBytesLen) ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                      alloca $ \outputLenPtr -> do
                        outPtr <- call_init (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen)
                                           (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                           amountWord
                                           (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                           (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                           outputLenPtr
                        if outPtr == nullPtr then return Nothing
                        else do
                          len <- peek outputLenPtr
                          bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr)
                          return (Just bs)
        wasmBytes = imWasmSource . miModule $ miface
        initCtxBytes = encodeChainMeta cm <> encode initCtx
        paramBytes = BSS.fromShort (parameter param)
        amountWord = _amount amnt
        nameBytes = Text.encodeUtf8 (initName iName)

processInterpreterResult :: Serialize a => Maybe BS.ByteString -> InterpreterEnergy -> Maybe (Either ContractExecutionFailure (SuccessfulResultData a), InterpreterEnergy)
processInterpreterResult result energy = case result of
    Nothing -> Just (Left RuntimeFailure, 0)
    Just bs ->
      let decoder = do
            tag <- getWord8
            case tag of
              0 -> return (Left ContractReject)
              1 -> Right <$> get
              _ -> fail $ "Invalid tag: " ++ show tag
      in
      case runGet decoder bs of
        Right x -> Just (x, energy)
        Left err -> error $ "Invariant violation. Could not interpret output from interpreter: " ++ err


-- |Apply a receive function which is assumed to be part of the given module.
applyReceiveFun
    :: ModuleInterface
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
    -- of execution with the amount of energy that was used.
applyReceiveFun miface cm receiveCtx rName param amnt cs energy = processInterpreterResult result energy
  where result = unsafeDupablePerformIO $ do
              BSU.unsafeUseAsCStringLen wasmBytes $ \(wasmBytesPtr, wasmBytesLen) ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen stateBytes $ \(stateBytesPtr, stateBytesLen) ->
                      BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                        alloca $ \outputLenPtr -> do
                          outPtr <- call_receive (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen)
                                                 (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                                 amountWord
                                                 (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                                 (castPtr stateBytesPtr) (fromIntegral stateBytesLen)
                                                 (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                                 outputLenPtr
                          if outPtr == nullPtr then return Nothing
                          else do
                            len <- peek outputLenPtr
                            bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr)
                            return (Just bs)
        wasmBytes = imWasmSource . miModule $ miface
        initCtxBytes = encodeChainMeta cm <> encodeReceiveContext receiveCtx
        amountWord = _amount amnt
        stateBytes = contractState cs
        paramBytes = BSS.fromShort (parameter param)
        nameBytes = Text.encodeUtf8 (receiveName rName)


-- |Process a module as received and make a module interface. This should
-- check the module is well-formed, and has the right imports and exports. It
-- should also do any pre-processing of the module (such as partial
-- compilation or instrumentation) that is needed to apply the exported
-- functions from it in an efficient way.
processModule :: WasmModule -> Maybe ModuleInterface
processModule modl =
  case LW.decode (wasmSource modl) of
    Right modul -> case LW.validate modul of
                    Right _ -> do
                      let miSize = fromIntegral (BS.length (wasmSource modl))
                          miModuleRef = getModuleRef modl
                          miModule = InstrumentedWasmModule{ imWasmVersion = wasmVersion modl, imWasmSource = wasmSource modl }
                      (miExposedInit, miExposedReceive) <- processExports modul
                      return ModuleInterface{..}
                    Left err -> error $ "Validation error: " ++ show err
    Left err -> error $ "Module not well-formed: " ++ show err

  where processExports modul = do
          -- get the exported functions only, we don't care about globals and whatnot.
          -- TODO: We will care about memory though, but that is not done right now.
          let exports = [name | LW.Export{..} <- LW.exports modul, case desc of
                                                                    LW.ExportFunc _ -> True
                                                                    _ -> False ]
          -- TODO: This does not do any groupings, and probably we want a
          -- different naming convention.
          foldM (\(i,r) name ->
                   let name' = LText.toStrict name in
                   -- TODO: We should also be checking here that there are no duplicates.
                   if "init" `Text.isPrefixOf` name' then Just (Set.insert (InitName name') i, r)
                   else if "receive" `Text.isPrefixOf` name' then Just (i, Set.insert (ReceiveName name') r)
                   else Nothing)
                (Set.empty, Set.empty)
                exports
          -- TODO: Validate import names as well. Only the module is validated by the validate function.
