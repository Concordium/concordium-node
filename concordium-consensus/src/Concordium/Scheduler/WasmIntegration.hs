{-# LANGUAGE OverloadedStrings #-}
module Concordium.Scheduler.WasmIntegration where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import qualified Data.Text.Encoding as Text
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import System.IO.Unsafe
import Control.Monad

import Concordium.Crypto.FFIHelpers(rs_free_array_len)
import Concordium.Types
import Concordium.Wasm

foreign import ccall unsafe "call_init"
   call_init :: Ptr Word8 -- ^Pointer to the Wasm module.
             -> CSize -- ^Length of the Wasm module.
             -> Ptr Word8 -- ^Pointer to the serialized init context. Lenght 32 bytes.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state and logs, if applicable, or null.


foreign import ccall unsafe "call_receive"
   call_receive :: Ptr Word8 -- ^Pointer to the Wasm module.
             -> CSize -- ^Length of the Wasm module.
             -> Ptr Word8 -- ^Pointer to the serialized init context. Lenght 32 bytes.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of the function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the current state of the smart contracts. This will not be modified.
             -> CSize -- ^Length of the state.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state, logs, and actions, if applicable, or null.


-- |Apply an init function which is assumed to be part of the given module.
applyInitFun
    :: ModuleInterface
    -> ChainMetadata -- ^Metadata available to the contract.
    -> InitContext
    -> InitName  -- ^Which method to invoke.
    -> Parameter -- ^Parameters available to the method.
    -> Amount  -- ^Amount the contract is initialized with.
    -> InterpreterEnergy  -- ^Amount of energy available for execution.
    -> Maybe (Either ContractExecutionFailure (SuccessfulResultData ()), InterpreterEnergy)
    -- ^Nothing if execution used up all the energy,
    -- and otherwise the result of execution with remaining interpreter energy.
applyInitFun miface cm initCtx iName param amnt energy = processInterpreterResult result energy
  where result = unsafeDupablePerformIO $ do
              BSU.unsafeUseAsCStringLen wasmBytes $ \(wasmBytesPtr, wasmBytesLen) ->
                BSU.unsafeUseAsCString initCtxBytes $ \initCtxBytesPtr ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    alloca $ \outputLenPtr -> do
                      outPtr <- call_init (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen)
                                         (castPtr initCtxBytesPtr)
                                         amountWord
                                         (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                         outputLenPtr
                      if outPtr == nullPtr then return Nothing
                      else do
                        len <- peek outputLenPtr
                        bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr)
                        return (Just bs)
        wasmBytes = imWasmSource . miModule $ miface
        initCtxBytes = encode initCtx
        amountWord = _amount amnt
        nameBytes = Text.encodeUtf8 (initName iName)

processInterpreterResult :: Serialize a => Maybe BS.ByteString -> InterpreterEnergy -> Maybe (Either ContractExecutionFailure (SuccessfulResultData a), InterpreterEnergy)
processInterpreterResult result energy = case result of
    Nothing -> Just (Left RuntimeFailure, 0)
    Just bs ->
      let decoder = do
            tag <- getWord8
            case tag of
              0 -> do
                len <- getWord32be
                evs <- replicateM (fromIntegral len) get
                return (Left (ContractReject evs))
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
    -> ReceiveContext
    -> ReceiveName  -- ^Which method to invoke.
    -> Parameter -- ^Parameters available to the method.
    -> Amount  -- ^Amount the contract is initialized with.
    -> ContractState -- ^State of the contract to start in.
    -> InterpreterEnergy  -- ^Amount of energy available for execution.
    -> Maybe (Either ContractExecutionFailure (SuccessfulResultData ReceiveExecutionResult), InterpreterEnergy)
    -- ^Nothing if execution used up all the energy,
    -- and otherwise the result of execution with remaining interpreter energy.
applyReceiveFun miface cm receiveCtx rName param amnt cs energy = processInterpreterResult result energy
  where result = unsafeDupablePerformIO $ do
              BSU.unsafeUseAsCStringLen wasmBytes $ \(wasmBytesPtr, wasmBytesLen) ->
                BSU.unsafeUseAsCString initCtxBytes $ \initCtxBytesPtr ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen stateBytes $ \(stateBytesPtr, stateBytesLen) ->
                      alloca $ \outputLenPtr -> do
                        outPtr <- call_receive (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen)
                                               (castPtr initCtxBytesPtr)
                                               amountWord
                                               (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                               (castPtr stateBytesPtr) (fromIntegral stateBytesLen)
                                               outputLenPtr
                        if outPtr == nullPtr then return Nothing
                        else do
                          len <- peek outputLenPtr
                          bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr)
                          return (Just bs)
        wasmBytes = imWasmSource . miModule $ miface
        initCtxBytes = encode receiveCtx
        amountWord = _amount amnt
        stateBytes = contractState cs
        nameBytes = Text.encodeUtf8 (receiveName rName)


-- |Process a module as received and make a module interface.
-- This should check the module is well-formed, and has the right imports and exports.
processModule :: WasmModule -> Maybe ModuleInterface
processModule modl = Just ModuleInterface{..}
  where miModuleRef = getModuleRef modl
        miExposedInit = Set.singleton (InitName "init")
        miExposedReceive = Set.singleton (ReceiveName "receive")
        miSize = fromIntegral (BS.length (wasmSource modl))
        miModule = InstrumentedWasmModule{ imWasmVersion = wasmVersion modl, imWasmSource = wasmSource modl }
        
