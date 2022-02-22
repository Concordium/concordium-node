{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
-- |This module provides most of the functionality that deals with calling V1 smart contracts, processing responses,
-- and resuming computations. It is used directly by the Scheduler to run smart contracts.
--
-- This module uses FFI very heavily. The functions that are imported are defined in smart-contracts/wasm-chain-integration/src/v1/ffi.rs
-- in the smart contracts submodule.
module Concordium.Scheduler.WasmIntegration.V1(
  InvokeMethod(..),
  InitResultData(..),
  ReceiveResultData(..),
  applyInitFun,
  cerToRejectReasonInit,
  cerToRejectReasonReceive,
  applyReceiveFun,
  resumeReceiveFun,
  processModule,
  ReturnValue,
  ReceiveInterruptedState,
  InvokeResponseCode(..),
  EnvFailure(..),
  ContractExecutionReject(..),
  ContractCallFailure(..),
  ccfToReturnValue,
  returnValueToByteString
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (new)
import Foreign.Storable
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Aeson as AE
import qualified Data.Text.Encoding as Text
import Data.Serialize
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import System.IO.Unsafe
import Foreign.ForeignPtr
import Control.Monad

import Concordium.Crypto.FFIHelpers(rs_free_array_len)
import Concordium.Types
import qualified Concordium.Types.Execution as Exec
import Concordium.Wasm
import Concordium.GlobalState.Wasm
import Concordium.Utils.Serialization
import Concordium.GlobalState.ContractStateFFIHelpers (LoadCallback)
import qualified Concordium.GlobalState.ContractStateV1 as StateV1

foreign import ccall unsafe "return_value_to_byte_array" return_value_to_byte_array :: Ptr ReturnValue -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "&box_vec_u8_free" freeReturnValue :: FunPtr (Ptr ReturnValue -> IO ())
foreign import ccall unsafe "&receive_interrupted_state_free" freeReceiveInterruptedState :: FunPtr (Ptr (Ptr ReceiveInterruptedState) -> IO ())

foreign import ccall "validate_and_process_v1"
   validate_and_process :: Ptr Word8 -- ^Pointer to the Wasm module source.
                        -> CSize -- ^Length of the module source.
                        -> Ptr CSize -- ^Total length of the output.
                        -> Ptr (Ptr ModuleArtifactV1) -- ^Null, or the processed module artifact. This is null if and only if the return value is null.
                        -> IO (Ptr Word8) -- ^Null, or exports.

-- |Return value of a V1 contract call. This is deliberately opaque so that we avoid redundant data copying
-- for return values for inner contract calls.
newtype ReturnValue = ReturnValue { rvPtr :: ForeignPtr ReturnValue }

{-# NOINLINE returnValueToByteString #-}
-- |Convert a return value to a byte array. This copies the data of the return value.
returnValueToByteString :: ReturnValue -> BS.ByteString
returnValueToByteString rv = unsafePerformIO $
  withReturnValue rv $ \p -> alloca $ \outputLenPtr -> do
    rp <- return_value_to_byte_array p outputLenPtr
    len <- peek outputLenPtr
    BSU.unsafePackCStringFinalizer rp (fromIntegral len) (rs_free_array_len rp (fromIntegral len))

-- json instance based on hex
instance AE.ToJSON ReturnValue where
  toJSON = AE.String . Text.decodeUtf8 . BS16.encode . returnValueToByteString

instance Show ReturnValue where
  show = BS8.unpack . BS16.encode . returnValueToByteString

-- |State of the Wasm module when a host operation is invoked (a host operation
-- is either a transfer to an account, or a contract call, at present). This can
-- only be resumed once. Calling resume on this twice will lead to unpredictable
-- behaviour, including the possibility of segmentation faults.
newtype ReceiveInterruptedState = ReceiveInterruptedState { risPtr :: ForeignPtr (Ptr ReceiveInterruptedState) }

withReturnValue :: ReturnValue -> (Ptr ReturnValue -> IO a) -> IO a
withReturnValue ReturnValue{..} = withForeignPtr rvPtr

-- |Use the (maybe) return value in a foreign computation. If the first argument
-- is 'Nothing' then the computation is given the null pointer.
withMaybeReturnValue :: Maybe ReturnValue -> (Ptr ReturnValue -> IO a) -> IO a
withMaybeReturnValue Nothing k = k nullPtr
withMaybeReturnValue (Just rv) k = withReturnValue rv k

withReceiveInterruptedState :: ReceiveInterruptedState -> (Ptr (Ptr ReceiveInterruptedState) -> IO a) -> IO a
withReceiveInterruptedState = withForeignPtr . risPtr

-- |Possible reasons why a contract call of a V1 contract failed.
data ContractCallFailure =
  -- |The contract call failed because the contract rejected execution for its own reason, or execution trapped.
  ExecutionReject !ContractExecutionReject
  -- |Contract call of a V1 contract failed due to other, environment reasons, such as the intended contract not existing.
  | EnvFailure !EnvFailure

-- |Convert a contract call failure to a return value. If a contract call fails
-- due to the contract itself, then it can return some data (e.g., an error
-- message). This function extracts that, if it can.
ccfToReturnValue :: ContractCallFailure -> Maybe ReturnValue
ccfToReturnValue (ExecutionReject LogicReject{..}) = Just cerReturnValue
ccfToReturnValue (ExecutionReject Trap) = Nothing
ccfToReturnValue (EnvFailure _) = Nothing

-- |Result of an invoke. This just adds Success to the contract call failure.
data InvokeResponseCode =
  Success
  | Error !ContractCallFailure
  | MessageSendFailed

-- |Possible reasons why invocation failed that are not directly logic failure of a V1 call.
data EnvFailure =
  AmountTooLarge !Address !Amount
  | MissingAccount !AccountAddress
  | MissingContract !ContractAddress
  | InvalidEntrypoint !ModuleRef !ReceiveName -- Attempting to invoke a non-existing entrypoint.
  deriving (Show)

-- |Encode the response into 64 bits. This is necessary since Wasm only allows
-- us to pass simple scalars as parameters. Everything else requires passing
-- data in memory, or via host functions, both of which are difficult.
-- The response is encoded as follows.
-- - success is encoded as 0
-- - every failure has all bits of the first (most significant) 3 bytes set
-- - in case of failure
--   - if the 4th byte is 0 then the remaining 4 bytes encode the rejection reason from the contract
--   - otherwise only the 4th byte is used, and encodes the enviroment failure.
invokeResponseToWord64 :: InvokeResponseCode -> Word64
invokeResponseToWord64 Success = 0
invokeResponseToWord64 (Error (EnvFailure e)) =
  case e of
    AmountTooLarge _ _ -> 0xffff_ff01_0000_0000
    MissingAccount _ -> 0xffff_ff02_0000_0000
    MissingContract _ -> 0xffff_ff03_0000_0000
    InvalidEntrypoint _ _ -> 0xffff_ff04_0000_0000
invokeResponseToWord64 MessageSendFailed = 0xffff_ff05_0000_0000
invokeResponseToWord64 (Error (ExecutionReject Trap)) = 0xffff_ff06_0000_0000
invokeResponseToWord64 (Error (ExecutionReject LogicReject{..})) =
  -- make the last 32 bits the value of the rejection reason
  let unsigned = fromIntegral cerRejectReason :: Word32 -- reinterpret the bits
  in 0xffff_ff00_0000_0000 .|. fromIntegral unsigned -- and cut away the upper 32 bits


foreign import ccall "call_init_v1"
   call_init :: LoadCallback -- Callbacks for loading state. Not needed in reality, but the way things are set it is. It does not hurt to pass.
             -> Ptr ModuleArtifactV1 -- ^Pointer to the Wasm artifact.
             -> Ptr Word8 -- ^Pointer to the serialized chain meta + init ctx.
             -> CSize -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Word64 -- ^Available energy.
             -> Ptr (Ptr ReturnValue) -- ^Location where the pointer to the return value will be written.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> Ptr (Ptr StateV1.MutableStateInner) -- ^Location where the pointer to the mutable state will be written.
             -> IO (Ptr Word8) -- ^New state and logs, if applicable, or null, signaling out-of-energy.


foreign import ccall "call_receive_v1"
   call_receive :: LoadCallback -- ^Callback in case any state needs to be loaded from block state storage.
             -> Ptr ModuleArtifactV1 -- ^Pointer to the Wasm artifact.
             -> Ptr Word8 -- ^Pointer to the serialized receive context.
             -> CSize  -- ^Length of the preceding data.
             -> Word64 -- ^Amount
             -> Ptr Word8 -- ^Pointer to the name of the function to invoke.
             -> CSize -- ^Length of the name.
             -> Ptr (Ptr StateV1.MutableStateInner)
             -- ^Pointer to the current state of the smart contracts. If
             -- successful, pointer to the new state will be written here.
             -> Ptr Word8 -- ^Pointer to the parameter.
             -> CSize -- ^Length of the parameter bytes.
             -> Word64 -- ^Available energy.
             -> Ptr (Ptr ReturnValue) -- ^Location where the pointer to the return value will be written.
             -> Ptr (Ptr ReceiveInterruptedState) -- ^Location where the pointer to interrupted config will be stored.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state, logs, and actions, if applicable, or null, signaling out-of-energy.


foreign import ccall "resume_receive_v1"
   resume_receive :: LoadCallback
             -> Ptr (Ptr ReceiveInterruptedState) -- ^Location where the pointer to interrupted config will be stored.
             -> Word8 -- ^Tag of whether the state  been updated or not. If this is 0 then the state has not been updated, otherwise it has.
             -> Ptr (Ptr StateV1.MutableStateInner) -- ^Pointer to the current state of the smart contracts.
             -> Word64 -- ^New balance of the contract.
             -> Word64 -- ^Return status from the interrupt.
             -> Ptr ReturnValue -- ^Return value from the call, if any. This will be replaced with an empty vector.
             -> Word64 -- ^Available energy.
             -> Ptr (Ptr ReturnValue) -- ^Location where the pointer to the return value will be written.
             -> Ptr CSize -- ^Length of the output byte array, if non-null.
             -> IO (Ptr Word8) -- ^New state, logs, and actions, if applicable, or null, signaling out-of-energy.


-- |Apply an init function which is assumed to be a part of the module.
{-# NOINLINE applyInitFun #-}
applyInitFun
    :: LoadCallback
    -> ModuleInterfaceV V1
    -> ChainMetadata -- ^Chain information available to the contracts.
    -> InitContext -- ^Additional parameters supplied by the chain and
                  -- available to the init method.
    -> InitName -- ^Which method to invoke.
    -> Parameter -- ^User-provided parameter to the init method.
    -> Amount -- ^Amount the contract is going to be initialized with.
    -> InterpreterEnergy -- ^Maximum amount of energy that can be used by the interpreter.
    -> Maybe (Either ContractExecutionReject InitResultData, InterpreterEnergy)
    -- ^Nothing if execution ran out of energy.
    -- Just (result, remainingEnergy) otherwise, where @remainingEnergy@ is the amount of energy that is left from the amount given.
applyInitFun cbk miface cm initCtx iName param amnt iEnergy = unsafePerformIO $ do
              withModuleArtifact wasmArtifact $ \wasmArtifactPtr ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                      alloca $ \returnValuePtrPtr -> alloca $ \statePtrPtr -> alloca $ \outputLenPtr -> do
                        outPtr <- call_init cbk
                                           wasmArtifactPtr
                                           (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                           amountWord
                                           (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                           (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                           energy
                                           returnValuePtrPtr
                                           outputLenPtr
                                           statePtrPtr
                        -- This case should not happen, it means a mismatch between two sides of FFI.
                        if outPtr == nullPtr then return (Just (Left Trap, 0))
                        else do
                          len <- peek outputLenPtr
                          bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                          returnValuePtr <- peek returnValuePtrPtr
                          statePtr <- peek statePtrPtr
                          processInitResult cbk bs returnValuePtr statePtr
    where
        wasmArtifact = imWasmArtifact miface
        initCtxBytes = encodeChainMeta cm <> encodeInitContext initCtx
        paramBytes = BSS.fromShort (parameter param)
        energy = fromIntegral iEnergy
        amountWord = _amount amnt
        nameBytes = Text.encodeUtf8 (initName iName)

-- |Allowed methods that a contract can invoke.
data InvokeMethod =
  -- |Transfer to an account.
  Transfer {
    imtTo :: !AccountAddress,
    imtAmount :: !Amount
    }
  -- |Call another smart contract with the given parameter.
  | Call {
    imcTo :: !ContractAddress,
    imcParam :: !Parameter,
    imcName :: !EntrypointName,
    imcAmount :: !Amount
    }

getInvokeMethod :: Get InvokeMethod
getInvokeMethod = getWord8 >>= \case
  0 -> Transfer <$> get <*> get
  1 -> Call <$> get <*> get <*> get <*> get
  n -> fail $ "Unsupported invoke method tag: " ++ show n

-- |Data return from the contract in case of successful initialization.
data InitResultData = InitSuccess {
  irdReturnValue :: !ReturnValue,
  irdNewState :: !StateV1.MutableState,
  irdLogs :: ![ContractEvent]
  }

-- |Data returned from the receive call. In contrast to an init call, a receive call may interrupt.
data ReceiveResultData =
  -- |Execution terminated with success.
  ReceiveSuccess {
    rrdReturnValue :: !ReturnValue,
    rrdNewState :: !StateV1.MutableState,
    rrdStateChanged :: !Bool,
    rrdLogs :: ![ContractEvent]
  } |
  -- |Execution invoked a method. The current state is returned.
  ReceiveInterrupt {
    rrdCurrentState :: !StateV1.MutableState,
    rrdStateChanged :: !Bool,
    rrdMethod :: !InvokeMethod,
    rrdLogs :: ![ContractEvent],
    rrdInterruptedConfig :: !ReceiveInterruptedState
  }

-- |Parse the logs that were produced. This must match serialization of logs on the other end of the ffi,
-- in smart-contracts/wasm-chain-integration/src/v1/ffi.rs
getLogs :: Get [ContractEvent]
getLogs = do
  len <- fromIntegral <$> getWord32be
  replicateM len get

-- |Reason for failure of contract execution.
data ContractExecutionReject =
  LogicReject { cerRejectReason :: !Int32,
                cerReturnValue :: !ReturnValue
              } -- ^Contract decided to terminate execution.
  | Trap -- ^A trap was triggered.
  deriving (Show)

cerToRejectReasonInit :: ContractExecutionReject -> Exec.RejectReason
cerToRejectReasonInit LogicReject{..} = Exec.RejectedInit cerRejectReason
cerToRejectReasonInit Trap = Exec.RuntimeFailure

-- |Parse the response from invoking a @call_init_v1@ method. This attempts to
-- parse the returned byte array and depending on its contents it will also
-- update the given pointers. See documentation of the above mentioned imported
-- function for the specification of the return value.
processInitResult ::
  LoadCallback -- ^State context.
  -> BS.ByteString  -- ^Serialized output.
  -> Ptr ReturnValue -- ^Location where the pointer to the return value is (potentially) stored.
  -> Ptr StateV1.MutableStateInner -- ^Location where the pointer to the initial state is written.
  -- |Result, and remaining energy. Returns 'Nothing' if and only if
  -- execution ran out of energy.
  -> IO (Maybe (Either ContractExecutionReject InitResultData, InterpreterEnergy))
processInitResult callbacks result returnValuePtr newStatePtr = case BS.uncons result of
  Nothing -> error "Internal error: Could not parse the result from the interpreter."
  Just (tag, payload) ->
    case tag of
      0 -> return Nothing
      1 -> let parser = -- runtime failure
                label "Init.remainingEnergy" getWord64be
          in let remainingEnergy = parseResult parser
             in do return (Just (Left Trap, fromIntegral remainingEnergy))
      2 -> let parser = do -- reject
                rejectReason <- label "Reject.rejectReason" getInt32be
                remainingEnergy <- label "Reject.remainingEnergy" getWord64be
                return (rejectReason, remainingEnergy)
          in let (cerRejectReason, remainingEnergy) = parseResult parser
             in do cerReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                   return (Just (Left LogicReject{..}, fromIntegral remainingEnergy))
      3 -> -- done
        let parser = do
              logs <- label "Done.logs" getLogs
              remainingEnergy <- label "Done.remainingEnergy" getWord64be
              return (logs, remainingEnergy)
        in let (irdLogs, remainingEnergy) = parseResult parser
           in do irdReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                 irdNewState <- StateV1.newMutableState callbacks newStatePtr
                 return (Just (Right InitSuccess{..}, fromIntegral remainingEnergy))
      _ -> fail $ "Invalid tag: " ++ show tag
    where parseResult parser =
            case runGet parser payload of
              Right x -> x
              Left err -> error $ "Internal error: Could not interpret output from interpreter: " ++ err

-- The input was allocated with alloca. We allocate a fresh one with malloc (via 'new') and register
-- a finalizer for it. The reason for doing this is that we don't want to pre-emptively use malloc
-- when it is very likely that there will be no interrupt of the contract.
newReceiveInterruptedState :: Ptr (Ptr ReceiveInterruptedState) -> IO ReceiveInterruptedState
newReceiveInterruptedState interruptedStatePtr = do
  fp <- newForeignPtr finalizerFree =<< new =<< peek interruptedStatePtr -- allocate a new persistent location and register it for freeing.
  -- this second finalizer will run **before** the first one we registered above
  -- (see https://hackage.haskell.org/package/base-4.16.0.0/docs/Foreign-ForeignPtr.html#v:addForeignPtrFinalizer)
  addForeignPtrFinalizer freeReceiveInterruptedState fp
  return (ReceiveInterruptedState fp)

-- |Convert a contract call failure to the Scheduler's reject reason.
cerToRejectReasonReceive :: ContractAddress -> ReceiveName -> Parameter -> ContractCallFailure -> Exec.RejectReason
cerToRejectReasonReceive contractAddress receiveName parameter (ExecutionReject LogicReject{..}) = Exec.RejectedReceive{rejectReason=cerRejectReason,..}
cerToRejectReasonReceive _ _ _ (ExecutionReject Trap) = Exec.RuntimeFailure
cerToRejectReasonReceive _ _ _ (EnvFailure e) = case e of
  AmountTooLarge ad am -> Exec.AmountTooLarge ad am
  MissingAccount aref -> Exec.InvalidAccountReference aref
  MissingContract cref -> Exec.InvalidContractAddress cref
  InvalidEntrypoint mref rn -> Exec.InvalidReceiveMethod mref rn

-- |Parse the response from invoking either a @call_receive_v1@ or
-- @resume_receive_v1@ method. This attempts to parse the returned byte array
-- and depending on its contents it will also update the given pointers. See
-- documentation of the above mentioned imported functions for the specification
-- of the return value.
processReceiveResult ::
  LoadCallback -- ^State context.
  -> StateV1.MutableState -- ^State execution started in.
  -> BS.ByteString -- ^Serialized output.
  -> Ptr ReturnValue -- ^Location where the pointer to the return value is (potentially) stored.
  -> Ptr StateV1.MutableStateInner -- ^Pointer to the state of the contract at the time of termination.
  -> Either ReceiveInterruptedState (Ptr (Ptr ReceiveInterruptedState)) -- ^Location where the pointer to interrupted config is (potentially) stored.
  -- |Result, and remaining energy. Returns 'Nothing' if and only if
  -- execution ran out of energy.
  -> IO (Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy))
processReceiveResult callbacks initialState result returnValuePtr statePtr eitherInterruptedStatePtr = case BS.uncons result of
  Nothing -> error "Internal error: Could not parse the result from the interpreter."
  Just (tag, payload) ->
    case tag of
      0 -> return Nothing
      1 -> let parser = -- runtime failure
                label "Reject.remainingEnergy" getWord64be
          in let remainingEnergy = parseResult parser
             in return (Just (Left Trap, fromIntegral remainingEnergy))
      2 -> let parser = do -- reject
                rejectReason <- label "Reject.rejectReason" getInt32be
                remainingEnergy <- label "Reject.remainingEnergy" getWord64be
                return (rejectReason, remainingEnergy)
          in let (cerRejectReason, remainingEnergy) = parseResult parser
             in do cerReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                   return (Just (Left LogicReject{..}, fromIntegral remainingEnergy))
      4 -> let parser = do -- interrupt
                remainingEnergy <- label "Interrupt.remainingEnergy" getWord64be
                logs <- label "Interrupt.logs" getLogs
                method <- label "Interrupt.method" getInvokeMethod
                return (remainingEnergy, logs, method)
          in let (remainingEnergy, rrdLogs, rrdMethod)= parseResult parser
             in do rrdInterruptedConfig <- case eitherInterruptedStatePtr of
                     Left rrid -> return rrid
                     Right interruptedStatePtr -> newReceiveInterruptedState interruptedStatePtr
                   if statePtr == nullPtr then
                     let rrdCurrentState = initialState
                         rrdStateChanged = False
                     in return (Just (Right ReceiveInterrupt{..}, fromIntegral remainingEnergy))
                   else do
                     let rrdStateChanged = True
                     rrdCurrentState <- StateV1.newMutableState callbacks statePtr
                     return (Just (Right ReceiveInterrupt{..}, fromIntegral remainingEnergy))
      3 -> -- done
        let parser = do
              logs <- label "Done.logs" getLogs
              remainingEnergy <- label "Done.remainingEnergy" getWord64be
              return (logs, remainingEnergy)
        in let (rrdLogs, remainingEnergy) = parseResult parser
           in do rrdReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                 if statePtr == nullPtr then
                   let rrdNewState = initialState
                       rrdStateChanged = False
                   in return (Just (Right ReceiveSuccess{..}, fromIntegral remainingEnergy))
                 else do
                   let rrdStateChanged = True
                   rrdNewState <- StateV1.newMutableState callbacks statePtr
                   return (Just (Right ReceiveSuccess{..}, fromIntegral remainingEnergy))

      _ -> fail $ "Invalid tag: " ++ show tag
    where parseResult parser =
            case runGet parser payload of
              Right x -> x
              Left err -> error $ "Internal error: Could not interpret output from interpreter: " ++ err


-- |Apply a receive function which is assumed to be part of the given module.
{-# NOINLINE applyReceiveFun #-}
applyReceiveFun
    :: ModuleInterfaceV V1
    -> ChainMetadata -- ^Metadata available to the contract.
    -> ReceiveContext -- ^Additional parameter supplied by the chain and
                     -- available to the receive method.
    -> ReceiveName  -- ^Which method to invoke.
    -> Parameter -- ^Parameters available to the method.
    -> Amount  -- ^Amount the contract is initialized with.
    -> StateV1.MutableState -- ^State of the contract to start in, and a way to use it.
    -> InterpreterEnergy  -- ^Amount of energy available for execution.
    -> Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy)
    -- ^Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
applyReceiveFun miface cm receiveCtx rName param amnt initialState initialEnergy = unsafePerformIO $ do
              withModuleArtifact wasmArtifact $ \wasmArtifactPtr ->
                BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
                  BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                    StateV1.withMutableState initialState $ \curStatePtr -> alloca $ \statePtrPtr -> do
                      poke statePtrPtr curStatePtr
                      BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                        alloca $ \outputLenPtr -> alloca $ \outputReturnValuePtrPtr -> alloca $ \outputInterruptedConfigPtrPtr -> do
                          outPtr <- call_receive callbacks
                                                wasmArtifactPtr
                                                (castPtr initCtxBytesPtr) (fromIntegral initCtxBytesLen)
                                                amountWord
                                                (castPtr nameBytesPtr) (fromIntegral nameBytesLen)
                                                statePtrPtr
                                                (castPtr paramBytesPtr) (fromIntegral paramBytesLen)
                                                energy
                                                outputReturnValuePtrPtr
                                                outputInterruptedConfigPtrPtr
                                                outputLenPtr
                          if outPtr == nullPtr then return (Just (Left Trap, 0)) -- this case should not happen
                          else do
                            len <- peek outputLenPtr
                            bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                            returnValuePtr <- peek outputReturnValuePtrPtr
                            statePtr <- peek statePtrPtr
                            processReceiveResult callbacks initialState bs returnValuePtr statePtr (Right outputInterruptedConfigPtrPtr)
    where
        wasmArtifact = imWasmArtifact miface
        initCtxBytes = encodeChainMeta cm <> encodeReceiveContext receiveCtx
        amountWord = _amount amnt
        energy = fromIntegral initialEnergy
        paramBytes = BSS.fromShort (parameter param)
        nameBytes = Text.encodeUtf8 (receiveName rName)
        callbacks = StateV1.msContext initialState
-- |Resume execution after processing the interrupt. This can only be called once on a single 'ReceiveInterruptedState'.
{-# NOINLINE resumeReceiveFun #-}
resumeReceiveFun ::
    ReceiveInterruptedState
    -> StateV1.MutableState -- ^State of the contract to resume in.
    -> Bool -- ^Whether the state has changed in the call.
    -> Amount -- ^Current balance of the contract, if it changed.
    -> InvokeResponseCode
    -> Maybe ReturnValue
    -> InterpreterEnergy  -- ^Amount of energy available for execution.
    -> Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy)
    -- ^Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
resumeReceiveFun is currentState stateChanged amnt statusCode rVal remainingEnergy = unsafePerformIO $ do
              withReceiveInterruptedState is $ \isPtr ->
                StateV1.withMutableState currentState $ \curStatePtr -> alloca $ \statePtrPtr -> do
                  poke statePtrPtr curStatePtr
                  withMaybeReturnValue rVal $ \rValPtr ->
                    alloca $ \outputLenPtr -> alloca $ \outputReturnValuePtrPtr -> do
                      outPtr <- resume_receive callbacks
                                              isPtr
                                              newStateTag
                                              statePtrPtr
                                              amountWord
                                              (invokeResponseToWord64 statusCode)
                                              rValPtr
                                              energy
                                              outputReturnValuePtrPtr
                                              outputLenPtr
                      if outPtr == nullPtr then return (Just (Left Trap, 0)) -- this case should not happen
                      else do
                        len <- peek outputLenPtr
                        bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                        returnValuePtr <- peek outputReturnValuePtrPtr
                        statePtr <- peek statePtrPtr
                        processReceiveResult callbacks currentState bs returnValuePtr statePtr (Left is)
    where
        newStateTag = if stateChanged then 1 else 0
        energy = fromIntegral remainingEnergy
        amountWord = _amount amnt
        callbacks = StateV1.msContext currentState

-- |Process a module as received and make a module interface.
-- This
-- - checks the module is well-formed, and has the right imports and exports for a V1 module.
-- - makes a module artifact and allocates it on the Rust side, returning a pointer and a finalizer.
{-# NOINLINE processModule #-}
processModule :: WasmModuleV V1 -> Maybe (ModuleInterfaceV V1)
processModule modl = do
  (bs, imWasmArtifactV1) <- ffiResult
  case getExports bs of
    Left _ -> Nothing
    Right (miExposedInit, miExposedReceive) ->
      let miModuleRef = getModuleRef modl
          miModule = InstrumentedWasmModuleV1{..}
      in Just ModuleInterface{miModuleSize = moduleSourceLength (wmvSource modl),..}

  where ffiResult = unsafePerformIO $ do
          unsafeUseModuleSourceAsCStringLen (wmvSource modl) $ \(wasmBytesPtr, wasmBytesLen) ->
              alloca $ \outputLenPtr ->
                alloca $ \outputModuleArtifactPtr -> do
                  outPtr <- validate_and_process (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen) outputLenPtr outputModuleArtifactPtr
                  if outPtr == nullPtr then return Nothing
                  else do
                    len <- peek outputLenPtr
                    bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                    moduleArtifact <- newModuleArtifactV1 =<< peek outputModuleArtifactPtr
                    return (Just (bs, moduleArtifact))

        getExports bs =
          flip runGet bs $ do
            len <- fromIntegral <$> getWord16be
            namesByteStrings <- replicateM len getByteStringWord16
            let names = foldM (\(inits, receives) name -> do
                          case Text.decodeUtf8' name of
                            Left _ -> Nothing
                            Right nameText | Just initName <- extractInitName nameText -> return (Set.insert initName inits, receives)
                                           | Just (initName, receiveName) <- extractInitReceiveNames nameText ->
                                               return (inits, Map.insertWith Set.union initName (Set.singleton receiveName) receives)
                                           -- ignore any other exported functions.
                                           -- This is different from V0 contracts, which disallow any extra function exports.
                                           -- This feature was requested by some users.
                                           | otherwise -> return (inits, receives)
                          ) (Set.empty, Map.empty) namesByteStrings
            case names of
              Nothing -> fail "Incorrect response from FFI call."
              Just x@(exposedInits, exposedReceives) ->
                if Map.keysSet exposedReceives `Set.isSubsetOf` exposedInits then return x else fail "Receive functions that do not correspond to any contract."
