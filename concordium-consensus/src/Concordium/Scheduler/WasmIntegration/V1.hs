{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |This module provides most of the functionality that deals with calling V1 smart contracts, processing responses,
-- and resuming computations. It is used directly by the Scheduler to run smart contracts.
--
-- This module uses FFI very heavily. The functions that are imported are defined in smart-contracts/wasm-chain-integration/src/v1/ffi.rs
-- in the smart contracts submodule.
module Concordium.Scheduler.WasmIntegration.V1 (
    InvokeMethod (..),
    InitResultData (..),
    ReceiveResultData (..),
    applyInitFun,
    cerToRejectReasonInit,
    cerToRejectReasonReceive,
    applyReceiveFun,
    resumeReceiveFun,
    processModule,
    ReturnValue,
    ReceiveInterruptedState,
    InvokeResponseCode (..),
    EnvFailure (..),
    ContractExecutionReject (..),
    ContractCallFailure (..),
    ccfToReturnValue,
    returnValueToByteString,
    byteStringToReturnValue,
) where

import Control.Monad
import qualified Data.Aeson as AE
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (new)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Concordium.Crypto.FFIHelpers (rs_free_array_len)
import Concordium.GlobalState.ContractStateFFIHelpers (LoadCallback)
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Wasm
import Concordium.Types
import qualified Concordium.Types.Execution as Exec
import Concordium.Utils.Serialization
import Concordium.Wasm
import qualified Concordium.Wasm as Wasm

foreign import ccall unsafe "return_value_to_byte_array" return_value_to_byte_array :: Ptr ReturnValue -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "&box_vec_u8_free" freeReturnValue :: FunPtr (Ptr ReturnValue -> IO ())
foreign import ccall unsafe "&receive_interrupted_state_free" freeReceiveInterruptedState :: FunPtr (Ptr (Ptr ReceiveInterruptedState) -> IO ())

-- |Allocate and return a Rust vector that contains the given data.
foreign import ccall "copy_to_vec_ffi" createReturnValue :: Ptr Word8 -> CSize -> IO (Ptr ReturnValue)

foreign import ccall "validate_and_process_v1"
    validate_and_process ::
        -- |Whether the current protocol version supports smart contract upgrades.
        Word8 ->
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

-- |Return value of a V1 contract call. This is deliberately opaque so that we avoid redundant data copying
-- for return values for inner contract calls.
newtype ReturnValue = ReturnValue {rvPtr :: ForeignPtr ReturnValue}

{-# NOINLINE returnValueToByteString #-}

-- |Convert a return value to a byte array. This copies the data of the return value.
returnValueToByteString :: ReturnValue -> BS.ByteString
returnValueToByteString rv = unsafePerformIO $
    withReturnValue rv $ \p -> alloca $ \outputLenPtr -> do
        rp <- return_value_to_byte_array p outputLenPtr
        len <- peek outputLenPtr
        BSU.unsafePackCStringFinalizer rp (fromIntegral len) (rs_free_array_len rp (fromIntegral len))

-- | Constructs a ReturnValue from a bytestring.
-- More specifically it copies the bytestring to a Vec<u8> in Rust, which returns a pointer used for the ReturnValue.
byteStringToReturnValue :: BS.ByteString -> ReturnValue
byteStringToReturnValue bs = unsafePerformIO $ do
    returnValuePtr <- BSU.unsafeUseAsCStringLen bs $ \(charPtr, len) -> createReturnValue (castPtr charPtr) (fromIntegral len)
    returnValueForeignPtr <- newForeignPtr freeReturnValue returnValuePtr
    return ReturnValue{rvPtr = returnValueForeignPtr}

-- json instance based on hex
instance AE.ToJSON ReturnValue where
    toJSON = AE.String . Text.decodeUtf8 . BS16.encode . returnValueToByteString

instance Show ReturnValue where
    show = BS8.unpack . BS16.encode . returnValueToByteString

-- |State of the Wasm module when a host operation is invoked (a host operation
-- is either a transfer to an account, a contract call, or an upgrade at present). This can
-- only be resumed once. Calling resume on this twice will lead to unpredictable
-- behaviour, including the possibility of segmentation faults.
newtype ReceiveInterruptedState = ReceiveInterruptedState {risPtr :: ForeignPtr (Ptr ReceiveInterruptedState)}

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
data ContractCallFailure
    = -- |The contract call failed because the contract rejected execution for its own reason, or execution trapped.
      ExecutionReject !ContractExecutionReject
    | -- |Contract call of a V1 contract failed due to other, environment reasons, such as the intended contract not existing.
      EnvFailure !EnvFailure

-- |Convert a contract call failure to a return value. If a contract call fails
-- due to the contract itself, then it can return some data (e.g., an error
-- message). This function extracts that, if it can.
ccfToReturnValue :: ContractCallFailure -> Maybe ReturnValue
ccfToReturnValue (ExecutionReject LogicReject{..}) = Just cerReturnValue
ccfToReturnValue (ExecutionReject Trap) = Nothing
ccfToReturnValue (EnvFailure _) = Nothing

-- |Result of an invoke. This just adds Success to the contract call failure.
data InvokeResponseCode
    = Success
    | Error !ContractCallFailure
    | MessageSendFailed
    | -- |Attempt to upgrade to a non existent module.
      UpgradeInvalidModuleRef !ModuleRef
    | -- |Attempt to upgrade to a module where the contract name did not match.
      UpgradeInvalidContractName !ModuleRef !InitName
    | -- |Attempt to upgrade to a non-supported module version.
      UpgradeInvalidVersion !ModuleRef !WasmVersion

-- |Possible reasons why invocation failed that are not directly logic failure of a V1 call.
data EnvFailure
    = AmountTooLarge !Address !Amount
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
--   - otherwise only the 4th byte is used, and encodes the environment failure.
invokeResponseToWord64 :: InvokeResponseCode -> Word64
invokeResponseToWord64 Success = 0
invokeResponseToWord64 (Error (EnvFailure e)) =
    case e of
        AmountTooLarge _ _ -> 0xffff_ff01_0000_0000
        MissingAccount _ -> 0xffff_ff02_0000_0000
        MissingContract _ -> 0xffff_ff03_0000_0000
        InvalidEntrypoint _ _ -> 0xffff_ff04_0000_0000
invokeResponseToWord64 MessageSendFailed = 0xffff_ff05_0000_0000
invokeResponseToWord64 (UpgradeInvalidModuleRef _) = 0xffff_ff07_0000_0000
invokeResponseToWord64 (UpgradeInvalidContractName _ _) = 0xffff_ff08_0000_0000
invokeResponseToWord64 (UpgradeInvalidVersion _ _) = 0xffff_ff09_0000_0000
invokeResponseToWord64 (Error (ExecutionReject Trap)) = 0xffff_ff06_0000_0000
invokeResponseToWord64 (Error (ExecutionReject LogicReject{..})) =
    -- make the last 32 bits the value of the rejection reason
    let unsigned = fromIntegral cerRejectReason :: Word32 -- reinterpret the bits
    in  0xffff_ff00_0000_0000 .|. fromIntegral unsigned -- and cut away the upper 32 bits

foreign import ccall "call_init_v1"
    call_init ::
        -- |Callbacks for loading state. Not needed in reality, but the way things are set it is. It does not hurt to pass.
        LoadCallback ->
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
        -- |Limit number of logs and size of return value.
        Word8 ->
        -- |Available energy.
        Word64 ->
        -- |Location where the pointer to the return value will be written.
        Ptr (Ptr ReturnValue) ->
        -- |Length of the output byte array, if non-null.
        Ptr CSize ->
        -- |Location where the pointer to the mutable state will be written.
        Ptr (Ptr StateV1.MutableStateInner) ->
        -- |New state and logs, if applicable, or null, signalling out-of-energy.
        IO (Ptr Word8)

foreign import ccall "call_receive_v1"
    call_receive ::
        -- |Callback in case any state needs to be loaded from block state storage.
        LoadCallback ->
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
        -- |Whether to invoke the default/fallback method instead.
        Word8 ->
        -- |Pointer to the current state of the smart contracts. If
        -- successful, pointer to the new state will be written here if the state has been modified.
        -- If the state has not been modified then a null pointer is written here.
        Ptr (Ptr StateV1.MutableStateInner) ->
        -- |Pointer to the parameter.
        Ptr Word8 ->
        -- |Length of the parameter bytes.
        CSize ->
        -- |Max parameter size.
        CSize ->
        -- |Limit number of logs and size of return value.
        Word8 ->
        -- |Available energy.
        Word64 ->
        -- |Location where the pointer to the return value will be written.
        Ptr (Ptr ReturnValue) ->
        -- |Location where the pointer to interrupted config will be stored.
        Ptr (Ptr ReceiveInterruptedState) ->
        -- |Length of the output byte array, if non-null.
        Ptr CSize ->
        -- |Non-zero to enable support of chain queries.
        Word8 ->
        -- |New state, logs, and actions, if applicable, or null, signalling out-of-energy.
        IO (Ptr Word8)

foreign import ccall "resume_receive_v1"
    resume_receive ::
        LoadCallback ->
        -- |Location where the pointer to interrupted config will be stored.
        Ptr (Ptr ReceiveInterruptedState) ->
        -- |Tag of whether the state has been updated or not. If this is 0 then the state has not been updated, otherwise, it has.
        Word8 ->
        -- |Pointer to the current state of the smart contracts. If
        -- successful, pointer to the new state will be written here if the state has been modified.
        -- If the state has not been modified then a null pointer is written here.
        Ptr (Ptr StateV1.MutableStateInner) ->
        -- |New balance of the contract.
        Word64 ->
        -- |Return status from the interrupt.
        Word64 ->
        -- |Return value from the call, if any. This will be replaced with an empty vector.
        Ptr ReturnValue ->
        -- |Available energy.
        Word64 ->
        -- |Location where the pointer to the return value will be written.
        Ptr (Ptr ReturnValue) ->
        -- |Length of the output byte array, if non-null.
        Ptr CSize ->
        -- |New state, logs, and actions, if applicable, or null, signalling out-of-energy.
        IO (Ptr Word8)

-- |Apply an init function which is assumed to be a part of the module.
{-# NOINLINE applyInitFun #-}
applyInitFun ::
    LoadCallback ->
    InstrumentedModuleV V1 ->
    -- |Chain information available to the contracts.
    ChainMetadata ->
    -- |Additional parameters supplied by the chain and
    -- available to the init method.
    InitContext ->
    -- |Which method to invoke.
    InitName ->
    -- |User-provided parameter to the init method.
    Parameter ->
    -- |Limit number of logs and size of return values.
    Bool ->
    -- |Amount the contract is going to be initialized with.
    Amount ->
    -- |Maximum amount of energy that can be used by the interpreter.
    InterpreterEnergy ->
    -- |Nothing if execution ran out of energy.
    -- Just (result, remainingEnergy) otherwise, where @remainingEnergy@ is the amount of energy that is left from the amount given.
    Maybe (Either ContractExecutionReject InitResultData, InterpreterEnergy)
applyInitFun cbk miface cm initCtx iName param limitLogsAndRvs amnt iEnergy = unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen wasmArtifactBytes $ \(wasmArtifactPtr, wasmArtifactLen) ->
        BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
            BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                    alloca $ \returnValuePtrPtr -> alloca $ \statePtrPtr -> alloca $ \outputLenPtr -> do
                        outPtr <-
                            call_init
                                cbk
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
                                returnValuePtrPtr
                                outputLenPtr
                                statePtrPtr
                        -- This case should not happen, it means a mismatch between two sides of FFI.
                        if outPtr == nullPtr
                            then return (Just (Left Trap, 0))
                            else do
                                len <- peek outputLenPtr
                                bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                                returnValuePtr <- peek returnValuePtrPtr
                                statePtr <- peek statePtrPtr
                                processInitResult cbk bs returnValuePtr statePtr
  where
    wasmArtifactBytes = imWasmArtifactBytes miface
    initCtxBytes = encodeChainMeta cm <> encodeInitContext initCtx
    paramBytes = BSS.fromShort (parameter param)
    energy = fromIntegral iEnergy
    amountWord = _amount amnt
    nameBytes = Text.encodeUtf8 (initName iName)

-- |Allowed methods that a contract can invoke.
data InvokeMethod
    = -- |Transfer to an account.
      Transfer
        { imtTo :: !AccountAddress,
          imtAmount :: !Amount
        }
    | -- |Call another smart contract with the given parameter.
      Call
        { imcTo :: !ContractAddress,
          imcParam :: !Parameter,
          imcName :: !EntrypointName,
          imcAmount :: !Amount
        }
    | -- |Upgrade a smart contract such that it uses the new 'ModuleRef' for execution.
      Upgrade
        { imuModRef :: !ModuleRef
        }
    | -- |Query the balance and staked balance of an account.
      QueryAccountBalance
        { imqabAddress :: !AccountAddress
        }
    | -- |Query the balance of a contract.
      QueryContractBalance
        { imqcbAddress :: !ContractAddress
        }
    | -- |Query the CCD/EUR and EUR/NRG exchange rates.
      QueryExchangeRates

getInvokeMethod :: Get InvokeMethod
getInvokeMethod =
    getWord8 >>= \case
        0 -> Transfer <$> get <*> get
        1 -> Call <$> get <*> Wasm.getParameterUnchecked <*> get <*> get
        2 -> Upgrade <$> get
        3 -> QueryAccountBalance <$> get
        4 -> QueryContractBalance <$> get
        5 -> return QueryExchangeRates
        n -> fail $ "Unsupported invoke method tag: " ++ show n

-- |Data return from the contract in case of successful initialization.
data InitResultData = InitSuccess
    { irdReturnValue :: !ReturnValue,
      irdNewState :: !StateV1.MutableState,
      irdLogs :: ![ContractEvent]
    }

-- |Data returned from the receive call. In contrast to an init call, a receive call may interrupt.
data ReceiveResultData
    = -- |Execution terminated with success.
      ReceiveSuccess
        { rrdReturnValue :: !ReturnValue,
          rrdNewState :: !StateV1.MutableState,
          rrdStateChanged :: !Bool,
          rrdLogs :: ![ContractEvent]
        }
    | -- |Execution invoked a method. The current state is returned.
      ReceiveInterrupt
        { rrdCurrentState :: !StateV1.MutableState,
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
data ContractExecutionReject
    = -- |Contract decided to terminate execution.
      LogicReject
        { cerRejectReason :: !Int32,
          cerReturnValue :: !ReturnValue
        }
    | -- |A trap was triggered.
      Trap
    deriving (Show)

cerToRejectReasonInit :: ContractExecutionReject -> Exec.RejectReason
cerToRejectReasonInit LogicReject{..} = Exec.RejectedInit cerRejectReason
cerToRejectReasonInit Trap = Exec.RuntimeFailure

-- |Parse the response from invoking a @call_init_v1@ method. This attempts to
-- parse the returned byte array and depending on its contents it will also
-- update the given pointers. See documentation of the above mentioned imported
-- function for the specification of the return value.
processInitResult ::
    -- |State context.
    LoadCallback ->
    -- |Serialized output.
    BS.ByteString ->
    -- |Location where the pointer to the return value is (potentially) stored.
    Ptr ReturnValue ->
    -- |Location where the pointer to the initial state is written.
    -- |Result, and remaining energy. Returns 'Nothing' if and only if
    -- execution ran out of energy.
    Ptr StateV1.MutableStateInner ->
    IO (Maybe (Either ContractExecutionReject InitResultData, InterpreterEnergy))
processInitResult callbacks result returnValuePtr newStatePtr = case BS.uncons result of
    Nothing -> error "Internal error: Could not parse the result from the interpreter."
    Just (tag, payload) ->
        case tag of
            0 -> return Nothing
            1 ->
                let parser =
                        -- runtime failure
                        label "Init.remainingEnergy" getWord64be
                in  let remainingEnergy = parseResult parser
                    in  do return (Just (Left Trap, fromIntegral remainingEnergy))
            2 ->
                let parser = do
                        -- reject
                        rejectReason <- label "Reject.rejectReason" getInt32be
                        remainingEnergy <- label "Reject.remainingEnergy" getWord64be
                        return (rejectReason, remainingEnergy)
                in  let (cerRejectReason, remainingEnergy) = parseResult parser
                    in  do
                            cerReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                            return (Just (Left LogicReject{..}, fromIntegral remainingEnergy))
            3 ->
                -- done
                let parser = do
                        logs <- label "Done.logs" getLogs
                        remainingEnergy <- label "Done.remainingEnergy" getWord64be
                        return (logs, remainingEnergy)
                in  let (irdLogs, remainingEnergy) = parseResult parser
                    in  do
                            irdReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                            irdNewState <- StateV1.newMutableState callbacks newStatePtr
                            return (Just (Right InitSuccess{..}, fromIntegral remainingEnergy))
            _ -> fail $ "Invalid tag: " ++ show tag
      where
        parseResult parser =
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
cerToRejectReasonReceive contractAddress receiveName parameter (ExecutionReject LogicReject{..}) = Exec.RejectedReceive{rejectReason = cerRejectReason, ..}
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
    -- |State context.
    LoadCallback ->
    -- |State execution started in.
    StateV1.MutableState ->
    -- |Serialized output.
    BS.ByteString ->
    -- |Location where the pointer to the return value is (potentially) stored.
    Ptr ReturnValue ->
    -- |Pointer to the state of the contract at the time of termination.
    Ptr StateV1.MutableStateInner ->
    -- |Location where the pointer to interrupted config is (potentially) stored.
    -- |Result, and remaining energy. Returns 'Nothing' if and only if
    -- execution ran out of energy.
    Either ReceiveInterruptedState (Ptr (Ptr ReceiveInterruptedState)) ->
    IO (Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy))
processReceiveResult callbacks initialState result returnValuePtr statePtr eitherInterruptedStatePtr = case BS.uncons result of
    Nothing -> error "Internal error: Could not parse the result from the interpreter."
    Just (tag, payload) ->
        case tag of
            0 -> return Nothing
            1 ->
                let parser =
                        -- runtime failure
                        label "Reject.remainingEnergy" getWord64be
                in  let remainingEnergy = parseResult parser
                    in  return (Just (Left Trap, fromIntegral remainingEnergy))
            2 ->
                let parser = do
                        -- reject
                        rejectReason <- label "Reject.rejectReason" getInt32be
                        remainingEnergy <- label "Reject.remainingEnergy" getWord64be
                        return (rejectReason, remainingEnergy)
                in  let (cerRejectReason, remainingEnergy) = parseResult parser
                    in  do
                            cerReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                            return (Just (Left LogicReject{..}, fromIntegral remainingEnergy))
            4 ->
                let parser = do
                        -- interrupt
                        remainingEnergy <- label "Interrupt.remainingEnergy" getWord64be
                        logs <- label "Interrupt.logs" getLogs
                        method <- label "Interrupt.method" getInvokeMethod
                        return (remainingEnergy, logs, method)
                in  let (remainingEnergy, rrdLogs, rrdMethod) = parseResult parser
                    in  do
                            rrdInterruptedConfig <- case eitherInterruptedStatePtr of
                                Left rrid -> return rrid
                                Right interruptedStatePtr -> newReceiveInterruptedState interruptedStatePtr
                            if statePtr == nullPtr
                                then
                                    let rrdCurrentState = initialState
                                        rrdStateChanged = False
                                    in  return (Just (Right ReceiveInterrupt{..}, fromIntegral remainingEnergy))
                                else do
                                    let rrdStateChanged = True
                                    rrdCurrentState <- StateV1.newMutableState callbacks statePtr
                                    return (Just (Right ReceiveInterrupt{..}, fromIntegral remainingEnergy))
            3 ->
                -- done
                let parser = do
                        logs <- label "Done.logs" getLogs
                        remainingEnergy <- label "Done.remainingEnergy" getWord64be
                        return (logs, remainingEnergy)
                in  let (rrdLogs, remainingEnergy) = parseResult parser
                    in  do
                            rrdReturnValue <- ReturnValue <$> newForeignPtr freeReturnValue returnValuePtr
                            if statePtr == nullPtr
                                then
                                    let rrdNewState = initialState
                                        rrdStateChanged = False
                                    in  return (Just (Right ReceiveSuccess{..}, fromIntegral remainingEnergy))
                                else do
                                    let rrdStateChanged = True
                                    rrdNewState <- StateV1.newMutableState callbacks statePtr
                                    return (Just (Right ReceiveSuccess{..}, fromIntegral remainingEnergy))
            _ -> fail $ "Invalid tag: " ++ show tag
      where
        parseResult parser =
            case runGet parser payload of
                Right x -> x
                Left err -> error $ "Internal error: Could not interpret output from interpreter: " ++ err

-- |Apply a receive function which is assumed to be part of the given module.
{-# NOINLINE applyReceiveFun #-}
applyReceiveFun ::
    InstrumentedModuleV V1 ->
    -- |Metadata available to the contract.
    ChainMetadata ->
    -- |Additional parameter supplied by the chain and
    -- available to the receive method.
    ReceiveContext ->
    -- |The method that was named
    ReceiveName ->
    -- |Whether to invoke the default method instead of the named one.
    Bool ->
    -- |Parameters available to the method.
    Parameter ->
    -- |Max parameter size.
    Word16 ->
    -- |Limit number of logs and size of return value.
    Bool ->
    -- |Amount the contract is initialized with.
    Amount ->
    -- |State of the contract to start in, and a way to use it.
    StateV1.MutableState ->
    -- | Enable support of chain queries.
    Bool ->
    -- |Amount of energy available for execution.
    InterpreterEnergy ->
    -- |Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
    Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy)
applyReceiveFun miface cm receiveCtx rName useFallback param maxParamLen limitLogsAndRvs amnt initialState supportChainQueries initialEnergy = unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen wasmArtifact $ \(wasmArtifactPtr, wasmArtifactLen) ->
        BSU.unsafeUseAsCStringLen initCtxBytes $ \(initCtxBytesPtr, initCtxBytesLen) ->
            BSU.unsafeUseAsCStringLen nameBytes $ \(nameBytesPtr, nameBytesLen) ->
                StateV1.withMutableState initialState $ \curStatePtr -> alloca $ \statePtrPtr -> do
                    poke statePtrPtr curStatePtr
                    BSU.unsafeUseAsCStringLen paramBytes $ \(paramBytesPtr, paramBytesLen) ->
                        alloca $ \outputLenPtr -> alloca $ \outputReturnValuePtrPtr -> alloca $ \outputInterruptedConfigPtrPtr -> do
                            outPtr <-
                                call_receive
                                    callbacks
                                    (castPtr wasmArtifactPtr)
                                    (fromIntegral wasmArtifactLen)
                                    (castPtr initCtxBytesPtr)
                                    (fromIntegral initCtxBytesLen)
                                    amountWord
                                    (castPtr nameBytesPtr)
                                    (fromIntegral nameBytesLen)
                                    (if useFallback then 1 else 0)
                                    statePtrPtr
                                    (castPtr paramBytesPtr)
                                    (fromIntegral paramBytesLen)
                                    (fromIntegral maxParamLen)
                                    (if limitLogsAndRvs then 1 else 0)
                                    energy
                                    outputReturnValuePtrPtr
                                    outputInterruptedConfigPtrPtr
                                    outputLenPtr
                                    (if supportChainQueries then 1 else 0)
                            if outPtr == nullPtr
                                then return (Just (Left Trap, 0)) -- this case should not happen
                                else do
                                    len <- peek outputLenPtr
                                    bs <- BSU.unsafePackCStringFinalizer outPtr (fromIntegral len) (rs_free_array_len outPtr (fromIntegral len))
                                    returnValuePtr <- peek outputReturnValuePtrPtr
                                    statePtr <- peek statePtrPtr
                                    processReceiveResult callbacks initialState bs returnValuePtr statePtr (Right outputInterruptedConfigPtrPtr)
  where
    wasmArtifact = imWasmArtifactBytes miface
    initCtxBytes = encodeChainMeta cm <> encodeReceiveContext receiveCtx
    amountWord = _amount amnt
    energy = fromIntegral initialEnergy
    paramBytes = BSS.fromShort (parameter param)
    nameBytes = Text.encodeUtf8 (receiveName rName)
    callbacks = StateV1.msContext initialState

-- |Resume execution after processing the interrupt. This can only be called once on a single 'ReceiveInterruptedState'.
{-# NOINLINE resumeReceiveFun #-}
resumeReceiveFun ::
    ReceiveInterruptedState ->
    -- |State of the contract to resume in.
    StateV1.MutableState ->
    -- |Whether the state has changed in the call.
    Bool ->
    -- |Current balance of the contract, if it changed.
    Amount ->
    InvokeResponseCode ->
    Maybe ReturnValue ->
    -- |Amount of energy available for execution.
    InterpreterEnergy ->
    -- |Nothing if execution used up all the energy, and otherwise the result
    -- of execution with the amount of energy remaining.
    Maybe (Either ContractExecutionReject ReceiveResultData, InterpreterEnergy)
resumeReceiveFun is currentState stateChanged amnt statusCode rVal remainingEnergy = unsafePerformIO $ do
    withReceiveInterruptedState is $ \isPtr ->
        StateV1.withMutableState currentState $ \curStatePtr -> alloca $ \statePtrPtr -> do
            poke statePtrPtr curStatePtr
            withMaybeReturnValue rVal $ \rValPtr ->
                alloca $ \outputLenPtr -> alloca $ \outputReturnValuePtrPtr -> do
                    outPtr <-
                        resume_receive
                            callbacks
                            isPtr
                            newStateTag
                            statePtrPtr
                            amountWord
                            (invokeResponseToWord64 statusCode)
                            rValPtr
                            energy
                            outputReturnValuePtrPtr
                            outputLenPtr
                    if outPtr == nullPtr
                        then return (Just (Left Trap, 0)) -- this case should not happen
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
processModule :: Bool -> WasmModuleV V1 -> Maybe (ModuleInterfaceV V1)
processModule supportUpgrade modl = do
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
                        outPtr <- validate_and_process (if supportUpgrade then 1 else 0) (castPtr wasmBytesPtr) (fromIntegral wasmBytesLen) outputLenPtr artifactLenPtr outputModuleArtifactPtr
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
                                return (Just (bs, instrumentedModuleFromBytes SV1 moduleArtifact))

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
                                    | Just initName <- extractInitName nameText -> return (Set.insert initName inits, receives)
                                    | Just (initName, receiveName) <- extractInitReceiveNames nameText ->
                                        return (inits, Map.insertWith Set.union initName (Set.singleton receiveName) receives)
                                    -- ignore any other exported functions.
                                    -- This is different from V0 contracts, which disallow any extra function exports.
                                    -- This feature was requested by some users.
                                    | otherwise -> return (inits, receives)
                        )
                        (Set.empty, Map.empty)
                        namesByteStrings
            case names of
                Nothing -> fail "Incorrect response from FFI call."
                Just x@(exposedInits, exposedReceives) ->
                    if Map.keysSet exposedReceives `Set.isSubsetOf` exposedInits then return x else fail "Receive functions that do not correspond to any contract."
