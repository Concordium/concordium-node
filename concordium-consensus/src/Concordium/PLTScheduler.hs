-- |
-- Bindings into the @plt-scheduler@ Rust library exposing safe wrappers.
--
-- Each foreign imported function must match the signature of functions found in @plt-scheduler/src/ffi.rs@.
module Concordium.PLTScheduler (
    executeTransaction,
    ExecutionOutcome (..),
    ExecutionAccepts (..),
    UpdateTokenAccountBalanceCallback,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as Tokens
import qualified Concordium.PLTScheduler.PLTBlockState as PLTBlockState
import qualified Concordium.Types as Types
import qualified Concordium.Types.Tokens as Tokens

-- | Execute a transaction payload modifying the `block_state` accordingly.
--
-- See @execute_transaction@ in @plt-scheduler@ rust crate for details.
executeTransaction ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block state to mutate.
    PLTBlockState.PLTBlockState ->
    -- | Callback for reading the token balance of an account.
    ReadTokenAccountBalanceCallback ->
    -- | Callback for updating the token balance of an account.
    UpdateTokenAccountBalanceCallback ->
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The account index of the account which signed as the sender of the transaction.
    Types.AccountIndex ->
    -- | Remaining energy.
    Types.Energy ->
    -- | Outcome of the execution
    m ExecutionOutcome
executeTransaction
    blockState
    readTokenAccountBalanceCallback
    updateTokenAccountBalanceCallback
    transactionPayload
    senderAccountIndex
    remainingEnergy = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        liftIO $ FFI.alloca $ \usedEnergyOut -> FFI.alloca $ \updatedBlockStatePtrOut -> do
            readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalanceCallback readTokenAccountBalanceCallback
            updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalanceCallback updateTokenAccountBalanceCallback
            -- Invoke the ffi call
            statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                BS.unsafeUseAsCStringLen transactionPayload $
                    \(transactionPayloadPtr, transactionPayloadLen) ->
                        ffiExecuteTransaction
                            loadCallback
                            readTokenAccountBalanceCallbackPtr
                            updateTokenAccountBalanceCallbackPtr
                            blockStatePtr
                            (FFI.castPtr transactionPayloadPtr)
                            (fromIntegral transactionPayloadLen)
                            (fromIntegral senderAccountIndex)
                            (fromIntegral remainingEnergy)
                            updatedBlockStatePtrOut
                            usedEnergyOut
            -- Process the and construct the outcome
            usedEnergy <- fromIntegral <$> FFI.peek usedEnergyOut
            status <- case statusCode of
                0 -> do
                    updatedBlockState <- FFI.peek updatedBlockStatePtrOut >>= PLTBlockState.wrapFFIPtr
                    return $
                        Right
                            ExecutionAccepts
                                { eaUpdatedPLTBlockState = updatedBlockState,
                                  eaEvents = ()
                                }
                1 -> return $ Left ()
                _ -> error "Unexpected status code from calling 'ffiExecuteTransaction'"
            return
                ExecutionOutcome
                    { erUsedEnergy = usedEnergy,
                      erStatus = status
                    }

foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Called to get the token account balance in the haskell-managed block state.
        ReadTokenAccountBalanceCallbackPtr ->
        -- | Called to set the token account balance in the haskell-managed block state.
        UpdateTokenAccountBalanceCallbackPtr ->
        -- | Pointer to the starting block state.
        FFI.Ptr PLTBlockState.PLTBlockState ->
        -- | Pointer to transaction payload bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of transaction payload.
        FFI.CSize ->
        -- | The account index of the account which signed as the sender of the transaction.
        Word.Word64 ->
        -- | Remaining energy
        Word.Word64 ->
        -- | Output location for the updated block state.
        FFI.Ptr (FFI.Ptr PLTBlockState.PLTBlockState) ->
        -- | Output location for the energy used by the execution.
        FFI.Ptr Word.Word64 ->
        -- | Status code
        IO Word.Word8

-- | The outcome of executing a transaction using the PLT scheduler.
data ExecutionOutcome = ExecutionOutcome
    { -- | The amount of energy used by the execution.
      erUsedEnergy :: Types.Energy,
      -- | The resulting execution status.
      erStatus :: Either () ExecutionAccepts
    }

-- | Additional execution outcome when the transaction gets accepted by the PLT scheduler.
data ExecutionAccepts = ExecutionAccepts
    { -- | The updated PLT block state after the execution
      eaUpdatedPLTBlockState :: PLTBlockState.PLTBlockState,
      -- | Events produced during the execution
      eaEvents :: ()
    }

-- | Callback function for reading a token account balance.
type ReadTokenAccountBalanceCallback =
    -- | Index of the account to read a token balance for.
    Types.AccountIndex ->
    -- | Index of the token to read the balance of.
    Tokens.TokenIndex ->
    -- | The balance.
    IO Tokens.TokenRawAmount

-- | Internal helper function for mapping the `ReadTokenAccountBalanceCallback` into the more
-- low-level function pointer which can be passed in FFI.
wrapReadTokenAccountBalanceCallback :: ReadTokenAccountBalanceCallback -> IO ReadTokenAccountBalanceCallbackPtr
wrapReadTokenAccountBalanceCallback callback =
    ffiWrapReadTokenAccountBalanceCallback $ wrapped
  where
    wrapped :: ReadTokenAccountBalanceCallbackFFI
    wrapped accountIndex tokenIndex = do
        amount <- callback (fromIntegral accountIndex) (fromIntegral tokenIndex)
        return $ Tokens.theTokenRawAmount amount

-- | Callback function for reading a token account balance.
--
-- This is passed as a function pointer in FFI to call, see also `ReadTokenAccountBalanceCallback`
-- for the more type-safe variant.
type ReadTokenAccountBalanceCallbackFFI =
    -- | Index of the account to read a token balance for.
    Word.Word64 ->
    -- | Index of the token to read the balance of.
    Word.Word64 ->
    -- | The balanacep
    IO Word.Word64

-- | The callback function pointer type for reading a token account balance.
type ReadTokenAccountBalanceCallbackPtr = FFI.FunPtr ReadTokenAccountBalanceCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapReadTokenAccountBalanceCallback ::
        ReadTokenAccountBalanceCallbackFFI -> IO ReadTokenAccountBalanceCallbackPtr

-- | Callback function for updating a token account balance.
type UpdateTokenAccountBalanceCallback =
    -- | Index of the account to update a token balance for.
    Types.AccountIndex ->
    -- | Index of the token to update the balance of.
    Tokens.TokenIndex ->
    -- | The token amount to add to or subtract from the balance.
    Tokens.TokenRawAmount ->
    -- | If True, add the amount to the balance. If False, subtract the amount from the balance.
    Bool ->
    -- | Status code, where non-null represents a balance overflow.
    IO Bool

-- | Internal helper function for mapping the `UpdateTokenAccountBalanceCallback` into the more
-- low-level function pointer which can be passed in FFI.
wrapUpdateTokenAccountBalanceCallback :: UpdateTokenAccountBalanceCallback -> IO UpdateTokenAccountBalanceCallbackPtr
wrapUpdateTokenAccountBalanceCallback callback =
    ffiWrapUpdateTokenAccountBalanceCallback $ wrapped
  where
    wrapped :: UpdateTokenAccountBalanceCallbackFFI
    wrapped accountIndex tokenIndex amount addAmount = do
        overflow <- callback (fromIntegral accountIndex) (fromIntegral tokenIndex) (fromIntegral amount) (if addAmount == 0 then False else True)
        return $ if overflow then 1 else 0

-- | Callback function for updating a token account balance.
--
-- This is passed as a function pointer in FFI to call, see also `UpdateTokenAccountBalanceCallback`
-- for the more type-safe variant.
type UpdateTokenAccountBalanceCallbackFFI =
    -- | Index of the account to update a token balance for.
    Word.Word64 ->
    -- | Index of the token to update the balance of.
    Word.Word64 ->
    -- | The token amount to add to or subtract from the balance.
    Word.Word64 ->
    -- | If 1, add the amount to the balance. If 0, subtract the amount from the balance.
    Word.Word8 ->
    -- | Status code, where non-null represents a balance overflow.
    IO Word.Word8

-- | The callback function pointer type for updating a token account balance.
type UpdateTokenAccountBalanceCallbackPtr = FFI.FunPtr UpdateTokenAccountBalanceCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapUpdateTokenAccountBalanceCallback ::
        UpdateTokenAccountBalanceCallbackFFI -> IO UpdateTokenAccountBalanceCallbackPtr
