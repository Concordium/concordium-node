-- | Bindings into the Rust PLT Scheduler library. The module contains bindings to query PLTs.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Queries (
    queryPLTList,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import qualified Concordium.Types as Types
import qualified Concordium.Utils.Serialization as CS

import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLTBlockState
import Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.BlockStateCallbacks
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Memory as Memory

-- | Query the list of PLTs in the block state.
queryPLTList ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block state to query.
    PLTBlockState.ForeignPLTBlockStatePtr ->
    -- | Callback for reading the token balance of an account.
    ReadTokenAccountBalance ->
    -- | Callback to get account index by account address
    GetAccountIndexByAddress ->
    -- | Callback to get account address by account index
    GetAccountAddressByIndex ->
    -- | Callback to get token account states for an account
    GetTokenAccountStates ->
    -- | The list of token ids
    m [Types.TokenId]
queryPLTList
    blockState
    readTokenAccountBalance
    getAccountIndexByAddress
    getAccountAddressByIndex
    getTokenAccountStates =
        do
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $ FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                do
                    readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance readTokenAccountBalance
                    getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress getAccountIndexByAddress
                    getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex getAccountAddressByIndex
                    getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates getTokenAccountStates
                    -- Invoke the ffi call
                    statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                        ffiQueryPLTList
                            loadCallbackPtr
                            readTokenAccountBalanceCallbackPtr
                            getAccountIndexByAddressCallbackPtr
                            getAccountAddressByIndexCallbackPtr
                            getTokenAccountStatesCallbackPtr
                            blockStatePtr
                            returnDataPtrOutPtr
                            returnDataLenOutPtr
                    -- Free the function pointers we have just created
                    -- (loadCallbackPtr is created in another context,
                    -- so we should not free it)
                    FFI.freeHaskellFunPtr readTokenAccountBalanceCallbackPtr
                    FFI.freeHaskellFunPtr getAccountIndexByAddressCallbackPtr
                    FFI.freeHaskellFunPtr getAccountAddressByIndexCallbackPtr
                    FFI.freeHaskellFunPtr getTokenAccountStatesCallbackPtr
                    -- Process the returned status and values returend via out pointers
                    returnDataLen <- FFI.peek returnDataLenOutPtr
                    returnDataPtr <- FFI.peek returnDataPtrOutPtr
                    returnData <-
                        BS.unsafePackCStringFinalizer
                            returnDataPtr
                            (fromIntegral returnDataLen)
                            (Memory.rs_free_array_len_2 returnDataPtr (fromIntegral returnDataLen))
                    case statusCode of
                        0 -> do
                            let getTokenIdList = S.isolate (BS.length returnData) $ CS.getListOf S.get
                            let tokenIdList =
                                    either
                                        (\message -> error $ "Token id list from Rust PLT Scheduler could not be deserialized: " ++ message)
                                        id
                                        $ S.runGet getTokenIdList returnData
                            return tokenIdList
                        _ -> error ("Unexpected status code from calling 'ffiQueryPLTList': " ++ show statusCode)

-- | C-binding for calling the Rust function `plt_scheduler::queries::query_plt_list`.
--
-- Returns a byte representing the result:
--
-- - `0`: The query was successful
foreign import ccall "ffi_query_plt_list"
    ffiQueryPLTList ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Called to get the token account balance in the haskell-managed block state.
        ReadTokenAccountBalanceCallbackPtr ->
        -- | Called to get account index by account address.
        GetAccountIndexByAddressCallbackPtr ->
        -- | Called to get account address by account index.
        GetAccountAddressByIndexCallbackPtr ->
        -- | Called to get token account states for account.
        GetTokenAccountStatesCallbackPtr ->
        -- | Pointer to the input PLT block state.
        FFI.Ptr PLTBlockState.RustPLTBlockState ->
        -- | Output location for array containing return data, which is a list of token ids.
        -- If the return value is `0`, the data is a serialized list of token ids.
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr FFI.CSize ->
        -- | Status code:
        -- * `0`: the query was successful.
        IO Word.Word8
