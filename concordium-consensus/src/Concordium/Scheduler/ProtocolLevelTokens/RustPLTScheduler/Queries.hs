{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bindings into the Rust PLT Scheduler library. The module contains bindings to query PLTs.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Queries (
    queryPLTList,
    queryTokenInfo,
    queryTokenAccountInfos,
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import qualified Concordium.Types as Types
import qualified Concordium.Types.Queries.Tokens as QueriesTypes
import qualified Concordium.Utils.Serialization as CS

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLTBlockState
import qualified Concordium.GlobalState.Types as BS
import Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.BlockStateCallbacks
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Memory as Memory

-- | Get the list of all tokens, for protocol version where the PLT state is managed in Rust.
queryPLTList ::
    forall m.
    (Types.PVSupportsRustManagedPLT (Types.MPV m), BS.BlockStateQuery m) =>
    -- | Block state to query.
    BS.BlockState m ->
    -- | The list of token ids
    m [Types.TokenId]
queryPLTList bs = do
    queryCallbacks <- unliftBlockStateQueryCallbacks bs
    pltState <- BS.getRustPLTBlockState bs
    BS.liftBlobStore $ queryPLTListInBlobStoreMonad pltState queryCallbacks
  where
    -- Query the list of PLTs in the blob store monad. The function is a wrapper around an FFI call
    -- to the Rust PLT Scheduler library.
    queryPLTListInBlobStoreMonad ::
        (BlobStore.MonadBlobStore m') =>
        -- Block state to query.
        PLTBlockState.ForeignPLTBlockStatePtr ->
        -- Callback need for queries.
        BlockStateQueryCallbacks ->
        -- The list of token ids
        m' [Types.TokenId]
    queryPLTListInBlobStoreMonad
        pltBlockState
        queryCallbacks =
            do
                loadCallbackPtr <- fst <$> BlobStore.getCallbacks
                liftIO $ FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                        -- Invoke the ffi call
                        statusCode <- PLTBlockState.withPLTBlockState pltBlockState $ \pltBlockStatePtr ->
                            ffiQueryPLTList
                                loadCallbackPtr
                                readTokenAccountBalanceCallbackPtr
                                getAccountIndexByAddressCallbackPtr
                                getAccountAddressByIndexCallbackPtr
                                getTokenAccountStatesCallbackPtr
                                pltBlockStatePtr
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
--
-- See the exported function in the Rust code for documentation of safety.
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

-- | Get token info for a given token, for protocol version where the PLT state is managed in Rust.
queryTokenInfo ::
    forall m.
    (Types.PVSupportsRustManagedPLT (Types.MPV m), BS.BlockStateQuery m) =>
    -- | Block state to query.
    BS.BlockState m ->
    -- | Token to find token info for.
    Types.TokenId ->
    -- | The token info.
    m (Maybe QueriesTypes.TokenInfo)
queryTokenInfo bs tokenId = do
    queryCallbacks <- unliftBlockStateQueryCallbacks bs
    pltState <- BS.getRustPLTBlockState bs
    BS.liftBlobStore $ queryTokenInfoInBlobStoreMonad pltState queryCallbacks
  where
    -- Get token info for the given token in the given block state. The function is a wrapper around an FFI call
    -- to the Rust PLT Scheduler library.
    queryTokenInfoInBlobStoreMonad ::
        (BlobStore.MonadBlobStore m') =>
        -- Block state to query.
        PLTBlockState.ForeignPLTBlockStatePtr ->
        -- Callback need for queries.
        BlockStateQueryCallbacks ->
        -- The token info.
        m' (Maybe QueriesTypes.TokenInfo)
    queryTokenInfoInBlobStoreMonad
        pltBlockState
        queryCallbacks =
            do
                loadCallbackPtr <- fst <$> BlobStore.getCallbacks
                liftIO $ FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                        -- Invoke the ffi call
                        statusCode <- PLTBlockState.withPLTBlockState pltBlockState $ \pltBlockStatePtr ->
                            BSS.useAsCStringLen (Types.tokenId tokenId) $ \(tokenIdPtr, tokenIdLen) ->
                                ffiQueryTokenInfo
                                    loadCallbackPtr
                                    readTokenAccountBalanceCallbackPtr
                                    getAccountIndexByAddressCallbackPtr
                                    getAccountAddressByIndexCallbackPtr
                                    getTokenAccountStatesCallbackPtr
                                    pltBlockStatePtr
                                    (FFI.castPtr tokenIdPtr)
                                    (fromIntegral tokenIdLen)
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
                                let getTokenInfo = S.isolate (BS.length returnData) S.get
                                let tokenInfo =
                                        either
                                            (\message -> error $ "Token info from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getTokenInfo returnData
                                return $ Just tokenInfo
                            1 -> do
                                return Nothing
                            _ -> error ("Unexpected status code from calling 'ffiQueryTokenInfo': " ++ show statusCode)

-- | C-binding for calling the Rust function `plt_scheduler::queries::query_token_info`.
--
-- Returns a byte representing the result:
--
-- - `0`: The query was successful
-- - `1`: The token does not exist
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_query_token_info"
    ffiQueryTokenInfo ::
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
        -- | Pointer to token id UTF-8 bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of token id UTF-8.
        FFI.CSize ->
        -- | Output location for array containing return data, which is the token info.
        -- If the return value is `0`, the data is the serialized token info.
        -- If the return value is `1`, the data is empty (zero bytes).
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr FFI.CSize ->
        -- | Status code:
        -- * `0`: the query was successful.
        -- * `1`: token does not exist
        IO Word.Word8

-- | Get token account infos for a given account, for protocol version where the PLT state is managed in Rust.
queryTokenAccountInfos ::
    forall m.
    (Types.PVSupportsRustManagedPLT (Types.MPV m), BS.BlockStateQuery m) =>
    -- | Block state to query.
    BS.BlockState m ->
    -- | Index of the account to find token account infos for.
    Types.AccountIndex ->
    -- | The token account infos.
    m [QueriesTypes.Token]
queryTokenAccountInfos bs accountIndex = do
    queryCallbacks <- unliftBlockStateQueryCallbacks bs
    pltState <- BS.getRustPLTBlockState bs
    BS.liftBlobStore $ queryTokenAccountInfosInBlobStoreMonad pltState queryCallbacks
  where
    -- Get token account infos for the given account in the given block state. The function is a wrapper around an FFI call
    -- to the Rust PLT Scheduler library.
    queryTokenAccountInfosInBlobStoreMonad ::
        (BlobStore.MonadBlobStore m') =>
        -- Block state to query.
        PLTBlockState.ForeignPLTBlockStatePtr ->
        -- Callback need for queries.
        BlockStateQueryCallbacks ->
        -- The token account infos.
        m' [QueriesTypes.Token]
    queryTokenAccountInfosInBlobStoreMonad
        pltBlockState
        queryCallbacks =
            do
                loadCallbackPtr <- fst <$> BlobStore.getCallbacks
                liftIO $ FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                        -- Invoke the ffi call
                        statusCode <- PLTBlockState.withPLTBlockState pltBlockState $ \pltBlockStatePtr ->
                            ffiQueryTokenAccountInfos
                                loadCallbackPtr
                                readTokenAccountBalanceCallbackPtr
                                getAccountIndexByAddressCallbackPtr
                                getAccountAddressByIndexCallbackPtr
                                getTokenAccountStatesCallbackPtr
                                pltBlockStatePtr
                                (fromIntegral accountIndex)
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
                                let getTokenAccountInfos = S.isolate (BS.length returnData) $ CS.getListOf S.get
                                let tokenAccountInfos =
                                        either
                                            (\message -> error $ "Token account infos from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getTokenAccountInfos returnData
                                return tokenAccountInfos
                            _ -> error ("Unexpected status code from calling 'ffiQueryTokenAccountInfos': " ++ show statusCode)

-- | C-binding for calling the Rust function `plt_scheduler::queries::query_token_account_infos`.
--
-- Returns a byte representing the result:
--
-- - `0`: The query was successful
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_query_token_account_infos"
    ffiQueryTokenAccountInfos ::
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
        -- | The account index to get token account infos for.
        Word.Word64 ->
        -- | Output location for array containing return data, which is a list of token account infos.
        -- If the return value is `0`, the data is the serialized list of token account infos.
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr FFI.CSize ->
        -- | Status code:
        -- * `0`: the query was successful.
        IO Word.Word8

-- | "Unlifts" the callback queries from the 'BlockStateQuery' monad into the IO monad, such that they can
-- be converted to FFI function pointers.
unliftBlockStateQueryCallbacks ::
    forall m.
    (Types.PVSupportsPLT (Types.MPV m), BS.BlockStateQuery m) =>
    BS.BlockState m ->
    m BlockStateQueryCallbacks
unliftBlockStateQueryCallbacks bs = BS.withUnliftBSQ $ \unlift ->
    do
        let readTokenAccountBalance accountIndex tokenIndex = do
                maybeAccount <- unlift $ BS.getAccountByIndex bs accountIndex
                let account = snd $ maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                unlift $ BS.getAccountTokenBalance account tokenIndex
            getAccountIndexByAddress accountAddress = do
                maybeAccount <- unlift $ BS.getAccount bs accountAddress
                return $ fst <$> maybeAccount
            getAccountAddressByIndex accountIndex = do
                maybeAccount <- unlift $ BS.getAccountByIndex bs accountIndex
                forM maybeAccount $ \(_, account) -> unlift $ BS.getAccountCanonicalAddress account
            getTokenAccountStates accountIndex = do
                maybeAccount <- unlift $ BS.getAccountByIndex bs accountIndex
                let account = snd $ maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                fmap Map.toList $ unlift $ BS.getAccountTokens account

        return BlockStateQueryCallbacks{..}
