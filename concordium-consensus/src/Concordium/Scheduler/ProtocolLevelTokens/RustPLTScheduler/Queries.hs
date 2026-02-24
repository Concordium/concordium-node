{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bindings into the Rust PLT Scheduler library. The module contains bindings to query PLTs.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Queries (
    queryPLTList,
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import Concordium.Types
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
    (PVSupportsRustManagedPLT (MPV m), BS.BlockStateQuery m) =>
    BS.BlockState m ->
    m [TokenId]
queryPLTList bs = do
    (_ :: ()) <- BS.liftBlobStore $ liftIO $ putStrLn "begin queryPLTList" -- todo ar
    queryCallbacks <- unliftBlockStateQueryCallbacks bs
    pltState <- BS.getRustPLTBlockState bs
    BS.liftBlobStore $ queryPLTListInBlobStoreMonad pltState queryCallbacks

-- | "Unlifts" the callback queries from the 'BlockStateQuery' monad into the IO monad, such that they can
-- be converted to FFI function pointers.
unliftBlockStateQueryCallbacks ::
    forall m.
    (PVSupportsPLT (MPV m), BS.BlockStateQuery m) =>
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

-- | Query the list of PLTs in the block state. The function is a wrapper around an FFI call
-- to the Rust PLT Scheduler library.
queryPLTListInBlobStoreMonad ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block state to query.
    PLTBlockState.ForeignPLTBlockStatePtr ->
    -- | Callback need for queries.
    BlockStateQueryCallbacks ->
    -- | The list of token ids
    m [TokenId]
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
                    putStrLn "call ffi_query_plt_list" -- todo ar
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
                    putStrLn "return ffi_query_plt_list" -- todo ar
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
