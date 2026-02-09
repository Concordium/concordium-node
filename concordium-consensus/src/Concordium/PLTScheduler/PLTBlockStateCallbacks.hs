{-# LANGUAGE TypeApplications #-}

-- | Bindings for the Rust PLT block state implementation to call back.
module Concordium.PLTScheduler.PLTBlockStateCallbacks (
    ReadTokenAccountBalance,
    ReadTokenAccountBalanceCallbackPtr,
    wrapReadTokenAccountBalance,
    UpdateTokenAccountBalance,
    UpdateTokenAccountBalanceCallbackPtr,
    wrapUpdateTokenAccountBalance,
    IncrementPltUpdateSequenceNumber,
    IncrementPltUpdateSequenceNumberCallbackPtr,
    wrapIncrementPltUpdateSequenceNumber,
    GetAccountIndexByAddress,
    GetAccountIndexByAddressCallbackPtr,
    wrapGetAccountIndexByAddress,
    GetAccountAddressByIndex,
    GetAccountAddressByIndexCallbackPtr,
    wrapGetAccountAddressByIndex,
    GetTokenAccountStates,
    GetTokenAccountStatesCallbackPtr,
    wrapGetTokenAccountStates,
) where

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI

import qualified Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens as AccountTokens
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as Tokens
import qualified Concordium.ID.Types as Types
import qualified Concordium.PLTScheduler.PLTMemory as Memory
import qualified Concordium.Types as Types
import qualified Concordium.Types.Tokens as Tokens
import qualified Concordium.Utils.Serialization as CS

-- | Callback function for reading a token account balance.
type ReadTokenAccountBalance =
    -- | Index of the account to read a token balance for. The account must exist.
    Types.AccountIndex ->
    -- | Index of the token to read the balance of. The token must exist.
    Tokens.TokenIndex ->
    -- | The balance.
    IO Tokens.TokenRawAmount

-- | Internal helper function for mapping the 'ReadTokenAccountBalance' into the more
-- low-level function pointer which can be passed in FFI.
wrapReadTokenAccountBalance :: ReadTokenAccountBalance -> IO ReadTokenAccountBalanceCallbackPtr
wrapReadTokenAccountBalance func =
    ffiWrapReadTokenAccountBalanceCallback callback
  where
    callback :: ReadTokenAccountBalanceCallbackFFI
    callback accountIndex tokenIndex = do
        amount <- func (fromIntegral accountIndex) (fromIntegral tokenIndex)
        return $ Tokens.theTokenRawAmount amount

-- | Callback function for reading a token account balance.
--
-- This is passed as a function pointer in FFI to call, see also 'ReadTokenAccountBalanceCallback'
-- for the more type-safe variant.
type ReadTokenAccountBalanceCallbackFFI =
    -- | Index of the account to read a token balance for. The account must exist.
    Word.Word64 ->
    -- | Index of the token to read the balance of. The token must exist.
    Word.Word64 ->
    -- | The balanace.
    IO Word.Word64

-- | The callback function pointer type for reading a token account balance.
type ReadTokenAccountBalanceCallbackPtr = FFI.FunPtr ReadTokenAccountBalanceCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapReadTokenAccountBalanceCallback ::
        ReadTokenAccountBalanceCallbackFFI -> IO ReadTokenAccountBalanceCallbackPtr

-- | Callback function for updating a token account balance.
type UpdateTokenAccountBalance =
    -- | Index of the account to update a token balance for.
    Types.AccountIndex ->
    -- | Index of the token to update the balance of.
    Tokens.TokenIndex ->
    -- | The change to account balance.
    AccountTokens.TokenAmountDelta ->
    -- | Status code, where 'False' represents a balance overflow.
    IO Bool

-- | Internal helper function for mapping the 'UpdateTokenAccountBalance' into the more
-- low-level function pointer which can be passed in FFI.
wrapUpdateTokenAccountBalance :: UpdateTokenAccountBalance -> IO UpdateTokenAccountBalanceCallbackPtr
wrapUpdateTokenAccountBalance func =
    ffiWrapUpdateTokenAccountBalanceCallback callback
  where
    callback :: UpdateTokenAccountBalanceCallbackFFI
    callback accountIndex tokenIndex amount addAmount = do
        let amountDelta =
                case addAmount of
                    0 -> AccountTokens.TokenAmountDelta $ -fromIntegral amount
                    1 -> AccountTokens.TokenAmountDelta $ fromIntegral amount
                    _ -> error ("Boolean argument must be 0 or 1, was " ++ (show addAmount))
        overflow <- func (fromIntegral accountIndex) (fromIntegral tokenIndex) amountDelta
        return $ if overflow then 1 else 0

-- | Callback function for updating a token account balance.
--
-- This is passed as a function pointer in FFI to call, see also 'UpdateTokenAccountBalance'
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

-- | Callback function for incrementing the PLT update sequence number.
type IncrementPltUpdateSequenceNumber =
    IO ()

-- | Internal helper function for mapping the 'IncrementPltUpdateSequenceNumber' into the more
-- low-level function pointer which can be passed in FFI.
wrapIncrementPltUpdateSequenceNumber :: IncrementPltUpdateSequenceNumber -> IO IncrementPltUpdateSequenceNumberCallbackPtr
wrapIncrementPltUpdateSequenceNumber func =
    ffiWrapIncrementPltUpdateSequenceNumberCallback callback
  where
    callback :: IncrementPltUpdateSequenceNumberCallbackFFI
    callback = func

-- | Callback function for incrementing the PLT update sequence number.
--
-- This is passed as a function pointer in FFI to call, see also 'IncrementPltUpdateSequenceNumber'
-- for the more type-safe variant.
type IncrementPltUpdateSequenceNumberCallbackFFI =
    IO ()

-- | The callback function pointer type for incrementing the PLT update sequence number.
type IncrementPltUpdateSequenceNumberCallbackPtr = FFI.FunPtr IncrementPltUpdateSequenceNumberCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapIncrementPltUpdateSequenceNumberCallback ::
        IncrementPltUpdateSequenceNumberCallbackFFI -> IO IncrementPltUpdateSequenceNumberCallbackPtr

-- | Callback function getting account index by address.
type GetAccountIndexByAddress =
    -- | Address of the account to find
    Types.AccountAddress ->
    -- | The index of the account. `Nothing` is returned if the account does not exist
    IO (Maybe Types.AccountIndex)

-- | Internal helper function for mapping the 'GetAccountIndexByAddress' into the more
-- low-level function pointer which can be passed in FFI.
wrapGetAccountIndexByAddress :: GetAccountIndexByAddress -> IO GetAccountIndexByAddressCallbackPtr
wrapGetAccountIndexByAddress func =
    ffiWrapGetAccountIndexByAddressCallback callback
  where
    callback :: GetAccountIndexByAddressCallbackFFI
    callback accountAddressPtr accountIndexOutPtr = do
        accountAddress <- Types.AccountAddress <$> (FFI.peek $ FFI.castPtr accountAddressPtr)

        accountIndexMaybe <- func accountAddress
        case accountIndexMaybe of
            Just accountIndex ->
                do
                    FFI.poke accountIndexOutPtr (fromIntegral accountIndex)
                    return 0
            Nothing ->
                return 1

-- | Callback function for getting account index by address.
--
-- This is passed as a function pointer in FFI to call, see also 'GetAccountIndexByAddress'
-- for the more type-safe variant.
type GetAccountIndexByAddressCallbackFFI =
    -- | Pointer for reading the 32 byte address of the account
    FFI.Ptr Word.Word8 ->
    -- | Pointer to where to write account index. Will be written to if status code is `0`.
    FFI.Ptr Word.Word64 ->
    -- | Status code: `0` if the account was found, else `1`.
    IO Word.Word8

-- | The callback function pointer type for getting account index by address.
type GetAccountIndexByAddressCallbackPtr = FFI.FunPtr GetAccountIndexByAddressCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapGetAccountIndexByAddressCallback ::
        GetAccountIndexByAddressCallbackFFI -> IO GetAccountIndexByAddressCallbackPtr

-- | Callback function getting account address by index.
type GetAccountAddressByIndex =
    -- | Index of the account to find
    Types.AccountIndex ->
    -- | The address of the account. `Nothing` is returned if the account does not exist
    IO (Maybe Types.AccountAddress)

-- | Internal helper function for mapping the 'GetAccountAddressByIndex' into the more
-- low-level function pointer which can be passed in FFI.
wrapGetAccountAddressByIndex :: GetAccountAddressByIndex -> IO GetAccountAddressByIndexCallbackPtr
wrapGetAccountAddressByIndex func =
    ffiWrapGetAccountAddressByIndexCallback callback
  where
    callback :: GetAccountAddressByIndexCallbackFFI
    callback accountIndex accountAddressOutPtr = do
        accountAddressMaybe <- func (fromIntegral accountIndex)
        case accountAddressMaybe of
            Just (Types.AccountAddress accountAddressBytes) ->
                do
                    FFI.poke (FFI.castPtr accountAddressOutPtr) accountAddressBytes
                    return 0
            Nothing ->
                return 1

-- | Callback function for getting account address by index.
--
-- This is passed as a function pointer in FFI to call, see also 'GetAccountAddressByIndex'
-- for the more type-safe variant.
type GetAccountAddressByIndexCallbackFFI =
    -- | The account index of the account.
    Word.Word64 ->
    -- | Pointer for writing the 32 byte address of the account.
    FFI.Ptr Word.Word8 ->
    -- | Status code: `0` if the account was found, else `1`.
    IO Word.Word8

-- | The callback function pointer type for getting account address by index.
type GetAccountAddressByIndexCallbackPtr = FFI.FunPtr GetAccountAddressByIndexCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapGetAccountAddressByIndexCallback ::
        GetAccountAddressByIndexCallbackFFI -> IO GetAccountAddressByIndexCallbackPtr

-- | Callback function getting token account states by account.
type GetTokenAccountStates =
    -- | Index of the account. The account must exist.
    Types.AccountIndex ->
    -- | The token account states for the account paired with the index for the token.
    IO [(Tokens.TokenIndex, AccountTokens.TokenAccountState)]

-- | Internal helper function for mapping the 'GetTokenAccountStates' into the more
-- low-level function pointer which can be passed in FFI.
wrapGetTokenAccountStates :: GetTokenAccountStates -> IO GetTokenAccountStatesCallbackPtr
wrapGetTokenAccountStates func =
    ffiWrapGetTokenAccountStatesCallback callback
  where
    callback :: GetTokenAccountStatesCallbackFFI
    callback accountIndex = do
        tokenAccountStates <- func (fromIntegral accountIndex)
        let putStates = CS.putListOf S.put tokenAccountStates
        let bytes = S.runPut putStates
        BS.unsafeUseAsCStringLen bytes $ \(sourcePtr, len) ->
            Memory.copyToRustVec2 (FFI.castPtr sourcePtr) (fromIntegral len)

-- | Callback function for getting token account states for an account.
--
-- This is passed as a function pointer in FFI to call, see also 'GetTokenAccountStates'
-- for the more type-safe variant.
type GetTokenAccountStatesCallbackFFI =
    -- | The account index of the account.
    Word.Word64 ->
    -- | Pointer to a Rust `Vec` allocated with `copy_to_vec_ffi_2` and which contains the
    -- list of token index and token account state pairs in binary serialization.
    IO (FFI.Ptr Memory.RustVec)

-- | The callback function pointer type for getting account address by index.
type GetTokenAccountStatesCallbackPtr = FFI.FunPtr GetTokenAccountStatesCallbackFFI

-- | Function to wrap Haskell functions or closures into a function pointer which can be passed over
-- FFI.
foreign import ccall "wrapper"
    ffiWrapGetTokenAccountStatesCallback ::
        GetTokenAccountStatesCallbackFFI -> IO GetTokenAccountStatesCallbackPtr
