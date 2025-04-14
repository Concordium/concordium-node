module Concordium.Scheduler.ProtocolLevelTokens.Kernel where

import Data.Word

import Concordium.Types

import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.GlobalState.Types

class PLTKernelQuery m where
    getTokenState :: TokenStateKey -> m (Maybe TokenStateValue)
    getAccount :: AccountAddress -> m (Maybe (Account m))
    getAccountBalance :: Account m -> m (Maybe TokenRawAmount)
    getAccountState :: Account m -> TokenStateKey -> m (Maybe TokenStateValue)
    getAccountCanonicalAddress :: Account m -> m AccountAddress
    getGovernanceAccount :: m (Account m)
    getCirculatingSupply :: m TokenRawAmount
    getDecimals :: m Word8

class (PLTKernelQuery m) => PLTKernelUpdate m where
    setTokenState :: TokenStateKey -> Maybe TokenStateValue -> m ()
    setAccountState :: Account m -> TokenStateKey -> Maybe TokenStateValue -> m ()

    -- | Transfer a token amount from one account to another, with an optional memo.
    --  The return value indicates if the transfer was successful.
    --  The transfer can fail if the sender has insufficient balance.
    transfer ::
        -- | Sender
        Account m ->
        -- | Receiver
        Account m ->
        -- | Amount
        TokenRawAmount ->
        -- | Memo
        Maybe Memo ->
        -- | Returns 'True' if successful, 'False' if the sender had insufficient balance.
        m Bool

class (PLTKernelUpdate m) => PLTKernelPrivilegedUpdate m where
    -- | Mint a specified amount and deposit it in the specified account.
    --  The return value indicates if this was successful.
    --  Minting can fail if the total supply would exceed the representable amount.
    mint :: Account m -> TokenRawAmount -> m Bool

    -- | Burn a specified amount from a specified account.
    --  The return value indicates if this was successful.
    --  Burning can fail if the amount in the account is less than the specified amount to burn.
    burn :: Account m -> TokenRawAmount -> m Bool
