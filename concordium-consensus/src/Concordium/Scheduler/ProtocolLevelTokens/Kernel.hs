{-# LANGUAGE TypeFamilies #-}

module Concordium.Scheduler.ProtocolLevelTokens.Kernel (
    TokenRawAmount (..),
    TokenStateKey,
    TokenStateValue,
    module Concordium.Scheduler.ProtocolLevelTokens.Kernel,
) where

import Data.Word

import Concordium.Types
import Concordium.Types.Tokens

import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

class PLTKernelQuery m where
    type PLTAccount m
    getTokenState :: TokenStateKey -> m (Maybe TokenStateValue)
    getAccount :: AccountAddress -> m (Maybe (PLTAccount m))
    getAccountIndex :: PLTAccount m -> m AccountIndex
    getAccountByIndex :: AccountIndex -> m (Maybe (PLTAccount m))
    getAccountBalance :: PLTAccount m -> m TokenRawAmount
    getAccountCanonicalAddress :: PLTAccount m -> m AccountAddress
    getCirculatingSupply :: m TokenRawAmount
    getDecimals :: m Word8

class (PLTKernelQuery m) => PLTKernelUpdate m where
    -- | Set or clear a value in the token state at the corresponding key.
    --
    --  Returns @Just True@ if there was an existing entry, @Just False@ if there was
    --  no existing entry, and @Nothing@ if the update failed because the key was locked
    --  by an iterator.
    setTokenState :: TokenStateKey -> Maybe TokenStateValue -> m (Maybe Bool)

    -- | Transfer a token amount from one account to another, with an optional memo.
    --  The return value indicates if the transfer was successful.
    --  The transfer can fail if the sender has insufficient balance.
    transfer ::
        -- | Sender
        PLTAccount m ->
        -- | Receiver
        PLTAccount m ->
        -- | Amount
        TokenRawAmount ->
        -- | Memo
        Maybe Memo ->
        -- | Returns 'True' if successful, 'False' if the sender had insufficient balance.
        m Bool

    -- | Log a token module event with the specified type and details.
    logTokenEvent :: TokenEventType -> TokenEventDetails -> m ()

    -- | Update the balance of the given account to zero if it didn't have a
    --  balance before.
    touch ::
        -- | The account to update
        PLTAccount m ->
        -- | Returns 'True' if the balance wasn't present on the given account
        --  and 'False' otherwise.
        m Bool

class (PLTKernelUpdate m) => PLTKernelPrivilegedUpdate m where
    -- | Mint a specified amount and deposit it in the specified account.
    --  The return value indicates if this was successful.
    --  Minting can fail if the total supply would exceed the representable amount.
    mint :: PLTAccount m -> TokenRawAmount -> m Bool

    -- | Burn a specified amount from a specified account.
    --  The return value indicates if this was successful.
    --  Burning can fail if the amount in the account is less than the specified amount to burn.
    burn :: PLTAccount m -> TokenRawAmount -> m Bool

class PLTKernelChargeEnergy m where
    -- | Reduce the available energy for the PLT module execution. If the
    --  available energy is smaller than the given amount, the containing
    --  transaction will abort and the effects of the transaction will be rolled
    --  back. The energy is charged in any case (also in case of failure).
    pltTickEnergy :: Energy -> m ()

class PLTKernelFail e m where
    -- | Abort the current operation by raising an error.
    pltError :: e -> m a
