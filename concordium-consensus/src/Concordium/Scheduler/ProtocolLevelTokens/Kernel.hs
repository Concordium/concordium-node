{-# LANGUAGE TypeFamilies #-}

module Concordium.Scheduler.ProtocolLevelTokens.Kernel (
    TokenRawAmount (..),
    module Concordium.Scheduler.ProtocolLevelTokens.Kernel,
) where

import Data.Word

import Concordium.Types

import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

class PLTKernelQuery m where
    type PLTAccount m
    getTokenState :: TokenStateKey -> m (Maybe TokenStateValue)
    getAccount :: AccountAddress -> m (Maybe (PLTAccount m))
    getAccountBalance :: PLTAccount m -> m TokenRawAmount
    getAccountState :: PLTAccount m -> TokenStateKey -> m (Maybe TokenStateValue)
    getAccountCanonicalAddress :: PLTAccount m -> m AccountAddress
    getGovernanceAccount :: m (PLTAccount m)
    getCirculatingSupply :: m TokenRawAmount
    getDecimals :: m Word8

class (PLTKernelQuery m) => PLTKernelUpdate m where
    setTokenState :: TokenStateKey -> Maybe TokenStateValue -> m ()
    setAccountState :: PLTAccount m -> TokenStateKey -> Maybe TokenStateValue -> m ()

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

class (PLTKernelUpdate m) => PLTKernelPrivilegedUpdate m where
    -- | Mint a specified amount and deposit it in the specified account.
    --  The return value indicates if this was successful.
    --  Minting can fail if the total supply would exceed the representable amount.
    mint :: PLTAccount m -> TokenRawAmount -> m Bool

    -- | Burn a specified amount from a specified account.
    --  The return value indicates if this was successful.
    --  Burning can fail if the amount in the account is less than the specified amount to burn.
    burn :: PLTAccount m -> TokenRawAmount -> m Bool

class PLTKernelFail e m where
    -- | Abort the current operation by raising an error.
    pltError :: e -> m a
