use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::types::{TokenAccountState, TokenIndex};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::tokens::RawTokenAmount;

/// Callbacks for block state queries.
pub struct ExternalBlockStateQueryCallbacks {
    /// External function for reading the token balance for an account.
    pub read_token_account_balance_ptr: ReadTokenAccountBalanceCallback,
    /// External function for fetching account address by index.
    pub get_account_address_by_index_ptr: GetCanonicalAddressByAccountIndexCallback,
    /// External function for fetching account index by address.
    pub get_account_index_by_address_ptr: GetAccountIndexByAddressCallback,
    /// External function for getting token account states.
    pub get_token_account_states_ptr: GetTokenAccountStatesCallback,
}

/// Callbacks for block state operations.
pub struct ExternalBlockStateOperationCallbacks {
    /// Callbacks for block state queries.
    pub queries: ExternalBlockStateQueryCallbacks,
    /// External function for updating the token balance for an account.
    pub update_token_account_balance_ptr: UpdateTokenAccountBalanceCallback,
    /// External function for incrementing the PLT update sequence number.
    pub increment_plt_update_sequence_number_ptr: IncrementPltUpdateSequenceNumberCallback,
}

impl ExternalBlockStateQuery for ExternalBlockStateQueryCallbacks {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        let value = (self.read_token_account_balance_ptr)(account.index, token.0);

        RawTokenAmount(value)
    }

    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        let mut account_address = AccountAddress([0; 32]);

        let result = (self.get_account_address_by_index_ptr)(
            account_index.index,
            account_address.0.as_mut_ptr(),
        );

        match result {
            0 => Ok(account_address),
            1 => Err(AccountNotFoundByIndexError(account_index)),
            _ => panic!(
                "Unrecognized result from GetCanonicalAddressByAccountIndexCallback: {}",
                result
            ),
        }
    }

    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        let mut account_index = AccountIndex { index: 0 };

        let result = (self.get_account_index_by_address_ptr)(
            account_address.0.as_ptr(),
            &mut account_index.index,
        );

        match result {
            0 => Ok(account_index),
            1 => Err(AccountNotFoundByAddressError(*account_address)),
            _ => panic!(
                "Unrecognized result from GetAccountIndexByAddressCallback: {}",
                result
            ),
        }
    }

    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        let bytes =
            unsafe { Box::from_raw((self.get_token_account_states_ptr)(account_index.index)) };
        common::from_bytes_complete(*bytes)
            .expect("Invalid serialization of (TokenIndex, TokenAccountState) list")
    }
}

impl ExternalBlockStateOperations for ExternalBlockStateOperationCallbacks {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        let result = (self.update_token_account_balance_ptr)(
            account.index,
            token.0,
            match amount_delta {
                RawTokenAmountDelta::Add(amount) => amount.0,
                RawTokenAmountDelta::Subtract(amount) => amount.0,
            },
            match amount_delta {
                RawTokenAmountDelta::Add(_) => 1,
                RawTokenAmountDelta::Subtract(_) => 0,
            },
        );

        match result {
            0 => Ok(()),
            1 => Err(OverflowError),
            _ => panic!(
                "Unrecognized result from UpdateTokenAccountBalanceCallback: {}",
                result
            ),
        }
    }

    fn increment_plt_update_sequence_number(&mut self) {
        (self.increment_plt_update_sequence_number_ptr)();
    }
}

impl ExternalBlockStateQuery for ExternalBlockStateOperationCallbacks {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        self.queries.read_token_account_balance(account, token)
    }

    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        self.queries
            .account_canonical_address_by_account_index(account_index)
    }

    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        self.queries
            .account_index_by_account_address(account_address)
    }

    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        self.queries.token_account_states(account_index)
    }
}

/// External function for updating the token balance for an account.
///
/// Returns `0` if the balance change was applied, and `1` if the balance change would result in a
/// negative balance or a balance above the representable amount.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid
///   account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
/// - `amount` The amount to add to or subtract from the balance.
/// - `add_amount` If `1`, the amount will be added to the balance. If `0`, it will be subtracted.
pub type UpdateTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64, amount: u64, add_amount: u8) -> u8;

/// External function for reading the token balance for an account.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
///   Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
pub type ReadTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64) -> u64;

/// External function for incrementing the PLT update instruction sequence number.
pub type IncrementPltUpdateSequenceNumberCallback = extern "C" fn();

/// External function for getting account canonical address by account index.
/// Returns `0` if the account exist, `1` if not.
/// If the account exists, its canonical address is written to `account_address_out`.
///
/// # Arguments
///
/// - `account_index` Index of the (possibly existing) account to get.
/// - `account_address_out` Pointer to where to write the canonical account address of 32 bytes.
///
/// # Safety
///
/// - Argument `account_address_out` must be non-null and valid for writes of 32 bytes.
pub type GetCanonicalAddressByAccountIndexCallback =
    extern "C" fn(account_index: u64, account_address_out: *mut u8) -> u8;

/// External function for getting account index by account address (canonical address or alias address).
/// Returns `0` if the account exist, `1` if not.
/// If the account exists, its index is written to `account_index_out`.
///
/// # Arguments
///
/// - `account_address` Address 32 bytes of the (possibly existing) account to get.
/// - `account_index_out` Pointer to where to write account index.
///
/// # Safety
///
/// - Argument `account_address` must be non-null and valid for reads for 32 bytes.
/// - Argument `account_index_out` must be a non-null and valid pointer for writing.
pub type GetAccountIndexByAddressCallback =
    extern "C" fn(account_address: *const u8, account_index: *mut u64) -> u8;

/// External function for getting token account states for an account.
/// Returns pointer to a `Vec<u8>` which must be taken ownership of and deallocated.
/// The bytes in the `Vec<u8>` contains binary serialized list of token indexes and token account states.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid
///   account index of an existing account.
pub type GetTokenAccountStatesCallback = extern "C" fn(account_index: u64) -> *mut Vec<u8>;
