use crate::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, IncrementPltUpdateSequenceNumber,
    ReadTokenAccountBalance, UpdateTokenAccountBalance,
};
use crate::block_state::types::TokenIndex;
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::tokens::RawTokenAmount;

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

impl UpdateTokenAccountBalance for UpdateTokenAccountBalanceCallback {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        let result = self(
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
}

/// External function for reading the token balance for an account.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
///   Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
pub type ReadTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64) -> u64;

impl ReadTokenAccountBalance for ReadTokenAccountBalanceCallback {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        let value = self(account.index, token.0);

        RawTokenAmount(value)
    }
}

/// External function for incrementing the PLT update instruction sequence number.
pub type IncrementPltUpdateSequenceNumberCallback = extern "C" fn();

impl IncrementPltUpdateSequenceNumber for IncrementPltUpdateSequenceNumberCallback {
    fn increment_plt_update_sequence_number(&mut self) {
        self();
    }
}

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

impl GetCanonicalAddressByAccountIndex for GetCanonicalAddressByAccountIndexCallback {
    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        let mut account_address = AccountAddress([0; 32]);

        let result = self(account_index.index, account_address.0.as_mut_ptr());

        match result {
            0 => Ok(account_address),
            1 => Err(AccountNotFoundByIndexError(account_index)),
            _ => panic!(
                "Unrecognized result from GetCanonicalAddressByAccountIndexCallback: {}",
                result
            ),
        }
    }
}

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

impl GetAccountIndexByAddress for GetAccountIndexByAddressCallback {
    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        let mut account_index = AccountIndex { index: 0 };

        let result = self(account_address.0.as_ptr(), &mut account_index.index);

        match result {
            0 => Ok(account_index),
            1 => Err(AccountNotFoundByAddressError(*account_address)),
            _ => panic!(
                "Unrecognized result from GetAccountIndexByAddressCallback: {}",
                result
            ),
        }
    }
}
