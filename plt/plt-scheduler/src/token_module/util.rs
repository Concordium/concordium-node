//! Internal utilities for the token module implementation.

use crate::token_module::errors::TokenAmountDecimalsMismatchError;
use concordium_base::protocol_level_tokens::TokenAmount;
use plt_block_state::entity::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Checks that token amount has the right number of decimals and converts it to a plain
/// integer and return [`RawTokenAmount`]
pub fn to_raw_token_amount(
    token_configuration: &TokenConfiguration,
    amount: TokenAmount,
) -> Result<RawTokenAmount, TokenAmountDecimalsMismatchError> {
    let kernel_decimals = token_configuration.decimals;
    if amount.decimals() != kernel_decimals {
        Err(TokenAmountDecimalsMismatchError {
            expected: kernel_decimals,
            found: amount.decimals(),
        })
    } else {
        Ok(RawTokenAmount(amount.value()))
    }
}

pub fn to_token_amount(
    token_configuration: &TokenConfiguration,
    amount: RawTokenAmount,
) -> TokenAmount {
    TokenAmount::from_raw(amount.0, token_configuration.decimals)
}
