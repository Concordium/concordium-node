use concordium_base::protocol_level_tokens::TokenAmount;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use crate::failure::HigherLevelProtocolError;

/// Token amount decimals mismatch
#[derive(Debug, thiserror::Error)]
#[error("Token amount decimals mismatch: expected {expected}, found {found}")]
pub struct TokenAmountDecimalsMismatchError {
    /// Expected decimals
    pub expected: u8,
    /// Actual decimals
    pub found: u8,
}

impl HigherLevelProtocolError for TokenAmountDecimalsMismatchError {}

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
