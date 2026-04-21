//! Internal utilities for the token module implementation.

use crate::token_module::errors::TokenAmountDecimalsMismatchError;
use concordium_base::common::cbor;
use concordium_base::common::cbor::{
    CborDeserialize, CborSerializationResult, SerializationOptions, UnknownMapKeys,
};
use concordium_base::protocol_level_tokens::TokenAmount;
use plt_block_state::block_state::types::TokenConfiguration;
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

/// Decode given CBOR using decode options set to suit the token module. The decode options
/// will generally be strict.
pub fn cbor_decode<T: CborDeserialize>(cbor: impl AsRef<[u8]>) -> CborSerializationResult<T> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    cbor::cbor_decode_with_options(cbor, decode_options)
}
