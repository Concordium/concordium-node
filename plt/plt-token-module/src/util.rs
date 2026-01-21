//! Internal utilities for the token module implementation.

use crate::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use crate::token_module::TokenAmountDecimalsMismatchError;
use concordium_base::common::cbor;
use concordium_base::common::cbor::{
    CborDeserialize, CborSerializationResult, SerializationOptions, UnknownMapKeys,
};
use concordium_base::protocol_level_tokens::TokenAmount;

/// Checks that token amount has the right number of decimals and converts it to a plain
/// integer and return [`RawTokenAmount`]
pub fn to_raw_token_amount(
    kernel: &impl TokenKernelQueries,
    amount: TokenAmount,
) -> Result<RawTokenAmount, TokenAmountDecimalsMismatchError> {
    let kernel_decimals = kernel.decimals();
    if amount.decimals() != kernel_decimals {
        Err(TokenAmountDecimalsMismatchError {
            expected: kernel_decimals,
            found: amount.decimals(),
        })
    } else {
        Ok(RawTokenAmount(amount.value()))
    }
}

pub fn to_token_amount(kernel: &impl TokenKernelQueries, amount: RawTokenAmount) -> TokenAmount {
    TokenAmount::from_raw(amount.0, kernel.decimals())
}

/// Decode given CBOR using decode options set to suit the token module. The decode options
/// will generally be strict.
pub fn cbor_decode<T: CborDeserialize>(cbor: impl AsRef<[u8]>) -> CborSerializationResult<T> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    cbor::cbor_decode_with_options(cbor, decode_options)
}
