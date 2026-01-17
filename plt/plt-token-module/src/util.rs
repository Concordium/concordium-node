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
    if amount.decimals() != kernel.decimals() {
        Err(TokenAmountDecimalsMismatchError {
            expected: kernel.decimals(),
            found: amount.decimals(),
        })
    } else {
        Ok(RawTokenAmount(amount.value()))
    }
}

/// Decode given CBOR using decode options set to suit the token module. The decode options
/// will generally be strict.
pub fn cbor_decode<T: CborDeserialize>(cbor: impl AsRef<[u8]>) -> CborSerializationResult<T> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    cbor::cbor_decode_with_options(cbor, decode_options)
}
