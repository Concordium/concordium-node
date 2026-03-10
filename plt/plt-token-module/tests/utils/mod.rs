// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use assert_matches::assert_matches;
use concordium_base::base::ProtocolVersion;
use concordium_base::protocol_level_tokens::{
    TokenModuleRejectReason, TokenModuleRejectReasonType,
};
use plt_token_module::token_module::{RejectReason, TokenUpdateError};

pub mod kernel_stub;

/// The most recent protocol version.
pub const LATEST_PROTOCOL_VERSION: ProtocolVersion = ProtocolVersion::P11;

fn decode_reject_reason(reject_reason: &RejectReason) -> TokenModuleRejectReason {
    let reject_reason_type =
        TokenModuleRejectReasonType::try_from_type_discriminator(&reject_reason.reason_type)
            .unwrap();
    TokenModuleRejectReason::decode_reject_reason(
        reject_reason_type,
        reject_reason.details.as_ref().unwrap(),
    )
    .unwrap()
}

pub fn assert_reject_reason(result: &Result<(), TokenUpdateError>) -> TokenModuleRejectReason {
    let reject_reason = assert_matches!(
        &result,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => reject_reason);
    decode_reject_reason(reject_reason)
}
