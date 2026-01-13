use assert_matches::assert_matches;
use concordium_base::protocol_level_tokens::{
    TokenModuleRejectReasonEnum, TokenModuleRejectReasonType,
};
use plt_token_module::token_module::{RejectReason, TokenUpdateError};

fn decode_reject_reason(reject_reason: &RejectReason) -> TokenModuleRejectReasonEnum {
    let reject_reason_type =
        TokenModuleRejectReasonType::try_from_type_discriminator(&reject_reason.reason_type)
            .unwrap();
    TokenModuleRejectReasonEnum::decode_reject_reason(
        reject_reason_type,
        reject_reason.details.as_ref().unwrap(),
    )
    .unwrap()
}

pub fn assert_reject_reason(result: &Result<(), TokenUpdateError>) -> TokenModuleRejectReasonEnum {
    let reject_reason = assert_matches!(
        &result,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => reject_reason);
    decode_reject_reason(reject_reason)
}
