use concordium_base::protocol_level_tokens::{
    TokenModuleRejectReasonEnum, TokenModuleRejectReasonType,
};
use plt_token_module::token_module::TokenModuleRejectReason;

pub fn decode_reject_reason(
    reject_reason: &TokenModuleRejectReason,
) -> TokenModuleRejectReasonEnum {
    let reject_reason_type =
        TokenModuleRejectReasonType::try_from_type_discriminator(&reject_reason.reason_type)
            .unwrap();
    TokenModuleRejectReasonEnum::decode_reject_reason(
        reject_reason_type,
        reject_reason.details.as_ref().unwrap(),
    )
    .unwrap()
}
