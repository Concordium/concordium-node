use assert_matches::assert_matches;
use concordium_base::protocol_level_tokens::{
    TokenId, TokenModuleRejectReason, TokenModuleRejectReasonType,
};
use plt_types::types::reject_reasons::{EncodedTokenModuleRejectReason, TransactionRejectReason};

fn decode_token_module_reject_reason(
    reject_reason: &EncodedTokenModuleRejectReason,
) -> TokenModuleRejectReason {
    let reject_reason_type =
        TokenModuleRejectReasonType::try_from_type_discriminator(&reject_reason.reason_type)
            .unwrap();
    TokenModuleRejectReason::decode_reject_reason(
        reject_reason_type,
        reject_reason.details.as_ref().unwrap(),
    )
    .unwrap()
}

pub fn assert_token_module_reject_reason(
    token_id: &TokenId,
    reject_reason: TransactionRejectReason,
) -> TokenModuleRejectReason {
    let reject_reason = assert_matches!(
        &reject_reason,
        TransactionRejectReason::TokenUpdateTransactionFailed(reject_reason) => reject_reason);
    assert_eq!(reject_reason.token_id, *token_id);
    decode_token_module_reject_reason(reject_reason)
}
