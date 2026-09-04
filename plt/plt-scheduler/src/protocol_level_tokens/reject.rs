//! Constructors for [`TransactionRejectReason`]s created by the token module.
//!
//! The token module reject reasons are represented as a [`TokenModuleRejectReason`] inside
//! the variant [`TransactionRejectReason::TokenUpdateTransactionFailed`], hence their construction
//! is more elaborate than other transaction reject reasons.

use crate::protocol_level_tokens::balance_operations::{
    InsufficientBalanceError, MintWouldOverflowError,
};
use crate::protocol_level_tokens::token_amount;
use crate::protocol_level_tokens::token_amount::TokenAmountDecimalsMismatchError;
use concordium_base::common::cbor::CborSerializationError;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason,
    MintWouldOverflowRejectReason, OperationNotPermittedRejectReason,
    TokenBalanceInsufficientRejectReason, TokenModuleRejectReason,
    UnsupportedOperationRejectReason,
};
use plt_block_state::external::AccountNotFoundByAddressError;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};

pub fn address_not_found(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    err: AccountNotFoundByAddressError,
) -> TransactionRejectReason {
    let reject = TokenModuleRejectReason::AddressNotFound(AddressNotFoundRejectReason {
        index: operation_index as u64,
        address: CborHolderAccount::from(err.0),
    });

    token_module_reject(token_configuration, reject)
}

pub fn insufficient_balance(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    err: InsufficientBalanceError,
) -> TransactionRejectReason {
    let reject =
        TokenModuleRejectReason::TokenBalanceInsufficient(TokenBalanceInsufficientRejectReason {
            index: operation_index as u64,
            available_balance: token_amount::to_token_amount(token_configuration, err.available),
            required_balance: token_amount::to_token_amount(token_configuration, err.required),
        });

    token_module_reject(token_configuration, reject)
}

pub fn mint_would_overflow(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    err: MintWouldOverflowError,
) -> TransactionRejectReason {
    let reject = TokenModuleRejectReason::MintWouldOverflow(MintWouldOverflowRejectReason {
        index: operation_index as u64,
        requested_amount: token_amount::to_token_amount(token_configuration, err.requested_amount),
        current_supply: token_amount::to_token_amount(token_configuration, err.current_supply),
        max_representable_amount: token_amount::to_token_amount(
            token_configuration,
            err.max_representable_amount,
        ),
    });

    token_module_reject(token_configuration, reject)
}

pub fn operation_not_permitted(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    account_address: Option<AccountAddress>,
    reason: String,
) -> TransactionRejectReason {
    let reject =
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: operation_index as u64,
            address: account_address.map(Into::into),
            reason: Some(reason),
        });

    token_module_reject(token_configuration, reject)
}

pub fn operation_not_permitted_paused(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    operation_type: &'static str,
) -> TransactionRejectReason {
    let reject =
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: operation_index as u64,
            address: None,
            reason: format!("token operation {operation_type} is paused")
                .to_string()
                .into(),
        });

    token_module_reject(token_configuration, reject)
}

pub fn unsupported_operation(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    operation_type: &'static str,
    reason: String,
) -> TransactionRejectReason {
    let reject = TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason {
        index: operation_index as u64,
        operation_type: operation_type.to_string(),
        reason: Some(reason),
    });

    token_module_reject(token_configuration, reject)
}

pub fn deserialization_failure(
    token_configuration: &TokenConfiguration,
    err: CborSerializationError,
) -> TransactionRejectReason {
    let reject =
        TokenModuleRejectReason::DeserializationFailure(DeserializationFailureRejectReason {
            cause: Some(err.to_string()),
        });

    token_module_reject(token_configuration, reject)
}

pub fn deserialization_failure_amount_decimals_mismatch(
    token_configuration: &TokenConfiguration,
    err: TokenAmountDecimalsMismatchError,
) -> TransactionRejectReason {
    let reject =
        TokenModuleRejectReason::DeserializationFailure(DeserializationFailureRejectReason {
            cause: Some(err.to_string()),
        });

    token_module_reject(token_configuration, reject)
}

/// Generic constructor for creating a [`TransactionRejectReason`]
/// from [`TokenModuleRejectReason`].
pub fn token_module_reject(
    token_configuration: &TokenConfiguration,
    reject_reason: TokenModuleRejectReason,
) -> TransactionRejectReason {
    let (reason_type, cbor) = reject_reason.encode_reject_reason();
    TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
        // Use the canonical token id from the token configuration
        token_id: token_configuration.token_id.clone(),
        reason_type: reason_type.to_type_discriminator(),
        details: Some(cbor),
    })
}
