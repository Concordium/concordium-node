//! Runtime interface for protocol-level lock controllers.

use crate::failure::ResultWithBlockStateFailure;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;
use concordium_base::protocol_level_tokens::CborHolderAccount;
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaLockCancelDetails, MetaLockFundDetails, MetaLockReturnDetails, MetaLockSendDetails,
};
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockControllerConfig, LockControllerSimpleV0, LockControllerSimpleV0Grant,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Runtime lock operation model. This corresponds to the "fund", "send", "return", and "cancel"
/// CBOR operations for interacting with locks from concordium-base.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {
    Fund(MetaLockFundDetails),
    Send(MetaLockSendDetails),
    Return(MetaLockReturnDetails),
    Cancel(MetaLockCancelDetails),
}

/// Approve or reject a lock operation. Returns `Ok(())` if the operation is authorized, or
/// a `TransactionRejectReason` if it is not.
///
/// * `sender_address`: account address of the sender
/// * `sender`: the transaction sender reference
/// * `operation`: the lock operation to approve/reject.
pub fn validate_operation(
    controller_config: &LockControllerConfig,
    sender_address: AccountAddress,
    sender: &Account,
    operation: &LockOperation,
) -> Result<(), TransactionRejectReason> {
    let LockControllerConfig::SimpleV0(controller_config) = controller_config;

    match operation {
        LockOperation::Fund(fund_details) => {
            if !controller_config.has_role(
                sender.account_index(),
                LockControllerSimpleV0Capability::Fund,
            ) {
                return Err(TransactionRejectReason::LockFundNotAuthorized(
                    fund_details.lock.clone(),
                    sender_address,
                ));
            }
            if !controller_config.tokens().contains(&fund_details.token) {
                return Err(TransactionRejectReason::LockTokenNotPermitted(
                    fund_details.lock.clone(),
                    fund_details.token.clone(),
                ));
            }
        }
        LockOperation::Send(send_details) => {
            if !controller_config.has_role(
                sender.account_index(),
                LockControllerSimpleV0Capability::Send,
            ) {
                return Err(TransactionRejectReason::LockSendNotAuthorized(
                    send_details.lock.clone(),
                    sender_address,
                ));
            }
        }
        LockOperation::Return(return_details) => {
            if !controller_config.has_role(
                sender.account_index(),
                LockControllerSimpleV0Capability::Return,
            ) {
                return Err(TransactionRejectReason::LockReturnNotAuthorized(
                    return_details.lock.clone(),
                    sender_address,
                ));
            }
        }
        LockOperation::Cancel(cancel_details) => {
            if !controller_config.has_role(
                sender.account_index(),
                LockControllerSimpleV0Capability::Cancel,
            ) {
                return Err(TransactionRejectReason::LockCancelNotAuthorized(
                    cancel_details.lock.clone(),
                    sender_address,
                ));
            }
        }
    }
    Ok(())
}

/// Construct this lock controller from the given configuration.
pub fn from_cbor_controller<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    cbor_controller: concordium_base::protocol_level_locks::LockController,
) -> ResultWithBlockStateFailure<LockControllerConfig, TransactionRejectReason> {
    let concordium_base::protocol_level_locks::LockController::SimpleV0(cbor_controller) =
        cbor_controller;

    let grants = cbor_controller
        .grants
        .into_iter()
        .map(|grant| {
            let account = context.account_by_address(&grant.account.address).map_err(
                |_err: AccountNotFoundByAddressError| {
                    TransactionRejectReason::InvalidAccountReference(grant.account.address)
                },
            )?;

            Ok(LockControllerSimpleV0Grant::new(
                account.account_index(),
                grant.roles,
            ))
        })
        .collect::<ResultWithBlockStateFailure<_, TransactionRejectReason>>()?;

    let tokens = cbor_controller
        .tokens
        .into_iter()
        .map(|token_id| {
            // Check that token exists
            let token = block_state.token_by_id(context, &token_id)?.map_err(
                |_err: TokenNotFoundByIdError| {
                    TransactionRejectReason::NonExistentTokenId(token_id.clone())
                },
            )?;

            // Return canonical token id
            Ok(token.token_p9_base.token_configuration(context)?.token_id)
        })
        .collect::<ResultWithBlockStateFailure<_, TransactionRejectReason>>()?;

    let lock_controller = LockControllerSimpleV0::new(
        grants,
        tokens,
        cbor_controller.keep_alive,
        cbor_controller.memo,
    )
    .map_err(|_| TransactionRejectReason::SerializationFailure)?;

    Ok(LockControllerConfig::SimpleV0(lock_controller))
}

/// Convert this controller configuration to its canonical CBOR
/// [`concordium_base::protocol_level_locks::LockController`] representation, used by the
/// `lock-info` payload returned from `query_lock_info`.
pub fn to_cbor_controller<C: EntityContextTypes>(
    context: &EntityContext<C>,
    controller_config: &LockControllerConfig,
) -> BlockStateResult<concordium_base::protocol_level_locks::LockController> {
    let LockControllerConfig::SimpleV0(controller_config) = controller_config;

    let grants = controller_config
        .grants()
        .iter()
        .map(|grant| {
            let with_addr = context.account_by_index(grant.account()).map_err(
                |err: AccountNotFoundByIndexError| {
                    BlockStateFailure::Invariant(format!(
                        "Account persisted in lock controller grants not found: {}",
                        err
                    ))
                },
            )?;
            Ok(
                concordium_base::protocol_level_locks::LockControllerSimpleV0Grant {
                    account: CborHolderAccount::from(with_addr.canonical_account_address),
                    roles: grant.roles().to_vec(),
                },
            )
        })
        .collect::<Result<_, _>>()?;
    Ok(
        concordium_base::protocol_level_locks::LockController::SimpleV0(
            concordium_base::protocol_level_locks::LockControllerSimpleV0 {
                grants,
                tokens: controller_config.tokens().to_vec(),
                keep_alive: controller_config.keep_alive,
                memo: controller_config.memo.clone(),
            },
        ),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::failure::WithBlockStateFailure;
    use plt_block_state::entity::entity_test_stub;

    #[test]
    fn oversized_lock_controller_configuration_is_a_serialization_failure() {
        let mut context = entity_test_stub::new_stubbed_context();
        let account_index = context.external.create_account().account_index();
        let account = context.external.account_canonical_address(account_index);
        let block_state = BlockStateP11::default();
        let controller = concordium_base::protocol_level_locks::LockController::SimpleV0(
            concordium_base::protocol_level_locks::LockControllerSimpleV0 {
                grants: vec![
                    concordium_base::protocol_level_locks::LockControllerSimpleV0Grant {
                        account: CborHolderAccount::from(account),
                        roles: Vec::new(),
                    };
                    u16::MAX as usize + 1
                ],
                tokens: Vec::new(),
                keep_alive: false,
                memo: None,
            },
        );

        assert!(matches!(
            from_cbor_controller(&context, &block_state, controller),
            Err(WithBlockStateFailure::Error(
                TransactionRejectReason::SerializationFailure
            ))
        ));
    }
}
