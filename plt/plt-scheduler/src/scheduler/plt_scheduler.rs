//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::locks::get_lock_config;
use crate::locks::lock_controller::LockController;
use crate::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenOperation};
use concordium_base::protocol_level_tokens::{
    TokenOperationsPayload,
    meta_operations::{
        LockOperation, MetaUpdateOperation, MetaUpdateOperations, MetaUpdatePayload,
    },
};
use concordium_base::transactions;
use concordium_base::updates::CreatePlt;
use plt_block_state::block_state_interface::BlockStateOperations;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::persistent::protocol_level_locks::p11::LockConfiguration;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_block_state::utils;
use plt_scheduler_types::types::events::{self, BlockItemEvent, TokenCreateEvent};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, FailureKind, TransactionOutcome};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};

/// Execute a meta-update transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason.
/// Energy must be charged during execution by calling [`TransactionExecution::tick_energy`]. If
/// execution is out of energy, the function `tick_energy` returns an error which means execution must be stopped,
/// and the [`OutOfEnergy`](TransactionRejectReason::OutOfEnergy) reject reason must be returned.
///
/// NOTICE: The caller must ensure to rollback state changes in case of the transaction being rejected.
///
/// # Arguments
///
/// - `transaction_execution` Context of transaction execution that allows accessing sending account
///   and charging energy.
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The meta-update transaction payload to execute.
///
/// # Errors
///
/// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
///   Returning this error will terminate the scheduler.
pub fn execute_meta_update_transaction<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    block_state: &mut BSO,
    payload: MetaUpdatePayload,
) -> Result<TransactionOutcome, TransactionExecutionError> {
    // Charge energy
    if let Err(err) =
        transaction_execution.tick_energy(transactions::cost::META_UPDATE_TRANSACTIONS)
    {
        let _: OutOfEnergyError = err; // assert type of error
        return Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        ));
    }

    let mut events = Vec::new();

    let operations: Vec<MetaUpdateOperation> =
        match utils::cbor_decode::<MetaUpdateOperations>(payload.operations) {
            Ok(payload) => payload.operations,
            Err(_) => {
                return Ok(TransactionOutcome::Rejected(
                    TransactionRejectReason::SerializationFailure,
                ));
            }
        };

    for (index, operation) in operations.into_iter().enumerate() {
        match operation.into() {
            MetaUpdateOperationKind::Token((token_id, operation)) => {
                // Lookup token
                let token = match block_state.token_by_id(&token_id) {
                    Ok(token) => token,
                    Err(TokenNotFoundByIdError(_)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::NonExistentTokenId(token_id),
                        ));
                    }
                };

                let token_configuration = block_state.token_configuration(&token);
                let mut token_module_state = block_state.mutable_token_key_value_state(&token);
                let mut token_module_state_dirty = false;
                let mut kernel = TokenOperationContext {
                    block_state,
                    token: &token,
                    token_configuration: &token_configuration,
                    token_module_state: &mut token_module_state,
                    token_module_state_dirty: &mut token_module_state_dirty,
                    events: &mut events,
                };
                let token_update_result = token_module::execute_token_update_operation_at_index(
                    transaction_execution,
                    &mut kernel,
                    index,
                    &operation,
                );
                match token_update_result {
                    Ok(()) => {
                        if token_module_state_dirty {
                            block_state.set_token_key_value_state(&token, token_module_state);
                        }
                    }
                    Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::TokenUpdateTransactionFailed(
                                EncodedTokenModuleRejectReason {
                                    token_id,
                                    reason_type: reject_reason.reason_type,
                                    details: reject_reason.details,
                                },
                            ),
                        ));
                    }
                    Err(TokenUpdateError::OutOfEnergy(_)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::OutOfEnergy,
                        ));
                    }
                    Err(TokenUpdateError::StateInvariantViolation(err)) => {
                        return Err(TransactionExecutionError::BlockStateFailure(
                            err.to_string(),
                        ));
                    }
                }
            }
            MetaUpdateOperationKind::Lock(lock_operation) => {
                let result = execute_lock_operation(
                    transaction_execution,
                    block_state,
                    index,
                    lock_operation,
                    &mut events,
                )?;
                if let Some(reject_reason) = result {
                    return Ok(TransactionOutcome::Rejected(reject_reason));
                }
            }
        }
    }
    Ok(TransactionOutcome::Success(events))
}

fn execute_lock_operation<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    block_state: &mut BSO,
    _index: usize,
    lock_operation: LockOperation,
    events: &mut Vec<BlockItemEvent>,
) -> Result<Option<TransactionRejectReason>, TransactionExecutionError> {
    match lock_operation {
        LockOperation::Fund(_meta_lock_fund_details) => todo!(),
        LockOperation::Send(_meta_lock_send_details) => todo!(),
        LockOperation::Return(_meta_lock_return_details) => todo!(),
        LockOperation::Create(meta_lock_create_details) => {
            let config = meta_lock_create_details.config;
            let account_index = block_state.account_index(transaction_execution.sender_account());
            let sequence_number = transaction_execution.transaction_sequence_number();
            let creation_order = transaction_execution.next_lock_creation_order();
            let lock_id = LockId::new(account_index, sequence_number, creation_order);
            let controller = match LockController::new(block_state, config.controller) {
                Ok(controller) => controller,
                Err(reject_reason) => return Ok(Some(reject_reason)),
            };

            let recipients = match config
                .recipients
                .iter()
                .map(
                    |recipient| match block_state.account_by_address(&recipient.address) {
                        Ok(account) => Ok(block_state.account_index(&account)),
                        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
                            recipient.address,
                        )),
                    },
                )
                .collect::<Result<Vec<_>, TransactionRejectReason>>()
            {
                Ok(recipients) => recipients,
                Err(reject_reason) => return Ok(Some(reject_reason)),
            };
            let configuration = LockConfiguration::new(recipients, config.expiry, controller);

            // We reconstruct the lock config for the event, rather than using
            // the original one from the transaction. This results in a config
            // that is in a canonical form.
            let config = get_lock_config(block_state, &configuration).map_err(|err| {
                TransactionExecutionError::BlockStateFailure(format!(
                    "Failed to get lock config for created lock: {err}"
                ))
            })?;
            let event = events::LockCreateEvent {
                lock_id: lock_id.clone(),
                lock_config: RawCbor::from(cbor::cbor_encode(&config)),
            };
            events.push(BlockItemEvent::LockCreated(event));

            block_state.create_lock(lock_id.clone(), configuration);

            Ok(None)
        }
        LockOperation::Cancel(_meta_lock_cancel_details) => todo!(),
    }
}

/// A discriminated version of [`MetaUpdateOperation`] for the purpose of
/// dispatching to the appropriate operation handler.
#[derive(PartialEq, Debug, Clone)]
pub enum MetaUpdateOperationKind {
    /// A [`TokenOperation`] for a specific [`TokenId`].
    Token((TokenId, TokenOperation)),
    /// A [`LockOperation`].
    Lock(LockOperation),
}

impl From<MetaUpdateOperation> for MetaUpdateOperationKind {
    fn from(value: MetaUpdateOperation) -> Self {
        match value {
            MetaUpdateOperation::Transfer(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Transfer(details)))
            }
            MetaUpdateOperation::Mint(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Mint(details)))
            }
            MetaUpdateOperation::Burn(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Burn(details)))
            }
            MetaUpdateOperation::AddAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AddAllowList(details)))
            }
            MetaUpdateOperation::RemoveAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RemoveAllowList(details)))
            }
            MetaUpdateOperation::AddDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AddDenyList(details)))
            }
            MetaUpdateOperation::RemoveDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RemoveDenyList(details)))
            }
            MetaUpdateOperation::Pause(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Pause(details)))
            }
            MetaUpdateOperation::Unpause(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Unpause(details)))
            }
            MetaUpdateOperation::AssignAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AssignAdminRoles(details)))
            }
            MetaUpdateOperation::RevokeAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RevokeAdminRoles(details)))
            }
            MetaUpdateOperation::UpdateMetadata(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::UpdateMetadata(details)))
            }
            MetaUpdateOperation::LockFund(details) => Self::Lock(LockOperation::Fund(details)),
            MetaUpdateOperation::LockSend(details) => Self::Lock(LockOperation::Send(details)),
            MetaUpdateOperation::LockReturn(details) => Self::Lock(LockOperation::Return(details)),
            MetaUpdateOperation::LockCreate(details) => Self::Lock(LockOperation::Create(details)),
            MetaUpdateOperation::LockCancel(details) => Self::Lock(LockOperation::Cancel(details)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use concordium_base::common;
    use concordium_base::transactions::Memo;

    #[test]
    fn test_meta_operation_token_operation_conversion() {
        use concordium_base::protocol_level_tokens::meta_operations::*;
        use concordium_base::protocol_level_tokens::*;
        // For each meta-update operation variant:
        // - construct a meta-update operation with some test data
        // - construct the corresponding token operation with the same test data
        // - convert in each direction and check that the result matches the original
        // - construct the meta-update operation using the `meta_operations` helper function
        //   and check that it matches the original
        let token_id: TokenId = "tokenid1".parse().unwrap();
        let amount = TokenAmount::from_raw(100000, 2);
        const ADDRESS: common::types::AccountAddress = common::types::AccountAddress([
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
            0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
            0x1D, 0x1E, 0x1F, 0x20,
        ]);
        let account = CborHolderAccount::from(ADDRESS);
        let cbor_memo = CborMemo::Raw(Memo::try_from(vec![1, 2, 3, 4]).unwrap());
        let memo = Some(cbor_memo.clone());

        let token_transfer = TokenOperation::Transfer(TokenTransfer {
            amount,
            recipient: account.clone(),
            memo: memo.clone(),
        });
        let meta_transfer = MetaUpdateOperation::Transfer(MetaTokenTransfer {
            token: token_id.clone(),
            amount,
            recipient: account.clone(),
            memo: memo.clone(),
        });
        assert_eq!(
            transfer_tokens_with_memo(token_id.clone(), ADDRESS, amount, cbor_memo.clone()),
            meta_transfer
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_transfer.clone())),
            meta_transfer
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_transfer)),
            meta_transfer.into(),
        );

        let token_mint = TokenOperation::Mint(TokenSupplyUpdateDetails { amount });
        let meta_mint = MetaUpdateOperation::Mint(MetaTokenSupplyUpdateDetails {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(mint_tokens(token_id.clone(), amount), meta_mint);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_mint.clone())),
            meta_mint
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_mint)),
            meta_mint.into(),
        );

        let token_burn = TokenOperation::Burn(TokenSupplyUpdateDetails { amount });
        let meta_burn = MetaUpdateOperation::Burn(MetaTokenSupplyUpdateDetails {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(burn_tokens(token_id.clone(), amount), meta_burn);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_burn.clone())),
            meta_burn
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_burn)),
            meta_burn.into(),
        );

        let token_add_allow_list = TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_add_allow_list = MetaUpdateOperation::AddAllowList(MetaTokenListUpdateDetails {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            add_token_allow_list(token_id.clone(), ADDRESS),
            meta_add_allow_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_add_allow_list.clone())),
            meta_add_allow_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_add_allow_list)),
            meta_add_allow_list.into(),
        );

        let token_remove_allow_list = TokenOperation::RemoveAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_remove_allow_list =
            MetaUpdateOperation::RemoveAllowList(MetaTokenListUpdateDetails {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            remove_token_allow_list(token_id.clone(), ADDRESS),
            meta_remove_allow_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_remove_allow_list.clone())),
            meta_remove_allow_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_remove_allow_list)),
            meta_remove_allow_list.into(),
        );

        let token_add_deny_list = TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_add_deny_list = MetaUpdateOperation::AddDenyList(MetaTokenListUpdateDetails {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            add_token_deny_list(token_id.clone(), ADDRESS),
            meta_add_deny_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_add_deny_list.clone())),
            meta_add_deny_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_add_deny_list)),
            meta_add_deny_list.into(),
        );

        let token_remove_deny_list = TokenOperation::RemoveDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_remove_deny_list =
            MetaUpdateOperation::RemoveDenyList(MetaTokenListUpdateDetails {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            remove_token_deny_list(token_id.clone(), ADDRESS),
            meta_remove_deny_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_remove_deny_list.clone())),
            meta_remove_deny_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_remove_deny_list)),
            meta_remove_deny_list.into(),
        );

        let token_pause = TokenOperation::Pause(TokenPauseDetails {});
        let meta_pause = MetaUpdateOperation::Pause(MetaTokenPauseDetails {
            token: token_id.clone(),
        });
        assert_eq!(pause(token_id.clone()), meta_pause);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_pause.clone())),
            meta_pause
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_pause)),
            meta_pause.into(),
        );

        let token_unpause = TokenOperation::Unpause(TokenPauseDetails {});
        let meta_unpause = MetaUpdateOperation::Unpause(MetaTokenPauseDetails {
            token: token_id.clone(),
        });
        assert_eq!(unpause(token_id.clone()), meta_unpause);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_unpause.clone())),
            meta_unpause
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_unpause)),
            meta_unpause.into(),
        );

        let assign_roles = vec![TokenAdminRole::Mint, TokenAdminRole::Pause];
        let token_assign_admin_roles =
            TokenOperation::AssignAdminRoles(TokenUpdateAdminRolesDetails {
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        let meta_assign_admin_roles =
            MetaUpdateOperation::AssignAdminRoles(MetaTokenUpdateAdminRolesDetails {
                token: token_id.clone(),
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            assign_admin_roles(token_id.clone(), ADDRESS, assign_roles.clone()),
            meta_assign_admin_roles
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_assign_admin_roles.clone())),
            meta_assign_admin_roles
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_assign_admin_roles)),
            meta_assign_admin_roles.into(),
        );

        let revoke_roles = vec![
            TokenAdminRole::Burn,
            TokenAdminRole::UpdateMetadata,
            TokenAdminRole::UpdateDenyList,
        ];
        let token_revoke_admin_roles =
            TokenOperation::RevokeAdminRoles(TokenUpdateAdminRolesDetails {
                roles: revoke_roles.clone(),
                account: account.clone(),
            });
        let meta_revoke_admin_roles =
            MetaUpdateOperation::RevokeAdminRoles(MetaTokenUpdateAdminRolesDetails {
                token: token_id.clone(),
                roles: revoke_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            revoke_admin_roles(token_id.clone(), ADDRESS, revoke_roles.clone()),
            meta_revoke_admin_roles
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_revoke_admin_roles.clone())),
            meta_revoke_admin_roles
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_revoke_admin_roles)),
            meta_revoke_admin_roles.into(),
        );

        let metadata_url = MetadataUrl {
            url: "https://example.com/metadata.json".to_string(),
            checksum_sha_256: Some([0u8; 32].into()),
            additional: Default::default(),
        };
        let token_update_metadata = TokenOperation::UpdateMetadata(metadata_url.clone());
        let meta_update_metadata = MetaUpdateOperation::UpdateMetadata(MetaMetadataUrlDetails {
            token: token_id.clone(),
            metadata_url: metadata_url.clone(),
        });
        assert_eq!(
            update_metadata(token_id.clone(), metadata_url.clone()),
            meta_update_metadata
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_update_metadata.clone())),
            meta_update_metadata
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_update_metadata)),
            meta_update_metadata.into(),
        );
    }
}
