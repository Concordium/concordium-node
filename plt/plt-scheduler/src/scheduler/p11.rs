use crate::scheduler::{plt_scheduler, ChainUpdateExecutionError, TransactionExecutionError};
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use crate::{protocol_level_tokens, TransactionContext};
use concordium_base::protocol_level_tokens::meta_operations::{
    LockOperation, MetaUpdateOperation, MetaUpdateOperations, MetaUpdatePayload,
};
use concordium_base::protocol_level_tokens::{TokenId, TokenOperation};
use concordium_base::transactions;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateResult;
use plt_block_state::utils;
use plt_scheduler_types::types::execution::{
    ChainUpdateOutcome, TransactionExecutionSummary, TransactionOutcome,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Execute a transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason. Additionally, the
/// amount of energy used by the execution is returned. The returned values are represented
/// via the type [`TransactionExecutionSummary`].
///
/// NOTICE: The caller must ensure to rollback state changes in case of the transaction being rejected.
///
/// # Arguments
///
/// - `sender_account` The account initiating the transaction (signer of the transaction)
/// - `transaction_context` Transacstion context containing sender, energy limit etc.
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The transaction payload to execute
///
/// # Errors
///
/// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
///   Returning this error will terminate the scheduler.
pub fn execute_transaction<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    block_state: &mut BlockStateP11,
    transaction_context: TransactionContext,
    sender_account: Account,
    payload: Payload,
) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
    let mut execution = TransactionExecution::new(transaction_context, sender_account);

    let outcome = match payload {
        Payload::TokenUpdate { payload } => {
            protocol_level_tokens::p11::execute_token_update_transaction(
                context,
                &mut execution,
                block_state,
                payload,
            )?
        }
        Payload::MetaUpdate { payload } => {
            execute_meta_update_transaction(context, &mut execution, block_state, payload)?
        }
        _ => return Err(TransactionExecutionError::UnexpectedPayload),
    };

    Ok(TransactionExecutionSummary {
        outcome,
        energy_used: execution.energy_used(),
    })
}

/// Execute [`MetaUpdatePayload`]
fn execute_meta_update_transaction<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    payload: MetaUpdatePayload,
) -> BlockStateResult<TransactionOutcome> {
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

    // Decode operations
    let operations: Vec<MetaUpdateOperation> =
        match utils::cbor_decode::<MetaUpdateOperations>(payload.operations) {
            Ok(payload) => payload.operations,
            Err(_) => {
                return Ok(TransactionOutcome::Rejected(
                    TransactionRejectReason::SerializationFailure,
                ));
            }
        };

    // Execute operations
    for operation in operations {
        match MetaUpdateOperationKind::from(operation) {
            MetaUpdateOperationKind::Token(token_id, token_operation) => {
                match protocol_level_tokens::p11::execute_token_update_operation(
                    context,
                    transaction_execution,
                    block_state,
                    0,
                    &token_id,
                    token_operation,
                    &mut events,
                )? {
                    Ok(()) => (),
                    Err(reject_reason) => {
                        return Ok(TransactionOutcome::Rejected(reject_reason));
                    }
                }
            }
            MetaUpdateOperationKind::Lock(lock_operation) => {
                match plt_scheduler::execute_lock_operation(
                    context,
                    transaction_execution,
                    block_state,
                    lock_operation,
                    &mut events,
                )? {
                    Ok(()) => (),
                    Err(reject_reason) => {
                        return Ok(TransactionOutcome::Rejected(reject_reason));
                    }
                }
            }
        }
    }

    // Return events
    Ok(TransactionOutcome::Success(events))
}

/// Execute a chain update modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a failure kind.
///
/// NOTICE: The caller must ensure to rollback state changes in case a failure kind is returned.
///
/// # Arguments
///
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The chain update payload to execute
///
/// # Errors
///
/// - [`ChainUpdateExecutionError`] If executing the chain update failed in an unrecoverable way.
///   Returning this error will terminate the scheduler.
pub fn execute_chain_update<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    block_state: &mut BlockStateP11,
    payload: UpdatePayload,
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            Ok(protocol_level_tokens::p11::execute_create_plt_chain_update(
                context,
                block_state,
                create_plt,
            )?)
        }
        _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
    }
}

/// A discriminated version of [`MetaUpdateOperation`] for the purpose of
/// dispatching to the appropriate operation handler.
#[derive(PartialEq, Debug, Clone)]
enum MetaUpdateOperationKind {
    /// A [`TokenOperation`] for a specific [`TokenId`].
    Token(TokenId, TokenOperation),
    /// A [`LockOperation`].
    Lock(LockOperation),
}

impl From<MetaUpdateOperation> for MetaUpdateOperationKind {
    fn from(value: MetaUpdateOperation) -> Self {
        match value {
            MetaUpdateOperation::Transfer(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Transfer(details))
            }
            MetaUpdateOperation::Mint(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Mint(details))
            }
            MetaUpdateOperation::Burn(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Burn(details))
            }
            MetaUpdateOperation::AddAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AddAllowList(details))
            }
            MetaUpdateOperation::RemoveAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RemoveAllowList(details))
            }
            MetaUpdateOperation::AddDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AddDenyList(details))
            }
            MetaUpdateOperation::RemoveDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RemoveDenyList(details))
            }
            MetaUpdateOperation::Pause(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Pause(details))
            }
            MetaUpdateOperation::Unpause(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Unpause(details))
            }
            MetaUpdateOperation::AssignAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AssignAdminRoles(details))
            }
            MetaUpdateOperation::RevokeAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RevokeAdminRoles(details))
            }
            MetaUpdateOperation::UpdateMetadata(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::UpdateMetadata(details))
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_transfer),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_mint),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_burn),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_add_allow_list),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_remove_allow_list),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_add_deny_list),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_remove_deny_list),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_pause),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_unpause),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_assign_admin_roles),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_revoke_admin_roles),
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
            MetaUpdateOperationKind::Token(token_id.clone(), token_update_metadata),
            meta_update_metadata.into(),
        );
    }
}
