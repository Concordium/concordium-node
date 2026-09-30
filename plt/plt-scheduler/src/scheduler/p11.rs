use crate::failure::ResultWithBlockStateFailureExt;
use crate::protocol_level_locks::lock_configuration::LockOperation;
use crate::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use crate::{TransactionContext, protocol_level_locks, protocol_level_tokens};
use concordium_base::protocol_level_tokens::{Operation, Operations, OperationsPayload};
use concordium_base::protocol_level_tokens::{TokenId, TokenOperation};
use concordium_base::transactions;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateResult;
use plt_block_state::persistent::chain_parameters::p11::PersistentChainParametersP11;
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
    chain_parameters: &PersistentChainParametersP11,
) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
    let mut execution = TransactionExecution::new(transaction_context, sender_account);

    let outcome = match payload {
        Payload::TokenUpdate {
            payload: concordium_base::transactions::TokenUpdatePayload::Scoped(payload),
        } => protocol_level_tokens::p11::execute_token_update_transaction(
            context,
            &mut execution,
            block_state,
            payload,
        )?,
        Payload::TokenUpdate {
            payload: concordium_base::transactions::TokenUpdatePayload::Unscoped(payload),
        } => execute_operations(
            context,
            &mut execution,
            block_state,
            payload,
            chain_parameters,
        )?,
        _ => return Err(TransactionExecutionError::UnexpectedPayload),
    };

    Ok(TransactionExecutionSummary {
        outcome,
        energy_used: execution.energy_used(),
    })
}

/// Execute unscoped Token Update operations.
fn execute_operations<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    payload: OperationsPayload,
    chain_parameters: &PersistentChainParametersP11,
) -> BlockStateResult<TransactionOutcome> {
    // Charge energy
    if let Err(err) =
        transaction_execution.tick_energy(transactions::cost::PLT_OPERATIONS_TRANSACTIONS)
    {
        let _: OutOfEnergyError = err; // assert type of error
        return Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        ));
    }

    let mut events = Vec::new();

    // Decode operations
    let operations: Vec<Operation> = match utils::cbor_decode::<Operations>(payload.operations) {
        Ok(payload) => payload.operations,
        Err(_) => {
            return Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::SerializationFailure,
            ));
        }
    };

    // Execute operations
    for (index, operation) in operations.into_iter().enumerate() {
        match OperationKind::from(operation) {
            OperationKind::Token(token_id, token_operation) => {
                match protocol_level_tokens::p11::execute_token_update_operation(
                    context,
                    transaction_execution,
                    block_state,
                    index,
                    &token_id,
                    token_operation,
                    &mut events,
                )
                .nest()?
                {
                    Ok(()) => (),
                    Err(reject_reason) => {
                        return Ok(TransactionOutcome::Rejected(reject_reason));
                    }
                }
            }
            OperationKind::Lock(lock_operation) => {
                match protocol_level_locks::p11::execute_lock_operation(
                    context,
                    transaction_execution,
                    block_state,
                    chain_parameters.max_lock_duration,
                    index,
                    lock_operation,
                    &mut events,
                )
                .nest()?
                {
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

/// Execute a chain update modifying P11 external chain parameters.
///
/// # Arguments
///
/// - `chain_parameters` External chain parameters to update.
/// - `payload` The chain update payload to execute.
///
/// # Errors
///
/// Returns [`ChainUpdateExecutionError::UnexpectedPayload`] if the payload is
/// not a P11 external chain-parameter update.
pub fn execute_chain_parameters_update(
    chain_parameters: &mut PersistentChainParametersP11,
    payload: UpdatePayload,
) -> Result<(), ChainUpdateExecutionError> {
    match payload {
        UpdatePayload::MaxLockDuration(duration) => {
            chain_parameters.max_lock_duration = duration;
            Ok(())
        }
        _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
    }
}

/// A discriminated version of [`Operation`] for the purpose of
/// dispatching to the appropriate operation handler.
#[derive(PartialEq, Debug, Clone)]
enum OperationKind {
    /// A [`TokenOperation`] for a specific [`TokenId`].
    Token(TokenId, TokenOperation),
    /// An internal lock operation.
    Lock(LockOperation),
}

impl From<Operation> for OperationKind {
    fn from(value: Operation) -> Self {
        match value {
            Operation::TokenTransfer(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Transfer(details))
            }
            Operation::TokenMint(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Mint(details))
            }
            Operation::TokenBurn(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Burn(details))
            }
            Operation::TokenAddAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AddAllowList(details))
            }
            Operation::TokenRemoveAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RemoveAllowList(details))
            }
            Operation::TokenAddDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AddDenyList(details))
            }
            Operation::TokenRemoveDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RemoveDenyList(details))
            }
            Operation::TokenPause(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Pause(details))
            }
            Operation::TokenUnpause(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::Unpause(details))
            }
            Operation::TokenAssignAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::AssignAdminRoles(details))
            }
            Operation::TokenRevokeAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::RevokeAdminRoles(details))
            }
            Operation::TokenUpdateMetadata(details) => {
                let (token_id, details) = details.into();
                Self::Token(token_id, TokenOperation::UpdateMetadata(details))
            }
            Operation::LockFund(details) => Self::Lock(LockOperation::Fund(details)),
            Operation::LockSend(details) => Self::Lock(LockOperation::Send(details)),
            Operation::LockRelease(details) => Self::Lock(LockOperation::Release(details)),
            Operation::LockCreate(details) => Self::Lock(LockOperation::Create(details)),
            Operation::LockCancel(details) => Self::Lock(LockOperation::Cancel(details)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use concordium_base::common;
    use concordium_base::contracts_common::Duration;
    use concordium_base::transactions::Memo;

    #[test]
    fn execute_max_lock_duration_update() {
        let mut chain_parameters = PersistentChainParametersP11 {
            max_lock_duration: Duration::from_millis(42),
        };
        execute_chain_parameters_update(
            &mut chain_parameters,
            UpdatePayload::MaxLockDuration(Duration::from_millis(123)),
        )
        .expect("max lock duration update should succeed");

        assert_eq!(
            chain_parameters.max_lock_duration,
            Duration::from_millis(123)
        );
    }

    #[test]
    fn test_unscoped_operation_token_operation_conversion() {
        use concordium_base::protocol_level_tokens::operations;
        use concordium_base::protocol_level_tokens::*;
        // For each unscoped Token Update operation variant:
        // - construct an unscoped Token Update operation with some test data
        // - construct the corresponding token operation with the same test data
        // - convert in each direction and check that the result matches the original
        // - construct the unscoped Token Update operation using the `operations` helper function
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
        let unscoped_transfer = Operation::TokenTransfer(TokenTransferWithId {
            token: token_id.clone(),
            amount,
            recipient: account.clone(),
            memo: memo.clone(),
        });
        assert_eq!(
            operations::transfer_tokens_with_memo(
                token_id.clone(),
                ADDRESS,
                amount,
                cbor_memo.clone()
            ),
            unscoped_transfer
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_transfer.clone())),
            unscoped_transfer
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_transfer),
            unscoped_transfer.into(),
        );

        let token_mint = TokenOperation::Mint(TokenSupplyUpdateDetails { amount });
        let unscoped_mint = Operation::TokenMint(TokenSupplyUpdateDetailsWithId {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(
            operations::mint_tokens(token_id.clone(), amount),
            unscoped_mint
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_mint.clone())),
            unscoped_mint
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_mint),
            unscoped_mint.into(),
        );

        let token_burn = TokenOperation::Burn(TokenSupplyUpdateDetails { amount });
        let unscoped_burn = Operation::TokenBurn(TokenSupplyUpdateDetailsWithId {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(
            operations::burn_tokens(token_id.clone(), amount),
            unscoped_burn
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_burn.clone())),
            unscoped_burn
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_burn),
            unscoped_burn.into(),
        );

        let token_add_allow_list = TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let unscoped_add_allow_list = Operation::TokenAddAllowList(TokenListUpdateDetailsWithId {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            operations::add_token_allow_list(token_id.clone(), ADDRESS),
            unscoped_add_allow_list
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_add_allow_list.clone())),
            unscoped_add_allow_list
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_add_allow_list),
            unscoped_add_allow_list.into(),
        );

        let token_remove_allow_list = TokenOperation::RemoveAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let unscoped_remove_allow_list =
            Operation::TokenRemoveAllowList(TokenListUpdateDetailsWithId {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            operations::remove_token_allow_list(token_id.clone(), ADDRESS),
            unscoped_remove_allow_list
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_remove_allow_list.clone())),
            unscoped_remove_allow_list
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_remove_allow_list),
            unscoped_remove_allow_list.into(),
        );

        let token_add_deny_list = TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let unscoped_add_deny_list = Operation::TokenAddDenyList(TokenListUpdateDetailsWithId {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            operations::add_token_deny_list(token_id.clone(), ADDRESS),
            unscoped_add_deny_list
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_add_deny_list.clone())),
            unscoped_add_deny_list
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_add_deny_list),
            unscoped_add_deny_list.into(),
        );

        let token_remove_deny_list = TokenOperation::RemoveDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let unscoped_remove_deny_list =
            Operation::TokenRemoveDenyList(TokenListUpdateDetailsWithId {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            operations::remove_token_deny_list(token_id.clone(), ADDRESS),
            unscoped_remove_deny_list
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_remove_deny_list.clone())),
            unscoped_remove_deny_list
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_remove_deny_list),
            unscoped_remove_deny_list.into(),
        );

        let token_pause = TokenOperation::Pause(TokenPauseDetails {});
        let unscoped_pause = Operation::TokenPause(TokenPauseDetailsWithId {
            token: token_id.clone(),
        });
        assert_eq!(operations::pause_token(token_id.clone()), unscoped_pause);
        assert_eq!(
            Operation::from((token_id.clone(), token_pause.clone())),
            unscoped_pause
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_pause),
            unscoped_pause.into(),
        );

        let token_unpause = TokenOperation::Unpause(TokenPauseDetails {});
        let unscoped_unpause = Operation::TokenUnpause(TokenPauseDetailsWithId {
            token: token_id.clone(),
        });
        assert_eq!(
            operations::unpause_token(token_id.clone()),
            unscoped_unpause
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_unpause.clone())),
            unscoped_unpause
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_unpause),
            unscoped_unpause.into(),
        );

        let assign_roles = vec![TokenAdminRole::Mint, TokenAdminRole::Pause];
        let token_assign_admin_roles =
            TokenOperation::AssignAdminRoles(TokenUpdateAdminRolesDetails {
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        let unscoped_assign_admin_roles =
            Operation::TokenAssignAdminRoles(TokenUpdateAdminRolesDetailsWithId {
                token: token_id.clone(),
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            operations::assign_token_admin_roles(token_id.clone(), ADDRESS, assign_roles.clone()),
            unscoped_assign_admin_roles
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_assign_admin_roles.clone())),
            unscoped_assign_admin_roles
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_assign_admin_roles),
            unscoped_assign_admin_roles.into(),
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
        let unscoped_revoke_admin_roles =
            Operation::TokenRevokeAdminRoles(TokenUpdateAdminRolesDetailsWithId {
                token: token_id.clone(),
                roles: revoke_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            operations::revoke_token_admin_roles(token_id.clone(), ADDRESS, revoke_roles.clone()),
            unscoped_revoke_admin_roles
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_revoke_admin_roles.clone())),
            unscoped_revoke_admin_roles
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_revoke_admin_roles),
            unscoped_revoke_admin_roles.into(),
        );

        let metadata_url = TokenMetadataUrlDetails {
            url: "https://example.com/metadata.json".to_string(),
            checksum_sha_256: Some([0u8; 32].into()),
        };
        let token_update_metadata = TokenOperation::UpdateMetadata(metadata_url.clone());
        let unscoped_update_metadata =
            Operation::TokenUpdateMetadata(TokenMetadataUrlDetailsWithId {
                token: token_id.clone(),
                url: metadata_url.url.clone(),
                checksum_sha_256: metadata_url.checksum_sha_256,
            });
        assert_eq!(
            operations::update_token_metadata(token_id.clone(), metadata_url.clone()),
            unscoped_update_metadata
        );
        assert_eq!(
            Operation::from((token_id.clone(), token_update_metadata.clone())),
            unscoped_update_metadata
        );
        assert_eq!(
            OperationKind::Token(token_id.clone(), token_update_metadata),
            unscoped_update_metadata.into(),
        );
    }
}
