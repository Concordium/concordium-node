use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;
use concordium_base::protocol_level_tokens::CborHolderAccount;
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockControllerSimpleV0, LockControllerSimpleV0Grant,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

use crate::locks::lock_controller::{LockController, LockOperation};
use crate::scheduler::TransactionFailure;

impl LockController for LockControllerSimpleV0 {
    fn validate_operation(
        &self,
        sender_address: AccountAddress,
        sender: &Account,
        operation: &LockOperation,
    ) -> Result<(), TransactionRejectReason> {
        match operation {
            LockOperation::Fund(fund_details) => {
                if !self.has_role(
                    sender.account_index(),
                    LockControllerSimpleV0Capability::Fund,
                ) {
                    return Err(TransactionRejectReason::LockFundNotAuthorized(
                        fund_details.lock.clone(),
                        sender_address,
                    ));
                }
                if !self.tokens.contains(&fund_details.token) {
                    return Err(TransactionRejectReason::LockTokenNotPermitted(
                        fund_details.lock.clone(),
                        fund_details.token.clone(),
                    ));
                }
            }
            LockOperation::Send(send_details) => {
                if !self.has_role(
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
                if !self.has_role(
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
                if !self.has_role(
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

    fn to_cbor_controller<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<concordium_base::protocol_level_locks::LockController> {
        let grants = self
            .grants
            .iter()
            .map(|grant| {
                let with_addr = context.account_by_index(grant.account).map_err(
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
                        roles: grant.roles.clone(),
                    },
                )
            })
            .collect::<Result<_, _>>()?;
        Ok(
            concordium_base::protocol_level_locks::LockController::SimpleV0(
                concordium_base::protocol_level_locks::LockControllerSimpleV0 {
                    grants,
                    tokens: self.tokens.clone(),
                    keep_alive: self.keep_alive,
                    memo: self.memo.clone(),
                },
            ),
        )
    }

    type ControllerConfig = concordium_base::protocol_level_locks::LockControllerSimpleV0;

    fn new<C: EntityContextTypes>(
        context: &EntityContext<C>,
        block_state: &BlockStateP11,
        config: Self::ControllerConfig,
    ) -> Result<Self, TransactionFailure>
    where
        Self: Sized,
    {
        let grants = config
            .grants
            .into_iter()
            .map(|grant| {
                let account = context.account_by_address(&grant.account.address).map_err(
                    |_err: AccountNotFoundByAddressError| {
                        TransactionRejectReason::InvalidAccountReference(grant.account.address)
                    },
                )?;

                Ok(LockControllerSimpleV0Grant {
                    account: account.account_index(),
                    roles: grant.roles,
                })
            })
            .collect::<Result<_, TransactionFailure>>()?;

        let tokens = config
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
            .collect::<Result<_, TransactionFailure>>()?;

        Ok(LockControllerSimpleV0 {
            grants,
            tokens,
            keep_alive: config.keep_alive,
            memo: config.memo,
        })
    }
}
