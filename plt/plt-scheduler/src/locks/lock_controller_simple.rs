use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;
use concordium_base::protocol_level_tokens::{CborHolderAccount, TokenId};
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockControllerSimpleV0, LockControllerSimpleV0Grant,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

use crate::locks::lock_controller::{LockController, LockOperation};

impl LockController for LockControllerSimpleV0 {
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        operation: &LockOperation,
    ) -> bool {
        let sender_index = bsq.account_index(sender);
        let role = match operation {
            LockOperation::Fund(_) => LockControllerSimpleV0Capability::Fund,
            LockOperation::Send(_) => LockControllerSimpleV0Capability::Send,
            LockOperation::Return(_) => LockControllerSimpleV0Capability::Return,
            LockOperation::Cancel(_) => LockControllerSimpleV0Capability::Cancel,
        };
        self.has_role(sender_index, role)
    }

    fn to_cbor_controller<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
    ) -> Result<concordium_base::protocol_level_locks::LockController, AccountNotFoundByIndexError>
    {
        let grants = self
            .grants
            .iter()
            .map(|grant| {
                let with_addr = bsq.account_by_index(grant.account)?;
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

    fn new<BSQ: BlockStateQuery>(
        bsq: &BSQ,
        config: Self::ControllerConfig,
    ) -> Result<Self, TransactionRejectReason>
    where
        Self: Sized,
    {
        let grants = config
            .grants
            .into_iter()
            .map(|grant| {
                let account_index = lookup_account_index(bsq, grant.account)?;
                Ok(LockControllerSimpleV0Grant {
                    account: account_index,
                    roles: grant.roles,
                })
            })
            .collect::<Result<_, _>>()?;
        let tokens = config
            .tokens
            .into_iter()
            .map(|token_id| lookup_token_id(bsq, token_id))
            .collect::<Result<_, _>>()?;

        Ok(LockControllerSimpleV0 {
            grants,
            tokens,
            keep_alive: config.keep_alive,
            memo: config.memo,
        })
    }
}

/// Look up the account index for a [`CborHolderAccount`]. If the account does
/// not exist, a [`TransactionRejectReason::InvalidAccountReference`] is
/// returned.
pub fn lookup_account_index<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    holder: CborHolderAccount,
) -> Result<AccountIndex, TransactionRejectReason> {
    match block_state.account_by_address(&holder.address) {
        Ok(account) => Ok(block_state.account_index(&account)),
        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
            holder.address,
        )),
    }
}

/// Look up the token ID in the block state. If the token ID does not exist, a
/// [`TransactionRejectReason::NonExistentTokenId`] is returned.
/// Otherwise, this returns the canonical representation of the token ID.
///
/// TODO: Consider returning token index instead.
pub fn lookup_token_id<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    token_id: TokenId,
) -> Result<TokenId, TransactionRejectReason> {
    match block_state.token_by_id(&token_id) {
        Ok(token) => Ok(block_state.token_configuration(&token).token_id),
        Err(TokenNotFoundByIdError(_)) => {
            Err(TransactionRejectReason::NonExistentTokenId(token_id))
        }
    }
}
