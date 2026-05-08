use concordium_base::protocol_level_tokens::CborHolderAccount;
use plt_block_state::block_state_interface::{AccountNotFoundByIndexError, BlockStateQuery};
use plt_scheduler_types::types::locks::{LockControllerSimpleV0, LockControllerSimpleV0Grant};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

use crate::locks::lock_controller::{LockController, LockControllerRejectReason, LockOperation};
use crate::scheduler::helpers::{lookup_account_index, lookup_token_id};

impl LockController for LockControllerSimpleV0 {
    // TODO: implemented as part of COR-2305
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        _bsq: &BSQ,
        _sender: &BSQ::Account,
        _operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        todo!()
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
