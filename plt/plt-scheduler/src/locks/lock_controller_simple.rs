use concordium_base::protocol_level_tokens::CborHolderAccount;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::locks::LockControllerSimpleV0;

use crate::locks::lock_controller::{LockController, LockControllerRejectReason, LockOperation};
use crate::queries::QueryLockError;

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
    ) -> Result<concordium_base::protocol_level_locks::LockController, QueryLockError> {
        let grants = self
            .grants
            .iter()
            .map(|grant| {
                let with_addr = bsq.account_by_index(grant.account).map_err(|_| {
                    QueryLockError::StateInvariantViolation(format!(
                        "controller grant account index {} recorded in lock configuration \
                         does not exist",
                        grant.account
                    ))
                })?;
                Ok(
                    concordium_base::protocol_level_locks::LockControllerSimpleV0Grant {
                        account: CborHolderAccount::from(with_addr.canonical_account_address),
                        roles: grant.roles.clone(),
                    },
                )
            })
            .collect::<Result<_, QueryLockError>>()?;
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
}
