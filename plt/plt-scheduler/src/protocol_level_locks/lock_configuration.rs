use concordium_base::{
    protocol_level_locks::{LockConfig, LockRecipients},
    protocol_level_tokens::CborHolderAccount,
};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockRecipients as BlockStateLockRecipients,
};

/// Get the recipients for a lock configuration, resolving [`AccountIndex`]es
/// to [`CborHolderAccount`]s.
pub fn get_recipients<C: EntityContextTypes>(
    context: &EntityContext<C>,
    configuration: &LockConfiguration,
) -> BlockStateResult<LockRecipients> {
    match &configuration.recipients {
        BlockStateLockRecipients::Any => Ok(LockRecipients::Any),
        BlockStateLockRecipients::Limited(recipients) => {
            let recipients = recipients
                .iter()
                .map(|account_index| {
                    let with_addr = context.account_by_index(*account_index).map_err(
                        |_err: AccountNotFoundByIndexError| {
                            BlockStateFailure::Invariant(format!(
                                "account index {} in lock recipients does not exist",
                                account_index
                            ))
                        },
                    )?;
                    Ok(CborHolderAccount::from(with_addr.canonical_account_address))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(LockRecipients::Limited(recipients))
        }
    }
}

/// Get the lock configuration as a CBOR-representable [`LockConfig`] with
/// resolved account addresses.
pub fn get_lock_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    configuration: &LockConfiguration,
) -> BlockStateResult<LockConfig> {
    let recipients = get_recipients(context, configuration)?;
    let controller =
        super::lock_controller::to_cbor_controller(context, &configuration.controller)?;

    Ok(LockConfig {
        recipients,
        expiry: configuration.expiry,
        controller,
        metadata: configuration.metadata.clone(),
    })
}
