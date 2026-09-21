//! Protocol-level lock configuration conversion and operation validation.

use crate::failure::ResultWithBlockStateFailure;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::{
    LockConfig as CborLockConfig, LockConfigSimpleV0 as CborLockConfigSimpleV0,
    LockControllerSimpleV0Capability, LockRecipients,
};
use concordium_base::protocol_level_tokens::CborHolderAccount;
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaLockCancelDetails, MetaLockFundDetails, MetaLockReleaseDetails, MetaLockSendDetails,
};
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Grant,
    LockRecipients as BlockStateLockRecipients,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Convert persistent recipients to their external CBOR representation.
///
/// Returns an invariant failure if a persisted account index does not exist.
pub fn get_recipients<C: EntityContextTypes>(
    context: &EntityContext<C>,
    config: &LockConfigSimpleV0,
) -> BlockStateResult<LockRecipients> {
    match &config.recipients {
        BlockStateLockRecipients::Any => Ok(LockRecipients::Any),
        BlockStateLockRecipients::Limited(recipients) => recipients
            .iter()
            .map(|account_index| {
                let account = context.account_by_index(*account_index).map_err(
                    |_: AccountNotFoundByIndexError| {
                        BlockStateFailure::Invariant(format!(
                            "account index {account_index} in lock recipients does not exist"
                        ))
                    },
                )?;
                Ok(CborHolderAccount::from(account.canonical_account_address))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(LockRecipients::Limited),
    }
}

/// Resolve external CBOR recipients to persistent account indices.
///
/// Returns an invalid-account rejection for an unknown address or a serialization rejection when
/// the recipient list exceeds the persistent size bound.
pub fn from_cbor_recipients<C: EntityContextTypes>(
    context: &EntityContext<C>,
    recipients: LockRecipients,
) -> Result<BlockStateLockRecipients, TransactionRejectReason> {
    match recipients {
        LockRecipients::Any => Ok(BlockStateLockRecipients::Any),
        LockRecipients::Limited(recipients) => {
            let recipients = recipients
                .into_iter()
                .map(|recipient| {
                    context
                        .account_by_address(&recipient.address)
                        .map(|account| account.account_index())
                        .map_err(|_: AccountNotFoundByAddressError| {
                            TransactionRejectReason::InvalidAccountReference(recipient.address)
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;
            BlockStateLockRecipients::try_from(recipients)
                .map_err(|_| TransactionRejectReason::SerializationFailure)
        }
    }
}

/// Lock operation requiring authorization against a lock configuration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {
    /// Fund a lock with a token amount.
    Fund(MetaLockFundDetails),
    /// Send locked funds to an eligible recipient.
    Send(MetaLockSendDetails),
    /// Release locked funds to their source account.
    Release(MetaLockReleaseDetails),
    /// Cancel a lock and return all remaining funds.
    Cancel(MetaLockCancelDetails),
}

/// Validate that the sender may perform an operation under the lock configuration.
///
/// Returns the operation-specific authorization rejection, or a token-not-permitted rejection
/// when a fund operation uses a token outside the configuration.
pub fn validate_operation(
    config: &LockConfig,
    sender_address: AccountAddress,
    sender: &Account,
    operation: &LockOperation,
) -> Result<(), TransactionRejectReason> {
    let LockConfig::SimpleV0(config) = config;
    let (role, lock) = match operation {
        LockOperation::Fund(details) => (LockControllerSimpleV0Capability::Fund, &details.lock),
        LockOperation::Send(details) => (LockControllerSimpleV0Capability::Send, &details.lock),
        LockOperation::Release(details) => {
            (LockControllerSimpleV0Capability::Release, &details.lock)
        }
        LockOperation::Cancel(details) => (LockControllerSimpleV0Capability::Cancel, &details.lock),
    };
    if !config.has_role(sender.account_index(), role) {
        return Err(match operation {
            LockOperation::Fund(_) => {
                TransactionRejectReason::LockFundNotAuthorized(lock.clone(), sender_address)
            }
            LockOperation::Send(_) => {
                TransactionRejectReason::LockSendNotAuthorized(lock.clone(), sender_address)
            }
            LockOperation::Release(_) => {
                TransactionRejectReason::LockReleaseNotAuthorized(lock.clone(), sender_address)
            }
            LockOperation::Cancel(_) => {
                TransactionRejectReason::LockCancelNotAuthorized(lock.clone(), sender_address)
            }
        });
    }
    if let LockOperation::Fund(details) = operation
        && !config.tokens().contains(&details.token)
    {
        return Err(TransactionRejectReason::LockTokenNotPermitted(
            details.lock.clone(),
            details.token.clone(),
        ));
    }
    Ok(())
}

/// Convert an external CBOR lock configuration to persistent state.
///
/// Resolves account addresses and token identifiers through the supplied context and block state.
/// Returns a transaction rejection when a referenced account or token does not exist or when a
/// persistent collection exceeds its serialization bound.
pub fn from_cbor_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    cbor_config: CborLockConfig,
) -> ResultWithBlockStateFailure<LockConfig, TransactionRejectReason> {
    let CborLockConfig::SimpleV0(cbor_config) = cbor_config;
    let grants = cbor_config
        .grants
        .into_iter()
        .map(|grant| {
            let account = context.account_by_address(&grant.account.address).map_err(
                |_: AccountNotFoundByAddressError| {
                    TransactionRejectReason::InvalidAccountReference(grant.account.address)
                },
            )?;
            Ok(LockControllerSimpleV0Grant::new(
                account.account_index(),
                grant.roles,
            ))
        })
        .collect::<ResultWithBlockStateFailure<_, _>>()?;
    let tokens = cbor_config
        .tokens
        .into_iter()
        .map(|token_id| {
            let token = block_state.token_by_id(context, &token_id)?.map_err(
                |_: TokenNotFoundByIdError| {
                    TransactionRejectReason::NonExistentTokenId(token_id.clone())
                },
            )?;
            Ok(token.token_p9_base.token_configuration(context)?.token_id)
        })
        .collect::<ResultWithBlockStateFailure<_, _>>()?;
    let recipients = from_cbor_recipients(context, cbor_config.recipients)?;
    Ok(LockConfig::SimpleV0(
        LockConfigSimpleV0::new(
            recipients,
            cbor_config.expiry,
            grants,
            tokens,
            cbor_config.keep_alive,
            cbor_config.memo,
            cbor_config.metadata,
        )
        .map_err(|_| TransactionRejectReason::SerializationFailure)?,
    ))
}

/// Convert a persistent lock configuration to its external CBOR representation.
///
/// Returns an invariant failure if a persisted grant or recipient references a missing account.
pub fn to_cbor_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    config: &LockConfig,
) -> BlockStateResult<CborLockConfig> {
    let LockConfig::SimpleV0(config) = config;
    let grants = config
        .grants()
        .iter()
        .map(|grant| {
            let account = context.account_by_index(grant.account()).map_err(
                |err: AccountNotFoundByIndexError| {
                    BlockStateFailure::Invariant(format!(
                        "Account persisted in lock grants not found: {err}"
                    ))
                },
            )?;
            Ok(
                concordium_base::protocol_level_locks::LockControllerSimpleV0Grant {
                    account: CborHolderAccount::from(account.canonical_account_address),
                    roles: grant.roles().to_vec(),
                },
            )
        })
        .collect::<Result<_, _>>()?;
    Ok(CborLockConfig::SimpleV0(CborLockConfigSimpleV0 {
        recipients: get_recipients(context, config)?,
        expiry: config.expiry,
        grants,
        tokens: config.tokens().to_vec(),
        keep_alive: config.keep_alive,
        memo: config.memo.clone(),
        metadata: config.metadata.clone(),
    }))
}
