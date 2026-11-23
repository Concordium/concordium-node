//! Protocol-level lock configuration conversion and operation validation.

use crate::failure::ResultWithBlockStateFailure;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::{
    LockCapability, LockConfig as CborLockConfig, LockConfigVersion, LockCreateConfig,
    LockRecipients, LockRole as CborLockRole, LockVariant, SimpleLockConfigCapability,
};
use concordium_base::protocol_level_tokens::CborHolderAccount;
use concordium_base::protocol_level_tokens::{
    LockCancelDetails, LockCreateDetails, LockFundDetails, LockReleaseDetails, LockSendDetails,
};
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfig, LockConfigSimpleV0, LockRecipients as BlockStateLockRecipients, LockRole,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Convert persistent recipients to canonical account addresses.
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

/// Resolve every recipient reference before normalizing account identities.
/// Rejects unknown addresses or a normalized collection exceeding its persistent bound.
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

/// Internal lock operation dispatched by the scheduler.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {
    /// Create a lock; no existing configuration authorization is required.
    Create(LockCreateDetails),
    /// Fund a lock with a token amount.
    Fund(LockFundDetails),
    /// Send locked funds to an eligible recipient.
    Send(LockSendDetails),
    /// Release locked funds to their source account.
    Release(LockReleaseDetails),
    /// Cancel a lock and return all remaining funds.
    Cancel(LockCancelDetails),
}

/// Check effective operation permissions independently of recipient eligibility.
/// Returns the operation-specific authorization rejection, or a token-not-permitted rejection
/// when funding with a token outside the configuration.
pub fn validate_operation(
    config: &LockConfig,
    sender_address: AccountAddress,
    sender: &Account,
    operation: &LockOperation,
) -> Result<(), TransactionRejectReason> {
    let LockConfig::SimpleV0(config) = config;
    let (role, lock) = match operation {
        LockOperation::Create(_) => return Ok(()),
        LockOperation::Fund(details) => (LockCapability::Fund, &details.lock),
        LockOperation::Send(details) => (LockCapability::Send, &details.lock),
        LockOperation::Release(details) => (LockCapability::Release, &details.lock),
        LockOperation::Cancel(details) => (LockCapability::Cancel, &details.lock),
    };
    if !config.has_role(sender.account_index(), role) {
        return Err(match operation {
            LockOperation::Create(_) => return Ok(()),
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

/// Resolve creation input and normalize effective roles, recipients, and registered tokens.
/// Rejects unknown references and normalized collection overflow.
pub fn from_cbor_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    cbor_config: LockCreateConfig,
) -> ResultWithBlockStateFailure<LockConfig, TransactionRejectReason> {
    let LockCreateConfig::SimpleV0(cbor_config) = cbor_config;
    let mut roles = Vec::with_capacity(cbor_config.holders.len() + cbor_config.roles.len());
    for holder in cbor_config.holders {
        let account = context.account_by_address(&holder.address).map_err(
            |_: AccountNotFoundByAddressError| {
                TransactionRejectReason::InvalidAccountReference(holder.address)
            },
        )?;
        roles.push(LockRole::new(
            account.account_index(),
            vec![LockCapability::Fund],
        ));
    }
    for role in cbor_config.roles {
        let account = context.account_by_address(&role.account.address).map_err(
            |_: AccountNotFoundByAddressError| {
                TransactionRejectReason::InvalidAccountReference(role.account.address)
            },
        )?;
        roles.push(LockRole::new(
            account.account_index(),
            role.roles
                .into_iter()
                .map(|capability| match capability {
                    SimpleLockConfigCapability::Release => LockCapability::Release,
                    SimpleLockConfigCapability::Send => LockCapability::Send,
                    SimpleLockConfigCapability::Cancel => LockCapability::Cancel,
                })
                .collect(),
        ));
    }
    let tokens = cbor_config
        .tokens
        .into_iter()
        .map(|token_id| {
            let token = block_state.token_by_id(context, &token_id)?.map_err(
                |_: TokenNotFoundByIdError| TransactionRejectReason::NonExistentTokenId(token_id),
            )?;
            Ok(token.token_p9_base.token_configuration(context)?.token_id)
        })
        .collect::<ResultWithBlockStateFailure<_, _>>()?;
    let recipients = from_cbor_recipients(context, cbor_config.recipients)?;
    Ok(LockConfig::SimpleV0(
        LockConfigSimpleV0::new(
            recipients,
            cbor_config.expiry,
            roles,
            tokens,
            cbor_config.metadata,
        )
        .map_err(|_| TransactionRejectReason::SerializationFailure)?,
    ))
}

/// Reconstruct the flat output configuration used by creation events and both lock queries.
/// Returns an invariant failure if a persisted role or recipient references a missing account.
pub fn to_cbor_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    config: &LockConfig,
) -> BlockStateResult<CborLockConfig> {
    let LockConfig::SimpleV0(config) = config;
    let mut holders = Vec::new();
    let mut roles = Vec::new();
    for role in config.roles() {
        let account = context.account_by_index(role.account()).map_err(
            |err: AccountNotFoundByIndexError| {
                BlockStateFailure::Invariant(format!(
                    "Account persisted in lock roles not found: {err}"
                ))
            },
        )?;
        let capabilities = role.roles();
        if capabilities.is_empty() {
            continue;
        }
        let account = CborHolderAccount::from(account.canonical_account_address);
        if capabilities.contains(&LockCapability::Fund) {
            holders.push(account.clone());
        }
        roles.push(CborLockRole {
            account,
            roles: capabilities,
        });
    }
    Ok(CborLockConfig {
        version: LockConfigVersion::V0,
        variant: LockVariant::SimpleV0,
        holders,
        roles,
        recipients: get_recipients(context, config)?,
        expiry: config.expiry,
        tokens: config.tokens().to_vec(),
        metadata: config.metadata.clone(),
    })
}
