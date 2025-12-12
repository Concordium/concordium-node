//! Implementation of the protocol-level token module.car
use crate::host_interface::*;
use anyhow::anyhow;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::Serial;
use concordium_base::common::cbor::value::Value;
use concordium_base::common::cbor::{
    CborSerializationError, SerializationOptions, UnknownMapKeys, cbor_decode_with_options,
    cbor_encode,
};
use concordium_base::common::upward::{CborUpward, Upward};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, RawCbor, TokenAmount,
    TokenModuleInitializationParameters, TokenModuleRejectReasonType, TokenOperation,
    TokenOperations,
};
use concordium_base::transactions::Memo;
use itertools::Itertools;

/// Extension trait for `HostOperations` to provide convenience wrappers for
/// module state access and updating.
trait HostOperationsExt: HostOperations {
    /// Set or clear a value in the token module state at the corresponding key.
    fn set_module_state<'a>(
        &mut self,
        key: impl IntoIterator<Item = &'a u8>,
        value: Option<StateValue>,
    ) -> Result<(), LockedStateKeyError> {
        self.set_token_state(module_state_key(key), value)?;
        Ok(())
    }

    /// Get a value in the token module state at the corresponding key.
    fn get_module_state<'a>(&self, key: impl IntoIterator<Item = &'a u8>) -> Option<StateValue> {
        self.get_token_state(module_state_key(key))
    }

    /// Set or clear a value in the account state at the corresponding key.
    fn set_account_state<'a>(
        &mut self,
        account_index: AccountIndex,
        key: impl IntoIterator<Item = &'a u8>,
        value: Option<StateValue>,
    ) -> Result<(), LockedStateKeyError> {
        self.set_token_state(account_state_key(account_index, key), value)?;
        Ok(())
    }

    /// Get a value in the account state at the corresponding key.
    fn get_account_state<'a>(
        &self,
        account_index: AccountIndex,
        key: impl IntoIterator<Item = &'a u8>,
    ) -> Option<StateValue> {
        self.get_token_state(account_state_key(account_index, key))
    }
}

impl<T: HostOperations> HostOperationsExt for T {}

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();
/// Little-endian prefix used to distinguish account state keys.
const ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();

/// Construct a [`StateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
fn module_state_key<'a>(key: impl IntoIterator<Item = &'a u8>) -> StateKey {
    let iter = key.into_iter();
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + iter.size_hint().0);
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend(iter);
    module_key
}

/// Construct a [`StateKey`] for an account key. This prefixes the key with
/// a tag and the account index to distinguish it from other keys.
fn account_state_key<'a>(
    account_index: AccountIndex,
    key: impl IntoIterator<Item = &'a u8>,
) -> StateKey {
    let iter = key.into_iter();
    let mut buf = Vec::with_capacity(10 + iter.size_hint().0);
    buf.extend_from_slice(&ACCOUNT_STATE_PREFIX);
    account_index.serial(&mut buf);
    buf.extend(iter);
    buf
}

/// Represents the reasons why [`initialize_token`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum InitError {
    #[error("Token initialization parameters could not be deserialized: {0}")]
    DeserializationFailure(anyhow::Error),
    #[error("{0}")]
    LockedStateKey(#[from] LockedStateKeyError),
    #[error("The given governance account does not exist: {0}")]
    GovernanceAccountDoesNotExist(AccountAddress),
    #[error("The initial mint amount was not valid: {0}")]
    InvalidMintAmount(anyhow::Error),
}

impl From<CborSerializationError> for InitError {
    fn from(value: CborSerializationError) -> Self {
        Self::DeserializationFailure(value.into())
    }
}

/// Represents the reasons why [`execute_token_update_transaction`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum UpdateError {}
/// Represents the reasons why a query to the token module can fail.
#[derive(Debug)]
pub enum QueryError {}

/// The context for a token-holder or token-governance transaction.
#[derive(Debug)]
pub struct TransactionContext<Account> {
    /// The sender account object.
    pub sender: Account,
    /// The sender account address. This is the account alias that is used by the transaction itself.
    pub sender_address: AccountAddress,
}

#[derive(Debug, thiserror::Error)]
pub enum TokenAmountError {
    #[error("Token amount decimals mismatch: expected {expected}, found {found}")]
    DecimalsMismatch { expected: u8, found: u8 },
}

fn to_token_raw_amount(
    amount: TokenAmount,
    actual_decimals: u8,
) -> Result<TokenRawAmount, TokenAmountError> {
    let decimals = amount.decimals();
    if decimals != actual_decimals {
        return Err(TokenAmountError::DecimalsMismatch {
            expected: actual_decimals,
            found: decimals,
        });
    }
    Ok(amount.value())
}

const STATE_KEY_NAME: &[u8] = b"name";
const STATE_KEY_METADATA: &[u8] = b"metadata";
const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
const STATE_KEY_MINTABLE: &[u8] = b"mintable";
const STATE_KEY_BURNABLE: &[u8] = b"burnable";
const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";
const STATE_KEY_PAUSED: &[u8] = b"paused";

/// Get the account index of the governance account. Returns `None` if the
/// governance account is not set in the token module state, or if it cannot
/// be deserialized.
fn get_governance_account(host: &impl HostOperations) -> Option<AccountIndex> {
    let governance_account_bytes = host.get_module_state(STATE_KEY_GOVERNANCE_ACCOUNT)?;
    let governance_account = u64::from_be_bytes(governance_account_bytes.try_into().ok()?);
    Some(AccountIndex {
        index: governance_account,
    })
}

/// Get whether the balance-affecting operations on the token are currently
/// paused.
fn get_paused(host: &impl HostOperations) -> bool {
    host.get_module_state(STATE_KEY_PAUSED).is_some()
}

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token(
    host: &mut impl HostOperations,
    token_parameter: Parameter,
) -> Result<(), InitError> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    let parameter: TokenModuleInitializationParameters =
        cbor_decode_with_options(token_parameter, decode_options)?;
    if !parameter.additional.is_empty() {
        return Err(InitError::DeserializationFailure(anyhow!(
            "Unknown additional parameters: {}",
            parameter.additional.keys().join(", ")
        )));
    }
    let Some(name) = parameter.name else {
        return Err(InitError::DeserializationFailure(anyhow!(
            "Token name is missing"
        )));
    };
    let Some(metadata) = parameter.metadata else {
        return Err(InitError::DeserializationFailure(anyhow!(
            "Token metadata is missing"
        )));
    };
    let Some(governance_account) = parameter.governance_account else {
        return Err(InitError::DeserializationFailure(anyhow!(
            "Token governance account is missing"
        )));
    };
    host.set_module_state(STATE_KEY_NAME, Some(name.into()))?;
    let encoded_metadata = cbor_encode(&metadata)?;
    host.set_module_state(STATE_KEY_METADATA, Some(encoded_metadata))?;
    if let Some(true) = parameter.allow_list {
        host.set_module_state(STATE_KEY_ALLOW_LIST, Some(vec![]))?;
    }
    if let Some(true) = parameter.deny_list {
        host.set_module_state(STATE_KEY_DENY_LIST, Some(vec![]))?;
    }
    if let Some(true) = parameter.mintable {
        host.set_module_state(STATE_KEY_MINTABLE, Some(vec![]))?;
    }
    if let Some(true) = parameter.burnable {
        host.set_module_state(STATE_KEY_BURNABLE, Some(vec![]))?;
    }
    let Some(governance_account) = host.account_by_address(&governance_account.address) else {
        return Err(InitError::GovernanceAccountDoesNotExist(
            governance_account.address,
        ));
    };
    let governance_account_index = host.account_index(&governance_account);
    host.set_module_state(
        STATE_KEY_GOVERNANCE_ACCOUNT,
        Some(governance_account_index.index.to_be_bytes().to_vec()),
    )?;
    if let Some(initial_supply) = parameter.initial_supply {
        let mint_amount = to_token_raw_amount(initial_supply, host.decimals())
            .map_err(|e| InitError::InvalidMintAmount(e.into()))?;
        host.mint(&governance_account, mint_amount)
            .map_err(|_| InitError::InvalidMintAmount(anyhow!("Kernel failed to mint")))?;
    }
    Ok(())
}

/// Execute a token update transaction using the [`HostOperations`] implementation on `host` to
/// update state and produce events.
///
/// When resulting in an `Err` signals a rejected operation and all of the calls to
/// [`HostOperations`] must be rolled back y the caller.
///
/// The process is as follows:
///
/// - Decode the transaction CBOR parameter.
/// - Check that amounts are within the representable range.
/// - For each transfer operation:
///
///    - Check that the module is not paused.
///    - Check that the recipient is valid.
///    - Check allowList/denyList restrictions.
///    - Transfer the amount from the sender to the recipient, if the sender's balance is
///      sufficient.
///
/// - For each list update operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module configuration allows the list operation.
///    - Check that the account to add/remove exists on-chain.
///
/// - For each mint operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows minting.
///    - Check that the minting process was successful.
///
/// - For each burn operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Check that the burning process was successful.
///
/// - For each pause/unpause operation:
///
///     - Check that the governance account is the sender.
///
/// # INVARIANTS:
///
///   - Token module state contains a correctly encoded governance account address.
pub fn execute_token_update_transaction<Host>(
    host: &mut Host,
    context: TransactionContext<Host::Account>,
    token_parameter: Parameter,
) -> Result<(), TokenModuleRejectReasonType>
where
    Host: HostOperations,
{
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    let transaction: TokenOperations = cbor_decode_with_options(token_parameter, decode_options)
        .map_err(|e| {
            TokenModuleRejectReasonType::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(e.to_string()),
                },
            )
        })?;
    let decimals = host.decimals();
    let operations = preprocess_transaction_operations(transaction.operations, decimals)?;
    let sender_index = host.account_index(&context.sender);

    for (op_index, op) in operations.into_iter().enumerate() {
        let operation_info = get_operation_info(&op);
        host.tick_energy(operation_info.cost);
        if operation_info.is_goverance {
            let governance_account = get_governance_account(host).ok_or_else(|| {
                operation_not_permitted(
                    op_index,
                    Some(context.sender_address.into()),
                    Some("sender is not the token governance account"),
                )
            })?;
            if sender_index != governance_account {
                return Err(operation_not_permitted(
                    op_index,
                    Some(context.sender_address.into()),
                    Some("sender is not the token governance account"),
                ));
            }
        }

        match op {
            TokenOperation::Transfer(token_transfer) => {
                if get_paused(host) {
                    return Err(operation_not_permitted(
                        op_index,
                        None,
                        Some("token operation transfer is paused"),
                    ));
                }
                let Some(recipient) = host.account_by_address(&token_transfer.recipient.address)
                else {
                    return Err(address_not_found(op_index, token_transfer.recipient));
                };
                let recipient_index = host.account_index(&recipient);
                if host.get_module_state(STATE_KEY_ALLOW_LIST).is_some() {
                    if host
                        .get_account_state(sender_index, STATE_KEY_ALLOW_LIST)
                        .is_none()
                    {
                        return Err(operation_not_permitted(
                            op_index,
                            Some(context.sender_address.into()),
                            Some("sender not in allow list"),
                        ));
                    }
                    if host
                        .get_account_state(recipient_index, STATE_KEY_ALLOW_LIST)
                        .is_none()
                    {
                        return Err(operation_not_permitted(
                            op_index,
                            Some(token_transfer.recipient),
                            Some("recipient not in allow list"),
                        ));
                    }
                }
                if host.get_module_state(STATE_KEY_DENY_LIST).is_some() {
                    if host
                        .get_account_state(sender_index, STATE_KEY_DENY_LIST)
                        .is_some()
                    {
                        return Err(operation_not_permitted(
                            op_index,
                            Some(context.sender_address.into()),
                            Some("sender in deny list"),
                        ));
                    }
                    if host
                        .get_account_state(recipient_index, STATE_KEY_DENY_LIST)
                        .is_some()
                    {
                        return Err(operation_not_permitted(
                            op_index,
                            Some(token_transfer.recipient),
                            Some("recipient in deny list"),
                        ));
                    }
                }
                if host
                    .transfer(
                        &context.sender,
                        &recipient,
                        token_transfer.amount.value(),
                        token_transfer.memo.map(Memo::from),
                    )
                    .is_err()
                {
                    let available_balance =
                        TokenAmount::from_raw(host.account_balance(&context.sender), decimals);
                    return Err(token_balance_insufficient(
                        op_index,
                        available_balance,
                        token_transfer.amount,
                    ));
                }
            }
            TokenOperation::Mint(mint_details) => {
                if get_paused(host) {
                    return Err(operation_not_permitted(
                        op_index,
                        None,
                        Some("token operation mint is paused"),
                    ));
                }
                if host.get_module_state(STATE_KEY_MINTABLE).is_none() {
                    return Err(feature_not_enabled(op_index, "mint".into()));
                }
                if host
                    .mint(&context.sender, mint_details.amount.value())
                    .is_err()
                {
                    let current_supply = TokenAmount::from_raw(host.circulating_supply(), decimals);
                    return Err(mint_would_overflow(
                        op_index,
                        mint_details.amount,
                        current_supply,
                    ));
                }
            }
            TokenOperation::Burn(burn_details) => {
                if get_paused(host) {
                    return Err(operation_not_permitted(
                        op_index,
                        None,
                        Some("token operation burn is paused"),
                    ));
                }
                if host.get_module_state(STATE_KEY_BURNABLE).is_none() {
                    return Err(feature_not_enabled(op_index, "burn".into()));
                }
                if host
                    .burn(&context.sender, burn_details.amount.value())
                    .is_err()
                {
                    let available_balance =
                        TokenAmount::from_raw(host.account_balance(&context.sender), decimals);
                    return Err(token_balance_insufficient(
                        op_index,
                        available_balance,
                        burn_details.amount,
                    ));
                }
            }
            TokenOperation::AddAllowList(token_list_update_details) => {
                if host.get_module_state(STATE_KEY_ALLOW_LIST).is_none() {
                    return Err(feature_not_enabled(op_index, "addAllowList".into()));
                }
                let Some(target) =
                    host.account_by_address(&token_list_update_details.target.address)
                else {
                    return Err(address_not_found(
                        op_index,
                        token_list_update_details.target,
                    ));
                };
                let target_index = host.account_index(&target);
                host.set_account_state(target_index, STATE_KEY_ALLOW_LIST, Some(Vec::new()));
            }
            TokenOperation::RemoveAllowList(token_list_update_details) => todo!(),
            TokenOperation::AddDenyList(token_list_update_details) => todo!(),
            TokenOperation::RemoveDenyList(token_list_update_details) => todo!(),
            TokenOperation::Pause(token_pause_details) => todo!(),
            TokenOperation::Unpause(token_pause_details) => todo!(),
        }
    }
    Ok(())
}

struct OperationInfo {
    cost: Energy,
    is_goverance: bool,
}

const TOKEN_TRANSFER_COST: Energy = Energy { energy: 100 };
const TOKEN_MINT_COST: Energy = Energy { energy: 50 };
const TOKEN_BURN_COST: Energy = Energy { energy: 50 };
const TOKEN_LIST_OPERATION_COST: Energy = Energy { energy: 50 };
const TOKEN_PAUSE_UNPAUSE_COST: Energy = Energy { energy: 50 };

fn get_operation_info(op: &TokenOperation) -> OperationInfo {
    match op {
        TokenOperation::Transfer(_) => OperationInfo {
            cost: TOKEN_TRANSFER_COST,
            is_goverance: false,
        },
        TokenOperation::Mint(_) => OperationInfo {
            cost: TOKEN_MINT_COST,
            is_goverance: true,
        },
        TokenOperation::Burn(_) => OperationInfo {
            cost: TOKEN_BURN_COST,
            is_goverance: true,
        },
        TokenOperation::AddAllowList(_) => OperationInfo {
            cost: TOKEN_LIST_OPERATION_COST,
            is_goverance: true,
        },
        TokenOperation::RemoveAllowList(_) => OperationInfo {
            cost: TOKEN_LIST_OPERATION_COST,
            is_goverance: true,
        },
        TokenOperation::AddDenyList(_) => OperationInfo {
            cost: TOKEN_LIST_OPERATION_COST,
            is_goverance: true,
        },
        TokenOperation::RemoveDenyList(_) => OperationInfo {
            cost: TOKEN_LIST_OPERATION_COST,
            is_goverance: true,
        },
        TokenOperation::Pause(_) => OperationInfo {
            cost: TOKEN_PAUSE_UNPAUSE_COST,
            is_goverance: true,
        },
        TokenOperation::Unpause(_) => OperationInfo {
            cost: TOKEN_PAUSE_UNPAUSE_COST,
            is_goverance: true,
        },
    }
}

fn preprocess_transaction_operations(
    transaction_operations: Vec<CborUpward<TokenOperation>>,
    decimals: u8,
) -> Result<Vec<TokenOperation>, TokenModuleRejectReasonType> {
    let mut operations = Vec::with_capacity(transaction_operations.len());
    for op in transaction_operations {
        let op = match op {
            Upward::Unknown(Value::Map(v)) => {
                if let Some((Value::Text(operation_type), _)) = v.first() {
                    return reject_deserialization_failure(Some(format!(
                        "Invalid token operation: {operation_type}"
                    )));
                } else {
                    return reject_deserialization_failure(Some("Invalid token operation."));
                }
            }
            Upward::Unknown(_) => {
                return reject_deserialization_failure(Some("Invalid token operation."));
            }
            Upward::Known(op) => op,
        };
        match &op {
            TokenOperation::Transfer(token_transfer) => {
                check_amount_representable(&token_transfer.amount, decimals, "Token transfer")?;
            }
            TokenOperation::Mint(mint_details) => {
                check_amount_representable(&mint_details.amount, decimals, "Token mint")?;
            }
            TokenOperation::Burn(burn_details) => {
                check_amount_representable(&burn_details.amount, decimals, "Token burn")?;
            }
            TokenOperation::AddAllowList(_) => {}
            TokenOperation::RemoveAllowList(_) => {}
            TokenOperation::AddDenyList(_) => {}
            TokenOperation::RemoveDenyList(_) => {}
            TokenOperation::Pause(_) => {}
            TokenOperation::Unpause(_) => {}
        }
        operations.push(op);
    }
    Ok(operations)
}

fn check_amount_representable(
    amount: &TokenAmount,
    decimals: u8,
    description: &str,
) -> Result<(), TokenModuleRejectReasonType> {
    let actual = amount.decimals();
    if actual != decimals {
        reject_deserialization_failure(Some(format!(
            "{description} amount outside representable range: expected {decimals} decimals, found {actual}",
        )))
    } else {
        Ok(())
    }
}

#[inline]
fn reject_deserialization_failure<A>(
    reason: Option<impl Into<String>>,
) -> Result<A, TokenModuleRejectReasonType> {
    Err(TokenModuleRejectReasonType::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: reason.map(|r| r.into()),
        },
    ))
}

#[inline]
fn operation_not_permitted(
    index: usize,
    address: Option<CborHolderAccount>,
    reason: Option<impl Into<String>>,
) -> TokenModuleRejectReasonType {
    TokenModuleRejectReasonType::OperationNotPermitted(
        concordium_base::protocol_level_tokens::OperationNotPermittedRejectReason {
            index,
            address,
            reason: reason.map(|r| r.into()),
        },
    )
}

#[inline]
fn address_not_found(index: usize, address: CborHolderAccount) -> TokenModuleRejectReasonType {
    TokenModuleRejectReasonType::AddressNotFound(
        concordium_base::protocol_level_tokens::AddressNotFoundRejectReason { index, address },
    )
}

#[inline]
fn token_balance_insufficient(
    index: usize,
    available_balance: TokenAmount,
    required_balance: TokenAmount,
) -> TokenModuleRejectReasonType {
    TokenModuleRejectReasonType::TokenBalanceInsufficient(
        concordium_base::protocol_level_tokens::TokenBalanceInsufficientRejectReason {
            index,
            available_balance,
            required_balance,
        },
    )
}

#[inline]
fn feature_not_enabled(index: usize, operation_type: String) -> TokenModuleRejectReasonType {
    TokenModuleRejectReasonType::UnsupportedOperation(
        concordium_base::protocol_level_tokens::UnsupportedOperationRejectReason {
            index,
            operation_type,
            reason: Some("feature not enabled".into()),
        },
    )
}

#[inline]
fn mint_would_overflow(
    index: usize,
    requested_amount: TokenAmount,
    current_supply: TokenAmount,
) -> TokenModuleRejectReasonType {
    TokenModuleRejectReasonType::MintWouldOverflow(
        concordium_base::protocol_level_tokens::MintWouldOverflowRejectReason {
            index,
            requested_amount,
            current_supply,
            max_representable_amount: TokenAmount::from_raw(u64::MAX, current_supply.decimals()),
        },
    )
}

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state(_host: &impl HostOperations) -> Result<RawCbor, QueryError> {
    todo!()
}

/// Get the CBOR-encoded representation of the token module account state.
pub fn query_account_state<Host>(
    _host: &Host,
    _account: Host::Account,
) -> Result<Option<RawCbor>, QueryError>
where
    Host: HostOperations,
{
    todo!()
}
