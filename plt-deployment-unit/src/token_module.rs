//! Implementation of the protocol-level token module.car
use crate::host_interface::*;
use anyhow::anyhow;
use concordium_base::common::cbor::{
    CborSerializationError, SerializationOptions, UnknownMapKeys, cbor_decode_with_options,
    cbor_encode,
};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenModuleInitializationParameters,
};
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
}

impl<T: HostOperations> HostOperationsExt for T {}

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();

/// Construct a [`StateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
fn module_state_key<'a>(key: impl IntoIterator<Item = &'a u8>) -> StateKey {
    let iter = key.into_iter();
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + iter.size_hint().0);
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend(iter);
    module_key
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
    _host: &mut Host,
    _context: TransactionContext<Host::Account>,
    _token_parameter: Parameter,
) -> Result<(), UpdateError>
where
    Host: HostOperations,
{
    todo!()
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
