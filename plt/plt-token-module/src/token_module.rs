//! Implementation of the protocol-level token module.
//!

use crate::token_kernel_interface::*;
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::common::cbor;
use concordium_base::common::cbor::{
    CborDeserialize, CborSerializationError, CborSerializationResult, SerializationOptions,
    UnknownMapKeys,
};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenModuleInitializationParameters,
    TokenModuleState,
};

/// Extension trait for `TokenKernelOperations` to provide convenience wrappers for
/// module state updates.
trait KernelOperationsExt: TokenKernelOperations {
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

impl<T: TokenKernelOperations> KernelOperationsExt for T {}

/// Extension trait for `TokenQueryOperations` to provide convenience wrappers for
/// module state access.
trait KernelQueriesExt: TokenKernelQueries {
    /// Get value from the token module state at the given key.
    fn get_module_state<'a>(&self, key: impl IntoIterator<Item = &'a u8>) -> Option<StateValue> {
        self.get_token_state(module_state_key(key))
    }
}

impl<T: TokenKernelQueries> KernelQueriesExt for T {}

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
pub enum TokenInitializationError {
    #[error("Invalid token initialization parameters: {0}")]
    InvalidInitializationParameters(String),
    #[error("CBOR serialization error during token initialization: {0}")]
    CborSerialization(#[from] CborSerializationError),
    #[error("{0}")]
    LockedStateKey(#[from] LockedStateKeyError),
    #[error("The given governance account does not exist: {0}")]
    GovernanceAccountDoesNotExist(AccountAddress),
    #[error("The initial mint amount has wrong number of decimals: {0}")]
    MintAmountDecimalsMismatch(#[from] TokenAmountDecimalsMismatchError),
    #[error("The initial mint amount is not representable: {0}")]
    MintAmountNotRepresentable(#[from] AmountNotRepresentableError),
}

/// Represents the reasons why [`execute_token_update_transaction`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenUpdateError {
    #[error("CBOR serialization error during token update: {0}")]
    CborSerialization(#[from] CborSerializationError),
}

/// Represents the reasons why a query to the token module can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenQueryError {
    #[error("CBOR serialization error during token query: {0}")]
    CborSerialization(#[from] CborSerializationError),
    #[error("Token module state invariant broken: {0}")]
    StateInvariantViolation(String),
}

/// The context for a token-holder or token-governance transaction.
#[derive(Debug)]
pub struct TransactionContext<Account> {
    /// The sender account object.
    pub sender: Account,
    /// The sender account address. This is the account alias that is used by the transaction itself.
    pub sender_address: AccountAddress,
}

#[derive(Debug, thiserror::Error)]
#[error("Token amount decimals mismatch: expected {expected}, found {found}")]
pub struct TokenAmountDecimalsMismatchError {
    pub expected: u8,
    pub found: u8,
}

/// Asserts that token amount has the right number of decimals and converts it to a plain
/// integer.
fn to_token_raw_amount(
    amount: TokenAmount,
    expected_decimals: u8,
) -> Result<RawTokenAmount, TokenAmountDecimalsMismatchError> {
    let decimals = amount.decimals();
    if decimals != expected_decimals {
        return Err(TokenAmountDecimalsMismatchError {
            expected: expected_decimals,
            found: decimals,
        });
    }
    Ok(RawTokenAmount(amount.value()))
}

const STATE_KEY_NAME: &[u8] = b"name";
const STATE_KEY_METADATA: &[u8] = b"metadata";
const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
const STATE_KEY_MINTABLE: &[u8] = b"mintable";
const STATE_KEY_BURNABLE: &[u8] = b"burnable";
const STATE_KEY_PAUSED: &[u8] = b"paused";
const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";

fn cbor_decode<T: CborDeserialize>(cbor: impl AsRef<[u8]>) -> CborSerializationResult<T> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    cbor::cbor_decode_with_options(cbor, decode_options)
}

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token(
    host: &mut impl TokenKernelOperations,
    initialization_parameters_cbor: RawCbor,
) -> Result<(), TokenInitializationError> {
    let init_params: TokenModuleInitializationParameters =
        cbor_decode(initialization_parameters_cbor)?;
    if !init_params.additional.is_empty() {
        return Err(TokenInitializationError::InvalidInitializationParameters(
            format!(
                "Unknown additional parameters: {}",
                init_params
                    .additional
                    .keys()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ));
    }
    let name = init_params.name.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token name is missing".to_string(),
        )
    })?;
    let metadata = init_params.metadata.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token metadata is missing".to_string(),
        )
    })?;
    let governance_account = init_params.governance_account.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token governance account is missing".to_string(),
        )
    })?;
    host.set_module_state(STATE_KEY_NAME, Some(name.into()))?;
    let encoded_metadata = cbor::cbor_encode(&metadata)?;
    host.set_module_state(STATE_KEY_METADATA, Some(encoded_metadata))?;
    if init_params.allow_list == Some(true) {
        host.set_module_state(STATE_KEY_ALLOW_LIST, Some(vec![]))?;
    }
    if init_params.deny_list == Some(true) {
        host.set_module_state(STATE_KEY_DENY_LIST, Some(vec![]))?;
    }
    if init_params.mintable == Some(true) {
        host.set_module_state(STATE_KEY_MINTABLE, Some(vec![]))?;
    }
    if init_params.burnable == Some(true) {
        host.set_module_state(STATE_KEY_BURNABLE, Some(vec![]))?;
    }

    let governance_account = host.account_by_address(&governance_account.address).ok_or(
        TokenInitializationError::GovernanceAccountDoesNotExist(governance_account.address),
    )?;
    let governance_account_index = host.account_index(&governance_account);
    host.set_module_state(
        STATE_KEY_GOVERNANCE_ACCOUNT,
        Some(common::to_bytes(&governance_account_index.index)),
    )?;
    if let Some(initial_supply) = init_params.initial_supply {
        let mint_amount = to_token_raw_amount(initial_supply, host.decimals())?;
        host.mint(&governance_account, mint_amount)
            .map_err(TokenInitializationError::MintAmountNotRepresentable)?;
    }
    Ok(())
}

/// Execute a token update transaction using the [`TokenKernelOperations`] implementation on `host` to
/// update state and produce events.
///
/// When resulting in an `Err` signals a rejected operation and all of the calls to
/// [`TokenKernelOperations`] must be rolled back y the caller.
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
pub fn execute_token_update_transaction<Kernel: TokenKernelOperations>(
    _kernel: &mut Kernel,
    _context: TransactionContext<Kernel::Account>,
    _token_operations: RawCbor,
) -> Result<(), TokenUpdateError> {
    todo!()
}

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<Kernel: TokenKernelQueries>(
    kernel: &impl TokenKernelQueries,
) -> Result<RawCbor, TokenQueryError> {
    let name = kernel
        .get_module_state(STATE_KEY_NAME)
        .ok_or_else(|| TokenQueryError::StateInvariantViolation("Name not present".to_string()))
        .and_then(|value| {
            String::from_utf8(value).map_err(|err| {
                TokenQueryError::StateInvariantViolation(format!("Name invalid UTF8: {}", err))
            })
        })?;

    let metadata_cbor = kernel.get_module_state(STATE_KEY_METADATA).ok_or_else(|| {
        TokenQueryError::StateInvariantViolation("Metadata not present".to_string())
    })?;
    let metadata: MetadataUrl = cbor_decode(metadata_cbor)?;

    let allow_list = kernel.get_module_state(STATE_KEY_ALLOW_LIST).is_some();
    let deny_list = kernel.get_module_state(STATE_KEY_DENY_LIST).is_some();
    let mintable = kernel.get_module_state(STATE_KEY_MINTABLE).is_some();
    let burnable = kernel.get_module_state(STATE_KEY_BURNABLE).is_some();
    let paused = kernel.get_module_state(STATE_KEY_PAUSED).is_some();

    let governance_account_index = AccountIndex::from(
        kernel
            .get_module_state(STATE_KEY_GOVERNANCE_ACCOUNT)
            .ok_or_else(|| {
                TokenQueryError::StateInvariantViolation(
                    "Governance account not present".to_string(),
                )
            })
            .and_then(|value| {
                common::from_bytes::<u64, _>(&mut value.as_slice()).map_err(|err| {
                    TokenQueryError::StateInvariantViolation(format!(
                        "Governance account index cannot be decoded: {}",
                        err
                    ))
                })
            })?,
    );
    let governance_account = kernel
        .account_by_index(governance_account_index)
        .ok_or_else(|| {
            TokenQueryError::StateInvariantViolation(format!(
                "Governance account with index {} does not exist",
                governance_account_index
            ))
        })?;
    let governance_account_address = kernel.account_canonical_address(&governance_account);

    let state = TokenModuleState {
        name: Some(name),
        metadata: Some(metadata),
        governance_account: Some(CborHolderAccount::from(governance_account_address)),
        allow_list: Some(allow_list),
        deny_list: Some(deny_list),
        mintable: Some(mintable),
        burnable: Some(burnable),
        paused: Some(paused),
        additional: Default::default(),
    };

    Ok(RawCbor::from(cbor::cbor_encode(&state)?))
}

/// Get the CBOR-encoded representation of the token module account state.
pub fn query_account_state<Kernel: TokenKernelQueries>(
    _kernel: &impl TokenKernelQueries,
    _account: Kernel::Account,
) -> Result<Option<RawCbor>, TokenQueryError> {
    todo!()
}
