use anyhow::anyhow;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor::{
    CborSerializationError, SerializationOptions, UnknownMapKeys, cbor_decode_with_options,
    cbor_encode,
};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use itertools::Itertools;

pub type StateKey = Vec<u8>;
pub type StateValue = Vec<u8>;
pub type TokenEventType = String;
pub type TokenEventDetails = RawCbor;
pub type Parameter = RawCbor;
pub type TokenRawAmount = u64;

/// Operations provided by the deployment unit host.
///
/// This is abstracted in a trait to allow for a testing stub.
pub trait HostOperations {
    /// The type for the account object.
    ///
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// Lookup the account using an account address.
    fn account_by_address(&self, address: AccountAddress) -> Option<Self::Account>;

    /// Lookup the account using an account index.
    fn account_by_index(&self, index: AccountIndex) -> Option<Self::Account>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::Account) -> AccountIndex;

    /// Get the canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress;

    /// Get the token balance of the account.
    fn account_balance(&self, account: &Self::Account) -> TokenRawAmount;

    /// Update the balance of the given account to zero if it didn't have a balance before.
    ///
    /// Returns `true` if the balance wasn't present on the given account and `false` otherwise.
    fn touch(&mut self, account: &Self::Account) -> bool;

    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenMintEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`AmountNotRepresentableError`] The total supply would exceed the representable amount.
    fn mint(
        &mut self,
        account: &Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), AmountNotRepresentableError>;

    /// Burn a specified amount from the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenBurnEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn burn(
        &mut self,
        account: &Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), InsufficientBalanceError>;

    /// Transfer a token amount from one account to another, with an optional memo.
    ///
    /// # Events
    ///
    /// This will produce a `TokenTransferEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: TokenRawAmount,
        memo: Option<Memo>,
    ) -> Result<(), InsufficientBalanceError>;

    /// The current token circulation supply.
    fn circulating_supply(&self) -> TokenRawAmount;

    /// The number of decimals used in the presentation of the token amount.
    fn decimals(&self) -> u8;

    /// Lookup a key in the token state.
    fn get_token_state(&self, key: StateKey) -> Option<StateValue>;

    /// Set or clear a value in the token state at the corresponding key.
    ///
    /// Returns whether there was an existing entry.
    ///
    /// # Errors
    ///
    /// - [`LockedStateKeyError`] if the update failed because the key was locked by an iterator.
    fn set_token_state(
        &mut self,
        key: StateKey,
        value: Option<StateValue>,
    ) -> Result<bool, LockedStateKeyError>;

    /// Reduce the available energy for the PLT module execution.
    ///
    /// If the available energy is smaller than the given amount, the containing transaction will
    /// abort and the effects of the transaction will be rolled back.
    /// The energy is charged in any case (also in case of failure).
    fn tick_energy(&mut self, energy: Energy);

    /// Log a token module event with the specified type and details.
    ///
    /// # Events
    ///
    /// This will produce a `TokenModuleEvent` in the logs.
    fn log_token_event(&mut self, event_type: TokenEventType, event_details: TokenEventDetails);
}

trait HostOperationsExt: HostOperations {
    /// Set or clear a value in the token module state at the corresponding key.
    fn set_module_state(
        &mut self,
        key: impl IntoIterator<Item = u8>,
        value: Option<StateValue>,
    ) -> Result<(), LockedStateKeyError> {
        self.set_token_state(module_state_key(key), value)?;
        Ok(())
    }
}

impl<T: HostOperations> HostOperationsExt for T {}

fn module_state_key(key: impl IntoIterator<Item = u8>) -> StateKey {
    let mut module_key = Vec::from([0, 0]);
    module_key.extend(key);
    module_key
}

/// The account has insufficient balance.
#[derive(Debug)]
pub struct InsufficientBalanceError;

/// Update to state key failed because the key was locked by an iterator.
#[derive(Debug, thiserror::Error)]
#[error("State key is locked")]
pub struct LockedStateKeyError;

/// Mint exceed the representable amount.
#[derive(Debug, thiserror::Error)]
#[error("Amount not representable")]
pub struct AmountNotRepresentableError;

/// Represents the reasons why [`initialize_token`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum InitError {
    #[error("Token initialization parameters could not be deserialized: {0}")]
    DeserializationFailure(anyhow::Error),
    #[error("{0}")]
    LockedStateKey(#[from] LockedStateKeyError),
    #[error("The given governance account does not exist: {0}")]
    GovernanceAccountDoesNotExist(AccountAddress),
    #[error("Invalid mint amount: {0}")]
    InvalidMintAmount(anyhow::Error),
}

impl From<CborSerializationError> for InitError {
    fn from(value: CborSerializationError) -> Self {
        Self::DeserializationFailure(value.into())
    }
}

/// Represents the reasons why [`execute_token_update_transaction`] can fail.
#[derive(Debug)]
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
    host.set_module_state(b"name".iter().copied(), Some(name.into()))?;
    let encoded_metadata = cbor_encode(&metadata)?;
    host.set_module_state(b"metadata".iter().copied(), Some(encoded_metadata))?;
    if let Some(true) = parameter.allow_list {
        host.set_module_state(b"allowList".iter().copied(), Some(vec![]))?;
    }
    if let Some(true) = parameter.deny_list {
        host.set_module_state(b"denyList".iter().copied(), Some(vec![]))?;
    }
    if let Some(true) = parameter.mintable {
        host.set_module_state(b"mintable".iter().copied(), Some(vec![]))?;
    }
    if let Some(true) = parameter.burnable {
        host.set_module_state(b"burnable".iter().copied(), Some(vec![]))?;
    }
    let Some(governance_account) = host.account_by_address(governance_account.address) else {
        return Err(InitError::GovernanceAccountDoesNotExist(
            governance_account.address,
        ));
    };
    let governance_account_index = host.account_index(&governance_account);
    host.set_module_state(
        b"governanceAccount".iter().copied(),
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
