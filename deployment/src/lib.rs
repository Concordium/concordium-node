use concordium_contracts_common::AccountAddress;

// Placeholder types below until we decide how to reuse content from concordium-base:
pub type Memo = Vec<u8>;
pub type StateKey = Vec<u8>;
pub type StateValue = Vec<u8>;
pub type TokenEventType = String;
pub type Cbor = Vec<u8>;
pub type TokenEventDetails = Cbor;
pub type Parameter = Cbor;
pub type AccountIndex = u64;
pub type Energy = u64;
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
    fn account_index(&self, account: Self::Account) -> AccountIndex;

    /// Get the canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    fn account_canonical_address(&self, account: Self::Account) -> AccountAddress;

    /// Get the token balance of the account.
    fn account_balance(&self, account: Self::Account) -> TokenRawAmount;

    /// Update the balance of the given account to zero if it didn't have a balance before.
    ///
    /// Returns `true` if the balance wasn't present on the given account and `false` otherwise.
    fn touch(&mut self, account: Self::Account) -> bool;

    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Errors
    ///
    /// - [`AmountNotRepresentableError`] The total supply would exceed the representable amount.
    fn mint(
        &mut self,
        account: Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), AmountNotRepresentableError>;

    /// Burn a specified amount from the account.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn burn(
        &mut self,
        account: Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), InsufficientBalanceError>;

    /// Transfer a token amount from one account to another, with an optional memo.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn transfer(
        &mut self,
        from: Self::Account,
        to: Self::Account,
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
    fn log_token_event(&mut self, event_type: TokenEventType, event_details: TokenEventDetails);
}

/// The account has insufficient balance.
#[derive(Debug)]
pub struct InsufficientBalanceError;

/// Update to state key failed because the key was locked by an iterator.
#[derive(Debug)]
pub struct LockedStateKeyError;

/// Mint exceed the representable amount.
#[derive(Debug)]
pub struct AmountNotRepresentableError;

/// Represents the reasons why [`initialize_Token`] can fail.
#[derive(Debug)]
pub enum InitError {}
#[derive(Debug)]
pub enum UpdateError {}
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

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token(
    _host: &mut impl HostOperations,
    _token_parameter: Parameter,
) -> Result<(), InitError> {
    todo!()
}

/// Execute a token update transaction.
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
pub fn query_token_module_state(_host: &impl HostOperations) -> Result<Cbor, QueryError> {
    todo!()
}

/// Get the CBOR-encoded representation of the token module account state.
pub fn query_account_state<Host>(
    _host: &Host,
    _account: Host::Account,
) -> Result<Option<Cbor>, QueryError>
where
    Host: HostOperations,
{
    todo!()
}
