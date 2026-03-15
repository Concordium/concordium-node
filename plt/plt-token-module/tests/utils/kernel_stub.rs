use std::collections::{BTreeMap, VecDeque};

use concordium_base::base::{AccountIndex, Energy, InsufficientEnergy, ProtocolVersion};
use concordium_base::common::{Serial, cbor};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenModuleCborTypeDiscriminator,
    TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use plt_block_state::block_state::types::AccountWithCanonicalAddress;
use plt_block_state::block_state::types::protocol_level_tokens::{TokenStateKey, TokenStateValue};
use plt_block_state::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError,
};
use plt_scheduler_interface::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenKernelOperations,
    TokenKernelQueries, TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use plt_scheduler_interface::transaction_execution_interface::{
    OutOfEnergyError, TransactionExecution,
};
use plt_scheduler_types::types::events::EncodedTokenModuleEvent;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use plt_token_module::token_module;

/// Token kernel stub providing an implementation of [`TokenKernelOperations`] and methods for
/// configuring the state of the kernel.
#[derive(Debug)]
pub struct KernelStub {
    /// Protocol version of the kernel implementation.
    protocol_version: ProtocolVersion,
    /// List of accounts existing.
    accounts: Vec<Account>,
    /// Token module managed state.
    state: BTreeMap<TokenStateKey, TokenStateValue>,
    /// Decimal places in token representation.
    decimals: u8,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
    /// Transfers
    transfers: VecDeque<(
        AccountStubIndex,
        AccountStubIndex,
        RawTokenAmount,
        Option<Memo>,
    )>,
    events: Vec<(TokenModuleCborTypeDiscriminator, RawCbor)>,
}

/// Internal representation of an Account in [`KernelStub`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Account {
    /// The index of the account
    pub index: AccountIndex,
    /// The canonical account address of the account.
    pub address: AccountAddress,
    /// The token balance of the account.
    pub balance: Option<RawTokenAmount>,
}

const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();
const ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();

const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
const STATE_KEY_PAUSED: &[u8] = b"paused";

fn module_state_key(key: &[u8]) -> TokenStateKey {
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + key.len());
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend_from_slice(key);
    module_key
}

fn account_state_key(account_index: AccountIndex, key: &[u8]) -> TokenStateKey {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_STATE_PREFIX.len() + size_of::<AccountIndex>() + key.len());
    account_key.extend_from_slice(&ACCOUNT_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key.extend_from_slice(key);
    account_key
}

impl KernelStub {
    /// Create new kernel stub
    pub fn with_decimals(decimals: u8, protocol_version: ProtocolVersion) -> Self {
        Self {
            protocol_version,
            accounts: vec![],
            state: Default::default(),
            decimals,
            circulating_supply: RawTokenAmount::default(),
            transfers: Default::default(),
            events: Default::default(),
        }
    }

    /// Create an account in the stub.
    ///
    /// # Example
    ///
    /// ```
    /// let mut stub = KernelStub::new(0);
    /// let account = stub.create_account();
    /// assert!(host.account_by_address(account_address1).is_some(), "Account must exist");
    /// ```
    pub fn create_account(&mut self) -> AccountStubIndex {
        let index = self.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index: account_index,
            address,
            balance: None,
        };
        let stub_index = AccountStubIndex(index);
        self.accounts.push(account);

        stub_index
    }

    pub fn account_canonical_address(&self, account: &AccountStubIndex) -> AccountAddress {
        self.accounts[account.0].address
    }

    pub fn account_touched(&self, account: &AccountStubIndex) -> bool {
        self.accounts[account.0].balance.is_some()
    }

    /// Set account balance in the stub
    pub fn set_account_balance(&mut self, account: AccountStubIndex, new_balance: RawTokenAmount) {
        let balance = self.accounts[account.0].balance.get_or_insert_default();
        self.circulating_supply.0 = self
            .circulating_supply
            .0
            .checked_add(new_balance.0)
            .unwrap()
            .checked_sub(balance.0)
            .unwrap();
        *balance = new_balance;
    }

    /// Set the account allow-list entry in token state.
    pub fn set_allow_list(&mut self, account: AccountStubIndex, value: bool) {
        self.set_token_state_value(
            account_state_key(self.account_index(&account), STATE_KEY_ALLOW_LIST),
            value.then_some(vec![]),
        );
    }

    /// Set the account deny-list entry in token state.
    pub fn set_deny_list(&mut self, account: AccountStubIndex, value: bool) {
        self.set_token_state_value(
            account_state_key(self.account_index(&account), STATE_KEY_DENY_LIST),
            value.then_some(vec![]),
        );
    }

    /// Set the module paused state in token state.
    pub fn set_paused(&mut self, value: bool) {
        self.set_token_state_value(module_state_key(STATE_KEY_PAUSED), value.then_some(vec![]));
    }

    /// Initialize token and return the governance account
    pub fn init_token(&mut self, params: TokenInitTestParams) -> AccountStubIndex {
        let gov_account = self.create_account();
        let gov_holder_account =
            CborHolderAccount::from(self.account_canonical_address(&gov_account));
        let metadata = MetadataUrl::from("https://plt.token".to_string());
        let parameters = TokenModuleInitializationParameters {
            name: Some("Protocol-level token".to_owned()),
            metadata: Some(metadata.clone()),
            governance_account: Some(gov_holder_account.clone()),
            allow_list: params.allow_list,
            deny_list: params.deny_list,
            initial_supply: None,
            mintable: params.mintable,
            burnable: params.burnable,
        };
        let encoded_parameters = cbor::cbor_encode(&parameters).into();
        token_module::initialize_token(self, encoded_parameters).expect("initialize token");
        gov_account
    }

    pub fn execution_with_sender(&self, account: AccountStubIndex) -> TransactionExecutionTestImpl {
        TransactionExecutionTestImpl::with_sender(account, self.account_canonical_address(&account))
    }

    pub fn execution_with_sender_and_energy(
        &self,
        account: AccountStubIndex,
        energy: Energy,
    ) -> TransactionExecutionTestImpl {
        TransactionExecutionTestImpl::with_sender_and_energy(
            account,
            self.account_canonical_address(&account),
            energy,
        )
    }

    /// The circulating supply
    pub fn circulating_supply(&self) -> RawTokenAmount {
        self.circulating_supply
    }

    /// Pop (from front) transfer registered in stub.
    pub fn pop_transfer(
        &mut self,
    ) -> Option<(
        AccountStubIndex,
        AccountStubIndex,
        RawTokenAmount,
        Option<Memo>,
    )> {
        self.transfers.pop_front()
    }

    /// Get events registered in stub.
    pub fn events(&self) -> &[(TokenModuleCborTypeDiscriminator, RawCbor)] {
        &self.events
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct TokenInitTestParams {
    allow_list: Option<bool>,
    deny_list: Option<bool>,
    mintable: Option<bool>,
    burnable: Option<bool>,
}

impl TokenInitTestParams {
    pub fn allow_list(self) -> Self {
        Self {
            allow_list: Some(true),
            ..self
        }
    }

    pub fn deny_list(self) -> Self {
        Self {
            deny_list: Some(true),
            ..self
        }
    }

    pub fn mintable(self) -> Self {
        Self {
            mintable: Some(true),
            ..self
        }
    }

    pub fn burnable(self) -> Self {
        Self {
            burnable: Some(true),
            ..self
        }
    }
}

/// Host stub account object.
///
/// When testing it is the index into the list of accounts tracked by the `KernelStub`.
/// Holding
#[derive(Debug, Clone, Copy)]
pub struct AccountStubIndex(usize);

impl TokenKernelQueries for KernelStub {
    type Account = AccountStubIndex;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.accounts
            .iter()
            .enumerate()
            .find_map(|(i, account)| {
                if account.address.is_alias(address) {
                    Some(AccountStubIndex(i))
                } else {
                    None
                }
            })
            .ok_or(AccountNotFoundByAddressError(*address))
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        if let Some(account) = self.accounts.get(index.index as usize) {
            let account_with_canonical_address = AccountWithCanonicalAddress {
                account: AccountStubIndex(index.index as usize),
                canonical_account_address: account.address,
            };
            Ok(account_with_canonical_address)
        } else {
            Err(AccountNotFoundByIndexError(index))
        }
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.accounts[account.0].index
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.accounts[account.0].balance.unwrap_or_default()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.state.get(&key).cloned()
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.protocol_version
    }
}

impl TokenKernelOperations for KernelStub {
    fn touch_account(&mut self, account: &Self::Account) {
        self.accounts[account.0].balance.get_or_insert_default();
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        account_address: AccountAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenMintError> {
        self.circulating_supply.0 =
            self.circulating_supply
                .0
                .checked_add(amount.0)
                .ok_or(MintWouldOverflowError {
                    requested_amount: amount,
                    current_supply: self.circulating_supply,
                    max_representable_amount: RawTokenAmount::MAX,
                })?;

        let balance = self.accounts[account.0].balance.get_or_insert_default();
        balance.0 = balance
            .0
            .checked_add(amount.0)
            .ok_or(TokenStateInvariantError("Overflow".to_string()))?;
        Ok(())
    }

    fn burn(
        &mut self,
        account: &Self::Account,
        account_address: AccountAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        let balance = self.accounts[account.0].balance.get_or_insert_default();
        balance.0 = balance
            .0
            .checked_sub(amount.0)
            .ok_or(TokenBurnError::InsufficientBalance(
                InsufficientBalanceError {
                    available: *balance,
                    required: amount,
                },
            ))?;

        self.circulating_supply.0 = self
            .circulating_supply
            .0
            .checked_sub(amount.0)
            .ok_or(TokenStateInvariantError("Underflow".to_string()))?;

        Ok(())
    }

    fn transfer(
        &mut self,
        from: &Self::Account,
        from_address: AccountAddress,
        to: &Self::Account,
        to_address: AccountAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        let from_balance = self.accounts[from.0].balance.get_or_insert_default();
        from_balance.0 = from_balance
            .0
            .checked_sub(amount.0)
            .ok_or(InsufficientBalanceError {
                available: *from_balance,
                required: amount,
            })?;

        let to_balance = self.accounts[to.0].balance.get_or_insert_default();
        to_balance.0 = to_balance
            .0
            .checked_add(amount.0)
            .ok_or_else(|| TokenStateInvariantError("Overflow".to_string()))?;

        self.transfers.push_back((*from, *to, amount, memo));

        Ok(())
    }

    fn set_token_state_value(&mut self, key: TokenStateKey, value: Option<TokenStateValue>) {
        match value {
            None => self.state.remove(&key).is_some(),
            Some(value) => self.state.insert(key, value).is_some(),
        };
    }

    fn log_token_event(&mut self, event_type: TokenModuleCborTypeDiscriminator, details: RawCbor) {
        self.events.push((event_type, details));
    }
}

/// Token kernel transaction execution context for test.
#[derive(Debug)]
pub struct TransactionExecutionTestImpl {
    sender: AccountStubIndex,
    sender_address: AccountAddress,
    remaining_energy: Energy,
}

impl TransactionExecutionTestImpl {
    pub fn with_sender_and_energy(
        sender: AccountStubIndex,
        sender_address: AccountAddress,
        remaining_energy: Energy,
    ) -> Self {
        Self {
            sender,
            sender_address,
            remaining_energy,
        }
    }

    pub fn with_sender(sender: AccountStubIndex, sender_address: AccountAddress) -> Self {
        Self::with_sender_and_energy(sender, sender_address, Energy::from(u64::MAX))
    }

    pub fn remaining_energy(&self) -> Energy {
        self.remaining_energy
    }
}

impl TransactionExecution for TransactionExecutionTestImpl {
    type Account = AccountStubIndex;

    fn sender_account(&self) -> &Self::Account {
        &self.sender
    }

    fn sender_account_address(&self) -> AccountAddress {
        self.sender_address
    }

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        self.remaining_energy
            .tick_energy(energy)
            .map_err(|_err: InsufficientEnergy| OutOfEnergyError)
    }
}
