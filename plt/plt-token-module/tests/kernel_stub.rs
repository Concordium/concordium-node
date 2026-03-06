// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use std::collections::{BTreeMap, VecDeque};

use concordium_base::base::{AccountIndex, Energy, InsufficientEnergy};
use concordium_base::common::{cbor, Serial};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenModuleCborTypeDiscriminator,
    TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use plt_block_state::block_state::types::{
    AccountWithAddress, AccountWithCanonicalAddress, TokenStateKey, TokenStateValue,
};
use plt_block_state::block_state::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
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
    /// List of accounts existing.
    accounts: Vec<Account>,
    /// Token module managed state.
    state: BTreeMap<TokenStateKey, TokenStateValue>,
    /// Decimal places in token representation.
    decimals: u8,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
    /// Transfers
    transfers: VecDeque<(AccountIndex, AccountIndex, RawTokenAmount, Option<Memo>)>,
    pub events: Vec<(TokenModuleCborTypeDiscriminator, RawCbor)>,
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
    pub fn with_decimals(decimals: u8) -> Self {
        Self {
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
    pub fn create_account(&mut self) -> AccountWithAddress {
        let index = self.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index: account_index,
            address,
            balance: None,
        };
        self.accounts.push(account);
        AccountWithAddress {
            index: account_index,
            address,
        }
    }

    pub fn account_address(&self, account: &AccountWithAddress) -> AccountAddress {
        account.address
    }

    pub fn account_touched(&self, account: &AccountWithAddress) -> bool {
        self.accounts[account.index.index as usize]
            .balance
            .is_some()
    }

    /// Set account balance in the stub
    pub fn set_account_balance(
        &mut self,
        account: AccountWithAddress,
        new_balance: RawTokenAmount,
    ) {
        let balance = self.accounts[account.index.index as usize]
            .balance
            .get_or_insert_default();
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
    pub fn set_allow_list(&mut self, account: AccountWithAddress, value: bool) {
        self.set_token_state_value(
            account_state_key(self.account_index(&account), STATE_KEY_ALLOW_LIST),
            value.then_some(vec![]),
        );
    }

    /// Set the account deny-list entry in token state.
    pub fn set_deny_list(&mut self, account: AccountWithAddress, value: bool) {
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
    pub fn init_token(&mut self, params: TokenInitTestParams) -> AccountWithAddress {
        let gov_account = self.create_account();
        let gov_holder_account = CborHolderAccount::from(self.account_address(&gov_account));
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
            additional: Default::default(),
        };
        let encoded_parameters = cbor::cbor_encode(&parameters).into();
        token_module::initialize_token(self, encoded_parameters).expect("initialize token");
        gov_account
    }

    /// The circulating supply
    pub fn circulating_supply(&self) -> RawTokenAmount {
        self.circulating_supply
    }

    pub fn pop_transfer(
        &mut self,
    ) -> Option<(AccountIndex, AccountIndex, RawTokenAmount, Option<Memo>)> {
        self.transfers.pop_front()
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

impl TokenKernelQueries for KernelStub {
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<AccountWithAddress, AccountNotFoundByAddressError> {
        self.accounts
            .iter()
            .enumerate()
            .find_map(|(i, account)| {
                if account.address.is_alias(address) {
                    let index = AccountIndex::from(i as u64);
                    Some(AccountWithAddress {
                        index,
                        address: *address,
                    })
                } else {
                    None
                }
            })
            .ok_or(AccountNotFoundByAddressError(*address))
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        if let Some(account) = self.accounts.get(index.index as usize) {
            Ok(AccountWithCanonicalAddress(AccountWithAddress {
                index,
                address: account.address,
            }))
        } else {
            Err(AccountNotFoundByIndexError(index))
        }
    }

    fn account_index(&self, account: &AccountWithAddress) -> AccountIndex {
        account.index
    }

    fn account_token_balance(&self, account: &AccountWithAddress) -> RawTokenAmount {
        self.accounts[account.index.index as usize]
            .balance
            .unwrap_or_default()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.state.get(&key).cloned()
    }
}

impl TokenKernelOperations for KernelStub {
    fn touch_account(&mut self, account: &AccountWithAddress) {
        self.accounts[account.index.index as usize]
            .balance
            .get_or_insert_default();
    }

    fn mint(
        &mut self,
        account: &AccountWithAddress,
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

        let balance = self.accounts[account.index.index as usize]
            .balance
            .get_or_insert_default();
        balance.0 = balance
            .0
            .checked_add(amount.0)
            .ok_or(TokenStateInvariantError("Overflow".to_string()))?;
        Ok(())
    }

    fn burn(
        &mut self,
        account: &AccountWithAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        let balance = self.accounts[account.index.index as usize]
            .balance
            .get_or_insert_default();
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
        from: &AccountWithAddress,
        to: &AccountWithAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        let from_balance = self.accounts[from.index.index as usize]
            .balance
            .get_or_insert_default();
        from_balance.0 = from_balance
            .0
            .checked_sub(amount.0)
            .ok_or(InsufficientBalanceError {
                available: *from_balance,
                required: amount,
            })?;

        let to_balance = self.accounts[to.index.index as usize]
            .balance
            .get_or_insert_default();
        to_balance.0 = to_balance
            .0
            .checked_add(amount.0)
            .ok_or_else(|| TokenStateInvariantError("Overflow".to_string()))?;

        self.transfers
            .push_back((from.index, to.index, amount, memo));

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

pub trait TransactionExecutionTestImpl {
    fn with_sender_and_energy(sender: AccountWithAddress, remaining_energy: Energy) -> Self;

    fn with_sender(sender: AccountWithAddress) -> Self;
}

impl TransactionExecutionTestImpl for TransactionExecution {
    fn with_sender_and_energy(sender: AccountWithAddress, remaining_energy: Energy) -> Self {
        TransactionExecution::new(remaining_energy, sender.index, sender.address)
    }

    fn with_sender(sender: AccountWithAddress) -> Self {
        TransactionExecution::new(Energy::from(u64::MAX), sender.index, sender.address)
    }
}

// Tests for the kernel stub

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// Test lookup account address and account from address
#[test]
fn test_account_lookup_address() {
    let mut stub = KernelStub::with_decimals(0);
    let account = stub.create_account();

    let address = stub.account_address(&account);
    stub.account_by_address(&address)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_address(&TEST_ACCOUNT2).is_err(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index
#[test]
fn test_account_lookup_index() {
    let mut stub = KernelStub::with_decimals(0);
    let account = stub.create_account();

    let index = stub.account_index(&account);
    stub.account_by_index(index)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_index(2.into()).is_err(),
        "Account is not expected to exist"
    );
}

/// Test get account balance
#[test]
fn test_account_balance() {
    let mut stub = KernelStub::with_decimals(0);
    let account0 = stub.create_account();
    let account1 = stub.create_account();
    stub.set_account_balance(account0, RawTokenAmount(245));

    let balance = stub.account_token_balance(&account0);
    assert_eq!(balance, RawTokenAmount(245));

    let balance = stub.account_token_balance(&account1);
    assert_eq!(balance, RawTokenAmount(0));
}

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut stub = KernelStub::with_decimals(0);

    let account = stub.create_account();
    let account_address = stub.account_address(&account);
    let account_by_alias = stub
        .account_by_address(&account_address.get_alias(0).unwrap())
        .unwrap();

    assert_eq!(
        stub.account_index(&account),
        stub.account_index(&account_by_alias)
    );
}
