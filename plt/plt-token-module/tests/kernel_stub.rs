// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use std::collections::{HashMap, VecDeque};

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, OutOfEnergyError,
    TransactionExecution,
};
use plt_token_module::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, ModuleStateKey, ModuleStateValue,
    RawTokenAmount, TokenBurnError, TokenKernelOperations, TokenKernelQueries, TokenMintError,
    TokenModuleEvent, TokenStateInvariantError, TokenTransferError,
};
use plt_token_module::token_module;

/// Token kernel stub providing an implementation of [`TokenKernelOperations`] and methods for
/// configuring the state of the kernel.
#[derive(Debug)]
pub struct KernelStub {
    /// List of accounts existing.
    accounts: Vec<Account>,
    /// Token module managed state.
    state: HashMap<ModuleStateKey, ModuleStateValue>,
    /// Decimal places in token representation.
    decimals: u8,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
    /// Counter for creating accounts in the stub
    next_account_index: AccountIndex,
    /// Transfers
    transfers: VecDeque<(
        AccountStubIndex,
        AccountStubIndex,
        RawTokenAmount,
        Option<Memo>,
    )>,
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

impl KernelStub {
    /// Create new kernel stub
    pub fn with_decimals(decimals: u8) -> Self {
        Self {
            accounts: vec![],
            state: Default::default(),
            decimals,
            circulating_supply: RawTokenAmount::default(),
            next_account_index: AccountIndex { index: 0 },
            transfers: Default::default(),
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
        let index = self.next_account_index;
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.index.to_be_bytes());
        let account = Account {
            index,
            address,
            balance: None,
        };
        let stub_index = AccountStubIndex(self.accounts.len());
        self.accounts.push(account);

        self.next_account_index.index += 1;

        stub_index
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
    ) -> Option<(
        AccountStubIndex,
        AccountStubIndex,
        RawTokenAmount,
        Option<Memo>,
    )> {
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
                if account.address == *address {
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
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.accounts
            .iter()
            .enumerate()
            .find_map(|(i, account)| {
                if account.index == index {
                    Some(AccountStubIndex(i))
                } else {
                    None
                }
            })
            .ok_or(AccountNotFoundByIndexError(index))
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.accounts[account.0].index
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.accounts[account.0].address
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.accounts[account.0].balance.unwrap_or_default()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.state.get(&key).cloned()
    }
}

impl TokenKernelOperations for KernelStub {
    fn touch_account(&mut self, account: &Self::Account) {
        self.accounts[account.0].balance.get_or_insert_default();
    }

    fn mint(
        &mut self,
        account: &Self::Account,
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
        to: &Self::Account,
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

    fn set_token_module_state_value(
        &mut self,
        key: ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        match value {
            None => self.state.remove(&key).is_some(),
            Some(value) => self.state.insert(key, value).is_some(),
        };
    }

    fn log_token_event(&mut self, _event: TokenModuleEvent) {
        todo!()
    }
}

/// Token kernel transaction execution context for test.
#[derive(Debug)]
pub struct TransactionExecutionTestImpl {
    sender: AccountStubIndex,
}

impl TransactionExecutionTestImpl {
    pub fn with_sender(sender: AccountStubIndex) -> Self {
        Self { sender }
    }
}

impl TransactionExecution for TransactionExecutionTestImpl {
    type Account = AccountStubIndex;

    fn sender_account(&self) -> Self::Account {
        self.sender
    }

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        todo!()
    }
}

// Tests for the kernel stub

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// Test lookup account address and account from address
#[test]
fn test_account_lookup_address() {
    let mut stub = KernelStub::with_decimals(0);
    let account = stub.create_account();

    let address = stub.account_canonical_address(&account);
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
