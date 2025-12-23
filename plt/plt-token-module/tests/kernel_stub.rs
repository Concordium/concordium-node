use std::collections::HashMap;

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenModuleEventType;
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::{
    AmountNotRepresentableError, InsufficientBalanceError, LockedStateKeyError, OutOfEnergyError,
    RawTokenAmount, StateKey, StateValue, TokenKernelOperations, TokenKernelQueries,
};

/// Token kernel stub providing an implementation of [`TokenKernelOperations`] and methods for
/// configuring the state of the host.
#[derive(Debug)]
pub struct KernelStub {
    /// List of accounts existing.
    accounts: Vec<Account>,
    /// Token managed state.
    pub state: HashMap<StateKey, StateValue>,
    /// Decimal places in token representation.
    decimals: u8,
    next_account_index: AccountIndex,
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
    /// Create
    pub fn new(decimals: u8) -> Self {
        Self {
            accounts: vec![],
            state: Default::default(),
            decimals,
            next_account_index: AccountIndex { index: 0 },
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
    pub fn set_account_balance(&mut self, account: AccountStubIndex, balance: RawTokenAmount) {
        self.accounts
            .get_mut(account.0)
            .expect("account in stub")
            .balance = Some(balance);
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

    fn account_by_address(&self, address: &AccountAddress) -> Option<Self::Account> {
        self.accounts.iter().enumerate().find_map(|(i, account)| {
            // TODO resolve an account alias as well here.
            if account.address == *address {
                Some(AccountStubIndex(i))
            } else {
                None
            }
        })
    }

    fn account_by_index(&self, index: AccountIndex) -> Option<Self::Account> {
        self.accounts.iter().enumerate().find_map(|(i, account)| {
            if account.index == index {
                Some(AccountStubIndex(i))
            } else {
                None
            }
        })
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.accounts[account.0].index
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.accounts[account.0].address
    }

    fn account_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.accounts[account.0].balance.unwrap_or_default()
    }

    fn circulating_supply(&self) -> RawTokenAmount {
        todo!()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn get_token_state(&self, key: StateKey) -> Option<StateValue> {
        self.state.get(&key).cloned()
    }
}

impl TokenKernelOperations for KernelStub {
    fn touch(&mut self, account: &Self::Account) -> bool {
        if self.accounts[account.0].balance.is_some() {
            false
        } else {
            self.accounts[account.0].balance = Some(RawTokenAmount::default());
            true
        }
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), AmountNotRepresentableError> {
        if let Some(balance) = self.accounts[account.0].balance {
            if balance > RawTokenAmount(u64::MAX - amount.0) {
                Err(AmountNotRepresentableError)
            } else {
                self.accounts[account.0].balance = Some(RawTokenAmount(balance.0 + amount.0));
                Ok(())
            }
        } else {
            self.accounts[account.0].balance = Some(amount);
            Ok(())
        }
    }

    fn burn(
        &mut self,
        _account: &Self::Account,
        _amount: RawTokenAmount,
    ) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn transfer(
        &mut self,
        _from: &Self::Account,
        _to: &Self::Account,
        _amount: RawTokenAmount,
        _memo: Option<Memo>,
    ) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn set_token_state(
        &mut self,
        key: StateKey,
        value: Option<StateValue>,
    ) -> Result<bool, LockedStateKeyError> {
        let res = match value {
            None => self.state.remove(&key).is_some(),
            Some(value) => self.state.insert(key, value).is_some(),
        };
        Ok(res)
    }

    fn tick_energy(&mut self, _energy: Energy) -> Result<(), OutOfEnergyError> {
        todo!()
    }

    fn log_token_event(&mut self, _event: TokenModuleEventType) {
        todo!()
    }
}

// Tests for the kernel stub

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// Test lookup account address and account from address
#[test]
fn test_account_lookup_address() {
    let mut stub = KernelStub::new(0);
    let account = stub.create_account();

    let address = stub.account_canonical_address(&account);
    stub.account_by_address(&address)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_address(&TEST_ACCOUNT2).is_none(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index
#[test]
fn test_account_lookup_index() {
    let mut stub = KernelStub::new(0);
    let account = stub.create_account();

    let index = stub.account_index(&account);
    stub.account_by_index(index)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_index(2.into()).is_none(),
        "Account is not expected to exist"
    );
}

/// Test get account balance
#[test]
fn test_account_balance() {
    let mut stub = KernelStub::new(0);
    let account0 = stub.create_account();
    let account1 = stub.create_account();
    stub.set_account_balance(account0, RawTokenAmount(245));

    let balance = stub.account_balance(&account0);
    assert_eq!(balance, RawTokenAmount(245));

    let balance = stub.account_balance(&account1);
    assert_eq!(balance, RawTokenAmount(0));
}

#[test]
fn test_account_lookup_canonical_address() {
    // TODO test lookup using alias.
}
