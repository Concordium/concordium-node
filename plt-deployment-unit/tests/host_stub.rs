use std::collections::HashMap;

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Memo;
use plt_deployment_unit::{
    AmountNotRepresentableError, HostOperations, InsufficientBalanceError, LockedStateKeyError,
    StateKey, StateValue, TokenEventDetails, TokenEventType,
};

/// The deployment host stub providing an implementation of [`HostOperations`] and methods for
/// configuring the state of the host.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HostStub {
    /// List of accounts existing.
    pub accounts: Vec<Account>,
    /// Token managed state.
    pub state: HashMap<StateKey, StateValue>,
    /// Decimal places in token representation.
    pub decimals: u8,
}

/// Internal representation of an Account in [`HostStub`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Account {
    /// The index of the account
    pub index: AccountIndex,
    /// The canonical account address of the account.
    pub address: AccountAddress,
    /// The token balance of the account.
    pub balance: Option<u64>,
}

impl HostStub {
    /// Construct a new `HostStub` with a number of accounts.
    ///
    /// # Example
    ///
    /// ```
    /// let account_address0 = [0u8; 32];
    /// let account_address1 = [1u8; 32];
    /// let host = HostStub::with_accounts([(0, account_address0, None), (1, account_address1, Some(42))]);
    /// assert!(host.account_by_address(account_address1).is_some(), "Account must exist");
    /// ```
    pub fn with_accounts(
        accounts: impl IntoIterator<Item = (AccountIndex, AccountAddress, Option<u64>)>,
    ) -> Self {
        let accounts = accounts
            .into_iter()
            .map(|(index, address, balance)| Account {
                index,
                address,
                balance,
            })
            .collect();

        Self {
            accounts,
            state: HashMap::new(),
            decimals: 0,
        }
    }
}

/// Host stub account object.
///
/// When testing it is the index into the list of accounts tracked by the `HostStub`.
/// Holding
#[derive(Debug, Clone, Copy)]
pub struct AccountStubIndex(usize);

impl HostOperations for HostStub {
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

    fn account_balance(&self, account: &Self::Account) -> u64 {
        self.accounts[account.0].balance.unwrap_or(0)
    }

    fn touch(&mut self, account: &Self::Account) -> bool {
        if self.accounts[account.0].balance.is_some() {
            false
        } else {
            self.accounts[account.0].balance = Some(0);
            true
        }
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        amount: u64,
    ) -> Result<(), AmountNotRepresentableError> {
        if let Some(balance) = self.accounts[account.0].balance {
            if balance > u64::MAX - amount {
                Err(AmountNotRepresentableError)
            } else {
                self.accounts[account.0].balance = Some(balance + amount);
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
        _amount: u64,
    ) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn transfer(
        &mut self,
        _from: &Self::Account,
        _to: &Self::Account,
        _amount: u64,
        _memo: Option<Memo>,
    ) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn circulating_supply(&self) -> u64 {
        todo!()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn get_token_state(&self, key: StateKey) -> Option<StateValue> {
        self.state.get(&key).cloned()
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

    fn tick_energy(&mut self, _energy: Energy) {
        todo!()
    }

    fn log_token_event(&mut self, _event_type: TokenEventType, _event_details: TokenEventDetails) {
        todo!()
    }
}

// Tests for the HostStub

pub const TEST_ACCOUNT0: AccountAddress = AccountAddress([0u8; 32]);
pub const TEST_ACCOUNT1: AccountAddress = AccountAddress([1u8; 32]);
pub const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

#[test]
fn test_account_lookup() {
    let host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);

    let _ = host
        .account_by_address(&TEST_ACCOUNT0)
        .expect("Account is expected to exist");
    let _ = host
        .account_by_address(&TEST_ACCOUNT1)
        .expect("Account is expected to exist");
    assert!(
        host.account_by_address(&TEST_ACCOUNT2).is_none(),
        "Account is not expected to exist"
    );
    // TODO test lookup using alias.

    let _ = host
        .account_by_index(0.into())
        .expect("Account is expected to exist");
    let _ = host
        .account_by_index(1.into())
        .expect("Account is expected to exist");
    assert!(
        host.account_by_index(2.into()).is_none(),
        "Account is not expected to exist"
    );
}

#[test]
fn test_account_balance() {
    let host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, Some(245)),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    {
        let account = host
            .account_by_address(&TEST_ACCOUNT0)
            .expect("Account is expected to exist");
        let balance = host.account_balance(&account);
        assert_eq!(balance, 245);
    }
    {
        let account = host
            .account_by_address(&TEST_ACCOUNT1)
            .expect("Account is expected to exist");
        let balance = host.account_balance(&account);
        assert_eq!(balance, 0);
    }
}

#[test]
fn test_account_canonical_address() {
    let host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, Some(245)),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    {
        let account = host
            .account_by_address(&TEST_ACCOUNT0)
            .expect("Account is expected to exist");
        let balance = host.account_balance(&account);
        assert_eq!(balance, 245);
    }
    {
        let account = host
            .account_by_address(&TEST_ACCOUNT1)
            .expect("Account is expected to exist");
        let balance = host.account_balance(&account);
        assert_eq!(balance, 0);
    }
}
