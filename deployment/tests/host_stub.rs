use concordium_contracts_common::AccountAddress;
use deployment::HostOperations;

/// The deployment host stub providing an implementation of [`HostOperations`] and methods for
/// configuring the state of the host.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HostStub {
    /// List of accounts existing.
    accounts: Vec<Account>,
}

/// Internal representation of an Account in [`HostStub`].
#[derive(Debug, Clone, PartialEq, Eq)]
struct Account {
    /// The index of the account
    index: deployment::AccountIndex,
    /// The canonical account address of the account.
    address: AccountAddress,
    /// The token balance of the account.
    balance: Option<u64>,
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
        accounts: impl IntoIterator<Item = (deployment::AccountIndex, AccountAddress, Option<u64>)>,
    ) -> Self {
        let accounts = accounts
            .into_iter()
            .map(|(index, address, balance)| Account {
                index,
                address,
                balance,
            })
            .collect();

        Self { accounts }
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

    fn account_by_address(&self, address: AccountAddress) -> Option<Self::Account> {
        self.accounts.iter().enumerate().find_map(|(i, account)| {
            // TODO resolve an account alias as well here.
            if account.address == address {
                Some(AccountStubIndex(i))
            } else {
                None
            }
        })
    }

    fn account_by_index(&self, index: deployment::AccountIndex) -> Option<Self::Account> {
        self.accounts.iter().enumerate().find_map(|(i, account)| {
            if account.index == index {
                Some(AccountStubIndex(i))
            } else {
                None
            }
        })
    }

    fn account_index(&self, account: Self::Account) -> deployment::AccountIndex {
        self.accounts[account.0].index
    }

    fn account_canonical_address(&self, account: Self::Account) -> AccountAddress {
        self.accounts[account.0].address
    }

    fn account_balance(&self, account: Self::Account) -> u64 {
        self.accounts[account.0].balance.unwrap_or(0)
    }

    fn touch(&mut self, account: Self::Account) -> bool {
        if self.accounts[account.0].balance.is_some() {
            false
        } else {
            self.accounts[account.0].balance = Some(0);
            true
        }
    }

    fn mint(
        &mut self,
        _account: Self::Account,
        _amount: u64,
    ) -> Result<(), deployment::AmountNotRepresentableError> {
        todo!()
    }

    fn burn(
        &mut self,
        _account: Self::Account,
        _amount: u64,
    ) -> Result<(), deployment::InsufficientBalanceError> {
        todo!()
    }

    fn transfer(
        &mut self,
        _from: Self::Account,
        _to: Self::Account,
        _amount: u64,
        _memo: Option<deployment::Memo>,
    ) -> Result<(), deployment::InsufficientBalanceError> {
        todo!()
    }

    fn circulating_supply(&self) -> u64 {
        todo!()
    }

    fn decimals(&self) -> u8 {
        todo!()
    }

    fn get_token_state(&self, _key: deployment::StateKey) -> Option<deployment::StateValue> {
        todo!()
    }

    fn set_token_state(
        &mut self,
        _key: deployment::StateKey,
        _value: Option<deployment::StateValue>,
    ) -> Result<bool, deployment::LockedStateKeyError> {
        todo!()
    }

    fn tick_energy(&mut self, _energy: deployment::Energy) {
        todo!()
    }

    fn log_token_event(
        &mut self,
        _event_type: deployment::TokenEventType,
        _event_details: deployment::TokenEventDetails,
    ) {
        todo!()
    }
}

// Tests for the HostStub

const TEST_ACCOUNT0: AccountAddress = AccountAddress([0u8; 32]);
const TEST_ACCOUNT1: AccountAddress = AccountAddress([1u8; 32]);
const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

#[test]
fn test_account_lookup() {
    let host = HostStub::with_accounts([(0, TEST_ACCOUNT0, None), (1, TEST_ACCOUNT1, None)]);

    let _ = host
        .account_by_address(TEST_ACCOUNT0)
        .expect("Account is expected to exist");
    let _ = host
        .account_by_address(TEST_ACCOUNT1)
        .expect("Account is expected to exist");
    assert!(
        host.account_by_address(TEST_ACCOUNT2).is_none(),
        "Account is not expected to exist"
    );
    // TODO test lookup using alias.

    let _ = host
        .account_by_index(0)
        .expect("Account is expected to exist");
    let _ = host
        .account_by_index(1)
        .expect("Account is expected to exist");
    assert!(
        host.account_by_index(2).is_none(),
        "Account is not expected to exist"
    );
}

#[test]
fn test_account_balance() {
    let host = HostStub::with_accounts([(0, TEST_ACCOUNT0, Some(245)), (1, TEST_ACCOUNT1, None)]);
    {
        let account = host
            .account_by_address(TEST_ACCOUNT0)
            .expect("Account is expected to exist");
        let balance = host.account_balance(account);
        assert_eq!(balance, 245);
    }
    {
        let account = host
            .account_by_address(TEST_ACCOUNT1)
            .expect("Account is expected to exist");
        let balance = host.account_balance(account);
        assert_eq!(balance, 0);
    }
}

#[test]
fn test_account_canonical_address() {
    let host = HostStub::with_accounts([(0, TEST_ACCOUNT0, Some(245)), (1, TEST_ACCOUNT1, None)]);
    {
        let account = host
            .account_by_address(TEST_ACCOUNT0)
            .expect("Account is expected to exist");
        let balance = host.account_balance(account);
        assert_eq!(balance, 245);
    }
    {
        let account = host
            .account_by_address(TEST_ACCOUNT1)
            .expect("Account is expected to exist");
        let balance = host.account_balance(account);
        assert_eq!(balance, 0);
    }
}
