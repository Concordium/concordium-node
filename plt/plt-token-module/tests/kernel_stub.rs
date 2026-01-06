use std::collections::{HashMap, VecDeque};

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError,  ModuleStateKey, ModuleStateValue,
    OutOfEnergyError, RawTokenAmount, TokenKernelOperations, TokenKernelQueries, TokenModuleEvent,
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

#[allow(unused)]
impl KernelStub {
    /// Create new kernel stub
    pub fn new(decimals: u8) -> Self {
        Self {
            accounts: vec![],
            state: Default::default(),
            decimals,
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
    pub fn set_account_balance(&mut self, account: AccountStubIndex, balance: RawTokenAmount) {
        self.accounts[account.0].balance = Some(balance);
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

#[allow(unused)]
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

    fn circulating_supply(&self) -> RawTokenAmount {
        todo!()
    }

    fn decimals(&self) -> u8 {
        self.decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
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
        let balance = self.accounts[account.0].balance.get_or_insert_default();
        if *balance > RawTokenAmount(u64::MAX - amount.0) {
            Err(AmountNotRepresentableError)
        } else {
            *balance = RawTokenAmount(balance.0 + amount.0);
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
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), InsufficientBalanceError> {
        if self.account_token_balance(from).0 < amount.0 {
            return Err(InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            });
        }

        self.transfers.push_back((*from, *to, amount, memo));

        if from.0 == to.0 {
            return Ok(());
        }

        let [from, to] = self.accounts.get_disjoint_mut([from.0, to.0]).unwrap();
        let from_balance = from.balance.get_or_insert_default();
        let to_balance = to.balance.get_or_insert_default();
        *from_balance = RawTokenAmount(from_balance.0 - amount.0);
        *to_balance = RawTokenAmount(to_balance.0 + amount.0);
        Ok(())
    }

    fn set_token_module_state_value(
        &mut self,
        key: ModuleStateKey,
        value: Option<ModuleStateValue>,
    )  {
        match value {
            None => self.state.remove(&key).is_some(),
            Some(value) => self.state.insert(key, value).is_some(),
        };
    }

    fn tick_energy(&mut self, _energy: Energy) -> Result<(), OutOfEnergyError> {
        todo!()
    }

    fn log_token_event(&mut self, _event: TokenModuleEvent) {
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
        stub.account_by_address(&TEST_ACCOUNT2).is_err(),
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
        stub.account_by_index(2.into()).is_err(),
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

    let balance = stub.account_token_balance(&account0);
    assert_eq!(balance, RawTokenAmount(245));

    let balance = stub.account_token_balance(&account1);
    assert_eq!(balance, RawTokenAmount(0));
}

