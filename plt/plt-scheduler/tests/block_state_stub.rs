// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenAmount, TokenId, TokenModuleInitializationParameters,
};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_scheduler::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, RawTokenAmountDelta, TokenConfiguration, TokenNotFoundByIdError,
    UnderOrOverflowError,
};
use plt_scheduler::scheduler;
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};
use plt_token_module::{TOKEN_MODULE_REF, token_module};
use std::collections::HashMap;

/// Block state stub providing an implementation of [`BlockStateQuery`] and methods for
/// configuring the state of the block state.
#[derive(Debug, Default)]
pub struct BlockStateStub {
    /// List of tokens in the stub
    tokens: Vec<Token>,
    /// List of accounts in the stub.
    accounts: Vec<Account>,
    /// PLT update instruction sequence number
    plt_update_instruction_sequence_number: u64,
}

/// Internal representation of a token in [`BlockStateStub`].
#[derive(Debug)]
struct Token {
    /// Module state
    module_state: StubTokenModuleState,
    /// Token configuration
    configuration: TokenConfiguration,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
}

/// Representation of module state in the stub
#[derive(Debug, Clone, Default)]
pub struct StubTokenModuleState {
    /// Token module managed state.
    state: HashMap<ModuleStateKey, ModuleStateValue>,
}

/// Internal representation of an account in [`BlockStateStub`].
#[derive(Debug)]
pub struct Account {
    /// The index of the account
    index: AccountIndex,
    /// The canonical account address of the account.
    address: AccountAddress,
    /// Tokens the account is holding
    tokens: HashMap<TokenStubIndex, AccountToken>,
}

/// Internal representation of a token in an account.
#[derive(Debug, Default)]
struct AccountToken {
    /// Account balance
    balance: RawTokenAmount,
}

impl BlockStateStub {
    /// Create block state stub
    pub fn new() -> Self {
        Self {
            tokens: Default::default(),
            accounts: Default::default(),
            plt_update_instruction_sequence_number: 0,
        }
    }

    /// Create account in the stub and return stub representation of the account.
    pub fn create_account(&mut self) -> AccountStubIndex {
        let index = self.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index: account_index,
            address,
            tokens: Default::default(),
        };
        let stub_index = AccountStubIndex(index);
        self.accounts.push(account);

        stub_index
    }

    /// Create and initialize token in the stub and return stub representation of the token, together with
    /// the governance account.
    pub fn create_and_init_token(
        &mut self,
        token_id: TokenId,
        params: TokenInitTestParams,
        decimals: u8,
        initial_supply: Option<RawTokenAmount>,
    ) -> (TokenStubIndex, AccountStubIndex) {
        let gov_account = self.create_account();
        let gov_holder_account = CborHolderAccount::from(self.accounts[gov_account.0].address);
        let metadata = MetadataUrl::from("https://plt.token".to_string());
        let parameters = TokenModuleInitializationParameters {
            name: Some("Protocol-level token".to_owned()),
            metadata: Some(metadata.clone()),
            governance_account: Some(gov_holder_account.clone()),
            allow_list: params.allow_list,
            deny_list: params.deny_list,
            initial_supply: initial_supply.map(|raw| TokenAmount::from_raw(raw.0, decimals)),
            mintable: params.mintable,
            burnable: params.burnable,
            additional: Default::default(),
        };
        let initialization_parameters = cbor::cbor_encode(&parameters).into();

        let payload = UpdatePayload::CreatePlt(CreatePlt {
            token_id: token_id.clone(),
            token_module: TOKEN_MODULE_REF,
            decimals,
            initialization_parameters,
        });
        scheduler::execute_update_instruction(self, payload).expect("create and initialize token");

        let token = self.token_by_id(&token_id).expect("created token");

        (token, gov_account)
    }

    /// Add amount to account balance in the stub
    pub fn increment_account_balance(
        &mut self,
        account: AccountStubIndex,
        token: TokenStubIndex,
        balance: RawTokenAmount,
    ) {
        self.tokens[token.0].circulating_supply.0 += balance.0;
        self.accounts[account.0]
            .tokens
            .entry(token)
            .or_default()
            .balance
            .0 += balance.0;
    }

    /// Return protocol-level token update instruction sequence number
    pub fn plt_update_instruction_sequence_number(&self) -> u64 {
        self.plt_update_instruction_sequence_number
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

/// Stub account object.
/// When testing it is the index into the list of accounts tracked by the `KernelStub`.
#[derive(Debug, Clone, Copy)]
pub struct AccountStubIndex(usize);

/// Stub token object.
/// When testing it is the index into the list of accounts tracked by the `KernelStub`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TokenStubIndex(usize);

impl BlockStateQuery for BlockStateStub {
    type MutableTokenModuleState = StubTokenModuleState;
    type Account = AccountStubIndex;
    type Token = TokenStubIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        self.tokens
            .iter()
            .map(|token| token.configuration.token_id.clone())
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        self.tokens
            .iter()
            .enumerate()
            .find_map(|(i, token)| {
                if token
                    .configuration
                    .token_id
                    .as_ref()
                    .eq_ignore_ascii_case(token_id.as_ref())
                {
                    Some(TokenStubIndex(i))
                } else {
                    None
                }
            })
            .ok_or(TokenNotFoundByIdError(token_id.clone()))
    }

    fn mutable_token_module_state(&self, token: &Self::Token) -> StubTokenModuleState {
        self.tokens[token.0].module_state.clone()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        self.tokens[token.0].configuration.clone()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        self.tokens[token.0].circulating_supply
    }

    fn lookup_token_module_state_value(
        &self,
        token_module_state: &StubTokenModuleState,
        key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        token_module_state.state.get(key).cloned()
    }

    fn update_token_module_state_value(
        &self,
        token_module_state: &mut StubTokenModuleState,
        key: &ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        if let Some(value) = value {
            token_module_state.state.insert(key.clone(), value);
        } else {
            token_module_state.state.remove(key);
        }
    }

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
        if self.accounts.get(index.index as usize).is_some() {
            Ok(AccountStubIndex(index.index as usize))
        } else {
            Err(AccountNotFoundByIndexError(index))
        }
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.accounts[account.0].index
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.accounts[account.0].address
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        self.accounts[account.0]
            .tokens
            .get(token)
            .map(|token| token.balance)
            .unwrap_or_default()
    }
}

impl BlockStateOperations for BlockStateStub {
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        self.tokens[token.0].circulating_supply = circulating_supply;
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        let stub_index = TokenStubIndex(self.tokens.len());
        let token = Token {
            module_state: Default::default(),
            configuration,
            circulating_supply: Default::default(),
        };
        self.tokens.push(token);
        stub_index
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), UnderOrOverflowError> {
        let balance = &mut self.accounts[account.0]
            .tokens
            .entry(*token)
            .or_default()
            .balance;
        match amount_delta {
            RawTokenAmountDelta::Add(add) => {
                balance.0 = balance.0.checked_add(add.0).ok_or(UnderOrOverflowError)?;
            }
            RawTokenAmountDelta::Subtract(subtract) => {
                balance.0 = balance
                    .0
                    .checked_sub(subtract.0)
                    .ok_or(UnderOrOverflowError)?;
            }
        }
        Ok(())
    }

    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account) {
        self.accounts[account.0].tokens.entry(*token).or_default();
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.plt_update_instruction_sequence_number += 1;
    }

    fn set_token_module_state(
        &mut self,
        token: &Self::Token,
        mutable_token_module_state: Self::MutableTokenModuleState,
    ) {
        self.tokens[token.0].module_state = mutable_token_module_state;
    }
}
