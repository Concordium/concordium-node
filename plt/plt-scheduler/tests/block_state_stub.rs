use std::collections::HashMap;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{CborHolderAccount, MetadataUrl, TokenId, TokenModuleInitializationParameters};
use plt_scheduler::block_state_interface::{
    BlockStateQuery,  TokenConfiguration, TokenNotFoundByIdError,
};
use plt_scheduler::TOKEN_MODULE_REF;
use plt_token_module::token_kernel_interface::{
    ModuleStateKey, ModuleStateValue
    , RawTokenAmount,
};
use plt_token_module::token_module;

/// Block state stub providing an implementation of [`BlockStateQuery`] and methods for
/// configuring the state of the block state.
#[derive(Debug)]
pub struct BlockStateStub {
    /// List of tokens in the stub
    tokens: Vec<Token>,
    /// List of accounts in the stub.
    accounts: Vec<Account>,
}

/// Internal representation of a token in [`BlockStateStub`].
#[derive(Debug)]
struct Token {
    module_state: StubTokenModuleState,
    /// Token configuration
    configuration: TokenConfiguration,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
}

#[derive(Debug, Clone, Default)]
pub struct StubTokenModuleState {
    /// Token module managed state.
    module_state: HashMap<ModuleStateKey, ModuleStateValue>,
}

/// Internal representation of an account in [`BlockStateStub`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Account {
    /// The index of the account
    pub index: AccountIndex,
    /// The canonical account address of the account.
    pub address: AccountAddress,
    // // The token balance of the account.
    // pub balance: Option<RawTokenAmount>,
}

#[allow(unused)]
impl BlockStateStub {
    /// Create block state stub
    pub fn new() -> Self {
        Self {
            tokens: Default::default(),
            accounts: Default::default(),
        }
    }

    /// Create account in the stub and return stub representation of the account.
    pub fn create_account(&mut self) -> AccountStubIndex {
        let index = self.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index:account_index,
            address,
        };
        let stub_index = AccountStubIndex(index);
        self.accounts.push(account);

        stub_index
    }

    /// Initialize token in the stub and return stub representation of the token.
    pub fn init_token(&mut self, params: TokenInitTestParams, decimals: u8) -> TokenStubIndex {
        // Add the token to the stub
        let stub_index = TokenStubIndex(self.tokens.len());
        let token_id = format!("tokenid{}", stub_index.0).parse().unwrap();
        let token = Token {
            module_state: Default::default(),
            configuration: TokenConfiguration {
                token_id,
                module_ref: TOKEN_MODULE_REF,
                decimals,
            },
            circulating_supply: Default::default(),
        };
        self.tokens.push(token);

        // Initialize the token in the token module
        let gov_account = self.create_account();
        let gov_holder_account =
            CborHolderAccount::from(self.accounts[gov_account.0].address);
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

        // todo initialize token in module
        // let encoded_parameters = cbor::cbor_encode(&parameters).into();
        // token_module::initialize_token(self, encoded_parameters).expect("initialize token");

        stub_index
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

/// Block state stub account object.
/// When testing it is the index into the list of accounts tracked by the `KernelStub`.
#[derive(Debug, Clone, Copy)]
pub struct AccountStubIndex(usize);

/// Block state stub token object.
/// When testing it is the index into the list of accounts tracked by the `KernelStub`.
#[derive(Debug, Clone, Copy)]
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
                if token.configuration.token_id == *token_id {
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
        token_module_state.module_state.get(key).cloned()
    }

    fn update_token_module_state_value(
        &self,
        token_module_state: &mut StubTokenModuleState,
        key: &ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        if let Some(value) = value {
            token_module_state.module_state.insert(key.clone(), value);
        } else {
            token_module_state.module_state.remove(key);
        }
    }
}

