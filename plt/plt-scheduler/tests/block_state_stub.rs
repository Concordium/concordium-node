use std::collections::{HashMap, VecDeque};

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenId, TokenModuleInitializationParameters,
};
use concordium_base::transactions::Memo;
use plt_scheduler::block_state_interface::{
    BlockStateQuery, MutableTokenModuleState, TokenConfiguration, TokenNotFoundByIdError,
};
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError, LockedStateKeyError, ModuleStateKey, ModuleStateValue,
    OutOfEnergyError, RawTokenAmount, TokenKernelOperations, TokenKernelQueries, TokenModuleEvent,
};
use plt_token_module::token_module;

/// Block state stub providing an implementation of [`BlockStateQuery`] and methods for
/// configuring the state of the block state.
#[derive(Debug)]
pub struct BlockStateStub {
    tokens: Vec<Token>,
    // /// Decimal places in token representation.
    // decimals: u8,
}

/// Block state stub providing an implementation of [`BlockStateQuery`] and methods for
/// configuring the state of the block state.
#[derive(Debug)]
struct Token {
    /// Token module managed state.
    module_state: HashMap<ModuleStateKey, ModuleStateValue>,
    /// Token configuration
    configuration: TokenConfiguration,
    /// Circulating supply
    circulating_supply: RawTokenAmount,
}

#[allow(unused)]
impl BlockStateStub {
    /// Create block state stub
    pub fn new() -> Self {
        Self {
            tokens: Default::default(),
        }
    }

    // /// Initialize token and return the governance account
    // pub fn init_token(&mut self, params: TokenInitTestParams) -> AccountStubIndex {
    //     let gov_account = self.create_account();
    //     let gov_holder_account =
    //         CborHolderAccount::from(self.account_canonical_address(&gov_account));
    //     let metadata = MetadataUrl::from("https://plt.token".to_string());
    //     let parameters = TokenModuleInitializationParameters {
    //         name: Some("Protocol-level token".to_owned()),
    //         metadata: Some(metadata.clone()),
    //         governance_account: Some(gov_holder_account.clone()),
    //         allow_list: params.allow_list,
    //         deny_list: params.deny_list,
    //         initial_supply: None,
    //         mintable: params.mintable,
    //         burnable: params.burnable,
    //         additional: Default::default(),
    //     };
    //     let encoded_parameters = cbor::cbor_encode(&parameters).into();
    //     token_module::initialize_token(self, encoded_parameters).expect("initialize token");
    //     gov_account
    // }
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

    fn mutable_token_module_state(&self, token: &Self::Token) -> MutableTokenModuleState {
        todo!()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        self.tokens[token.0].configuration.clone()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        self.tokens[token.0].circulating_supply
    }

    fn lookup_token_module_state_value(
        &self,
        token_module_state: &MutableTokenModuleState,
        key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        // todo are token module state associated type?
        todo!()
    }

    fn update_token_module_state_value(
        &self,
        token_module_state: &MutableTokenModuleState,
        key: &ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        todo!()
    }
}

// Tests for the kernel stub

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);
