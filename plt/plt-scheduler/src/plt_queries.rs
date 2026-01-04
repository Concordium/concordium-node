use crate::TOKEN_MODULE_REF;
use crate::block_state_interface::{BlockStateQuery, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenAmount, TokenId, TokenModuleRef};
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, RawTokenAmount, StateKey,
    StateValue, TokenKernelQueries,
};
use plt_token_module::token_module;
use plt_token_module::token_module::QueryTokenModuleError;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn plt_list(block_state: &impl BlockStateQuery) -> Vec<TokenId> {
    block_state.get_plt_list().collect()
}

/// Token state at the block level
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenState {
    /// The reference of the module implementing this token.
    pub token_module_ref: TokenModuleRef,
    /// Number of decimals in the decimal number representation of amounts.
    pub decimals: u8,
    /// The total available token supply.
    pub total_supply: TokenAmount,
    /// Token module specific state, such as token name, feature flags, meta
    /// data.
    pub module_state: RawCbor,
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenStateError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("The token does not exist: {0}")]
    TokenDoesNotExist(String), // todo ar implement Display on TokenId and replace String with TokenId
}

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn token_state(
    block_state: &impl BlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenState, QueryTokenStateError> {
    let token_index = block_state
        .get_token_index(token_id)
        .ok_or_else(|| QueryTokenStateError::TokenDoesNotExist(token_id.as_ref().to_string()))?;

    let token_configuration = block_state.get_token_configuration(token_index);
    let circulating_supply = block_state.get_token_circulating_supply(token_index);

    let total_supply = TokenAmount::from_raw(circulating_supply.0, token_configuration.decimals);

    let kernel = TokenKernelQueriesImpl {
        block_state,
        token_index,
    };

    let module_state = token_module::query_token_module_state(&kernel)?;

    let token_state = TokenState {
        token_module_ref: TOKEN_MODULE_REF,
        decimals: token_configuration.decimals,
        total_supply,
        module_state,
    };

    Ok(token_state)
}

struct TokenKernelQueriesImpl<'a, BSQ: BlockStateQuery> {
    block_state: &'a BSQ,
    token_index: TokenIndex,
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, BSQ> {
    type Account = ();

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        todo!()
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        todo!()
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        todo!()
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        todo!()
    }

    fn account_balance(&self, account: &Self::Account) -> RawTokenAmount {
        todo!()
    }

    fn circulating_supply(&self) -> RawTokenAmount {
        self.block_state
            .get_token_circulating_supply(self.token_index)
    }

    fn decimals(&self) -> u8 {
        self.block_state
            .get_token_configuration(self.token_index)
            .decimals
    }

    fn get_token_state(&self, key: StateKey) -> Option<StateValue> {
        // todo ar get state from
        todo!()
    }
}

// todo ar test + query account token
