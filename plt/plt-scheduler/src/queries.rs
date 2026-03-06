//! Implementation of queries related to protocol-level tokens.

use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::block_state::external::ExternalBlockStateQuery;
use plt_block_state::block_state_interface::TokenNotFoundByIdError;
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenInfo};
use plt_token_module::token_module::QueryTokenModuleError;

mod p10;
mod p11;

pub trait SchedulerQueries {
    /// Get the token state associated with the given token id.
    fn query_token_info(
        &self,
        external: &impl ExternalBlockStateQuery,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError>;

    /// Get the list of tokens on an account
    fn query_token_account_infos(
        &self,
        external: &impl ExternalBlockStateQuery,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo>;

    /// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
    ///
    /// If the protocol version does not support protocol-level tokens, this will return the empty
    /// list.
    fn query_plt_list(&self) -> Vec<TokenId>;
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenInfoError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("{0}")]
    TokenDoesNotExist(#[from] TokenNotFoundByIdError),
}
