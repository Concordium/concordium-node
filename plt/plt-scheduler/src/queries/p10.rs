use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::block_state::external::ExternalBlockStateQuery;
use plt_block_state::block_state::p10;
use plt_scheduler_types::types::queries::{
    TokenAccountInfo, TokenAccountState, TokenInfo, TokenState,
};
use plt_scheduler_types::types::tokens::TokenAmount;
use plt_token_module::token_module;

use crate::token_kernel::TokenKernelQueriesImpl;

use super::{QueryTokenInfoError, SchedulerQueries};

impl SchedulerQueries for p10::PltBlockStateP10 {
    fn query_token_info(
        &self,
        external: &impl ExternalBlockStateQuery,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError> {
        query_token_info(&self.tokens, external, token_id)
    }

    fn query_token_account_infos(
        &self,
        external: &impl ExternalBlockStateQuery,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo> {
        query_token_account_infos(&self.tokens, external, account)
    }

    fn query_plt_list(&self) -> Vec<TokenId> {
        self.tokens.plt_list().collect()
    }
}

/// Get the token state associated with the given token id.
pub fn query_token_info(
    tokens: &p10::Tokens,
    external: &impl ExternalBlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenInfo, QueryTokenInfoError> {
    let token = tokens.token_by_id(token_id)?;

    let token_configuration = tokens.token_configuration(token);
    let circulating_supply = tokens.token_circulating_supply(token);

    let total_supply = TokenAmount {
        amount: circulating_supply,
        decimals: token_configuration.decimals,
    };

    let token_module_state = tokens.mutable_token_key_value_state(token);

    let kernel = TokenKernelQueriesImpl {
        tokens,
        external,
        token,
        token_module_state: &token_module_state,
    };

    let module_state = token_module::query_token_module_state(&kernel)?;

    let token_state = TokenState {
        token_module_ref: token_configuration.module_ref,
        decimals: token_configuration.decimals,
        total_supply,
        module_state,
    };

    let token_info = TokenInfo {
        // The token configuration contains the canonical token id specified in the original casing
        token_id: token_configuration.token_id,
        state: token_state,
    };

    Ok(token_info)
}

/// Get the list of tokens on an account
pub fn query_token_account_infos(
    tokens: &p10::Tokens,
    external: &impl ExternalBlockStateQuery,
    account: AccountIndex,
) -> Vec<TokenAccountInfo> {
    external
        .token_account_states(account)
        .into_iter()
        .map(|(token, state)| {
            let token_configuration = tokens.token_configuration(token);

            let token_module_state = tokens.mutable_token_key_value_state(token);

            let kernel = TokenKernelQueriesImpl {
                tokens,
                external,
                token,
                token_module_state: &token_module_state,
            };
            let module_state = token_module::query_token_module_account_state(&kernel, account);

            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };

            let account_state = TokenAccountState {
                balance,
                module_state: Some(module_state),
            };

            TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            }
        })
        .collect()
}
