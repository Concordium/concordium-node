use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::block_state::external::ExternalBlockStateQuery;
use plt_block_state::block_state::p11;
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenInfo};

use super::{QueryTokenInfoError, SchedulerQueries};

impl SchedulerQueries for p11::PltBlockStateP11 {
    fn query_token_info(
        &self,
        external: &impl ExternalBlockStateQuery,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError> {
        super::p10::query_token_info(&self.tokens, external, token_id)
    }

    fn query_token_account_infos(
        &self,
        external: &impl ExternalBlockStateQuery,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo> {
        super::p10::query_token_account_infos(&self.tokens, external, account)
    }

    fn query_plt_list(&self) -> Vec<TokenId> {
        self.tokens.plt_list().collect()
    }
}
