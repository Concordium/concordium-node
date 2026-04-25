use crate::block_state::blob_store::BlobStoreLoad;
use crate::block_state::external::ExternalBlockStateOperations;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::BlockStateResult;
use crate::entity::block_state::p9::BlockStateP9;
use crate::entity::protocol_level_tokens::{PlTokenEntity, SupportsPlTokens, TokenConfiguration};
use concordium_base::protocol_level_tokens::TokenId;

/// P10 block state.
#[derive(Debug)]
pub struct BlockStateP10<'a, L, E> {
    /// P9 block state
    p9_block_state: BlockStateP9<'a, L, E>,
}

impl<L: BlobStoreLoad, E: ExternalBlockStateOperations> SupportsPlTokens
    for BlockStateP10<'_, L, E>
{
    fn plt_list(&self) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
        self.p9_block_state.plt_list()
    }

    fn token_by_id(&self, token_id: &TokenId) -> BlockStateResult<Option<PlTokenEntity<'_, L>>> {
        self.p9_block_state.token_by_id(token_id)
    }

    fn create_token(
        &mut self,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<PlTokenEntity<'_, impl BlobStoreLoad>> {
        self.p9_block_state.create_token(configuration)
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.p9_block_state
            .increment_plt_update_instruction_sequence_number()
    }
}
