use crate::block_state::blob_store::BlobStoreLoad;
use crate::block_state::external::ExternalBlockStateOperations;
use crate::block_state::hash::Hashable;
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::BlockStateResult;
use crate::entity::protocol_level_tokens::{
    PlTokenEntity, PlTokens, SupportsPlTokens, TokenConfiguration,
};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use concordium_base::protocol_level_tokens::TokenId;

/// P9 block state.
#[derive(Debug)]
pub struct BlockStateP9<'a, L, E> {
    /// Persistent block state.
    persistent: OwnedOrBorrowed<'a, PersistentBlockStateP9>,
    /// Blob store loader.
    store_loader: &'a L,
    /// Part of block state that is managed externally.
    external: E,
}

impl<'a, L: BlobStoreLoad, E> BlockStateP9<'a, L, E> {
    fn pl_tokens(&self) -> PlTokens<'a, L> {
        PlTokens::new(
            OwnedOrBorrowed::Borrowed(&self.persistent.tokens),
            self.store_loader,
        )
    }
}

impl<L: BlobStoreLoad, E: ExternalBlockStateOperations> SupportsPlTokens
    for BlockStateP9<'_, L, E>
{
    fn plt_list(&self) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
        self.pl_tokens().plt_list()
    }

    fn token_by_id(
        &self,
        token_id: &TokenId,
    ) -> BlockStateResult<Option<PlTokenEntity<'_, impl BlobStoreLoad>>> {
        self.pl_tokens().token_by_id(token_id)
    }

    fn create_token(
        &mut self,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<PlTokenEntity<'_, impl BlobStoreLoad>> {
        let mut pl_tokens = self.pl_tokens();
        let token = pl_tokens.create_token(configuration)?;
        self.persistent.to_mut().tokens = pl_tokens.into_persistent();
        Ok(token)
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.external.increment_plt_update_sequence_number()
    }
}
