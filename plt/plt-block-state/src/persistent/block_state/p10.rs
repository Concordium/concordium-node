use crate::failure::BlockStateResult;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::migration::Migrate;

/// P10 block state.
pub type PersistentBlockStateP10 = PersistentBlockStateP9;

impl Migrate<PersistentBlockStateP10> for PersistentBlockStateP9 {
    fn migrate(
        &self,
        from_loader: &impl BlobStoreLoad,
        to_storer: &mut impl BlobStoreStore,
    ) -> BlockStateResult<PersistentBlockStateP10>
    where
        Self: Sized,
    {
        let new_tokens = self.tokens.migrate(from_loader, to_storer)?;

        Ok(PersistentBlockStateP10 { tokens: new_tokens })
    }
}
