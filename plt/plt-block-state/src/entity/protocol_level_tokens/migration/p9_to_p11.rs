use crate::entity::protocol_level_tokens::p9::TokenP9Base;
use crate::entity::protocol_level_tokens::p11::TokenP11;
use crate::failure::BlockStateResult;
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::test_stub::UnreachableBlobStore;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreMovable, BlobStoreStore};
use crate::persistent::lfmb_tree::LfmbTree;
use crate::persistent::protocol_level_tokens::p9::PersistentTokensP9;

/// Migrate the P10 block state to P11.
pub fn migrate_from_p10_to_p11(
    mut persistent_tokens: PersistentTokensP9,
    from_loader: &impl BlobStoreLoad,
    to_storer: &mut impl BlobStoreStore,
) -> BlockStateResult<PersistentTokensP9> {
    if true {
        return persistent_tokens.move_blob_store(from_loader, to_storer);
    }

    let mut new_tokens = LfmbTree::empty();
    for item in persistent_tokens.tokens.values(from_loader) {
        let (token_index, persistent_token) = item?;

        // let moved_persistent_token = persistent_token.move_blob_store(from_loader, to_storer)?;

        let mut new_token = TokenP11 {
            token_p9_base: TokenP9Base {
                token_index,
                mutable_key_value_state: persistent_token
                    .key_value_state
                    .value(from_loader)?
                    .thaw(),
                persistent: persistent_token.clone(),
            },
        };

        // todo ar set roles

        if new_token.token_p9_base.mutable_key_value_state.is_dirty() {
            new_token.token_p9_base.persistent.key_value_state = HashedCacheableRef::new(
                new_token
                    .token_p9_base
                    .mutable_key_value_state
                    .freeze(from_loader),
            );
        }

        let moved_persistent_token = new_token
            .token_p9_base
            .persistent
            .move_blob_store(from_loader, to_storer)?;

        // todo ar make UnreachableBlobStore return error instead of panic
        let new_token_index = new_tokens.insert_value(&UnreachableBlobStore, moved_persistent_token)?;
        // todo ar check equal to old
    }



    persistent_tokens.tokens = new_tokens;

    Ok(persistent_tokens)
}
