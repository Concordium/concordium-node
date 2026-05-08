use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::persistent::protocol_level_tokens::p9::PersistentTokensP9;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::Read;

/// P9 block state.
#[derive(Debug, Clone, Default)]
pub struct PersistentBlockStateP9 {
    /// Protocol-level tokens
    pub tokens: PersistentTokensP9,
}

impl Loadable for PersistentBlockStateP9 {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self { tokens })
    }
}

impl Storable for PersistentBlockStateP9 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP9 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP9 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.tokens.hash(loader)
    }
}

#[cfg(test)]
mod test {
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::BlobStoreLocation;
    use crate::block_state::blob_store::test_stub::BlobStoreStub;
    use crate::block_state::hash::Hashable;
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::entity_test_stub::NoExternalBlockStateTypes;
    use crate::entity::protocol_level_tokens::p9::TokenConfiguration;
    use crate::entity::{EntityContext, entity_test_stub};
    use crate::external::test_stub::NoExternalBlockStateStub;
    use crate::persistent::block_state::p9::PersistentBlockStateP9;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// Store state with PLTs to blob store and load it again.
    #[test]
    fn test_store_and_load_plts() {
        let context = entity_test_stub::new_context_no_external();
        let block_state = BlockStateP9::default();

        // Create tokens
        let configuration1 = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token1 = block_state.create_token(configuration1.clone());
        block_state.set_token_circulating_supply(&token1, RawTokenAmount(100));
        let mut key_value_state1 = block_state.mutable_token_key_value_state(&token1);
        block_state.update_token_state_value(
            &mut key_value_state1,
            &TokenStateKey(vec![0, 1]),
            Some(TokenStateValue(vec![0, 0])),
        );
        block_state.update_token_state_value(
            &mut key_value_state1,
            &TokenStateKey(vec![0, 2]),
            Some(TokenStateValue(vec![1, 1])),
        );
        block_state.set_token_key_value_state(&token1, key_value_state1);
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token2 = block_state.create_token(configuration2.clone());

        // Store block state
        let blob_ref = blob_store::store_to_store(
            &mut block_state.blob_store_load,
            block_state.internal_block_state.into_immutable(),
        );

        // Load block state
        let immutable_state = BlockState::load_from_store(
            &block_state.blob_store_load,
            blob_ref,
            ProtocolVersion::P11,
        )
        .expect("load block state");
        let block_state =
            block_state_no_external::with_block_state(block_state.blob_store_load, immutable_state);

        // Assert loaded state
        assert_eq!(block_state.plt_list().len(), 2);
        let token1 = block_state.token_by_id(&"token1".parse().unwrap()).unwrap();
        assert_eq!(
            block_state.token_circulating_supply(&token1),
            RawTokenAmount(100)
        );
        assert_eq!(block_state.token_configuration(&token1), configuration1);
        let key_value_state1 = block_state.mutable_token_key_value_state(&token1);
        let value =
            block_state.lookup_token_state_value(&key_value_state1, &TokenStateKey(vec![0, 1]));
        assert_eq!(value, Some(TokenStateValue(vec![0, 0])));
        let value =
            block_state.lookup_token_state_value(&key_value_state1, &TokenStateKey(vec![0, 2]));
        assert_eq!(value, Some(TokenStateValue(vec![1, 1])));
        let token2 = block_state.token_by_id(&"token2".parse().unwrap()).unwrap();
        assert_eq!(
            block_state.token_circulating_supply(&token2),
            RawTokenAmount(0)
        );
        assert_eq!(block_state.token_configuration(&token2), configuration2);
    }

    /// Assert that hash of an empty block state matches a fixed/snapshot hash. The hash
    /// must remain stable.
    #[test]
    fn snapshot_test_hash_empty() {
        let context = entity_test_stub::new_context_no_external();
        let persistent_block_state = PersistentBlockStateP9::default();

        // Assert hash
        let hash = persistent_block_state.hash(&context.loader).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05"
        );
    }

    /// Assert that hash of block state with some simple PLTs matches a fixed/snapshot hash. The hash
    /// must remain stable.
    #[test]
    fn snapshot_test_hash_simple_plts() {
        let context = entity_test_stub::new_context_no_external();
        let mut block_state = BlockStateP9::default();

        // Create tokens
        let configuration1 = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token_index1 = block_state
            .create_token(&context, configuration1.clone())
            .unwrap();
        let mut token1 = block_state.token_by_index(&context, token_index1).unwrap();
        token1.set_token_circulating_supply(RawTokenAmount(100));
        token1
            .mutable_key_value_state
            .insert_value(&context.loader, &[0, 1], vec![0, 0])
            .unwrap();
        token1
            .mutable_key_value_state
            .insert_value(&context.loader, &[0, 2], vec![1, 1])
            .unwrap();
        block_state.update_token(&context, token1).unwrap();
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token2 = block_state.create_token(&context, configuration2.clone());

        // Assert hash
        let hash = block_state.persistent.hash(&context.loader).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "d202e9153fea3fdd22c594be21d471c07e9619abc0baad3faca5c81f0bb1504b"
        );
    }

    /// Load empty block state from storage bytes fixture.
    /// The fixture bytes must not change and must be compatible with Haskell PLT state implementation.
    #[test]
    fn fixture_test_storage_empty() {
        let store = BlobStoreStub(hex::decode("00000000000000080000000000000000").unwrap());
        let context = EntityContext::<NoExternalBlockStateTypes> {
            external: NoExternalBlockStateStub,
            loader: store,
        };

        // Load block state
        let persistent_block_state: PersistentBlockStateP9 =
            blob_store::load_from_store(&context.loader, BlobStoreLocation(0))
                .expect("load block state");
        let block_state = BlockStateP9 {
            persistent: persistent_block_state,
        };

        // Assert loaded state
        assert_eq!(block_state.plt_list(&context).len(), 0);
    }

    /// Load block state with some simple PLTs from storage bytes fixture.
    /// The fixture bytes must not change and must be compatible with Haskell PLT state implementation.
    #[test]
    fn fixture_test_storage_simple_plts() {
        let store = BlobStoreStub(hex::decode("000000000000002806746f6b656e310505050505050505050505050505050505050505050505050505050505050505020000000000000025edbda48b85971b3a874334ca94f07e55e6a6e63eabca968d1257a3223e1b84e14002010100000000000000002503b0eab929105fd6df1ec793cbaf1b554a7a385520a9f7c902adf0219ace6dab4002000000000000000000003648b07111a93452374c7bcf66ee01959af6b4a52cb7cd299341e9ea77b378b0230300000201000000000000005d020000000000000030000000000000000901000000000000008a0000000000000011000000000000000000000000000000c86400000000000000090000000000000000d9000000000000002806746f6b656e3205050505050505050505050505050505050505050505050505050505050505050400000000000000010000000000000000110000000000000103000000000000013300000000000000000900000000000000013c0000000000000021000000000000000201000000000000000000000000000000f20000000000000155").unwrap());

        let context = EntityContext::<NoExternalBlockStateTypes> {
            external: NoExternalBlockStateStub,
            loader: store,
        };

        // Load block state
        let persistent_block_state: PersistentBlockStateP9 =
            blob_store::load_from_store(&context.loader, BlobStoreLocation(358))
                .expect("load block state");
        let block_state = BlockStateP9 {
            persistent: persistent_block_state,
        };

        // Assert loaded state
        assert_eq!(block_state.plt_list(&context).len(), 2);
        let token1 = block_state
            .token_by_id(&context, &"token1".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(token1.token_circulating_supply(), RawTokenAmount(100));
        let configuration1 = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        assert_eq!(
            token1.token_configuration(&context).unwrap(),
            configuration1
        );
        let value = token1
            .mutable_key_value_state
            .lookup_value(&context.loader, &[0, 1]);
        assert_eq!(value, Some(vec![0, 0]));

        let value = token1
            .mutable_key_value_state
            .lookup_value(&context.loader, &[0, 2]);
        assert_eq!(value, Some(vec![1, 1]));
        let token2 = block_state
            .token_by_id(&context, &"token2".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(token2.token_circulating_supply(), RawTokenAmount(0));
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        assert_eq!(
            token2.token_configuration(&context).unwrap(),
            configuration2
        );
    }
}
