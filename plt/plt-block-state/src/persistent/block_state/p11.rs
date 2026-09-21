use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use crate::persistent::protocol_level_locks::p11::PersistentLocksP11;
use crate::persistent::protocol_level_tokens::p9::PersistentTokensP9;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::Read;

/// P11 block state.
#[derive(Debug, Clone, Default)]
pub struct PersistentBlockStateP11 {
    /// Protocol-level tokens
    pub(crate) tokens: HashedCacheableRef<PersistentTokensP9>,
    /// Protocol-level locks
    pub(crate) locks: HashedCacheableRef<PersistentLocksP11>,
}

impl Loadable for PersistentBlockStateP11 {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(&mut buffer, loader)?;
        let locks = Loadable::load_from_buffer(&mut buffer, loader)?;

        Ok(Self { tokens, locks })
    }
}

impl Storable for PersistentBlockStateP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
        self.locks.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        self.locks.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP11 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let tokens = self.tokens.hash(loader)?;
        let locks = self.locks.hash(loader)?;

        Ok(hash::hash_of_hashes(tokens, locks))
    }
}

#[cfg(test)]
mod test {
    use crate::entity::block_state::p11::BlockStateP11;
    use crate::entity::entity_test_stub;
    use crate::persistent::blob_store;
    use crate::persistent::block_state::p11::PersistentBlockStateP11;
    use crate::persistent::hash::Hashable;
    use crate::persistent::protocol_level_locks::p11::{
        LockConfigSimpleV0, LockConfiguration, LockControllerSimpleV0Grant, LockRecipients,
    };
    use crate::persistent::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};
    use concordium_base::base::AccountIndex;
    use concordium_base::common::types::TransactionTime;
    use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
    use concordium_base::protocol_level_tokens::{CborMemo, TokenModuleRef};
    use concordium_base::transactions::Memo;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    // Store state with PLLs to blob store and load it again.
    #[test]
    fn test_store_and_load_locks() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut block_state = BlockStateP11::default();

        // Create locks
        let lock_id1 = LockId {
            account_index: 1,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration1 = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::try_from(vec![AccountIndex::from(1), AccountIndex::from(2)])
                    .unwrap(),
                TransactionTime::from(100u64),
                vec![LockControllerSimpleV0Grant::new(
                    AccountIndex::from(1),
                    vec![
                        LockControllerSimpleV0Capability::Cancel,
                        LockControllerSimpleV0Capability::Fund,
                    ],
                )],
                vec!["tokenid1".parse().unwrap(), "tokenid2".parse().unwrap()],
                true,
                Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
                None,
            )
            .unwrap(),
        );

        block_state
            .create_lock(&context, &lock_id1, configuration1.clone())
            .unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(0), TokenIndex(0))
            .unwrap();
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(1), TokenIndex(1))
            .unwrap();
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::try_from(vec![]).unwrap(),
                TransactionTime::from(0u64),
                Vec::new(),
                Vec::new(),
                false,
                None,
                None,
            )
            .unwrap(),
        );
        block_state
            .create_lock(&context, &lock_id2, configuration2.clone())
            .unwrap();

        // Create a third lock and then delete it
        let lock_id3 = LockId {
            account_index: 3,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration3 = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::try_from(vec![]).unwrap(),
                TransactionTime::from(0u64),
                Vec::new(),
                Vec::new(),
                false,
                None,
                None,
            )
            .unwrap(),
        );
        block_state
            .create_lock(&context, &lock_id3, configuration3)
            .unwrap();
        let was_deleted = block_state.delete_lock(&context, &lock_id3).unwrap();
        assert!(was_deleted, "lock3 should be deleted");

        // Store and load block state
        let blob_ref = blob_store::store_to_store(&mut context.store, block_state.persistent);
        let block_state = entity_test_stub::load_block_state_p11(&context, blob_ref);

        // Assert loaded state
        assert_eq!(block_state.lock_list(&context).unwrap().len(), 2);

        // Assert the deleted lock is absent
        block_state
            .lock_by_id(&context, &lock_id3)
            .unwrap()
            .expect_err("lock3 should not exist after deletion and reload");
        let lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .unwrap();
        assert_eq!(
            lock1.lock_balance_refs(&context).unwrap(),
            vec![
                (AccountIndex::from(0), TokenIndex(0)),
                (AccountIndex::from(1), TokenIndex(1))
            ]
        );
        assert_eq!(
            lock1.lock_configuration(&context).unwrap().into_owned(),
            configuration1
        );
        let lock2 = block_state
            .lock_by_id(&context, &lock_id2)
            .unwrap()
            .unwrap();
        assert_eq!(lock2.lock_balance_refs(&context).unwrap(), vec![]);
        assert_eq!(
            lock2.lock_configuration(&context).unwrap().into_owned(),
            configuration2
        );
    }

    #[test]
    fn balance_updates_reuse_lock_configuration_and_preserve_hashes_after_reload() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut block_state = BlockStateP11::default();
        let lock_id = LockId::new(1, 1, 0);
        let configuration = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(100),
                Vec::new(),
                Vec::new(),
                false,
                None,
                None,
            )
            .unwrap(),
        );

        block_state
            .create_lock(&context, &lock_id, configuration)
            .unwrap();
        let mut lock = block_state.lock_by_id(&context, &lock_id).unwrap().unwrap();
        let configuration_ptr = lock
            .persistent
            .configuration
            .value(&context.store)
            .unwrap()
            .as_ref() as *const _;
        let configuration_hash = lock.persistent.configuration.hash(&context.store).unwrap();

        lock.add_lock_balance_ref(&context, AccountIndex::from(0), TokenIndex(0))
            .unwrap();
        lock.add_lock_balance_ref(&context, AccountIndex::from(1), TokenIndex(1))
            .unwrap();
        assert!(std::ptr::eq(
            configuration_ptr,
            lock.persistent
                .configuration
                .value(&context.store)
                .unwrap()
                .as_ref(),
        ));
        assert_eq!(
            lock.persistent.configuration.hash(&context.store).unwrap(),
            configuration_hash
        );
        let lock_hash = lock.persistent.hash(&context.store).unwrap();
        block_state.update_lock(&context, lock).unwrap();
        let state_hash = block_state.persistent.hash(&context.store).unwrap();

        let location = blob_store::store_to_store(&mut context.store, &block_state.persistent);
        let loaded = entity_test_stub::load_block_state_p11(&context, location);
        let loaded_lock = loaded.lock_by_id(&context, &lock_id).unwrap().unwrap();
        assert_eq!(
            loaded_lock.persistent.hash(&context.store).unwrap(),
            lock_hash
        );
        assert_eq!(loaded.persistent.hash(&context.store).unwrap(), state_hash);
    }

    /// Assert that hash and stored bytes of an empty block state matches snapshot.
    /// The hash and bytes should remain fixed.
    #[test]
    fn snapshot_test_hash_and_storage_empty() {
        let mut context = entity_test_stub::new_no_external_context();
        let persistent_block_state = PersistentBlockStateP11::default();

        // Assert hash
        let hash = persistent_block_state.hash(&context.store).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "21238c14891e14e616aed254c3033fb56386834711f9fb4dd2840b4fe642fca5"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &persistent_block_state);
        assert_eq!(
            hex::encode(context.store.0),
            "00000000000000080000000000000000000000000000001300000000000000000000000000000000000000000000000000001000000000000000000000000000000010"
        );
    }

    /// Assert that hash and stored bytes of a block state with simple
    /// tokens and locks matches snapshot.
    /// The hash and bytes should remain fixed.
    #[test]
    fn snapshot_test_hash_and_storage_simple_tokens_and_locks() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut block_state = BlockStateP11::default();

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
        token1
            .token_p9_base
            .set_token_circulating_supply(RawTokenAmount::from(100));
        token1
            .token_p9_base
            .mutable_key_value_state
            .insert_value(&context.store, &[0, 1], vec![0, 0])
            .unwrap();
        token1
            .token_p9_base
            .mutable_key_value_state
            .insert_value(&context.store, &[0, 2], vec![1, 1])
            .unwrap();
        block_state.update_token(&context, token1).unwrap();
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token2 = block_state.create_token(&context, configuration2.clone());

        // Create locks
        let lock_id1 = LockId {
            account_index: 1,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration1 = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::try_from(vec![AccountIndex::from(1), AccountIndex::from(2)])
                    .unwrap(),
                TransactionTime::from(100u64),
                vec![LockControllerSimpleV0Grant::new(
                    AccountIndex::from(1),
                    vec![
                        LockControllerSimpleV0Capability::Cancel,
                        LockControllerSimpleV0Capability::Fund,
                    ],
                )],
                vec!["tokenid1".parse().unwrap(), "tokenid2".parse().unwrap()],
                true,
                Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
                None,
            )
            .unwrap(),
        );
        block_state
            .create_lock(&context, &lock_id1, configuration1)
            .unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(0), TokenIndex(0))
            .unwrap();
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(1), TokenIndex(1))
            .unwrap();
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfiguration::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::try_from(vec![]).unwrap(),
                TransactionTime::from(0u64),
                Vec::new(),
                Vec::new(),
                false,
                None,
                None,
            )
            .unwrap(),
        );
        block_state
            .create_lock(&context, &lock_id2, configuration2)
            .unwrap();

        // Assert hash
        let hash = block_state.persistent.hash(&context.store).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "cb16308c0b2f55a341e206c597ffa7998188c43858d417b8582a774681dbd4ec"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &block_state.persistent);
        assert_eq!(
            hex::encode(context.store.0),
            "000000000000002806746f6b656e310505050505050505050505050505050505050505050505050505050505050505020000000000000025edbda48b85971b3a874334ca94f07e55e6a6e63eabca968d1257a3223e1b84e14002010100000000000000002503b0eab929105fd6df1ec793cbaf1b554a7a385520a9f7c902adf0219ace6dab4002000000000000000000003648b07111a93452374c7bcf66ee01959af6b4a52cb7cd299341e9ea77b378b0230300000201000000000000005d020000000000000030000000000000000901000000000000008a0000000000000011000000000000000000000000000000c86400000000000000090000000000000000d9000000000000002806746f6b656e3205050505050505050505050505050505050505050505050505050505050505050400000000000000010000000000000000110000000000000103000000000000013300000000000000000900000000000000013c0000000000000021000000000000000201000000000000000000000000000000f2000000000000015500000000000000150100000000000000000900000000000000000000000000000000000015010000000000000000090100000000000000010000000000000000002400000000000000000700000000000000000200000000000000018f0100000000000001ac00000000000000450001000200000000000000010000000000000002000000000000006400010000000000000001020003000208746f6b656e69643108746f6b656e6964320101000002000100000000000000004001000000000000000200000000000000000000010000000000000001c900000000000001f500000000000000110100000000000000010000000000000000000000000000000000130001000000000000000000000000000000000000000000000000370100000000000000000000000000000000000000000000000000028a00000000000000110200000000000000070000000000000000000000000000000000240000000000000000070000000000000000020100000000000002420200000000000002a5000000000000001c000000000000000200000000000000000000010000000000000002e4000000000000001000000000000001660000000000000310"
        );
    }
}
