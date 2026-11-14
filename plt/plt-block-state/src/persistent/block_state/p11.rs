use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use crate::persistent::protocol_level_locks::p11::{
    PersistentLockExpiryIndexP11, PersistentLocksP11,
};
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
    /// Persisted derived indexes, excluded from the block-state hash.
    pub(crate) indexes: BlockStateIndexes,
}

/// Derived indexes persisted with the block state but excluded from its hash.
#[derive(Debug, Clone, Default)]
pub(crate) struct BlockStateIndexes {
    /// Locks ordered by expiry, absolute creation height, and lock ID.
    pub(crate) lock_expiry_index: HashedCacheableRef<PersistentLockExpiryIndexP11>,
}

impl Loadable for PersistentBlockStateP11 {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(&mut buffer, loader)?;
        let locks = Loadable::load_from_buffer(&mut buffer, loader)?;

        let lock_expiry_index = Loadable::load_from_buffer(&mut buffer, loader)?;
        Ok(Self {
            tokens,
            locks,
            indexes: BlockStateIndexes { lock_expiry_index },
        })
    }
}

impl Storable for PersistentBlockStateP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
        self.locks.store_to_buffer(&mut buffer, storer);
        self.indexes
            .lock_expiry_index
            .store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        self.locks.cache_reference_values(loader)?;
        self.indexes
            .lock_expiry_index
            .cache_reference_values(loader)?;
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
    use crate::failure::BlockStateFailure;
    use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::{BlobStoreStore, Loadable};
    use crate::persistent::block_state::p11::PersistentBlockStateP11;
    use crate::persistent::hash::Hashable;
    use crate::persistent::protocol_level_locks::p11::{
        LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Grant, LockExpiryKey, LockRecipients,
        PersistentLockExpiryIndexP11,
    };
    use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;
    use concordium_base::base::AccountIndex;
    use concordium_base::common::to_bytes;
    use concordium_base::common::types::TransactionTime;
    use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
    use concordium_base::protocol_level_tokens::{CborMemo, TokenId, TokenModuleRef};
    use concordium_base::transactions::Memo;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// The expiry schedule tracks lock lifecycle changes.
    #[test]
    fn expiry_schedule_tracks_lock_lifecycle() {
        let context = entity_test_stub::new_no_external_context();
        let mut state = BlockStateP11::default();
        let empty_hash = state.persistent.hash(&context.store).unwrap();
        assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 0);
        let configuration = LockConfig::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                100.into(),
                vec![],
                vec![],
                false,
                None,
                None,
            )
            .unwrap(),
        );
        let id = LockId::new(1, 2, 3);
        let key = LockExpiryKey {
            expiry: 100.into(),
            created_at_height: 1234.into(),
            lock_id: id.clone(),
        };
        // Creation records the schedule; a duplicate must preserve the original lock and height.
        state
            .create_lock(&context, &id, configuration.clone(), key.created_at_height)
            .unwrap();
        let created_hash = state.persistent.hash(&context.store).unwrap();
        assert_ne!(created_hash, empty_hash);
        assert!(
            state
                .create_lock(&context, &id, configuration.clone(), 9999.into())
                .is_err()
        );
        assert_eq!(state.persistent.hash(&context.store).unwrap(), created_hash);
        let lock_hash = state.persistent.locks.hash(&context.store).unwrap();
        let schedule_hash = state
            .persistent
            .indexes
            .lock_expiry_index
            .hash(&context.store)
            .unwrap();
        // Balance updates must preserve the immutable creation height and schedule.
        let mut lock = state.lock_by_id(&context, &id).unwrap().unwrap();
        let token: TokenId = "Token1".parse().unwrap();
        lock.add_lock_balance_ref(&context, 1.into(), token.clone())
            .unwrap();
        state.update_lock(&context, lock).unwrap();
        let mut lock = state.lock_by_id(&context, &id).unwrap().unwrap();
        assert_eq!(lock.persistent.created_at_height, key.created_at_height);
        assert!(
            lock.remove_lock_balance_ref(&context, 1.into(), token)
                .unwrap()
        );
        state.update_lock(&context, lock).unwrap();
        assert_eq!(
            state
                .persistent
                .indexes
                .lock_expiry_index
                .hash(&context.store)
                .unwrap(),
            schedule_hash
        );
        assert_eq!(state.persistent.hash(&context.store).unwrap(), created_hash);
        // Deletion removes both entries and restores the empty state.
        assert!(state.delete_lock(&context, &id).unwrap());
        assert!(!state.delete_lock(&context, &id).unwrap());
        assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 0);
        assert_eq!(state.persistent.hash(&context.store).unwrap(), empty_hash);
        assert!(state.lock_by_id(&context, &id).unwrap().is_err());

        // Creation height is committed through the lock hash, independently of the index.
        state
            .create_lock(&context, &id, configuration, 1235.into())
            .unwrap();
        assert_ne!(state.persistent.hash(&context.store).unwrap(), created_hash);
        assert_ne!(
            state.persistent.locks.hash(&context.store).unwrap(),
            lock_hash
        );
        assert_ne!(
            state
                .persistent
                .indexes
                .lock_expiry_index
                .hash(&context.store)
                .unwrap(),
            schedule_hash
        );
        // Derived index contents do not affect the committed block-state hash.
        let hash = state.persistent.hash(&context.store).unwrap();
        state.persistent.indexes.lock_expiry_index =
            HashedCacheableRef::new(PersistentLockExpiryIndexP11::empty());
        assert_eq!(state.persistent.hash(&context.store).unwrap(), hash);
    }

    /// Failed lifecycle operations leave the published lock and expiry-index roots unchanged.
    #[test]
    fn lifecycle_failures_do_not_publish_partial_roots() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut state = BlockStateP11::default();
        let configuration = LockConfig::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                100.into(),
                vec![],
                vec![],
                false,
                None,
                None,
            )
            .unwrap(),
        );
        let id = LockId::new(1, 1, 0);
        state
            .create_lock(&context, &id, configuration.clone(), 1234.into())
            .unwrap();
        let lock_hash = state.persistent.locks.hash(&context.store).unwrap();
        // A malformed index makes create and delete fail after preparing, but not publishing, locks.
        let location = context.store.store_raw([]);
        state.persistent.indexes.lock_expiry_index =
            HashedCacheableRef::load_from_buffer(to_bytes(&location).as_slice(), &context.store)
                .unwrap();
        assert!(matches!(
            state.create_lock(&context, &LockId::new(2, 1, 0), configuration, 1234.into()),
            Err(BlockStateFailure::BlobStoreDecode(_))
        ));
        assert_eq!(
            state.persistent.locks.hash(&context.store).unwrap(),
            lock_hash
        );
        assert!(matches!(
            state.delete_lock(&context, &id),
            Err(BlockStateFailure::BlobStoreDecode(_))
        ));
        assert_eq!(
            state.persistent.locks.hash(&context.store).unwrap(),
            lock_hash
        );
        assert!(state.lock_expiry_index(&context).is_err());
        // An absent lock returns false without consulting the malformed index.
        assert!(!state.delete_lock(&context, &LockId::new(3, 1, 0)).unwrap());
        // A missing schedule entry is an invariant failure and must not delete the existing lock.
        state.persistent.indexes.lock_expiry_index =
            HashedCacheableRef::new(PersistentLockExpiryIndexP11::empty());
        assert!(matches!(
            state.delete_lock(&context, &id),
            Err(BlockStateFailure::Invariant(_))
        ));
        assert_eq!(
            state.persistent.locks.hash(&context.store).unwrap(),
            lock_hash
        );
        assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 0);
    }

    // Store state with PLLs to blob store and load it again.
    #[test]
    fn test_store_and_load_locks() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut block_state = BlockStateP11::default();
        let token_id: TokenId = "Token1".parse().unwrap();
        let token_id_2: TokenId = "Token2".parse().unwrap();

        // Create locks
        let lock_id1 = LockId {
            account_index: 1,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration1 = LockConfig::SimpleV0(
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
                vec![token_id.clone(), token_id_2.clone()],
                true,
                Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
                None,
            )
            .unwrap(),
        );

        block_state
            .create_lock(&context, &lock_id1, configuration1.clone(), 1234.into())
            .unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(0), token_id.clone())
            .unwrap();
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(1), token_id_2.clone())
            .unwrap();
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfig::SimpleV0(
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
            .create_lock(&context, &lock_id2, configuration2.clone(), 1234.into())
            .unwrap();

        // Create a third lock and then delete it
        let lock_id3 = LockId {
            account_index: 3,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration3 = LockConfig::SimpleV0(
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
            .create_lock(&context, &lock_id3, configuration3, 1234.into())
            .unwrap();
        let was_deleted = block_state.delete_lock(&context, &lock_id3).unwrap();
        assert!(was_deleted, "lock3 should be deleted");

        // Store and load block state
        let blob_ref = blob_store::store_to_store(&mut context.store, block_state.persistent);
        let block_state = entity_test_stub::load_block_state_p11(&context, blob_ref);

        // Assert loaded state
        assert_eq!(block_state.lock_list(&context).unwrap().len(), 2);
        assert_eq!(
            block_state
                .lock_expiry_index(&context)
                .unwrap()
                .iter(&context.store)
                .map(|entry| entry.unwrap().0.lock_id)
                .collect::<Vec<_>>(),
            vec![lock_id2.clone(), lock_id1.clone()]
        );

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
                (AccountIndex::from(0), token_id),
                (AccountIndex::from(1), token_id_2)
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
        let configuration = LockConfig::SimpleV0(
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
            .create_lock(&context, &lock_id, configuration, 1234.into())
            .unwrap();
        let mut lock = block_state.lock_by_id(&context, &lock_id).unwrap().unwrap();
        let configuration_ptr = lock
            .persistent
            .configuration
            .value(&context.store)
            .unwrap()
            .as_ref() as *const _;
        let configuration_hash = lock.persistent.configuration.hash(&context.store).unwrap();

        lock.add_lock_balance_ref(
            &context,
            AccountIndex::from(0),
            "Token1".parse::<TokenId>().unwrap(),
        )
        .unwrap();
        lock.add_lock_balance_ref(
            &context,
            AccountIndex::from(1),
            "Token2".parse::<TokenId>().unwrap(),
        )
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
            "56885fb4a622ab251473c851455ae4bb4ad0c52038f306d77cf61dfad44635b2"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &persistent_block_state);
        assert_eq!(
            hex::encode(context.store.0),
            "00000000000000080000000000000000000000000000000d00000000000000000000000000000000000000000d000000000000000000000000000000000000000018000000000000000000000000000000100000000000000025"
        );
    }

    /// Assert that hash and stored bytes of a block state with simple
    /// tokens and locks matches snapshot.
    /// The hash and bytes should remain fixed.
    #[test]
    fn snapshot_test_hash_and_storage_simple_tokens_and_locks() {
        let mut context = entity_test_stub::new_no_external_context();
        let mut block_state = BlockStateP11::default();
        let token_id: TokenId = "Token1".parse().unwrap();
        let token_id_2: TokenId = "Token2".parse().unwrap();

        // Create tokens
        let configuration1 = TokenConfiguration {
            token_id: token_id.clone(),
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
            token_id: token_id_2.clone(),
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
        let configuration1 = LockConfig::SimpleV0(
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
                vec![token_id.clone(), token_id_2.clone()],
                true,
                Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
                None,
            )
            .unwrap(),
        );
        block_state
            .create_lock(&context, &lock_id1, configuration1, 1234.into())
            .unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(0), token_id)
            .unwrap();
        lock1
            .add_lock_balance_ref(&context, AccountIndex::from(1), token_id_2)
            .unwrap();
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfig::SimpleV0(
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
            .create_lock(&context, &lock_id2, configuration2, 1234.into())
            .unwrap();

        // Assert hash
        let hash = block_state.persistent.hash(&context.store).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "ec55d2d4e9f79bc2cf5bd72c6488f72ba10555f092ad668d86d081ef9772a93a"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &block_state.persistent);
        assert_eq!(
            hex::encode(context.store.0),
            "000000000000002806546f6b656e310505050505050505050505050505050505050505050505050505050505050505020000000000000025edbda48b85971b3a874334ca94f07e55e6a6e63eabca968d1257a3223e1b84e14002010100000000000000002503b0eab929105fd6df1ec793cbaf1b554a7a385520a9f7c902adf0219ace6dab4002000000000000000000003648b07111a93452374c7bcf66ee01959af6b4a52cb7cd299341e9ea77b378b0230300000201000000000000005d020000000000000030000000000000000901000000000000008a0000000000000011000000000000000000000000000000c86400000000000000090000000000000000d9000000000000002806546f6b656e3205050505050505050505050505050505050505050505050505050505050505050400000000000000010000000000000000110000000000000103000000000000013300000000000000000900000000000000013c0000000000000021000000000000000201000000000000000000000000000000f20000000000000155000000000000000c01000706546f6b656e310000000000000000000c01000706546f6b656e320000000000000000001d000006000000000000000200000000000000018f0100000000000001a3000000000000003f000100020000000000000001000000000000000200000000000000640001000000000000000109000206546f6b656e3106546f6b656e320101000002000100000000000000003b01000000000000000200000000010000000000000001b700000000000001dc00000000000004d2001000000000000000010000000000000000000000000000000000130001000000000000000000000000000000000000000000000000320100000000000000000000000000000000000000026600000000000004d20010000000000000000700000000000000000000000000000000001d00000600000000000000020100000000000002230200000000000002810000000000000016000000000000000200000000010000000000000002bb000000000000002501002000000000000004d20000000000000002000000000000000700000000000000000000000000000000002501002000000000000004d20000000000000001000000000000000100000000000000000000000000000000001d00000600000000000000020000000000000002fe64000000000000032b0000000000000016000000000000000200000000010000000000000003580000000000000018000000000000016600000000000002e0000000000000037d"
        );
    }
}
