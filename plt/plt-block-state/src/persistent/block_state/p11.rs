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
        LockConfiguration, LockControllerConfig, LockControllerSimpleV0,
        LockControllerSimpleV0Grant,
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
        let configuration1 = LockConfiguration::new(
            lock_id1.clone(),
            vec![AccountIndex::from(1), AccountIndex::from(2)],
            TransactionTime::from(100u64),
            LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: vec![LockControllerSimpleV0Grant {
                    account: AccountIndex::from(1),
                    roles: vec![
                        LockControllerSimpleV0Capability::Cancel,
                        LockControllerSimpleV0Capability::Fund,
                    ],
                }],
                tokens: vec!["tokenid1".parse().unwrap(), "tokenid2".parse().unwrap()],
                keep_alive: true,
                memo: Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
            }),
        );

        block_state
            .create_lock(&context, configuration1.clone())
            .unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1.add_lock_balance_ref(AccountIndex::from(0), TokenIndex(0));
        lock1.add_lock_balance_ref(AccountIndex::from(1), TokenIndex(1));
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfiguration::new(
            lock_id2.clone(),
            vec![],
            TransactionTime::from(0u64),
            LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: Vec::new(),
                tokens: Vec::new(),
                keep_alive: false,
                memo: None,
            }),
        );
        block_state
            .create_lock(&context, configuration2.clone())
            .unwrap();

        // Create a third lock and then delete it
        let lock_id3 = LockId {
            account_index: 3,
            sequence_number: 1,
            creation_order: 0,
        };
        let configuration3 = LockConfiguration::new(
            lock_id3.clone(),
            vec![],
            TransactionTime::from(0u64),
            LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: Vec::new(),
                tokens: Vec::new(),
                keep_alive: false,
                memo: None,
            }),
        );
        block_state.create_lock(&context, configuration3).unwrap();
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
            lock1.lock_balance_refs(),
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
        assert_eq!(lock2.lock_balance_refs(), vec![]);
        assert_eq!(
            lock2.lock_configuration(&context).unwrap().into_owned(),
            configuration2
        );
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
            "db35d91962f8f0315adb99d687d65c796ac67f2956b02e80fb667589f64efcb5"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &persistent_block_state);
        assert_eq!(
            hex::encode(context.store.0),
            "0000000000000008000000000000000000000000000000080000000000000000000000000000001000000000000000000000000000000010"
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
            .set_token_circulating_supply(RawTokenAmount(100));
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
        let configuration1 = LockConfiguration::new(
            lock_id1.clone(),
            vec![AccountIndex::from(1), AccountIndex::from(2)],
            TransactionTime::from(100u64),
            LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: vec![LockControllerSimpleV0Grant {
                    account: AccountIndex::from(1),
                    roles: vec![
                        LockControllerSimpleV0Capability::Cancel,
                        LockControllerSimpleV0Capability::Fund,
                    ],
                }],
                tokens: vec!["tokenid1".parse().unwrap(), "tokenid2".parse().unwrap()],
                keep_alive: true,
                memo: Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
            }),
        );
        block_state.create_lock(&context, configuration1).unwrap();
        let mut lock1 = block_state
            .lock_by_id(&context, &lock_id1)
            .unwrap()
            .expect("lock should exist");
        lock1.add_lock_balance_ref(AccountIndex::from(0), TokenIndex(0));
        lock1.add_lock_balance_ref(AccountIndex::from(1), TokenIndex(1));
        block_state.update_lock(&context, lock1).unwrap();
        let lock_id2 = LockId {
            account_index: 2,
            sequence_number: 7,
            creation_order: 0,
        };
        let configuration2 = LockConfiguration::new(
            lock_id2.clone(),
            vec![],
            TransactionTime::from(0u64),
            LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: Vec::new(),
                tokens: Vec::new(),
                keep_alive: false,
                memo: None,
            }),
        );
        block_state.create_lock(&context, configuration2).unwrap();

        // Assert hash
        let hash = block_state.persistent.hash(&context.store).expect("hash");
        assert_eq!(
            format!("{}", hash),
            "d4c2f4dae46bab4c1eb62d4dfa57a9fd998778adf4c74e1fdf94957eda559922"
        );

        // Assert storage
        blob_store::store_to_store(&mut context.store, &block_state.persistent);
        assert_eq!(
            hex::encode(context.store.0),
            "000000000000002806746f6b656e310505050505050505050505050505050505050505050505050505050505050505020000000000000025edbda48b85971b3a874334ca94f07e55e6a6e63eabca968d1257a3223e1b84e14002010100000000000000002503b0eab929105fd6df1ec793cbaf1b554a7a385520a9f7c902adf0219ace6dab4002000000000000000000003648b07111a93452374c7bcf66ee01959af6b4a52cb7cd299341e9ea77b378b0230300000201000000000000005d020000000000000030000000000000000901000000000000008a0000000000000011000000000000000000000000000000c86400000000000000090000000000000000d9000000000000002806746f6b656e3205050505050505050505050505050505050505050505050505050505050505050400000000000000010000000000000000110000000000000103000000000000013300000000000000000900000000000000013c0000000000000021000000000000000201000000000000000000000000000000f20000000000000155000000000000005b00000000000000010000000000000001000000000000000000020000000000000001000000000000000200000000000000640000010000000000000001020300000208746f6b656e69643108746f6b656e6964320101000002000100000000000000310100000000000000020000000000000000000000000000000000000000000000010000000000000001000000000000018f00000000000000090000000000000001f2000000000000002900000000000000020000000000000007000000000000000000000000000000000000000000000000000000000000000011010000000000000000000000000000023c000000000000000900000000000000026d00000000000000210000000000000002010000000000000000000000000000022b0000000000000286000000000000001000000000000001660000000000000297"
        );
    }
}
