use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreMovable, BlobStoreStore, Loadable, ParseResultExt, Storable,
    StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use crate::persistent::trie::{Trie, TrieKey};
use concordium_base::base::{AbsoluteBlockHeight, AccountIndex};
use concordium_base::common::types::TransactionTime;
use concordium_base::common::{
    Buffer, Deserial, Get, ParseResult, ReadBytesExt, Serial, Serialize, SliceBuffer,
    deserial_vector_no_length, from_bytes_complete, to_bytes,
};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_locks::{LockCapability, LockId};
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use std::io::Read;

/// Persistent collection of protocol-level locks on P11 and later protocols.
// Trie inline key length 24 matches the size of `LockId`
pub type PersistentLocksP11 = Trie<24, LockId, PersistentLockP11>;

const LOCK_EXPIRY_KEY_SIZE: usize = 40;

/// Expiry-ordered set of scheduled P11 locks.
pub type PersistentLockExpiryIndexP11 =
    Trie<LOCK_EXPIRY_KEY_SIZE, LockExpiryKey, StoreSerialized<()>>;

/// Internal scheduling key ordered by expiry, absolute creation block height, and lock ID.
/// Expiry ties process earlier creation blocks first for scheduling fairness;
/// locks created at the same height are ordered by lock ID.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockExpiryKey {
    pub expiry: TransactionTime,
    pub created_at_height: AbsoluteBlockHeight,
    pub lock_id: LockId,
}

impl TrieKey for LockExpiryKey {
    fn to_bytes(&self) -> impl std::borrow::Borrow<[u8]> {
        let mut bytes = [0; LOCK_EXPIRY_KEY_SIZE];
        let mut buffer = SliceBuffer::Bytes(&mut bytes);
        self.serial(&mut buffer);
        assert!(
            buffer.into_result().is_ok(),
            "lock expiry key exceeds its fixed size"
        );
        bytes
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self> {
        from_bytes_complete(key).map_parse_err_to_block_state_err()
    }
}

/// Trie key for a locked account/token balance reference.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize)]
pub(crate) struct BalanceReferenceKey(pub(crate) AccountIndex, pub(crate) TokenId);

impl TrieKey for LockId {
    fn to_bytes(&self) -> impl std::borrow::Borrow<[u8]> {
        let mut bytes = [0; 24];
        bytes[..8].copy_from_slice(&self.account_index.to_be_bytes());
        bytes[8..16].copy_from_slice(&self.sequence_number.to_be_bytes());
        bytes[16..].copy_from_slice(&self.creation_order.to_be_bytes());
        bytes
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self> {
        from_bytes_complete(key).map_err(|err| {
            BlockStateFailure::BlobStoreDecode(format!(
                "Stored lock ID key cannot be decoded: {err}"
            ))
        })
    }
}

impl TrieKey for BalanceReferenceKey {
    fn to_bytes(&self) -> impl std::borrow::Borrow<[u8]> {
        to_bytes(self)
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self> {
        from_bytes_complete(key).map_err(|err| {
            BlockStateFailure::BlobStoreDecode(format!(
                "Stored balance reference key cannot be decoded: {err}"
            ))
        })
    }
}

type BalanceReferences = Trie<24, BalanceReferenceKey, StoreSerialized<()>>;

/// The block state for a single protocol-level lock.
#[derive(Debug, Clone)]
pub struct PersistentLockP11 {
    /// Immutable absolute height of the creation block, needed to retrieve its expiry index entry.
    pub(crate) created_at_height: AbsoluteBlockHeight,
    /// References to the account/token balances locked within this lock.
    pub(crate) locked_balances: BalanceReferences,
    /// Immutable configuration parameters for the lock.
    pub(crate) configuration: HashedCacheableRef<StoreSerialized<LockConfig>>,
}

impl PersistentLockP11 {
    /// Construct an empty lock with the supplied immutable configuration.
    pub(crate) fn new(configuration: LockConfig, created_at_height: AbsoluteBlockHeight) -> Self {
        Self {
            created_at_height,
            locked_balances: Trie::empty(),
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
        }
    }
}

impl Storable for PersistentLockP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.locked_balances.store_to_buffer(&mut buffer, storer);
        self.configuration.store_to_buffer(&mut buffer, storer);
        self.created_at_height.serial(&mut buffer);
    }
}

impl Loadable for PersistentLockP11 {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        Ok(Self {
            locked_balances: Loadable::load_from_buffer(&mut buffer, loader)?,
            configuration: Loadable::load_from_buffer(&mut buffer, loader)?,
            created_at_height: buffer.get().map_parse_err_to_block_state_err()?,
        })
    }
}

impl Cacheable for PersistentLockP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.locked_balances.cache_reference_values(loader)?;
        self.configuration.cache_reference_values(loader)
    }
}

impl Hashable for PersistentLockP11 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(hash::hash_of_serialization((
            self.locked_balances.hash(loader)?,
            self.configuration.hash(loader)?,
            self.created_at_height,
        )))
    }
}

impl BlobStoreMovable for PersistentLockP11 {
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self> {
        Ok(Self {
            locked_balances: self.locked_balances.move_blob_store(from_store, to_store)?,
            configuration: self.configuration.move_blob_store(from_store, to_store)?,
            created_at_height: self.created_at_height,
        })
    }
}

/// Error returned when a persistent container exceeds its serialized capacity.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ContainerSizeOverflow {
    pub field: &'static str,
    pub length: usize,
    pub max_length: usize,
}

const U16_MAX_LENGTH: usize = u16::MAX as usize;

fn check_u16_length(field: &'static str, length: usize) -> Result<(), ContainerSizeOverflow> {
    if length > U16_MAX_LENGTH {
        return Err(ContainerSizeOverflow {
            field,
            length,
            max_length: U16_MAX_LENGTH,
        });
    }
    Ok(())
}

// Represents a list of lock recipients. This type enforces that the inner list is always sorted
// to enable binary search.
#[derive(Debug, Clone, Eq, PartialEq, Serial)]
pub struct LockRecipientsList {
    #[size_length = 2]
    recipients: Vec<AccountIndex>,
}

impl LockRecipientsList {
    /// Create a new list of lock recipients from the given account index list
    pub fn new(mut recipients: Vec<AccountIndex>) -> Result<Self, ContainerSizeOverflow> {
        recipients.sort_unstable();
        recipients.dedup();
        check_u16_length("LockRecipientsList.recipients", recipients.len())?;
        Ok(Self { recipients })
    }

    /// Get an iterator of the account indices in the list
    pub fn iter(&self) -> impl Iterator<Item = &AccountIndex> {
        self.recipients.iter()
    }

    /// Check whether the given account is a member
    pub fn is_recipient(&self, account: &AccountIndex) -> bool {
        self.recipients.binary_search(account).is_ok()
    }

    /// Get the length of the list
    pub fn len(&self) -> usize {
        self.recipients.len()
    }

    /// Check whether the list is empty
    pub fn is_empty(&self) -> bool {
        self.recipients.is_empty()
    }
}

impl Deserial for LockRecipientsList {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let count: u16 = source.get()?;
        let recipients: Vec<AccountIndex> = deserial_vector_no_length(source, count.into())?;
        if !recipients.windows(2).all(|pair| pair[0] < pair[1]) {
            return Err(
                BlockStateFailure::Invariant("Non-canonical lock recipients".into()).into(),
            );
        }
        Ok(Self { recipients })
    }
}

/// Accounts that can receive funds from this lock in block state.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub enum LockRecipients {
    /// Any eligible account can receive funds from this lock.
    Any,
    /// Only the listed accounts can receive funds from this lock.
    Limited(LockRecipientsList),
}

impl LockRecipients {
    /// Check whether this representation allows any recipient.
    pub fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }

    /// Check if the given account is a recipient.
    pub fn is_recipient(&self, account: &AccountIndex) -> bool {
        match self {
            Self::Any => true,
            Self::Limited(recipients) => recipients.recipients.binary_search(account).is_ok(),
        }
    }
}

impl TryFrom<Vec<AccountIndex>> for LockRecipients {
    type Error = ContainerSizeOverflow;
    fn try_from(recipients: Vec<AccountIndex>) -> Result<Self, Self::Error> {
        Ok(Self::Limited(LockRecipientsList::new(recipients)?))
    }
}

/// Lock configuration at the block state level.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub enum LockConfig {
    /// SimpleV0 lock configuration.
    SimpleV0(LockConfigSimpleV0),
}

/// Configuration for a SimpleV0 lock.
#[derive(Debug, Clone, Eq, PartialEq, Serial)]
pub struct LockConfigSimpleV0 {
    /// Accounts that can receive funds from this lock.
    pub recipients: LockRecipients,
    /// Expiry time of the lock (seconds since epoch).
    pub expiry: TransactionTime,
    /// Effective operation permissions in ascending account-index order.
    #[size_length = 2]
    roles: Vec<LockRole>,
    /// Tokens affected by this lock.
    #[size_length = 2]
    tokens: Vec<TokenId>,
    /// Optional raw CBOR-encoded user-facing lock metadata.
    pub metadata: Option<RawCbor>,
}

impl LockConfigSimpleV0 {
    /// Normalize effective roles and resolved tokens in registered spelling.
    /// Returns [`ContainerSizeOverflow`] if either normalized collection exceeds its u16 bound.
    pub fn new(
        recipients: LockRecipients,
        expiry: TransactionTime,
        mut roles: Vec<LockRole>,
        mut tokens: Vec<TokenId>,
        metadata: Option<RawCbor>,
    ) -> Result<Self, ContainerSizeOverflow> {
        roles.sort_unstable_by_key(|role| role.account);
        roles.dedup_by(|next, previous| {
            if next.account == previous.account {
                previous.roles = previous.roles.union(next.roles);
                true
            } else {
                false
            }
        });
        check_u16_length("LockConfigSimpleV0.roles", roles.len())?;
        tokens.sort_unstable();
        tokens.dedup();
        check_u16_length("LockConfigSimpleV0.tokens", tokens.len())?;
        Ok(Self {
            recipients,
            expiry,
            roles,
            tokens,
            metadata,
        })
    }

    /// Return unique effective roles in ascending account-index order.
    pub fn roles(&self) -> &[LockRole] {
        &self.roles
    }
    /// Return tokens that may be funded into the lock.
    pub fn tokens(&self) -> &[TokenId] {
        &self.tokens
    }
    /// Return whether an account has the requested lock capability.
    pub fn has_role(&self, account: AccountIndex, role: LockCapability) -> bool {
        self.roles
            .binary_search_by_key(&account, |role| role.account)
            .is_ok_and(|index| self.roles[index].roles.contains(role))
    }
}

impl Deserial for LockConfigSimpleV0 {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let recipients = source.get()?;
        let expiry = source.get()?;
        let count: u16 = source.get()?;
        let roles: Vec<LockRole> = deserial_vector_no_length(source, count.into())?;
        if !roles
            .windows(2)
            .all(|pair| pair[0].account < pair[1].account)
        {
            return Err(
                BlockStateFailure::Invariant("Non-canonical lock roles".to_string()).into(),
            );
        }
        let count: u16 = source.get()?;
        Ok(Self {
            recipients,
            expiry,
            roles,
            tokens: deserial_vector_no_length(source, count.into())?,
            metadata: source.get()?,
        })
    }
}

/// SimpleV0 operation capabilities bitmap.
#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Serial)]
pub struct LockCapabilities(u8);

impl LockCapabilities {
    fn bit(role: LockCapability) -> u8 {
        match role {
            LockCapability::Fund => 0x01,
            LockCapability::Release => 0x02,
            LockCapability::Send => 0x04,
            LockCapability::Cancel => 0x08,
        }
    }

    /// Construct from supported capabilities, ignoring repetitions and input order.
    pub fn new(roles: impl IntoIterator<Item = LockCapability>) -> Self {
        Self(
            roles
                .into_iter()
                .fold(0, |mask, role| mask | Self::bit(role)),
        )
    }

    /// Check whether `role` belongs to this set.
    pub fn contains(self, role: LockCapability) -> bool {
        self.0 & Self::bit(role) != 0
    }

    /// Return the union with `other`.
    pub fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Iterate in canonical Fund, Release, Send, Cancel order.
    pub fn iter(self) -> impl Iterator<Item = LockCapability> {
        CANONICAL_ROLES
            .into_iter()
            .filter(move |role| self.contains(role.clone()))
    }
}

const INVALID_ROLES_MASK: u8 = !0x0f;

impl Deserial for LockCapabilities {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let bitmap: u8 = source.get()?;
        if bitmap & INVALID_ROLES_MASK != 0 {
            return Err(BlockStateFailure::Invariant(
                "Found unknown lock capabilities bit".to_string(),
            )
            .into());
        }
        Ok(Self(bitmap))
    }
}

const CANONICAL_ROLES: [LockCapability; 4] = [
    LockCapability::Fund,
    LockCapability::Release,
    LockCapability::Send,
    LockCapability::Cancel,
];

/// Effective operation capabilities for an account, independent of recipient membership.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockRole {
    account: AccountIndex,
    roles: LockCapabilities,
}

impl LockRole {
    /// Construct an effective role, removing duplicate capabilities.
    /// Empty roles authorize no operation.
    pub fn new(account: AccountIndex, roles: Vec<LockCapability>) -> Self {
        Self {
            account,
            roles: LockCapabilities::new(roles),
        }
    }

    /// Return the account receiving these capabilities.
    pub fn account(&self) -> AccountIndex {
        self.account
    }

    /// Return capabilities in Fund, Release, Send, Cancel order.
    pub fn roles(&self) -> Vec<LockCapability> {
        self.roles.iter().collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::test_stub::BlobStoreStub;
    use crate::persistent::hash::Hashable;
    use concordium_base::common;
    use std::borrow::Borrow;

    fn config_with_roles(roles: Vec<LockRole>) -> LockConfigSimpleV0 {
        LockConfigSimpleV0::new(
            LockRecipients::Any,
            TransactionTime::from(0),
            roles,
            vec![],
            None,
        )
        .unwrap()
    }

    #[test]
    fn capability_bitmap_roundtrips_and_rejects_unknown_bits() {
        for mask in 0u8..=255 {
            let parsed = common::from_bytes_complete::<LockCapabilities>(&[mask]);

            // any bitmask value above 15 is not supported and should fail
            if mask <= 15 {
                let roles = parsed.unwrap();
                assert_eq!(to_bytes(&roles), vec![mask]);
                assert_eq!(LockCapabilities::new(roles.iter()), roles);
                for (index, role) in CANONICAL_ROLES.into_iter().enumerate() {
                    assert_eq!(roles.contains(role), mask & (1 << index) != 0);
                }
            } else {
                assert!(parsed.is_err());
            }
        }
    }

    #[test]
    fn roles_normalize_union_lookup_and_hash_deterministically() {
        use LockCapability::*;
        let role = |account, roles| LockRole::new(AccountIndex::from(account), roles);
        // Persistent state permits empty roles, though the node never creates them.
        let input = vec![
            role(9, vec![Send]),
            role(1, vec![Fund]),
            role(9, vec![Release, Fund]),
            role(5, vec![]),
        ];
        // Input order must not affect the normalized configuration or its encoding.
        let first = config_with_roles(input.clone());
        let second = config_with_roles(input.into_iter().rev().collect());
        assert_eq!(first, second);
        assert_eq!(to_bytes(&first), to_bytes(&second));
        // Normalization sorts accounts, merges duplicates, and retains empty roles.
        assert_eq!(
            first
                .roles()
                .iter()
                .map(|role| role.account().index)
                .collect::<Vec<_>>(),
            vec![1, 5, 9]
        );
        assert!(first.roles()[1].roles().is_empty());
        assert_eq!(first.roles()[2].roles(), vec![Fund, Release, Send]);
        // Lookup finds original capabilities and the union of account 9's roles.
        assert!(first.has_role(AccountIndex::from(1), Fund));
        for role in [Fund, Release, Send] {
            assert!(first.has_role(AccountIndex::from(9), role));
        }
        // Missing capabilities, empty roles, and missing accounts deny access.
        assert!(!first.has_role(AccountIndex::from(9), Cancel));
        for capability in CANONICAL_ROLES {
            assert!(!first.has_role(AccountIndex::from(5), capability));
        }
        assert!(!first.has_role(AccountIndex::from(7), Fund));
        assert!(!config_with_roles(vec![]).has_role(AccountIndex::from(1), Fund));
        // Equivalent normalized configurations must produce identical lock hashes.
        let mut store = BlobStoreStub::default();
        let lock = PersistentLockP11::new(LockConfig::SimpleV0(first.clone()), 1234.into());
        let other = PersistentLockP11::new(LockConfig::SimpleV0(second), 1234.into());
        assert_eq!(lock.hash(&store).unwrap(), other.hash(&store).unwrap());
        // Persistence must preserve both the canonical configuration and its hash.
        let location = blob_store::store_to_store(&mut store, &lock);
        let loaded: PersistentLockP11 = blob_store::load_from_store(&store, location).unwrap();
        assert_eq!(
            loaded.configuration.value(&store).unwrap().0,
            LockConfig::SimpleV0(first)
        );
        assert_eq!(loaded.hash(&store).unwrap(), lock.hash(&store).unwrap());
    }

    #[test]
    fn role_capacity_is_checked_after_merging_duplicates() {
        // Oversized duplicate input fits because only one unique account role is stored.
        let role = LockRole::new(AccountIndex::from(1), vec![LockCapability::Fund]);
        let config = config_with_roles(vec![role; U16_MAX_LENGTH + 1]);
        assert_eq!(config.roles().len(), 1);
        assert!(config.has_role(AccountIndex::from(1), LockCapability::Fund));
    }

    #[test]
    fn tokens_normalize_duplicates_and_order() {
        let config = |tokens| {
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(0),
                vec![],
                tokens,
                None,
            )
            .unwrap()
        };
        let alpha: TokenId = "Alpha".parse().unwrap();
        let beta: TokenId = "Beta".parse().unwrap();
        let first = config(vec![beta.clone(), alpha.clone(), beta.clone()]);
        let second = config(vec![alpha.clone(), beta.clone(), alpha.clone()]);
        assert_eq!(first.tokens(), &[alpha, beta]);
        assert_eq!(first, second);
        assert_eq!(to_bytes(&first), to_bytes(&second));
    }

    #[test]
    fn token_capacity_is_checked_after_removing_duplicates() {
        let config = LockConfigSimpleV0::new(
            LockRecipients::Any,
            TransactionTime::from(0),
            vec![],
            vec!["ExampleToken".parse().unwrap(); U16_MAX_LENGTH + 1],
            None,
        )
        .unwrap();
        assert_eq!(
            config.tokens(),
            &["ExampleToken".parse::<TokenId>().unwrap()]
        );
    }

    #[test]
    fn persisted_roles_reject_noncanonical_accounts_and_masks() {
        // Accounts serialized out of order or repeated are rejected.
        for accounts in [[2, 1], [1, 1]] {
            let mut config = config_with_roles(vec![]);
            config.roles = accounts
                .into_iter()
                .map(|a| LockRole::new(AccountIndex::from(a), vec![]))
                .collect();
            assert!(common::from_bytes_complete::<LockConfigSimpleV0>(&to_bytes(&config)).is_err());
        }

        // unsupported bitmap bits rejected
        let config = config_with_roles(vec![LockRole::new(AccountIndex::from(1), vec![])]);
        let mut bytes = to_bytes(&config);
        bytes[19] = 0x10; // add non-supported bitmap
        assert!(matches!(
            StoreSerialized::<LockConfigSimpleV0>::load_from_buffer(
                bytes.as_slice(),
                &BlobStoreStub::default()
            ),
            Err(BlockStateFailure::BlobStoreDecode(_))
        ));
        assert!(common::from_bytes_complete::<LockConfigSimpleV0>(&bytes).is_err());
        assert_eq!(
            common::from_bytes_complete::<LockConfigSimpleV0>(&to_bytes(&config)).unwrap(),
            config
        );
    }

    #[test]
    fn test_lock_configuration_serial() {
        use concordium_base::common::types::TransactionTime;
        use concordium_base::protocol_level_locks::LockCapability;

        let lock_config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::try_from(vec![
                AccountIndex::from(1u64),
                AccountIndex::from(2u64),
            ])
            .unwrap(),
            expiry: TransactionTime::from(1000u64),

            roles: vec![LockRole::new(
                AccountIndex::from(1u64),
                vec![LockCapability::Fund],
            )],
            tokens: vec!["token1".parse().unwrap()],
            metadata: None,
        });

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(
            hex::encode(&bytes),
            "000100020000000000000001000000000000000200000000000003e80001000000000000000101000106746f6b656e3100"
        );

        let deserialized: LockConfig = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_lock_configuration_serial_empty_recipients() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::try_from(vec![]).unwrap(),
            expiry: TransactionTime::from(500u64),

            roles: vec![],
            tokens: vec![],
            metadata: None,
        });

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(hex::encode(&bytes), "0001000000000000000001f40000000000");

        let deserialized: LockConfig = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_lock_configuration_serial_with_metadata() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(500u64),
            roles: vec![],
            tokens: vec![],
            metadata: Some(RawCbor::from(vec![
                0xa1, 0x64, b'n', b'a', b'm', b'e', 0x64, b't', b'e', b's', b't',
            ])),
        });

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(
            hex::encode(&bytes),
            "000000000000000001f400000000010000000ba1646e616d656474657374"
        );

        let deserialized: LockConfig = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    fn lock_configuration() -> LockConfig {
        LockConfig::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(1000),
                Vec::new(),
                Vec::new(),
                None,
            )
            .unwrap(),
        )
    }

    #[test]
    fn trie_keys_are_deterministic_and_ordered() {
        let lock_ids = [LockId::new(1, 0, 0), LockId::new(1, 0, 1)];
        let balance_refs = [
            BalanceReferenceKey(AccountIndex::from(1), "a".parse().unwrap()),
            BalanceReferenceKey(AccountIndex::from(1), "mixed-length%id".parse().unwrap()),
            BalanceReferenceKey(AccountIndex::from(1), "a".repeat(128).parse().unwrap()),
        ];

        for lock_id in &lock_ids {
            let bytes = TrieKey::to_bytes(lock_id);
            assert_eq!(bytes.borrow().len(), 24);
            assert_eq!(LockId::try_from_bytes(bytes.borrow()).unwrap(), *lock_id);
        }
        for balance_ref in &balance_refs {
            let bytes = balance_ref.to_bytes();
            assert_eq!(&bytes.borrow()[..8], &balance_ref.0.index.to_be_bytes());
            assert_eq!(bytes.borrow().len(), 8 + 1 + balance_ref.1.as_ref().len());
            assert_eq!(
                BalanceReferenceKey::try_from_bytes(bytes.borrow()).unwrap(),
                balance_ref.clone()
            );
        }
        assert!(lock_ids[0].to_bytes().borrow() < lock_ids[1].to_bytes().borrow());
        assert!(balance_refs[0].to_bytes().borrow() < balance_refs[1].to_bytes().borrow());
    }

    #[test]
    fn malformed_trie_keys_are_decode_failures() {
        assert!(matches!(
            LockId::try_from_bytes(&[0]),
            Err(BlockStateFailure::BlobStoreDecode(_))
        ));
        assert!(matches!(
            BalanceReferenceKey::try_from_bytes(&[0; 17]),
            Err(BlockStateFailure::BlobStoreDecode(_))
        ));
    }

    #[test]
    fn expiry_keys_use_fixed_width_ordered_encoding() {
        let store = BlobStoreStub::default();
        let keys = [
            LockExpiryKey {
                expiry: 255.into(),
                created_at_height: 9999.into(),
                lock_id: LockId::new(9, 9, 9),
            },
            LockExpiryKey {
                expiry: 256.into(),
                created_at_height: 255.into(),
                lock_id: LockId::new(9, 9, 9),
            },
            LockExpiryKey {
                expiry: 256.into(),
                created_at_height: 256.into(),
                lock_id: LockId::new(1, 1, 255),
            },
            LockExpiryKey {
                expiry: 256.into(),
                created_at_height: 256.into(),
                lock_id: LockId::new(1, 1, 256),
            },
            LockExpiryKey {
                expiry: 256.into(),
                created_at_height: 256.into(),
                lock_id: LockId::new(1, 2, 0),
            },
            LockExpiryKey {
                expiry: 256.into(),
                created_at_height: 256.into(),
                lock_id: LockId::new(2, 0, 0),
            },
        ];
        let mut index = PersistentLockExpiryIndexP11::empty();
        for key in keys.iter().rev() {
            let bytes = key.to_bytes();
            let bytes = bytes.borrow();
            assert_eq!(bytes.len(), LOCK_EXPIRY_KEY_SIZE);
            assert_eq!(&bytes[..8], &key.expiry.seconds.to_be_bytes());
            assert_eq!(&bytes[8..16], &key.created_at_height.height.to_be_bytes());
            assert_eq!(&bytes[16..], key.lock_id.to_bytes().borrow());
            assert_eq!(LockExpiryKey::try_from_bytes(bytes).unwrap(), *key);
            index = index
                .insert_or_update_entry(&store, key, StoreSerialized(()))
                .unwrap();
        }
        assert_eq!(
            index
                .iter(&store)
                .map(|entry| entry.unwrap().0)
                .collect::<Vec<_>>(),
            keys
        );
        for length in [0, 39, 41] {
            assert!(matches!(
                LockExpiryKey::try_from_bytes(&vec![0; length]),
                Err(BlockStateFailure::BlobStoreDecode(_))
            ));
        }
    }

    #[test]
    fn persistent_lock_store_load_preserves_values_and_hash() {
        let mut store = BlobStoreStub::default();
        let balance_ref = BalanceReferenceKey(AccountIndex::from(1), "Token2".parse().unwrap());
        let mut lock = PersistentLockP11::new(lock_configuration(), 1234.into());
        lock.locked_balances = lock
            .locked_balances
            .insert_or_update_entry(&store, &balance_ref, StoreSerialized(()))
            .unwrap();
        let hash = lock.hash(&store).unwrap();
        let mut later_lock = lock.clone();
        later_lock.created_at_height = 1235.into();
        assert_ne!(later_lock.hash(&store).unwrap(), hash);

        let location = blob_store::store_to_store(&mut store, &lock);
        let loaded: PersistentLockP11 = blob_store::load_from_store(&store, location).unwrap();
        assert_eq!(loaded.hash(&store).unwrap(), hash);
        assert_eq!(loaded.created_at_height, 1234.into());
        assert!(
            loaded
                .locked_balances
                .contains_key(&store, &balance_ref)
                .unwrap()
        );
        assert_eq!(
            loaded.configuration.value(&store).unwrap().0,
            lock_configuration()
        );
        loaded.cache_reference_values(&store).unwrap();

        let mut moved_store = BlobStoreStub::default();
        let moved = loaded.move_blob_store(&store, &mut moved_store).unwrap();
        assert_eq!(moved.hash(&moved_store).unwrap(), hash);
        assert_eq!(moved.created_at_height, 1234.into());
        assert!(
            moved
                .locked_balances
                .contains_key(&moved_store, &balance_ref)
                .unwrap()
        );
    }

    #[test]
    fn typed_key_deletion_removes_entry() {
        let balance_ref = BalanceReferenceKey(AccountIndex::from(1), "Token2".parse().unwrap());
        let trie = BalanceReferences::empty()
            .insert_or_update_entry(&BlobStoreStub::default(), &balance_ref, StoreSerialized(()))
            .unwrap();
        assert!(
            trie.delete_entry(&BlobStoreStub::default(), &balance_ref)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn test_lock_recipients_limited_sorts_and_deduplicates_accounts() {
        let recipients = LockRecipients::try_from(vec![
            AccountIndex::from(2u64),
            AccountIndex::from(1u64),
            AccountIndex::from(2u64),
        ])
        .unwrap();
        let canonical =
            LockRecipients::try_from(vec![AccountIndex::from(1u64), AccountIndex::from(2u64)])
                .unwrap();
        assert_eq!(recipients, canonical);
        assert_eq!(to_bytes(&recipients), to_bytes(&canonical));
        assert_eq!(
            common::from_bytes_complete::<LockRecipients>(&to_bytes(&recipients)).unwrap(),
            recipients
        );
        assert!(recipients.is_recipient(&AccountIndex::from(1)));
        assert!(recipients.is_recipient(&AccountIndex::from(2)));
        assert!(!recipients.is_recipient(&AccountIndex::from(3)));

        for accounts in [[2, 1], [1, 1]] {
            let list = LockRecipientsList {
                recipients: accounts.into_iter().map(AccountIndex::from).collect(),
            };
            assert!(common::from_bytes_complete::<LockRecipientsList>(&to_bytes(&list)).is_err());
            let mut config = config_with_roles(vec![]);
            config.recipients = LockRecipients::Limited(list);
            assert!(common::from_bytes_complete::<LockConfigSimpleV0>(&to_bytes(&config)).is_err());
        }
    }

    #[test]
    fn test_lock_configuration_serial_any_recipient_sentinel() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(500u64),

            roles: vec![],
            tokens: vec![],
            metadata: None,
        });

        let LockConfig::SimpleV0(config) = &lock_config;
        assert!(config.recipients.is_any());
        assert!(config.recipients.is_recipient(&AccountIndex::from(0u64)));
        assert!(config.recipients.is_recipient(&AccountIndex::from(42u64)));

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(hex::encode(&bytes), "000000000000000001f40000000000");

        let deserialized: LockConfig = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
        let LockConfig::SimpleV0(config) = &deserialized;
        assert!(config.recipients.is_any());
    }

    #[test]
    fn test_lock_role_serial() {
        let role = LockRole::new(
            AccountIndex::from(42u64),
            vec![LockCapability::Fund, LockCapability::Release],
        );

        let bytes = common::to_bytes(&role);
        assert_eq!(hex::encode(&bytes), "000000000000002a03");

        let deserialized: LockRole = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, role);
    }

    #[test]
    fn test_lock_configuration_simple_v0_serial() {
        let config = LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(0u64),
            roles: vec![LockRole::new(
                AccountIndex::from(1u64),
                vec![LockCapability::Fund],
            )],
            tokens: vec!["token1".parse::<TokenId>().unwrap()],
            metadata: None,
        };

        let bytes = common::to_bytes(&config);
        assert_eq!(
            hex::encode(&bytes),
            "0000000000000000000001000000000000000101000106746f6b656e3100"
        );

        let deserialized: LockConfigSimpleV0 =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, config);
    }

    #[test]
    fn test_lock_configuration_simple_v0_serial_minimal() {
        let config = LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(0u64),
            roles: vec![],
            tokens: vec![],
            metadata: None,
        };

        let bytes = common::to_bytes(&config);
        assert_eq!(hex::encode(&bytes), "0000000000000000000000000000");

        let deserialized: LockConfigSimpleV0 =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, config);
    }

    #[test]
    fn role_capabilities_are_canonicalized() {
        let role = LockRole::new(
            AccountIndex::from(0),
            vec![
                LockCapability::Cancel,
                LockCapability::Fund,
                LockCapability::Fund,
            ],
        );
        assert_eq!(role.roles(), [LockCapability::Fund, LockCapability::Cancel]);
    }

    #[test]
    fn persistent_container_capacity_limits() {
        assert!(LockRecipients::try_from(vec![AccountIndex::from(0); U16_MAX_LENGTH]).is_ok());
        let duplicates =
            LockRecipientsList::new(vec![AccountIndex::from(0); U16_MAX_LENGTH + 1]).unwrap();
        assert_eq!(duplicates.len(), 1);
        assert_eq!(
            LockRecipients::try_from(
                (0..=U16_MAX_LENGTH)
                    .map(|i| AccountIndex::from(i as u64))
                    .collect::<Vec<_>>()
            ),
            Err(ContainerSizeOverflow {
                field: "LockRecipientsList.recipients",
                length: U16_MAX_LENGTH + 1,
                max_length: U16_MAX_LENGTH,
            })
        );

        assert_eq!(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(0u64),
                (0..=U16_MAX_LENGTH)
                    .map(|i| LockRole::new(
                        AccountIndex::from(i as u64),
                        vec![LockCapability::Fund]
                    ))
                    .collect(),
                Vec::new(),
                None,
            ),
            Err(ContainerSizeOverflow {
                field: "LockConfigSimpleV0.roles",
                length: U16_MAX_LENGTH + 1,
                max_length: U16_MAX_LENGTH,
            })
        );
        assert!(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(0u64),
                Vec::new(),
                (0..U16_MAX_LENGTH)
                    .map(|i| format!("token{i}").parse().unwrap())
                    .collect(),
                None,
            )
            .is_ok()
        );
        assert_eq!(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(0u64),
                Vec::new(),
                (0..=U16_MAX_LENGTH)
                    .map(|i| format!("token{i}").parse().unwrap())
                    .collect(),
                None,
            ),
            Err(ContainerSizeOverflow {
                field: "LockConfigSimpleV0.tokens",
                length: U16_MAX_LENGTH + 1,
                max_length: U16_MAX_LENGTH,
            })
        );
        assert_eq!(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                TransactionTime::from(0u64),
                (0..=U16_MAX_LENGTH)
                    .map(|i| LockRole::new(AccountIndex::from(i as u64), vec![]))
                    .collect(),
                vec!["token".parse().unwrap(); U16_MAX_LENGTH + 1],
                None,
            ),
            Err(ContainerSizeOverflow {
                field: "LockConfigSimpleV0.roles",
                length: U16_MAX_LENGTH + 1,
                max_length: U16_MAX_LENGTH,
            })
        );
    }

    #[test]
    fn test_lock_configuration_enum_serial() {
        let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(0u64),
            roles: vec![LockRole::new(
                AccountIndex::from(1u64),
                vec![LockCapability::Fund],
            )],
            tokens: vec!["token1".parse::<TokenId>().unwrap()],
            metadata: None,
        });

        let bytes = common::to_bytes(&config);
        assert_eq!(
            hex::encode(&bytes),
            "000000000000000000000001000000000000000101000106746f6b656e3100"
        );

        let deserialized: LockConfig = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, config);
    }
}
