use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash::Hashable;
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::common::types::TransactionTime;
use concordium_base::common::{Buffer, Deserial, ParseResult, ReadBytesExt, Serial, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::{CborMemo, TokenId};
use std::collections::{BTreeMap, BTreeSet};
use std::io::Read;

/// Block state for protocol level locks on P11 and later protocols that uses the same representation.
#[derive(Debug, Clone, Default)]
pub struct PersistentLocksP11 {
    pub(crate) locks: StoreSerialized<BTreeMap<LockId, PersistentLockP11>>,
}

impl Loadable for PersistentLocksP11 {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let locks = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self { locks })
    }
}

impl Storable for PersistentLocksP11 {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.locks.store_to_buffer(buffer, storer)
    }
}

impl Cacheable for PersistentLocksP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.locks.cache_reference_values(loader)
    }
}

impl Hashable for PersistentLocksP11 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.locks.hash(loader)
    }
}

/// The block state for a single protocol-level lock.
#[derive(Debug, Clone, Serialize)]
pub struct PersistentLockP11 {
    /// Contains references to the tokens with balances locked within this lock
    pub locked_balances: BTreeSet<(AccountIndex, TokenIndex)>,
    /// The configuration parameters for the lock.
    pub configuration: LockConfiguration,
}

/// Sentinel account index used to represent an any-recipient lock in block state.
///
/// This temporary representation is local to node block-state code and maps
/// back to the external `"any"` representation during query/event conversion.
/// TODO: COR-2418 - proper "any" recipient representation in block state should remove this.
const ANY_RECIPIENT_SENTINEL: AccountIndex = AccountIndex { index: u64::MAX };

// Represents a list of lock recipients. This type enforces that the inner list is always sorted
// to enable binary search.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LockRecipientsList(Vec<AccountIndex>);

impl LockRecipientsList {
    /// Create a new list of lock recipients from the given account index list
    pub fn new(mut recipients: Vec<AccountIndex>) -> Self {
        recipients.sort();
        Self(recipients)
    }

    /// Get an iterator of the account indices in the list
    pub fn iter(&self) -> impl Iterator<Item = &AccountIndex> {
        self.0.iter()
    }

    /// Check whether the given account is a member
    pub fn is_recipient(&self, account: &AccountIndex) -> bool {
        self.0.binary_search(account).is_ok()
    }

    /// Get the length of the list
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check whether the list is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Accounts that can receive funds from this lock in block state.
#[derive(Debug, Clone, Eq, PartialEq)]
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
            Self::Limited(recipients) => recipients.0.binary_search(account).is_ok(),
        }
    }
}

impl From<Vec<AccountIndex>> for LockRecipients {
    fn from(recipients: Vec<AccountIndex>) -> Self {
        Self::Limited(LockRecipientsList::new(recipients))
    }
}

impl Serial for LockRecipients {
    fn serial<B: Buffer>(&self, out: &mut B) {
        match self {
            Self::Any => {
                1u16.serial(out);
                ANY_RECIPIENT_SENTINEL.serial(out);
            }
            Self::Limited(recipients) => {
                (recipients.len() as u16).serial(out);
                for recipient in recipients.iter() {
                    recipient.serial(out);
                }
            }
        }
    }
}

impl Deserial for LockRecipients {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let len = u16::deserial(source)? as usize;
        let mut recipients = Vec::with_capacity(len);
        for _ in 0..len {
            recipients.push(AccountIndex::deserial(source)?);
        }

        Ok(if recipients.as_slice() == [ANY_RECIPIENT_SENTINEL] {
            Self::Any
        } else {
            Self::from(recipients)
        })
    }
}

/// Lock configuration at the block state level.
///
/// TODO: COR-2295 - proper state implementation
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockConfiguration {
    /// Accounts that can receive funds from this lock.
    pub recipients: LockRecipients,
    /// Expiry time of the lock (seconds since epoch).
    pub expiry: TransactionTime,
    /// Controller configuration for the lock.
    pub controller: LockControllerConfig,
}

/// Top-level lock controller type.
///
/// Each variant represents a different controller version.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub enum LockControllerConfig {
    /// SimpleV0 lock controller configuration.
    SimpleV0(LockControllerSimpleV0),
}

/// Configuration for a SimpleV0 lock controller.
///
/// Contains the list of capability grants, which tokens are affected,
/// a keep-alive flag, and an optional memo.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockControllerSimpleV0 {
    /// Capability grants to accounts.
    #[size_length = 2]
    pub grants: Vec<LockControllerSimpleV0Grant>,
    /// Tokens affected by this lock controller.
    #[size_length = 2]
    // todo change to TokenIndex?
    pub tokens: Vec<TokenId>,
    /// Whether the lock should be kept alive after all funds are
    /// returned.
    pub keep_alive: bool,
    /// Optional memo attached to the lock.
    pub memo: Option<CborMemo>,
}

impl LockControllerSimpleV0 {
    /// Check if an account has a specified role.
    pub fn has_role(&self, account: AccountIndex, role: LockControllerSimpleV0Capability) -> bool {
        self.grants
            .iter()
            .any(|grant| grant.account == account && grant.roles.contains(&role))
    }
}

/// A grant of capabilities to a specific account for a SimpleV0 lock
/// controller.
///
/// Each grant assigns one or more [`LockControllerSimpleV0Capability`] roles
/// to the given account, authorizing it to perform the corresponding lock
/// operations.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockControllerSimpleV0Grant {
    /// The account receiving the grant.
    pub account: AccountIndex,
    /// The capabilities granted to the account.
    #[size_length = 1]
    pub roles: Vec<LockControllerSimpleV0Capability>,
}

#[cfg(test)]
mod test {
    use super::*;
    use concordium_base::common;
    use concordium_base::transactions::Memo;

    #[test]
    fn test_lock_configuration_serial() {
        use concordium_base::common::types::TransactionTime;
        use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;

        let lock_config = LockConfiguration {
            recipients: LockRecipients::from(vec![
                AccountIndex::from(1u64),
                AccountIndex::from(2u64),
            ]),
            expiry: TransactionTime::from(1000u64),
            controller: LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: vec![LockControllerSimpleV0Grant {
                    account: AccountIndex::from(1u64),
                    roles: vec![LockControllerSimpleV0Capability::Fund],
                }],
                tokens: vec!["token1".parse().unwrap()],
                keep_alive: true,
                memo: None,
            }),
        };

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(
            hex::encode(&bytes),
            "00020000000000000001000000000000000200000000000003e800000100000000000000010100000106746f6b656e310100"
        );

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_lock_configuration_serial_empty_recipients() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfiguration {
            recipients: LockRecipients::from(vec![]),
            expiry: TransactionTime::from(500u64),
            controller: LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: vec![],
                tokens: vec![],
                keep_alive: false,
                memo: None,
            }),
        };

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(hex::encode(&bytes), "000000000000000001f400000000000000");

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_lock_recipients_limited_sorts_accounts() {
        let recipients =
            LockRecipients::from(vec![AccountIndex::from(2u64), AccountIndex::from(1u64)]);

        assert_eq!(
            recipients,
            LockRecipients::from(vec![AccountIndex::from(1u64), AccountIndex::from(2u64)])
        );
    }

    #[test]
    fn test_lock_configuration_serial_any_recipient_sentinel() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfiguration {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(500u64),
            controller: LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants: vec![],
                tokens: vec![],
                keep_alive: false,
                memo: None,
            }),
        };

        assert!(lock_config.recipients.is_any());
        assert!(
            lock_config
                .recipients
                .is_recipient(&AccountIndex::from(0u64))
        );
        assert!(
            lock_config
                .recipients
                .is_recipient(&AccountIndex::from(42u64))
        );

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(
            hex::encode(&bytes),
            "0001ffffffffffffffff00000000000001f400000000000000"
        );

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
        assert!(deserialized.recipients.is_any());
    }

    #[test]
    fn test_lock_controller_simple_v0_grant_serial() {
        let grant = LockControllerSimpleV0Grant {
            account: AccountIndex::from(42u64),
            roles: vec![
                LockControllerSimpleV0Capability::Fund,
                LockControllerSimpleV0Capability::Return,
            ],
        };

        let bytes = common::to_bytes(&grant);
        assert_eq!(hex::encode(&bytes), "000000000000002a020001");

        let deserialized: LockControllerSimpleV0Grant =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, grant);
    }

    #[test]
    fn test_lock_controller_simple_v0_serial() {
        let controller = LockControllerSimpleV0 {
            grants: vec![LockControllerSimpleV0Grant {
                account: AccountIndex::from(1u64),
                roles: vec![LockControllerSimpleV0Capability::Fund],
            }],
            tokens: vec!["token1".parse::<TokenId>().unwrap()],
            keep_alive: true,
            memo: Some(CborMemo::Raw(
                Memo::try_from(vec![0x01, 0x02, 0x03]).unwrap(),
            )),
        };

        let bytes = common::to_bytes(&controller);
        assert_eq!(
            hex::encode(&bytes),
            "000100000000000000010100000106746f6b656e310101000003010203"
        );

        let deserialized: LockControllerSimpleV0 =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, controller);
    }

    #[test]
    fn test_lock_controller_simple_v0_serial_minimal() {
        let controller = LockControllerSimpleV0 {
            grants: vec![],
            tokens: vec![],
            keep_alive: false,
            memo: None,
        };

        let bytes = common::to_bytes(&controller);
        assert_eq!(hex::encode(&bytes), "000000000000");

        let deserialized: LockControllerSimpleV0 =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, controller);
    }

    #[test]
    fn test_lock_controller_serial() {
        let controller = LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
            grants: vec![LockControllerSimpleV0Grant {
                account: AccountIndex::from(1u64),
                roles: vec![LockControllerSimpleV0Capability::Fund],
            }],
            tokens: vec!["token1".parse::<TokenId>().unwrap()],
            keep_alive: true,
            memo: None,
        });

        let bytes = common::to_bytes(&controller);
        assert_eq!(
            hex::encode(&bytes),
            "00000100000000000000010100000106746f6b656e310100"
        );

        let deserialized: LockControllerConfig =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, controller);
    }
}
