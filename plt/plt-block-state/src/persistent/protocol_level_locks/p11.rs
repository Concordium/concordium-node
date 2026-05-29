use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash::Hashable;
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::common::types::TransactionTime;
use concordium_base::common::{Buffer, Serialize};
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

/// Lock configuration at the block state level.
///
/// TODO: COR-2295 - proper state implementation
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockConfiguration {
    /// Accounts that can receive funds from this lock.
    ///
    /// The recipients are stored as a sorted vector of account indices, and
    /// the number of recipients is limited to `u16::MAX` to ensure that the
    /// serialized form fits within the size limits of the block state.
    // todo use newtype for the type for this field and make all fields public (for consistency with the other block state types)
    #[size_length = 2]
    recipients: Vec<AccountIndex>,
    /// Expiry time of the lock (seconds since epoch).
    expiry: TransactionTime,
    /// Controller configuration for the lock.
    controller: LockControllerConfig,
}

impl LockConfiguration {
    pub fn new(
        mut recipients: Vec<AccountIndex>,
        expiry: TransactionTime,
        controller: LockControllerConfig,
    ) -> Self {
        assert!(recipients.len() <= u16::MAX as usize, "Too many recipients");
        recipients.sort();
        Self {
            recipients,
            expiry,
            controller,
        }
    }

    /// Get an iterator over the recipient accounts.
    pub fn recipients_iter(&self) -> impl Iterator<Item = &AccountIndex> {
        self.recipients.iter()
    }

    /// Check if the given account is a recipient.
    pub fn is_recipient(&self, account: &AccountIndex) -> bool {
        self.recipients.binary_search(account).is_ok()
    }

    /// Get the expiry time of the lock.
    pub fn expiry(&self) -> TransactionTime {
        self.expiry
    }

    /// Get the lock controller configuration.
    pub fn controller(&self) -> &LockControllerConfig {
        &self.controller
    }
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
            recipients: vec![AccountIndex::from(1u64), AccountIndex::from(2u64)],
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
            recipients: vec![],
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
