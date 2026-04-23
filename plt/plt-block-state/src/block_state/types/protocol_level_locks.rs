use concordium_base::base::AccountIndex;
use concordium_base::common::Serialize;
use concordium_base::common::types::TransactionTime;
use plt_scheduler_types::types::locks::LockControllerConfig;

/// Lock configuration at the block state level.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockConfiguration {
    /// Accounts that can receive funds from this lock.
    #[size_length = 2]
    pub recipients: Vec<AccountIndex>,
    /// Expiry time of the lock (seconds since epoch).
    pub expiry: TransactionTime,
    /// Controller configuration for the lock.
    pub controller: LockControllerConfig,
}

#[cfg(test)]
mod test {
    use crate::block_state::types::protocol_level_locks::LockConfiguration;
    use concordium_base::base::AccountIndex;
    use concordium_base::common;
    use plt_scheduler_types::types::locks::LockControllerConfig;

    #[test]
    fn test_lock_configuration_serial() {
        use concordium_base::common::types::TransactionTime;
        use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;
        use plt_scheduler_types::types::locks::{
            LockControllerSimpleV0, LockControllerSimpleV0Grant,
        };

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
            controller: LockControllerConfig::SimpleV0(
                plt_scheduler_types::types::locks::LockControllerSimpleV0 {
                    grants: vec![],
                    tokens: vec![],
                    keep_alive: false,
                    memo: None,
                },
            ),
        };

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(hex::encode(&bytes), "000000000000000001f400000000000000");

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }
}
