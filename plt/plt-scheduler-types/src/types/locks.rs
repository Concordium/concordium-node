use concordium_base::{
    base::AccountIndex,
    common::Serialize,
    protocol_level_locks::LockControllerSimpleV0Capability,
    protocol_level_tokens::{CborMemo, TokenId},
};

/// Top-level lock controller type.
///
/// Each variant represents a different controller version.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub enum LockController {
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
    pub tokens: Vec<TokenId>,
    /// Whether the lock should be kept alive after all funds are
    /// returned.
    pub keep_alive: bool,
    /// Optional memo attached to the lock.
    pub memo: Option<CborMemo>,
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
        let controller = LockController::SimpleV0(LockControllerSimpleV0 {
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

        let deserialized: LockController = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, controller);
    }
}
