use concordium_base::{
    base::AccountIndex,
    common::Serialize,
    protocol_level_locks::LockControllerSimpleV0Capability,
    protocol_level_tokens::{
        CborMemo, TokenId, TokenOperation,
        meta_operations::{LockOperation, MetaUpdateOperation},
    },
};

/// A discriminated version of [`MetaUpdateOperation`] for the purpose of
/// dispatching to the appropriate operation handler.
#[derive(PartialEq, Debug, Clone)]
pub enum MetaUpdateOperationKind {
    /// A [`TokenOperation`] for a specific [`TokenId`].
    Token((TokenId, TokenOperation)),
    /// A [`LockOperation`].
    Lock(LockOperation),
}

impl From<MetaUpdateOperation> for MetaUpdateOperationKind {
    fn from(value: MetaUpdateOperation) -> Self {
        match value {
            MetaUpdateOperation::Transfer(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Transfer(details)))
            }
            MetaUpdateOperation::Mint(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Mint(details)))
            }
            MetaUpdateOperation::Burn(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Burn(details)))
            }
            MetaUpdateOperation::AddAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AddAllowList(details)))
            }
            MetaUpdateOperation::RemoveAllowList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RemoveAllowList(details)))
            }
            MetaUpdateOperation::AddDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AddDenyList(details)))
            }
            MetaUpdateOperation::RemoveDenyList(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RemoveDenyList(details)))
            }
            MetaUpdateOperation::Pause(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Pause(details)))
            }
            MetaUpdateOperation::Unpause(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::Unpause(details)))
            }
            MetaUpdateOperation::AssignAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::AssignAdminRoles(details)))
            }
            MetaUpdateOperation::RevokeAdminRoles(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::RevokeAdminRoles(details)))
            }
            MetaUpdateOperation::UpdateMetadata(details) => {
                let (token_id, details) = details.into();
                Self::Token((token_id, TokenOperation::UpdateMetadata(details)))
            }
            MetaUpdateOperation::LockFund(details) => Self::Lock(LockOperation::Fund(details)),
            MetaUpdateOperation::LockSend(details) => Self::Lock(LockOperation::Send(details)),
            MetaUpdateOperation::LockReturn(details) => Self::Lock(LockOperation::Return(details)),
            MetaUpdateOperation::LockCreate(details) => Self::Lock(LockOperation::Create(details)),
            MetaUpdateOperation::LockCancel(details) => Self::Lock(LockOperation::Cancel(details)),
        }
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
    fn test_meta_operation_token_operation_conversion() {
        use concordium_base::protocol_level_tokens::meta_operations::*;
        use concordium_base::protocol_level_tokens::*;
        // For each meta-update operation variant:
        // - construct a meta-update operation with some test data
        // - construct the corresponding token operation with the same test data
        // - convert in each direction and check that the result matches the original
        // - construct the meta-update operation using the `meta_operations` helper function
        //   and check that it matches the original
        let token_id: TokenId = "tokenid1".parse().unwrap();
        let amount = TokenAmount::from_raw(100000, 2);
        const ADDRESS: common::types::AccountAddress = common::types::AccountAddress([
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
            0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
            0x1D, 0x1E, 0x1F, 0x20,
        ]);
        let account = CborHolderAccount::from(ADDRESS);
        let cbor_memo = CborMemo::Raw(Memo::try_from(vec![1, 2, 3, 4]).unwrap());
        let memo = Some(cbor_memo.clone());

        let token_transfer = TokenOperation::Transfer(TokenTransfer {
            amount,
            recipient: account.clone(),
            memo: memo.clone(),
        });
        let meta_transfer = MetaUpdateOperation::Transfer(MetaTokenTransfer {
            token: token_id.clone(),
            amount,
            recipient: account.clone(),
            memo: memo.clone(),
        });
        assert_eq!(
            transfer_tokens_with_memo(token_id.clone(), ADDRESS, amount, cbor_memo.clone()),
            meta_transfer
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_transfer.clone())),
            meta_transfer
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_transfer)),
            meta_transfer.into(),
        );

        let token_mint = TokenOperation::Mint(TokenSupplyUpdateDetails { amount });
        let meta_mint = MetaUpdateOperation::Mint(MetaTokenSupplyUpdateDetails {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(mint_tokens(token_id.clone(), amount), meta_mint);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_mint.clone())),
            meta_mint
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_mint)),
            meta_mint.into(),
        );

        let token_burn = TokenOperation::Burn(TokenSupplyUpdateDetails { amount });
        let meta_burn = MetaUpdateOperation::Burn(MetaTokenSupplyUpdateDetails {
            token: token_id.clone(),
            amount,
        });
        assert_eq!(burn_tokens(token_id.clone(), amount), meta_burn);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_burn.clone())),
            meta_burn
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_burn)),
            meta_burn.into(),
        );

        let token_add_allow_list = TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_add_allow_list = MetaUpdateOperation::AddAllowList(MetaTokenListUpdateDetails {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            add_token_allow_list(token_id.clone(), ADDRESS),
            meta_add_allow_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_add_allow_list.clone())),
            meta_add_allow_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_add_allow_list)),
            meta_add_allow_list.into(),
        );

        let token_remove_allow_list = TokenOperation::RemoveAllowList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_remove_allow_list =
            MetaUpdateOperation::RemoveAllowList(MetaTokenListUpdateDetails {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            remove_token_allow_list(token_id.clone(), ADDRESS),
            meta_remove_allow_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_remove_allow_list.clone())),
            meta_remove_allow_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_remove_allow_list)),
            meta_remove_allow_list.into(),
        );

        let token_add_deny_list = TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_add_deny_list = MetaUpdateOperation::AddDenyList(MetaTokenListUpdateDetails {
            token: token_id.clone(),
            target: account.clone(),
        });
        assert_eq!(
            add_token_deny_list(token_id.clone(), ADDRESS),
            meta_add_deny_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_add_deny_list.clone())),
            meta_add_deny_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_add_deny_list)),
            meta_add_deny_list.into(),
        );

        let token_remove_deny_list = TokenOperation::RemoveDenyList(TokenListUpdateDetails {
            target: account.clone(),
        });
        let meta_remove_deny_list =
            MetaUpdateOperation::RemoveDenyList(MetaTokenListUpdateDetails {
                token: token_id.clone(),
                target: account.clone(),
            });
        assert_eq!(
            remove_token_deny_list(token_id.clone(), ADDRESS),
            meta_remove_deny_list
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_remove_deny_list.clone())),
            meta_remove_deny_list
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_remove_deny_list)),
            meta_remove_deny_list.into(),
        );

        let token_pause = TokenOperation::Pause(TokenPauseDetails {});
        let meta_pause = MetaUpdateOperation::Pause(MetaTokenPauseDetails {
            token: token_id.clone(),
        });
        assert_eq!(pause(token_id.clone()), meta_pause);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_pause.clone())),
            meta_pause
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_pause)),
            meta_pause.into(),
        );

        let token_unpause = TokenOperation::Unpause(TokenPauseDetails {});
        let meta_unpause = MetaUpdateOperation::Unpause(MetaTokenPauseDetails {
            token: token_id.clone(),
        });
        assert_eq!(unpause(token_id.clone()), meta_unpause);
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_unpause.clone())),
            meta_unpause
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_unpause)),
            meta_unpause.into(),
        );

        let assign_roles = vec![TokenAdminRole::Mint, TokenAdminRole::Pause];
        let token_assign_admin_roles =
            TokenOperation::AssignAdminRoles(TokenUpdateAdminRolesDetails {
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        let meta_assign_admin_roles =
            MetaUpdateOperation::AssignAdminRoles(MetaTokenUpdateAdminRolesDetails {
                token: token_id.clone(),
                roles: assign_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            assign_admin_roles(token_id.clone(), ADDRESS, assign_roles.clone()),
            meta_assign_admin_roles
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_assign_admin_roles.clone())),
            meta_assign_admin_roles
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_assign_admin_roles)),
            meta_assign_admin_roles.into(),
        );

        let revoke_roles = vec![
            TokenAdminRole::Burn,
            TokenAdminRole::UpdateMetadata,
            TokenAdminRole::UpdateDenyList,
        ];
        let token_revoke_admin_roles =
            TokenOperation::RevokeAdminRoles(TokenUpdateAdminRolesDetails {
                roles: revoke_roles.clone(),
                account: account.clone(),
            });
        let meta_revoke_admin_roles =
            MetaUpdateOperation::RevokeAdminRoles(MetaTokenUpdateAdminRolesDetails {
                token: token_id.clone(),
                roles: revoke_roles.clone(),
                account: account.clone(),
            });
        assert_eq!(
            revoke_admin_roles(token_id.clone(), ADDRESS, revoke_roles.clone()),
            meta_revoke_admin_roles
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_revoke_admin_roles.clone())),
            meta_revoke_admin_roles
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_revoke_admin_roles)),
            meta_revoke_admin_roles.into(),
        );

        let metadata_url = MetadataUrl {
            url: "https://example.com/metadata.json".to_string(),
            checksum_sha_256: Some([0u8; 32].into()),
            additional: Default::default(),
        };
        let token_update_metadata = TokenOperation::UpdateMetadata(metadata_url.clone());
        let meta_update_metadata = MetaUpdateOperation::UpdateMetadata(MetaMetadataUrlDetails {
            token: token_id.clone(),
            metadata_url: metadata_url.clone(),
        });
        assert_eq!(
            update_metadata(token_id.clone(), metadata_url.clone()),
            meta_update_metadata
        );
        assert_eq!(
            MetaUpdateOperation::from((token_id.clone(), token_update_metadata.clone())),
            meta_update_metadata
        );
        assert_eq!(
            MetaUpdateOperationKind::Token((token_id.clone(), token_update_metadata)),
            meta_update_metadata.into(),
        );
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
