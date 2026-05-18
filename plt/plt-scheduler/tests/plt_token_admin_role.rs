//! Tests for token RBAC admin role operations via the scheduler.

use crate::utils::TokenInitTestParams;
use assert_matches::assert_matches;
use concordium_base::base::{Energy, ProtocolVersion};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAdminRole, TokenAuthorizations, TokenId, TokenOperation,
    TokenOperationsPayload, TokenPauseDetails, TokenUpdateAdminRolesDetails,
};
use concordium_base::transactions::Payload;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::execution::TransactionOutcome;

mod utils;

/// The governance account receives every role except for disabled features.
#[test]
fn test_rbac_initial_governance_account_have_every_role() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().deny_list(),
        2,
        None,
    );

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(
        auth.update_admin_roles
            .unwrap()
            .accounts
            .contains(&gov.into())
    );
    assert!(auth.mint.unwrap().accounts.contains(&gov.into()));
    assert!(auth.burn.is_none());
    assert!(auth.update_allow_list.is_none());
    assert!(
        auth.update_deny_list
            .unwrap()
            .accounts
            .contains(&gov.into())
    );
    assert!(auth.pause.unwrap().accounts.contains(&gov.into()));
    assert!(auth.update_metadata.unwrap().accounts.contains(&gov.into()));
}

/// Assign multiple roles to a new account succeeds.
#[test]
fn test_rbac_assign_roles() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // Assign mint and burn to account2.
    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(
        auth.update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov.into())
    );
    assert!(auth.mint.as_ref().unwrap().accounts.contains(&gov.into()));
    assert!(auth.burn.as_ref().unwrap().accounts.contains(&gov.into()));
    assert!(auth.update_allow_list.is_none());
    assert!(auth.update_deny_list.is_none());
    assert!(auth.pause.as_ref().unwrap().accounts.contains(&gov.into()));
    assert!(
        auth.update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov.into())
    );

    let acc = stub.account_canonical_address(&account2).into();
    assert!(
        !auth
            .update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
    assert!(auth.mint.as_ref().unwrap().accounts.contains(&acc));
    assert!(auth.burn.as_ref().unwrap().accounts.contains(&acc));
    assert!(auth.update_allow_list.is_none());
    assert!(auth.update_deny_list.is_none());
    assert!(!auth.pause.as_ref().unwrap().accounts.contains(&acc));
    assert!(
        !auth
            .update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
}

/// Assign the same role to the same account twice succeeds.
#[test]
fn test_rbac_assign_same_roles() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // Assign mint and burn to account2.
    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr.clone(),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Assign mint to account2 again.
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        2.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let acc = stub.account_canonical_address(&account2).into();
    assert!(
        !auth
            .update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
    assert!(auth.mint.as_ref().unwrap().accounts.contains(&acc));
    assert!(auth.burn.as_ref().unwrap().accounts.contains(&acc));
    assert!(!auth.pause.as_ref().unwrap().accounts.contains(&acc));
    assert!(
        !auth
            .update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
}

/// Assign rejects when not holding the admin role.
#[test]
fn test_rbac_assign_unauthorization_sender_rejects() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Assign fails on P10 (RBAC not supported).
#[test]
fn test_rbac_assign_rejects_p10() {
    let mut stub = BlockStateWithExternalStateStubbed::new(ProtocolVersion::P10);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Assign succeeds when paused.
#[test]
fn test_rbac_assign_role_works_when_paused() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // Pause the token.
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Pause(
            TokenPauseDetails {},
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Assign mint and burn to account2 while paused.
    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        2.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let acc = stub.account_canonical_address(&account2).into();
    assert!(
        !auth
            .update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
    assert!(auth.mint.as_ref().unwrap().accounts.contains(&acc));
    assert!(auth.burn.as_ref().unwrap().accounts.contains(&acc));
    assert!(!auth.pause.as_ref().unwrap().accounts.contains(&acc));
}

/// Assign rejects when using a role for a feature which is not enabled.
#[test]
fn test_rbac_assign_rejects_for_unabled_burn() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(), // burn not enabled
        2,
        None,
    );
    let account2 = stub.create_account();

    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Revoke roles from the governance account.
#[test]
fn test_rbac_revoke_roles() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default()
            .mintable()
            .burnable()
            .allow_list(),
        2,
        None,
    );

    // Revoke mint and burn from gov_account.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let gov = stub.account_canonical_address(&gov_account).into();
    assert!(auth.update_admin_roles.unwrap().accounts.contains(&gov));
    assert!(!auth.mint.as_ref().unwrap().accounts.contains(&gov));
    assert!(!auth.burn.as_ref().unwrap().accounts.contains(&gov));
    assert!(auth.update_allow_list.unwrap().accounts.contains(&gov));
    assert!(auth.update_deny_list.is_none()); // Not enabled.
    assert!(auth.pause.as_ref().unwrap().accounts.contains(&gov));
    assert!(
        auth.update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov)
    );
}

/// Revoke the same role from the same account twice succeeds.
#[test]
fn test_rbac_revoke_same_roles() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default()
            .mintable()
            .burnable()
            .allow_list(),
        2,
        None,
    );

    // Revoke mint and burn.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr.clone(),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Revoke mint again.
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        2.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let gov = stub.account_canonical_address(&gov_account).into();
    assert!(
        auth.update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov)
    );
    assert!(!auth.mint.as_ref().unwrap().accounts.contains(&gov));
    assert!(!auth.burn.as_ref().unwrap().accounts.contains(&gov));
    assert!(
        auth.update_allow_list
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov)
    );
    assert!(auth.pause.as_ref().unwrap().accounts.contains(&gov));
    assert!(
        auth.update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov)
    );
}

/// Revoke rejects when not holding the admin role.
#[test]
fn test_rbac_revoke_rejects_without_admin_role() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // account2 tries to revoke from gov — no admin role.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Revoke fails on P10 (RBAC not supported).
#[test]
fn test_rbac_revoke_rejects_p10() {
    let mut stub = BlockStateWithExternalStateStubbed::new(ProtocolVersion::P10);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );

    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Revoke succeeds when paused.
#[test]
fn test_rbac_revoke_role_works_when_paused() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );

    // Pause the token.
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Pause(
            TokenPauseDetails {},
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Revoke mint and burn while paused.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        2.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();

    let acc = stub.account_canonical_address(&gov_account).into();
    assert!(
        auth.update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
    assert!(!auth.mint.as_ref().unwrap().accounts.contains(&acc));
    assert!(!auth.burn.as_ref().unwrap().accounts.contains(&acc));
    assert!(auth.pause.as_ref().unwrap().accounts.contains(&acc));
    assert!(
        auth.update_metadata
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
}

/// Revoke rejects when revoking the admin role from the sender themselves.
#[test]
fn test_rbac_revoke_admin_role_from_sender_rejects() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);

    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAdminRoles],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Revoke rejects when using a role for a feature which is not enabled.
#[test]
fn test_rbac_revoke_rejects_for_unabled_burn() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(), // burn not enabled
        2,
        None,
    );

    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Rejected(_));
}

/// Admin role rotation: assign admin role to account2, then account2 revokes gov's admin role.
#[test]
fn test_rbac_admin_role_rotation_succeeds() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // Assign updateAdminRoles to account2.
    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let sender_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAdminRoles],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // account2 revokes updateAdminRoles from gov.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let sender_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAdminRoles],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let auth: TokenAuthorizations = cbor::cbor_decode(
        &queries::query_token_authorizations(stub.state(), &token_id)
            .unwrap()
            .details,
    )
    .unwrap();
    let gov_acc = stub.account_canonical_address(&gov_account).into();
    assert!(
        !auth
            .update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&gov_acc)
    );
    let acc = stub.account_canonical_address(&account2).into();
    assert!(
        auth.update_admin_roles
            .as_ref()
            .unwrap()
            .accounts
            .contains(&acc)
    );
}
