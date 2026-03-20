use concordium_base::base::ProtocolVersion;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAdminRole, TokenAuthorizations, TokenOperation,
    TokenPauseDetails, TokenUpdateAdminRolesDetails,
};
use plt_token_module::token_module;
use utils::kernel_stub::{KernelStub, TokenInitTestParams};

mod utils;

/// The governance account receives every role except for disabled features.
#[test]
fn test_rbac_initial_governance_account_have_every_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().deny_list());

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(auth.update_admin_roles.contains(&gov));
    assert!(auth.mint.contains(&gov));
    assert!(!auth.burn.contains(&gov));
    assert!(!auth.update_allow_list.contains(&gov));
    assert!(auth.update_deny_list.contains(&gov));
    assert!(auth.pause.contains(&gov));
    assert!(auth.update_metadata.contains(&gov));
}

/// Assign multiple roles to a new account succeeds.
#[test]
fn test_rbac_assign_roles() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(auth.update_admin_roles.contains(&gov));
    assert!(auth.mint.contains(&gov));
    assert!(auth.burn.contains(&gov));
    assert!(!auth.update_allow_list.contains(&gov)); // Not enabled.
    assert!(!auth.update_deny_list.contains(&gov)); // Not enabled.
    assert!(auth.pause.contains(&gov));
    assert!(auth.update_metadata.contains(&gov));

    let acc = stub.account_canonical_address(&account2);
    assert!(!auth.update_admin_roles.contains(&acc));
    assert!(auth.mint.contains(&acc));
    assert!(auth.burn.contains(&acc));
    assert!(!auth.update_allow_list.contains(&acc));
    assert!(!auth.update_deny_list.contains(&acc));
    assert!(!auth.pause.contains(&acc));
    assert!(!auth.update_metadata.contains(&acc));
}

/// Assign the same role to the same account twice succeeds.
#[test]
fn test_rbac_assign_same_roles() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // transaction: assign mint to account2 again
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let acc = stub.account_canonical_address(&account2);
    assert!(!auth.update_admin_roles.contains(&acc));
    assert!(auth.mint.contains(&acc));
    assert!(auth.burn.contains(&acc));
    assert!(!auth.update_allow_list.contains(&acc));
    assert!(!auth.update_deny_list.contains(&acc));
    assert!(!auth.pause.contains(&acc));
    assert!(!auth.update_metadata.contains(&acc));
}

/// Assign rejects when not holding the admin role.
#[test]
fn test_rbac_assign_unauthorization_sender_rejects() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(account2);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Assign fails on P10.
#[test]
fn test_rbac_assign_rejects_p10() {
    let mut stub = KernelStub::with_decimals(2, ProtocolVersion::P10);
    stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(account2);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Assign succeeds when paused.
#[test]
fn test_rbac_assign_role_works_when_paused() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // 1st transaction: pause the token
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let acc = stub.account_canonical_address(&account2);
    assert!(!auth.update_admin_roles.contains(&acc));
    assert!(auth.mint.contains(&acc));
    assert!(auth.burn.contains(&acc));
    assert!(!auth.update_allow_list.contains(&acc));
    assert!(!auth.update_deny_list.contains(&acc));
    assert!(!auth.pause.contains(&acc));
    assert!(!auth.update_metadata.contains(&acc));
}

/// Assign rejects when using role for a feature which is not enabled.
#[test]
fn test_rbac_assign_rejects_for_unabled_burn() {
    let mut stub = KernelStub::with_decimals(2, ProtocolVersion::P10);
    stub.init_token(TokenInitTestParams::default().mintable());
    let account2 = stub.create_account();

    // transaction: assign mint and burn to account2
    let mut execution = stub.execution_with_sender(account2);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Revoke roles from the governance account.
#[test]
fn test_rbac_revoke_roles() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(
        TokenInitTestParams::default()
            .mintable()
            .burnable()
            .allow_list(),
    );

    // transaction: Revoke mint and burn from gov_account
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(auth.update_admin_roles.contains(&gov));
    assert!(!auth.mint.contains(&gov));
    assert!(!auth.burn.contains(&gov));
    assert!(auth.update_allow_list.contains(&gov)); // Not enabled.
    assert!(!auth.update_deny_list.contains(&gov)); // Not enabled.
    assert!(auth.pause.contains(&gov));
    assert!(auth.update_metadata.contains(&gov));
}

/// Revoke the same role from the same account twice succeeds.
#[test]
fn test_rbac_revoke_same_roles() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(
        TokenInitTestParams::default()
            .mintable()
            .burnable()
            .allow_list(),
    );

    // transaction: Revoke mint and burn from gov_account
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // transaction: Revoke mint from gov_account again
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let gov = stub.account_canonical_address(&gov_account);
    assert!(auth.update_admin_roles.contains(&gov));
    assert!(!auth.mint.contains(&gov));
    assert!(!auth.burn.contains(&gov));
    assert!(auth.update_allow_list.contains(&gov));
    assert!(!auth.update_deny_list.contains(&gov));
    assert!(auth.pause.contains(&gov));
    assert!(auth.update_metadata.contains(&gov));
}

/// Revoke rejects when not holding the admin role.
#[test]
fn test_rbac_revoke_rejects_without_admin_role() {
    let mut stub = KernelStub::with_decimals(2, ProtocolVersion::P10);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // transaction: revoke mint and burn from gov account
    let mut execution = stub.execution_with_sender(account2);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Revoke fails on P10.
#[test]
fn test_rbac_revoke_rejects_p10() {
    let mut stub = KernelStub::with_decimals(2, ProtocolVersion::P10);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());

    // transaction: Revoke mint and burn from gov_account
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Revoke succeeds when paused.
#[test]
fn test_rbac_revoke_role_works_when_paused() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());

    // 1st transaction: pause the token
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: revoke mint and burn from gov account
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();

    let acc = stub.account_canonical_address(&gov_account);
    assert!(auth.update_admin_roles.contains(&acc));
    assert!(!auth.mint.contains(&acc));
    assert!(!auth.burn.contains(&acc));
    assert!(!auth.update_allow_list.contains(&acc));
    assert!(!auth.update_deny_list.contains(&acc));
    assert!(auth.pause.contains(&acc));
    assert!(auth.update_metadata.contains(&acc));
}

/// Revoke rejects when revoking admin role from sender.
#[test]
fn test_rbac_revoke_admin_role_from_sender_rejects() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // transaction: Revoke updateAdminRole from gov_account
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAdminRoles],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Revoke rejects when using role for a feature which is not enabled.
#[test]
fn test_rbac_revoke_rejects_for_unabled_burn() {
    let mut stub = KernelStub::with_decimals(2, ProtocolVersion::P10);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable());

    // transaction: Revoke mint and burn to from gov
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint, TokenAdminRole::Burn],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .unwrap_err();
}

/// Revoke succeeds when revoking admin role from another account.
#[test]
fn test_rbac_admin_role_rotation_succeeds() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable().burnable());
    let account2 = stub.create_account();

    // 1st transaction: Assign account2 the updateAdminRole.
    let mut execution = stub.execution_with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAdminRoles],
            account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: revoke updateAdminRole from gov_account
    let mut execution = stub.execution_with_sender(account2);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAdminRoles],
            account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let auth = token_module::query_token_authorizations(&stub).unwrap();
    let auth: TokenAuthorizations = cbor::cbor_decode(auth).unwrap();
    let gov_acc = stub.account_canonical_address(&gov_account);
    assert!(!auth.update_admin_roles.contains(&gov_acc));
    let acc = stub.account_canonical_address(&account2);
    assert!(auth.update_admin_roles.contains(&acc));
}
