use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, MintWouldOverflowRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAdminRole, TokenAmount,
    TokenModuleRejectReason, TokenOperation, TokenSupplyUpdateDetails,
    TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use plt_token_module::token_module;
use utils::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};

mod utils;

/// Test successful mints.
#[test]
fn test_mint() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable());

    // First mint
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(1000)
    );

    // Second mint
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(5000)
    );
}

/// Rejects mint operations from non-governance accounts.
#[test]
fn test_unauthorized_mint() {
    // Arrange a token and an unauthorized sender.
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let non_governance_account = stub.create_account();

    // Attempt to mint as a non-governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(non_governance_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    // Assert the operation is rejected with the unauthorized sender details.
    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index,
            address,
            ..
        }) => {
            assert_eq!(index, 0);
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_address(&non_governance_account)
                ))
            );
        }
    );

    // Assert balances remain unchanged.
    assert_eq!(stub.account_token_balance(&gov_account), RawTokenAmount(0));
    assert_eq!(
        stub.account_token_balance(&non_governance_account),
        RawTokenAmount(0)
    );

    // and that no events have been logged
    assert_eq!(stub.events().len(), 0);
}

/// Rejects mint operations from non-governance accounts. Test
/// send operation using alias account address. Check that
/// address in reject reason is the alias and not the canonical address.
#[test]
fn test_unauthorized_mint_using_alias() {
    // Arrange a token and an unauthorized sender.
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    stub.init_token(TokenInitTestParams::default());
    let non_gov_account = stub.create_account();

    let non_gov_account_address_alias =
        stub.account_address(&non_gov_account).get_alias(5).unwrap();
    let non_gov_account_alias = stub
        .account_by_address(&non_gov_account_address_alias)
        .unwrap();

    // Attempt to mint as a non-governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(non_gov_account_alias);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address,
            ..
        }) => {
            // Assert the address alias is used in the reject reason.
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_address(&non_gov_account_alias)
                ))
            );
        }
    );
}

/// Test mint that would overflow circulating supply
#[test]
fn test_mint_overflow() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable());
    stub.set_account_balance(gov_account, RawTokenAmount(1000));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(
        MintWouldOverflowRejectReason {
            requested_amount,
            current_supply,
            max_representable_amount,
            ..
        }) => {
            assert_eq!(requested_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2));
            assert_eq!(current_supply, TokenAmount::from_raw(1000, 2));
            assert_eq!(max_representable_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0, 2));
    });
}

/// Test mint with initial supply specified with wrong number of decimals
#[test]
fn test_mint_decimals_mismatch() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

/// Reject "mint" operations while token is paused
#[test]
fn test_mint_paused() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    stub.set_paused(true);

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );
}

/// Reject "mint" operation if the feature is not enabled.
#[test]
fn test_not_mintable() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];

    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason{
            index: 0,
            operation_type,
            reason: Some(reason)
        }) if reason == "feature not enabled" && operation_type == "mint"
    );
}

/// Reject when governance account is not holding the mint role.
#[test]
fn test_reject_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable());

    // 1st transaction: removing mint role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: attempting to mint as governance account
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(200, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: Some(address),
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

/// Succeeds when another account holds the mint role.
#[test]
fn test_new_account_with_role_succeeds() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().mintable());
    let account2 = stub.create_account();

    // 1st transaction: Assign the mint role to an account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::Mint],
            account: CborHolderAccount::from(account2.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: Mint as account.
    let mut execution = TransactionExecutionTestImpl::with_sender(account2);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(200, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(stub.account_token_balance(&gov_account), RawTokenAmount(0));
    assert_eq!(stub.account_token_balance(&account2), RawTokenAmount(200));
}
