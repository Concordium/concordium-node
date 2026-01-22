use assert_matches::assert_matches;
use concordium_base::{
    common::cbor,
    protocol_level_tokens::{
        OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenModuleRejectReasonEnum,
        TokenModuleState, TokenOperation, TokenPauseDetails, TokenSupplyUpdateDetails,
    },
};
use plt_token_module::{
    token_kernel_interface::{RawTokenAmount, TokenKernelQueries},
    token_module,
};

use crate::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};

mod kernel_stub;
mod utils;

/// Test that pause/unpause operations modify the token module state as expected
#[test]
fn test_token_pause_state() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // Assert initial state matches expectations
    assert_eq!(stub.lookup_token_state_value(b"\0\0paused".into()), None);
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(false));

    // First we pause the token
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // Assert pause has taken effect
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0paused".into()),
        Some(vec![])
    );
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(true));

    // Then we unpause the token
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // Assert unpause has taken effect
    assert_eq!(stub.lookup_token_state_value(b"\0\0paused".into()), None);
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(false));
}

/// Accept performing a "pause" operation on a token that is already paused is permitted. This
/// ensures that "pause" operations are _not_:
/// - rejected due to being redundant
/// - affected by the paused state of the token, which is only meant to affect balance-changing
///   operations.
#[test]
fn test_double_pause() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // First we try to perform a double "pause" operation within the same transaction.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![
        TokenOperation::Pause(TokenPauseDetails {}),
        TokenOperation::Pause(TokenPauseDetails {}),
    ];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // Then we try to perform an "pause" operation on top of this state in a subsequent
    // transaction (with a new transaction execution context, for good measure).
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(true));
}

/// Accept performing an "unpause" operation on a token that is _not_ paused is permitted
#[test]
fn test_redundant_unpause() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // We already verified that the token moodule is initially _not_ paused, so performing an
    // "unpause" operation on this state is already redundant.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(false));
}

/// Rejects pause operations from non-governance accounts.
#[test]
fn test_unauthorized_pause() {
    // Arrange a token and an unauthorized sender.
    let mut stub = KernelStub::with_decimals(0);
    stub.init_token(TokenInitTestParams::default());
    let non_governance_account = stub.create_account();

    // Attempt to pause as a non-governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(non_governance_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let _res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    // TODO: test authorization PSR-26
    // Assert the operation is rejected with the unauthorized sender details.
    // let reject_reason = utils::assert_reject_reason(&res);
    // assert_matches!(
    //     reject_reason,
    //     TokenModuleRejectReasonEnum::OperationNotPermitted(OperationNotPermittedRejectReason {
    //         index,
    //         address,
    //         ..
    //     }) => {
    //         assert_eq!(index, 0);
    //         assert_eq!(
    //             address,
    //             Some(CborHolderAccount::from(
    //                 stub.account_canonical_address(&non_governance_account)
    //             ))
    //         );
    //     }
    // );
    //
    // // Assert the token remains unpaused.
    // let state: TokenModuleState =
    //     cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    // assert_eq!(state.paused, Some(false));
}

/// Rejects unpause operations from non-governance accounts.
#[test]
fn test_unauthorized_unpause() {
    // Arrange a token and an unauthorized sender.
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let non_governance_account = stub.create_account();

    // First we pause the token in order to verify that the state is not changed from performing
    // the unauthorized "unpause" operation subsequently.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // Attempt to unpause as a non-governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(non_governance_account);
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let _res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    // TODO: test authorization PSR-26
    // Assert the operation is rejected with the unauthorized sender details.
    // let reject_reason = utils::assert_reject_reason(&res);
    // assert_matches!(
    //     reject_reason,
    //     TokenModuleRejectReasonEnum::OperationNotPermitted(OperationNotPermittedRejectReason {
    //         index,
    //         address,
    //         ..
    //     }) => {
    //         assert_eq!(index, 0);
    //         assert_eq!(
    //             address,
    //             Some(CborHolderAccount::from(
    //                 stub.account_canonical_address(&non_governance_account)
    //             ))
    //         );
    //     }
    // );
    //
    // // Assert the token remains unpaused.
    // let state: TokenModuleState =
    //     cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    // assert_eq!(state.paused, Some(true));
}

/// Rejects token update transactions with a "pause" operation and a subsequent operation not
/// permitted due to the paused token state.
#[test]
fn test_pause_multiple_ops() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // We test that a transaction consisting of a "pause" and "mint" operation fails, as minting is
    // not allowed while a token is paused.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![
        TokenOperation::Pause(TokenPauseDetails {}),
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(1000, 2),
        }),
    ];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReasonEnum::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 1,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );

    // Assert that no tokens were minted
    assert_eq!(stub.account_token_balance(&gov_account), RawTokenAmount(0));
    assert_eq!(stub.circulating_supply(), RawTokenAmount(0));
}

/// Accepts token update transactions with an "unpause" operation and a subsequent operation not
/// permitted while token is paused.
#[test]
fn test_unpause_multiple_ops() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // First we set the token to paused.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // We test that a transaction consisting of an "unpause" and "mint" operation succeeds when
    // executed sequentially within the same transaction.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![
        TokenOperation::Unpause(TokenPauseDetails {}),
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(1000, 2),
        }),
    ];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // Assert the state changes happen as expected.
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.paused, Some(false));
    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(1000)
    );
}
