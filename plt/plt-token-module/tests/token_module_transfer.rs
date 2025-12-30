use assert_matches::assert_matches;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, RawCbor, TokenModuleRejectReasonType,
};
use kernel_stub::KernelStub;
use plt_token_module::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{
    self, TokenInitializationError, TokenUpdateError, TransactionContext,
};

mod kernel_stub;

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// In this example, the parameters are not a valid encoding.
#[test]
fn test_update_token_decode_failure() {
    let mut stub = KernelStub::new(0);
    let account = stub.create_account();
    let context = TransactionContext {
        sender: account,
        sender_address: stub.account_canonical_address(&account),
    };
    let res =
        token_module::execute_token_update_transaction(&mut stub, context, RawCbor::from(vec![]));
    assert_matches!(
        &res,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            let reject_reason = reject_reason.decode_reject_reason().unwrap();

            assert_matches!(reject_reason, TokenModuleRejectReasonType::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(cause)
                }) => {
                assert!(cause.contains("IO error"), "cause: {}", cause);
            });

        }
    );
}

// /// In this example, minimal parameters are specified to check defaulting
// /// behaviour.
// #[test]
// fn test_initialize_token_default_values() {
//     let mut stub = KernelStub::new(0);
//     let gov_account = stub.create_account();
//     let gov_holder_account = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
//     let metadata = MetadataUrl::from("https://plt.token".to_string());
//     let encoded_metadata = cbor::cbor_encode(&metadata).unwrap();
//     let parameters = TokenModuleInitializationParameters {
//         name: Some("Protocol-level token".to_owned()),
//         metadata: Some(metadata.clone()),
//         governance_account: Some(gov_holder_account.clone()),
//         allow_list: None,
//         deny_list: None,
//         initial_supply: None,
//         mintable: None,
//         burnable: None,
//         additional: Default::default(),
//     };
//     let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
//     token_module::initialize_token(&mut stub, encoded_parameters).unwrap();
//
//     // assertions directly on token state
//     assert_eq!(
//         stub.get_token_state(b"\0\0name".into()),
//         Some(b"Protocol-level token".into())
//     );
//     assert_eq!(
//         stub.get_token_state(b"\0\0metadata".into()),
//         Some(encoded_metadata)
//     );
//     assert_eq!(
//         stub.get_token_state(b"\0\0governanceAccount".into()),
//         Some(stub.account_index(&gov_account).index.to_be_bytes().into())
//     );
//     assert_eq!(stub.get_token_state(b"\0\0allowList".into()), None);
//     assert_eq!(stub.get_token_state(b"\0\0denyList".into()), None);
//     assert_eq!(stub.get_token_state(b"\0\0mintable".into()), None);
//     assert_eq!(stub.get_token_state(b"\0\0burnable".into()), None);
//     assert_eq!(stub.get_token_state(b"\0\0paused".into()), None);
//     // assert governance account balance
//     assert_eq!(stub.account_balance(&gov_account), RawTokenAmount(0));
//     // assertions using token module state query
//     let state: TokenModuleState =
//         cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
//     assert_eq!(state.name, Some("Protocol-level token".to_owned()));
//     assert_eq!(state.metadata, Some(metadata));
//     assert_eq!(state.governance_account, Some(gov_holder_account));
//     assert_eq!(state.allow_list, Some(false));
//     assert_eq!(state.deny_list, Some(false));
//     assert_eq!(state.mintable, Some(false));
//     assert_eq!(state.burnable, Some(false));
//     assert_eq!(state.paused, Some(false));
//     assert!(state.additional.is_empty());
// }
//
//
// /// In this example, the parameters specify an initial supply with less precision
// /// than the token allows.
// #[test]
// fn test_initialize_token_insufficient_mint_decimals() {
//     let mut stub = KernelStub::new(6);
//     let gov_account = stub.create_account();
//     let metadata = "https://plt.token".to_owned().into();
//     let parameters = TokenModuleInitializationParameters {
//         name: Some("Protocol-level token".to_owned()),
//         metadata: Some(metadata),
//         governance_account: Some(stub.account_canonical_address(&gov_account).into()),
//         allow_list: Some(false),
//         deny_list: Some(false),
//         initial_supply: Some(TokenAmount::from_raw(500000, 2)),
//         mintable: Some(false),
//         burnable: Some(false),
//         additional: Default::default(),
//     };
//     let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
//     let res = token_module::initialize_token(&mut stub, encoded_parameters);
//     assert_matches!(
//         res,
//         Err(TokenInitializationError::MintAmountDecimalsMismatch(
//             TokenAmountDecimalsMismatchError {
//                 expected: 6,
//                 found: 2
//             }
//         ))
//     );
// }
