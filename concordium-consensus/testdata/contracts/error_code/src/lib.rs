#![cfg_attr(not(feature = "std"), no_std)]

use concordium_std::*;

// This module defines `InitError` and `ReceiveError` enums. We will test that returning
// errors of these types results in the correct Wasm error codes in the scheduler.
// We also create extra error types to test the wrapping of multiple existing error types
// into enum variants.

#[contract_state(contract = "error_codes")]
#[derive(Serialize, SchemaType)]
pub struct State {}

#[derive(Reject)]
enum InitError {
    VeryBadError, // will be converted to -1
    // testing that we can successfully return both errors from a function that returns an `InitError`
    #[from(ParseError, SpecialInitError)]
    ParseErrorWrapper, // will be converted to -2
    #[from(MostSpecialInitError)]
    SomeOtherError, // will be converted to -3
}

impl Default for InitError {
    fn default() -> Self {
        InitError::VeryBadError
    }
}

#[derive(Reject)]
enum SpecialInitError {
    VerySpecialError, // will be converted to -2
}

#[derive(Reject)]
enum MostSpecialInitError {
    SuperSpecialError, // will be converted to -3
    TheAmazingError,   // will be converted to -3
}

#[init(contract = "error_codes", payable)]
#[inline(always)]
fn init_error_codes(_ctx: &impl HasInitContext<()>, amount: Amount) -> Result<State, InitError> {
    if amount.micro_gtu == 1 {
        Err(InitError::VeryBadError)
    } else if amount.micro_gtu == 2 {
        Err(ParseError::default().into())
    } else if amount.micro_gtu == 3 {
        Err(InitError::ParseErrorWrapper)
    } else if amount.micro_gtu == 4 {
        Err(SpecialInitError::VerySpecialError.into())
    } else if amount.micro_gtu == 5 {
        Err(MostSpecialInitError::SuperSpecialError.into())
    } else if amount.micro_gtu == 6 {
        Err(MostSpecialInitError::TheAmazingError.into())
    } else {
        Ok(State {})
    }
}

#[derive(Reject)]
enum ReceiveError {
    VeryBadError, // will be converted to -1
    // testing that we can successfully return both errors from a function that returns an `InitError`
    #[from(ParseError, SpecialReceiveError)]
    ParseErrorWrapper, // will be converted to -2
}

#[derive(Reject)]
enum SpecialReceiveError {
    VerySpecialError, // will be converted to -2
}

#[derive(Serialize)]
enum ReceiveParameter {
    Something,
}

#[receive(
    contract = "error_codes",
    name = "receive",
    payable,
    parameter = "ReceiveParameter"
)]
fn receive_error_codes<A: HasActions>(
    ctx: &impl HasReceiveContext<()>,
    amount: Amount,
    _state: &mut State,
) -> Result<A, ReceiveError> {
    if amount.micro_gtu == 1 {
        Err(ReceiveError::VeryBadError)
    } else if amount.micro_gtu == 2 {
        let _param: ReceiveParameter = ctx.parameter_cursor().get()?; // Should return ParseError
        Ok(A::accept())
    } else if amount.micro_gtu == 3 {
        Err(ReceiveError::ParseErrorWrapper)
    } else {
        Err(SpecialReceiveError::VerySpecialError.into())
    }
}

#[receive(contract = "error_codes", name = "receive2")]
fn receive_error_codes2<A: HasActions>(
    _ctx: &impl HasReceiveContext<()>,
    _state: &mut State,
) -> Result<A, ParseError> {
    Err(ParseError::default())
}

#[receive(contract = "error_codes", name = "receive3")]
fn receive_error_codes3<A: HasActions>(
    _ctx: &impl HasReceiveContext<()>,
    _state: &mut State,
) -> Result<A, ()> {
    Err(())
}

#[receive(contract = "error_codes", name = "receive_send")]
fn receive_error_codes_send<A: HasActions>(
    _ctx: &impl HasReceiveContext<()>,
    _state: &mut State,
) -> Result<A, ()> {
    // This should result in an error code corresponding to ParseError
    Ok(A::send(&ContractAddress{ index: 0, subindex: 0 }, "error_codes.receive2", Amount::zero(), &[]))
}

