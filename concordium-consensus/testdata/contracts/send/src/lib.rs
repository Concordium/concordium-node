#![cfg_attr(not(feature = "std"), no_std)]

use concordium_std::*;

// This module defines two contracts, c10 (address <1, 0>) and c20 (address <2, 0>)
// created in the `ReceiveContextTest`. This test has transactions which will
// 1. deploy this module
// 2. initialize contract c10
// 3. initialize contract c20
// 4. invoke function c20.call_c10 which will invoke function c10.check_receive_context.
//    The latter will check that its receive context matches the data in `ReceiveContextTest`.

#[contract_state(contract = "c1")]
#[derive(Serialize, SchemaType)]
pub struct State {}

#[init(contract = "c20")]
#[inline(always)]
fn c20_init(_ctx: &impl HasInitContext<()>) -> InitResult<State> {
    Ok(State{})
}

#[init(contract = "c10", payable)]
#[inline(always)]
fn c10_init(_ctx: &impl HasInitContext<()>, _amount: Amount) -> InitResult<State> {
    Ok(State{})
}

#[receive(contract = "c20", name = "call_c10", payable)]
fn call_c10<A: HasActions>(
    ctx: &impl HasReceiveContext<()>,
    amount: Amount,
    _state: &mut State,
) -> ReceiveResult<A> {
    ensure!(ctx.sender() == Address::Account(THOMAS_ACCOUNT));
    Ok(A::send_raw(&ContractAddress{ index: 1, subindex: 0 }, ReceiveName::new_unchecked("c10.check_receive_gcontext"), amount, &[]))
}

const ALES_ACCOUNT: AccountAddress = AccountAddress([1; 32]);
const THOMAS_ACCOUNT: AccountAddress = AccountAddress([2; 32]);

#[receive(contract = "c10", name = "check_receive_context", payable)]
fn check_receive_context<A: HasActions>(
    ctx: &impl HasReceiveContext<()>,
    _amount: Amount,
    _state: &mut State,
) -> ReceiveResult<A> {
    if ctx.invoker() == THOMAS_ACCOUNT &&
       ctx.self_address() == (ContractAddress {index: 1, subindex: 0}) &&
       ctx.self_balance() == (Amount {micro_gtu: 42}) && // The newly added balance should not be part of the ctx balance
       ctx.sender() == Address::Contract(ContractAddress {index: 2, subindex: 0}) &&
       ctx.owner() == ALES_ACCOUNT {
       Ok(A::accept())
    } else {
        bail!()
    }
}
