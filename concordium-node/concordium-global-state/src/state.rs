use crate::common::Account;

#[derive(Debug, Default)]
pub struct BlockState {
    accounts: Box<[Account]>,
    // instances: Box<[Instance]>,
    // modules: Box<[Module]>,
    // bank: ,
    // identity_providers: ,
    // parameters: BirkParameters,
}
