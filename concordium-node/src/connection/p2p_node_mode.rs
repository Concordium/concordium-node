#[derive(Clone, Copy, Debug, PartialEq)]
pub enum P2PNodeMode {
    NormalMode,
    NormalPrivateMode,
    BootstrapperMode,
    BootstrapperPrivateMode,
}
