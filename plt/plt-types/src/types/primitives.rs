/// Token amount without decimals specified. The token amount represented by
/// this type must always be represented with the number of decimals
/// the token natively has.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct RawTokenAmount(pub u64);

impl RawTokenAmount {
    /// Maximum representable raw token amount.
    pub const MAX: Self = Self(u64::MAX);
}
