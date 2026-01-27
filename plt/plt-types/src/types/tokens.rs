use concordium_base::common::{Buffer, Put, Serial};

/// Token amount without decimals specified. The token amount represented by
/// this type must always be represented with the number of decimals
/// the token natively has.
///
/// Corresponding Haskell type: `Concordium.Types.Tokens.TokenRawAmount`
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct RawTokenAmount(pub u64);

impl RawTokenAmount {
    /// Maximum representable raw token amount.
    pub const MAX: Self = Self(u64::MAX);
}

/// Serialization of 'TokenRawAmount' is as a variable length quantity (VLQ). We disallow
/// 0-padding to enforce canonical serialization.
///
/// The VLQ encoding represents a value in big-endian base 128. Each byte of the encoding uses
/// the high-order bit to indicate if further bytes follow (when set). The remaining bits represent
/// the positional value in base 128. See https://en.wikipedia.org/wiki/Variable-length_quantity
impl Serial for RawTokenAmount {
    fn serial<B: Buffer>(&self, out: &mut B) {
        let mut val = self.0;
        let mut bytes = Vec::new();

        // The least significant byte. This byte is always there
        // and never has the continuation byte set, since it is the last.
        let byte = val as u8 & 0x7fu8;
        bytes.push(byte);
        val >>= 7;

        // Following bytes in order of more significant. Continuation
        // byte (0x80) is always set, since there is always a following byte.
        while val != 0 {
            let byte = 0x80u8 | (val as u8 & 0x7fu8);
            bytes.push(byte);
            val >>= 7;
        }

        for byte in bytes.iter().rev() {
            out.put(byte);
        }
    }
}

/// Protocol level token (PLT) amount representation. The numerical amount
/// represented is `value * 10^(-decimals)`.
/// The number of decimals in the token amount should always match the number of
/// decimals for the token it represents an amount for.
///
/// Corresponding Haskell type: `Concordium.Types.Tokens.TokenAmount`
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serial)]
pub struct TokenAmount {
    /// The amount of tokens as an unscaled integer value.
    pub amount: RawTokenAmount,
    /// The number of decimals in the token amount.
    pub decimals: u8,
}

#[cfg(test)]
mod test {
    use crate::types::tokens::{RawTokenAmount, TokenAmount};
    use concordium_base::common;

    #[test]
    fn test_raw_token_amount_serial() {
        let token_amount = RawTokenAmount(0);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "00");

        let token_amount = RawTokenAmount(1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "01");

        let token_amount = RawTokenAmount(2);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "02");

        let token_amount = RawTokenAmount(127);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "7f");

        let token_amount = RawTokenAmount(128);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "8100");

        let token_amount = RawTokenAmount(129);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "8101");

        let token_amount = RawTokenAmount(128 * 128 - 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "ff7f");

        let token_amount = RawTokenAmount(128 * 128);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "818000");

        let token_amount = RawTokenAmount(128 * 128 + 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "818001");

        let token_amount = RawTokenAmount(u64::MAX);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "81ffffffffffffffff7f");
    }

    #[test]
    fn test_token_amount_serial() {
        let token_amount = TokenAmount {
            amount: RawTokenAmount(1000),
            decimals: 4,
        };

        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "876804");
    }
}
