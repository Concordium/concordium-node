use concordium_base::common::__serialize_private::anyhow::bail;
use concordium_base::common::{
    Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial, Serialize,
};

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

/// Serialization of 'TokenRawAmount' is as a variable length quantity (VLQ).
///
/// The VLQ encoding represents a value in big-endian base 128. Each byte of the encoding uses
/// the high-order bit to indicate if further bytes follow (when set). The remaining bits represent
/// the positional value in base 128. See https://en.wikipedia.org/wiki/Variable-length_quantity
impl Serial for RawTokenAmount {
    fn serial<B: Buffer>(&self, out: &mut B) {
        // Maximum number of bytes a u64 can be serialized to.
        const MAX_BYTES: usize = 10;
        // Use buffer for the serialized bytes since we produce bytes
        // in the opposite order of which we want to write them
        // (we produce the least significant first).
        let mut buffer = [0; MAX_BYTES];
        let mut buffer_index = MAX_BYTES - 1;

        let mut val = self.0;

        // The least significant byte. This byte is always there
        // and never has the continuation byte set, since it is the last.
        let byte = val as u8 & 0x7fu8;
        buffer[buffer_index] = byte;
        val >>= 7;

        // Following bytes in order of more significant. Continuation
        // bit (0x80) is always set, since there is always a following byte.
        while val != 0 {
            let byte = 0x80u8 | (val as u8 & 0x7fu8);
            buffer_index -= 1;
            buffer[buffer_index] = byte;
            val >>= 7;
        }

        // Write bytes in correct order.
        for byte in &buffer[buffer_index..MAX_BYTES] {
            out.put(&byte);
        }
    }
}

/// Deserialization of 'TokenRawAmount' is as a variable length quantity (VLQ). We disallow
/// 0-padding to enforce canonical serialization.
///
/// The VLQ encoding represents a value in big-endian base 128. Each byte of the encoding uses
/// the high-order bit to indicate if further bytes follow (when set). The remaining bits represent
/// the positional value in base 128. See https://en.wikipedia.org/wiki/Variable-length_quantity
impl Deserial for RawTokenAmount {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        // Decode first byte and use 7 bits from it.
        let mut byte: u8 = source.get()?;
        let mut value = 0x7f & byte as u64;

        // Check for 0-padding.
        if 0x80 & byte != 0 && value == 0 {
            bail!("Token amount padded with zeros");
        }

        // Decode additional byte if 8'th bit is set.
        while 0x80 & byte != 0 {
            // Check if shifting would overflow the value.
            if value & (0x7f << (64 - 7)) != 0 {
                bail!("Token amount not representable as u64");
            }

            // Decode additional byte and use 7 bits from it as least significant bytes in value.
            byte = source.get()?;
            value <<= 7;
            value |= 0x7f & byte as u64;
        }

        Ok(Self(value))
    }
}

/// Protocol level token (PLT) amount representation. The numerical amount
/// represented is `value * 10^(-decimals)`.
/// The number of decimals in the token amount should always match the number of
/// decimals for the token it represents an amount for.
///
/// Corresponding Haskell type: `Concordium.Types.Tokens.TokenAmount`
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
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
    use concordium_base::common::ParseResult;
    use proptest::prelude::ProptestConfig;
    use proptest::{prop_assert, prop_assert_eq, proptest};

    /// Test special cases for successful raw token amount serialization/deserialization
    /// and for deserialization failures. This test is supplemented
    /// with the property test [`prop_test_raw_token_amount_serial`].
    #[test]
    fn test_raw_token_amount_serial() {
        let token_amount = RawTokenAmount(0);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "00");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "01");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(2);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "02");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(127);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "7f");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "8100");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(129);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "8101");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128 - 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "ff7f");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "818000");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128 + 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "818001");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128 * 128 - 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "ffff7f");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128 * 128);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "81808000");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(128 * 128 * 128 + 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "81808001");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(u64::MAX - 1);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "81ffffffffffffffff7e");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        let token_amount = RawTokenAmount(u64::MAX);
        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "81ffffffffffffffff7f");
        let token_amount_deserialized: RawTokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);

        // Test out of range value
        let bytes = hex::decode("82808080808080808000").unwrap();
        let err = common::from_bytes_complete::<RawTokenAmount>(&mut bytes.as_slice())
            .expect_err("deserialize");
        assert!(
            err.to_string()
                .contains("Token amount not representable as u64"),
            "err: {}",
            err
        );

        // Test out of range value
        let bytes = hex::decode("84808080808080808000").unwrap();
        let err = common::from_bytes_complete::<RawTokenAmount>(&mut bytes.as_slice())
            .expect_err("deserialize");
        assert!(
            err.to_string()
                .contains("Token amount not representable as u64"),
            "err: {}",
            err
        );

        // Test value padded with 0
        let bytes = hex::decode("8001").unwrap();
        let err = common::from_bytes_complete::<RawTokenAmount>(&mut bytes.as_slice())
            .expect_err("deserialize");
        assert!(
            err.to_string().contains("Token amount padded with zeros"),
            "err: {}",
            err
        );

        // Test value padded with 0
        let bytes = hex::decode("808101").unwrap();
        let err = common::from_bytes_complete::<RawTokenAmount>(&mut bytes.as_slice())
            .expect_err("deserialize");
        assert!(
            err.to_string().contains("Token amount padded with zeros"),
            "err: {}",
            err
        );
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10000))]

        /// Property test of raw token amount serialization/deserialization. Special cases
        /// are tested in the test [`test_raw_token_amount_serial`].
        #[test]
        fn prop_test_raw_token_amount_serial(value: u64) {
            let token_amount = RawTokenAmount(value);
            let bytes = common::to_bytes(&token_amount);
            let token_amount_deserialized_result: ParseResult<RawTokenAmount> =
                common::from_bytes_complete(bytes.as_slice());
            prop_assert!(token_amount_deserialized_result.is_ok());
            let token_amount_deserialized = token_amount_deserialized_result.unwrap();
            prop_assert_eq!(token_amount_deserialized, token_amount);
        }
    }

    #[test]
    fn test_token_amount_serial() {
        let token_amount = TokenAmount {
            amount: RawTokenAmount(1000),
            decimals: 4,
        };

        let bytes = common::to_bytes(&token_amount);
        assert_eq!(hex::encode(&bytes), "876804");

        let token_amount_deserialized: TokenAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_amount_deserialized, token_amount);
    }
}
