use base58::ToBase58;
use concordium_crypto_eddsa_ed25519;

use sha2::{Digest, Sha224};
pub const ADDRESS_SCHEME: u8 = 2;

#[derive(Default, Debug)]
pub struct KeyPair {
    pub public_key:  [u8; 32],
    pub private_key: [u8; 32],
}

impl KeyPair {
    pub fn new() -> Self {
        let mut sk: [u8; 32] = [0; 32];
        let mut pk: [u8; 32] = [0; 32];
        concordium_crypto_eddsa_ed25519::eddsa_priv_key(&mut sk);
        concordium_crypto_eddsa_ed25519::eddsa_pub_key(&sk, &mut pk);
        KeyPair {
            public_key:  pk,
            private_key: sk,
        }
    }

    pub fn private_key_as_base64(&self) -> String { base64::encode(&self.private_key) }

    pub fn public_key_as_base64(&self) -> String { base64::encode(&self.public_key) }

    // Address is generated following next rule:
    // `<ADDRESS_SCHEME> + MostSignificantBits_160( SHA_224( public_key))`
    pub fn address(&self) -> String {
        let hasher: Sha224 = Sha224::default();
        let pk_hash = hasher.chain(&self.public_key).result();

        format!("{}{}", ADDRESS_SCHEME, pk_hash[..20].to_base58())
    }
}

#[cfg(test)]
mod unit_test {
    use super::*;

    #[test]
    pub fn key_pair_ctor() {
        let kp_1 = KeyPair::new();
        let kp_1_sk = kp_1.private_key_as_base64();
        let kp_1_pk = kp_1.public_key_as_base64();

        assert_eq!(kp_1_sk.is_empty(), false);
        assert_eq!(kp_1_pk.is_empty(), false);
    }
}
