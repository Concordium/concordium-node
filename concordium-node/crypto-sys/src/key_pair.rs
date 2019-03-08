use super::{ ed25519_public_key, ed25519_secret_key, priv_key, public_key, ADDRESS_SCHEME };
use sha2::{ Sha224, Digest};
use base58::{ ToBase58 };



#[derive (Debug)]
pub struct KeyPair
{
    pub public_key: ed25519_public_key,
    pub private_key: ed25519_secret_key,
}

impl KeyPair
{
    pub fn new() -> Self
    {
        // Generate private key.
        let mut sk :ed25519_secret_key;
        unsafe {
            sk = std::mem::uninitialized();
            priv_key( sk.as_mut_ptr());
        }

        // Generate public key based on private.
        let mut pk :ed25519_public_key;
        unsafe {
            pk = std::mem::uninitialized();
            public_key( pk.as_mut_ptr(), sk.as_mut_ptr());
        }

        KeyPair {
            public_key: pk,
            private_key: sk
        }
    }

    pub fn private_key_as_base64(&self) -> String
    {
        base64::encode( &self.private_key)
    }

    pub fn public_key_as_base64(&self) -> String
    {
        base64::encode( &self.public_key)
    }

    pub fn address(&self) -> String
    {
        let hasher :Sha224 = Sha224::default();
        let pk_hash = hasher.chain( &self.public_key).result();

        format!( "{}{}", ADDRESS_SCHEME, pk_hash[..20].to_base58())
    }

}

#[cfg(test)]
mod unit_test
{
    use super::*;

    #[test]
    pub fn key_pair_ctor()
    {
        let kp_1 = KeyPair::new();
        let kp_1_sk = kp_1.private_key_as_base64();
        let kp_1_pk = kp_1.public_key_as_base64();

        assert_eq!( kp_1_sk.is_empty(), false);
        assert_eq!( kp_1_pk.is_empty(), false);
    }
}


