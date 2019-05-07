use base58::ToBase58;
use eddsa_ed25519;
use failure::Fallible;
use openssl::{
    asn1::Asn1Time,
    bn::{BigNum, MsbOption},
    ec::{EcGroup, EcKey},
    hash::MessageDigest,
    pkey::{PKey, Private},
    x509::{extension::SubjectAlternativeName, X509Builder, X509NameBuilder, X509},
};
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
        eddsa_ed25519::eddsa_priv_key(&mut sk);
        eddsa_ed25519::eddsa_pub_key(&sk, &mut pk);
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

/// Export a KeyPair in standard PEM format
///
/// PEM encoding follows several rules:
///
/// 1. It starts with the header  "-----BEGIN EC PRIVATE KEY-----".
///
/// 2. The key encoded following the ASN.1 syntax
///            and then encoded in base64
///            and then splitted into lines of 64 characters long
///            goes second
///
/// In our case, the ASN.1 syntax leads to this scheme:
/// - 30 // start of an ASN.1 sequence
/// - 2e // length of such sequence in # of bytes (46)
/// - 02 // start of an integer
/// - 01 // length of the integer in # of bytes
/// - 00 // integer (0)
/// - 30 // start of an ASN.1 sequence
/// - 05 // length of such sequence
/// - 06 // OID tag
/// - 03 // OID tag length in # of bytes (3)
/// - 2b 65 6e // OID of curveX25519 (1.3.101.110)
/// - 04 // start of octet string
/// - 22 // length of octet string in # of bytes (34)
/// - 04 // start of octet string (OpenSSL does it this way, repeating
///   OctetString tag)
/// - 20 // length of octet string in # of bytes (32)
/// - private key (in hex encoding)
///
/// 3. It ends with the footer "-----END EC PRIVATE KEY-----"
pub fn crypto_key_to_pem(input: &KeyPair) -> Vec<u8> {
    let pemheader = b"-----BEGIN EC PRIVATE KEY-----\n";

    let pemcontent = &mut [48, 46, 2, 1, 0, 48, 5, 6, 3, 43, 101, 110, 4, 34, 4, 32][..].to_vec();
    pemcontent.append(&mut input.private_key.to_vec());
    let pemcontent = base64::encode(&pemcontent);

    let pemfooter = b"\n-----END EC PRIVATE KEY-----";

    [pemheader, pemcontent.as_bytes(), pemfooter].concat()
}

pub struct Cert {
    pub x509:        X509,
    pub private_key: openssl::pkey::PKey<Private>,
}

pub fn generate_certificate(id: &str) -> Fallible<Cert> {
    // let ec_kp = crypto_sys::KeyPair::new();
    //
    // We can generate a KeyPair using our crypto_sys crate and we have functions to
    // sign with it but as we are using the openssl crate, an
    // openssl::x509::X509 certificate has to be build with the openssl::x509::
    // X509Builder and in order to use the sign method in it, the function for
    // signing with our curve would need to be of type ENV_MD
    //
    // Currently, openssl supports ed25519 but the openssl crate doesn't map such
    // function so there is no way to use it through the crate.
    //
    // A way to sign x509 certificates with it on our own or a wrapper over openssl
    // crate (and openssl-sys crate) is needed in order to use it for tls
    // connections.
    let group = EcGroup::from_curve_name(openssl::nid::Nid::SECP384R1)?;
    let ec_kp = EcKey::generate(&group)?;
    let kp_as_pk = PKey::from_ec_key(ec_kp)?;

    // these are static or well known values for the x509 cert so it should not fail
    let mut builder = X509Builder::new()?;
    let mut name_builder = X509NameBuilder::new()?;

    name_builder.append_entry_by_text("C", "EU")?;
    name_builder.append_entry_by_text("O", "Concordium")?;
    name_builder.append_entry_by_text("CN", id)?;

    let name = name_builder.build();
    builder.set_subject_name(&name)?;
    builder.set_issuer_name(&name)?;
    builder.set_pubkey(&kp_as_pk)?;
    builder.set_version(2)?;

    let not_before = Asn1Time::days_from_now(0)?;
    builder.set_not_before(&not_before)?;

    let not_after = Asn1Time::days_from_now(365)?;
    builder.set_not_after(&not_after)?;

    let mut serial = BigNum::new()?;
    serial.rand(128, MsbOption::MAYBE_ZERO, false)?;
    let serial_number_as1 = serial.to_asn1_integer()?;
    builder.set_serial_number(&serial_number_as1)?;

    let subject_alternative_name = SubjectAlternativeName::new()
        .dns(&format!("{}.node.concordium.com", id))
        .build(&builder.x509v3_context(None, None))?;
    builder.append_extension(subject_alternative_name)?;

    builder.sign(&kp_as_pk, MessageDigest::sha384())?;

    Ok(Cert {
        x509:        builder.build(),
        private_key: kp_as_pk,
    })
}

#[cfg(test)]
mod tests {
    use crate::crypto::{crypto_key_to_pem, KeyPair};

    #[test]
    pub fn test_keypair_import_openssl() {
        let kp = KeyPair::new();
        assert!(openssl::pkey::PKey::private_key_from_pem(&crypto_key_to_pem(&kp)).is_ok());
    }
}
