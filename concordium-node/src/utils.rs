use hacl_star::sha2;
use openssl::x509::{X509, X509Builder, X509NameBuilder, X509Name};
use openssl::ec::{EcGroup, EcKey};
use openssl::nid::Nid;
use openssl::pkey::PKey;
use openssl::bn::{BigNum, MsbOption};
use openssl::hash::MessageDigest;
use std::io::Error;

pub fn sha256(input: &str) -> [u8;32] {
    let mut output = [0; 32];
    sha2::Sha256::hash(&mut output, input.as_bytes());
    output
}

pub fn to_hex_string(bytes: [u8;32]) -> String {
  let strs: Vec<String> = bytes.iter()
                               .map(|b| format!("{:02X}", b))
                               .collect();
  strs.join("")
}

pub fn generate_certificate(id: String) -> Result<X509, Error> {
  let group = EcGroup::from_curve_name(Nid::SECP256K1).unwrap();
  match EcKey::generate(&group) {
    Ok(private_key) => {
      match X509Builder::new() {
        Ok(mut builder) => {
          match X509NameBuilder::new() {
            Ok(mut name_builder) => {
              name_builder.append_entry_by_text("C", "EU").unwrap();
              name_builder.append_entry_by_text("O", "Concordium").unwrap();
              name_builder.append_entry_by_text("CN", &id).unwrap();

              let pkey = PKey::from_ec_key(private_key).unwrap();

              let name = name_builder.build();
              builder.set_subject_name(&name).unwrap();
              builder.set_pubkey(&pkey).unwrap();

              let mut serial = BigNum::new().unwrap();
              serial.rand(128, MsbOption::MAYBE_ZERO, false).unwrap();
              builder.set_serial_number(&serial.to_asn1_integer().unwrap()).unwrap();

              builder.sign(&pkey, MessageDigest::sha256()).unwrap();

              Ok(builder.build())

            },
            Err(e) => {
              Err(Error::from(e))
            }
          }
        },
        Err(e) => {
          Err(Error::from(e))
        }
      }
    },
    Err(e) => {
      Err(Error::from(e))
    }
  }
}