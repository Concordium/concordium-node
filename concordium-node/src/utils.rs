use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use hacl_star::ed25519::{keypair, PublicKey, SecretKey, Signature};
use hacl_star::sha2;
use hex;
use openssl::asn1::Asn1Time;
use openssl::bn::{BigNum, MsbOption};
use openssl::ec::{EcGroup, EcKey};
use openssl::hash::MessageDigest;
use openssl::nid::Nid;
use openssl::pkey::{PKey, Private};
use openssl::rsa::Rsa;
use openssl::x509::extension::SubjectAlternativeName;
use openssl::x509::{X509Builder, X509NameBuilder, X509};
use rand::OsRng;
use std::io::Cursor;
use std::io::Error;
use std::net::IpAddr;
use std::str;
use std::str::FromStr;
use ldns_sys::dns;

pub fn sha256(input: &str) -> [u8; 32] {
    let mut output = [0; 32];
    sha2::Sha256::hash(&mut output, input.as_bytes());
    output
}

pub fn to_hex_string(bytes: &[u8]) -> String {
    bytes.iter()
         .map(|b| format!("{:02X}", b))
         .collect::<String>()
}

pub struct Cert {
    pub x509: X509,
    pub private_key: Rsa<Private>,
}

pub fn generate_certificate(id: String) -> Result<Cert, Error> {
    let group = EcGroup::from_curve_name(Nid::SECP224R1).unwrap();
    match EcKey::generate(&group) {
        Ok(_) => {
            match X509Builder::new() {
                Ok(mut builder) => {
                    match X509NameBuilder::new() {
                        Ok(mut name_builder) => {
                            name_builder.append_entry_by_text("C", "EU").unwrap();
                            name_builder.append_entry_by_text("O", "Concordium")
                                        .unwrap();
                            name_builder.append_entry_by_text("CN", &id).unwrap();

                            //let pkey = PKey::from_ec_key(private_key.clone()).unwrap();
                            let private_part = Rsa::generate(2048).unwrap();
                            let pkey = PKey::from_rsa(private_part.clone()).unwrap();

                            let name = name_builder.build();
                            builder.set_subject_name(&name).unwrap();
                            builder.set_issuer_name(&name).unwrap();
                            builder.set_pubkey(&pkey).unwrap();
                            builder.set_version(2).unwrap();

                            builder.set_not_before(&Asn1Time::days_from_now(0).unwrap())
                                   .unwrap();

                            builder.set_not_after(&Asn1Time::days_from_now(365).unwrap())
                                   .unwrap();

                            let mut serial = BigNum::new().unwrap();
                            serial.rand(128, MsbOption::MAYBE_ZERO, false).unwrap();
                            builder.set_serial_number(&serial.to_asn1_integer().unwrap())
                                   .unwrap();

                            let subject_alternative_name = SubjectAlternativeName::new()
                .dns(&format!("{}.node.concordium.com", id))
                .build(&builder.x509v3_context(None, None))
                .unwrap();
                            builder.append_extension(subject_alternative_name).unwrap();

                            builder.sign(&pkey, MessageDigest::sha256()).unwrap();

                            Ok(Cert { x509: builder.build(),
                                      private_key: private_part, })
                        }
                        Err(e) => Err(Error::from(e)),
                    }
                }
                Err(e) => Err(Error::from(e)),
            }
        }
        Err(e) => Err(Error::from(e)),
    }
}

pub fn parse_ip_port(input: &String) -> Option<(IpAddr, u16)> {
    match input.rfind(":") {
        Some(n) => {
            let ip = input.chars().take(n).collect::<String>();;
            let port = input.chars()
                            .skip(n + 1)
                            .take(input.len())
                            .collect::<String>();;
            match IpAddr::from_str(&ip) {
                Ok(ip) => {
                    match port.parse::<usize>() {
                        Ok(port) => Some((ip, port as u16)),
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn get_bootstrap_nodes(bootstrap_name: String) -> Result<Vec<(IpAddr, u16)>, &'static str> {
    let resolver_addresses = vec![ IpAddr::from_str("8.8.8.8").unwrap()];
    match dns::resolve_dns_txt_record(&bootstrap_name, &resolver_addresses) {
        Ok(res) => read_peers_from_dns_entries(res, super::get_dns_public_key()),
        Err(_) => Err(&"Error looking up bootstrap nodes"),
    }
}

pub fn serialize_bootstrap_peers(peers: &Vec<String>) -> Result<String, &'static str> {
    let mut buffer = String::new();
    buffer.push_str(&format!("{:05}", peers.len()));
    for peer in peers {
        match parse_ip_port(peer) {
            Some((ref ip, ref port)) => {
                match ip {
                    IpAddr::V4(ip4) => {
                        buffer.push_str(&format!("IP4{:03}{:03}{:03}{:03}{:05}",
                                                 ip4.octets()[0],
                                                 ip4.octets()[1],
                                                 ip4.octets()[2],
                                                 ip4.octets()[3],
                                                 port));
                    }
                    IpAddr::V6(ip6) => {
                        buffer.push_str(&format!("IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:05}", ip6.octets()[0], ip6.octets()[1], ip6.octets()[2], ip6.octets()[3], ip6.octets()[4], ip6.octets()[5], ip6.octets()[6], ip6.octets()[7],ip6.octets()[8], ip6.octets()[9], ip6.octets()[10], ip6.octets()[11], ip6.octets()[12], ip6.octets()[13], ip6.octets()[14], ip6.octets()[15], port));
                    }
                }
            }
            _ => return Err("Invalid IP:port"),
        }
    }
    Ok(buffer)
}

pub fn generate_bootstrap_dns(input_key: [u8; 32],
                              record_length: usize,
                              peers: &Vec<String>)
                              -> Result<Vec<String>, &'static str> {
    let secret_key = SecretKey { 0: input_key };
    let public_key = secret_key.get_public();

    let mut return_buffer = String::new();

    return_buffer.push_str(&hex::encode(&public_key.0.to_vec()));

    match serialize_bootstrap_peers(peers) {
        Ok(ref content) => {
            return_buffer.push_str(content);
            let signature = secret_key.signature(content.as_bytes());
            return_buffer.push_str(&hex::encode(&signature.0.to_vec()).to_string());
        }
        Err(_) => {
            return Err("Couldn't parse peers given");
        }
    }

    let mut ret = String::new();

    let mut return_size = vec![];
    return_size.write_u16::<NetworkEndian>(return_buffer.len() as u16)
               .unwrap();
    ret.push_str(&to_hex_string(&return_size));
    ret.push_str(&return_buffer);

    let mut element = 0;
    Ok(ret.as_bytes()
          .chunks(record_length)
          .map(|buf| {
                   let ret = format!("{:02} {}", element, str::from_utf8(&buf).unwrap());
                   element += 1;
                   ret
               })
          .collect::<Vec<String>>())
}

pub fn read_peers_from_dns_entries(entries: Vec<String>, public_key_str: &str)
                                   -> Result<Vec<(IpAddr, u16)>, &'static str> {
    let mut internal_entries = entries.clone();
    let mut ret: Vec<(IpAddr, u16)> = vec![];
    internal_entries.sort_by(|a, b| a.cmp(b));
    let buffer: String = internal_entries.iter()
                                         .map(|x| if x.len() > 3 { &x[3..] } else { &x })
                                         .collect::<String>();
    if buffer.len() > 4 {
        match hex::decode(&buffer[..4].to_string()) {
            Ok(size_bytes) => {
                let mut bytes_buf = Cursor::new(size_bytes);;
                match bytes_buf.read_u16::<NetworkEndian>() {
                    Ok(size) => {
                        if size as usize == buffer.len() - 4 {
                            if buffer[4..68].to_string().to_lowercase() != public_key_str.to_string().to_lowercase() {
                                return Err("Invalid public key");
                            }
                            match hex::decode(&buffer[4..68].to_string()) {
                                Ok(input_pub_key_bytes) => {
                                    let mut pub_key_bytes: [u8;
                                                            32] = [0; 32];
                                    for i in 0..32 {
                                        pub_key_bytes[i] = input_pub_key_bytes[i];
                                    }
                                    let public_key = PublicKey { 0: pub_key_bytes };
                                    let mut bytes_taken_for_nodes = 0;
                                    match &buffer[68..73].parse::<u16>() {
                                        Ok(nodes_count) => {
                                            let mut inner_buffer = &buffer[73..];
                                            for _ in 0..*nodes_count {
                                                match &buffer[(73 + bytes_taken_for_nodes)
                                                              ..(76 + bytes_taken_for_nodes)]
                                                {
                                                    "IP4" => {
                                                        let ip = &inner_buffer
                                                            [(bytes_taken_for_nodes + 3)
                                                             ..(bytes_taken_for_nodes + 3 + 12)];
                                                        let port = &inner_buffer
                                                            [(bytes_taken_for_nodes + 3 + 12)
                                                             ..(bytes_taken_for_nodes
                                                                + 3
                                                                + 12
                                                                + 5)];
                                                        match IpAddr::from_str(&format!("{}.{}.{}.{}",
                                                            &ip[..3],
                                                            &ip[3..6],
                                                            &ip[6..9],
                                                            &ip[9..12])[..]) {
                                                                Ok(ip) => {
                                                                    match port.parse::<u16>() {
                                                                        Ok(port) => {
                                                                            ret.push((ip,port));
                                                                        },
                                                                        Err(_) => return Err("Could not parse port for node")
                                                                    }
                                                                },
                                                                Err(_) => return Err("Can't parse IP for node")
                                                        }
                                                        bytes_taken_for_nodes += 3 + 12 + 5;
                                                    }
                                                    "IP6" => {
                                                        let ip = &inner_buffer
                                                            [(bytes_taken_for_nodes + 3)
                                                             ..(bytes_taken_for_nodes + 3 + 32)];
                                                        let port = &inner_buffer
                                                            [(bytes_taken_for_nodes + 3 + 32)
                                                             ..(bytes_taken_for_nodes
                                                                + 3
                                                                + 32
                                                                + 5)];
                                                        match IpAddr::from_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}",
                                                                &ip[..4],
                                                                &ip[4..8],
                                                                &ip[8..12],
                                                                &ip[12..16],
                                                                &ip[16..20],
                                                                &ip[20..24],
                                                                &ip[24..28],
                                                                &ip[28..32])[..]) {
                                                                    Ok(ip) => {
                                                                        match port.parse::<u16>() {
                                                                            Ok(port) => {
                                                                                ret.push((ip,port));
                                                                            },
                                                                            Err(_) => return Err("Could not parse port for node")
                                                                        }
                                                                    },
                                                                    Err(_) => return Err("Can't parse IP for node")
                                                                }
                                                        bytes_taken_for_nodes += 3 + 32 + 5;
                                                    }
                                                    _ => return Err("Invalid data for node"),
                                                }
                                            }
                                            match hex::decode(&buffer[(4+64+5+bytes_taken_for_nodes)..].to_string()) {
                                                Ok(signature_bytes) => {
                                                    if signature_bytes.len() == 64 {
                                                        let mut sig_bytes:[u8;64] = [0;64];
                                                        for i in 0..64 {
                                                            sig_bytes[i] = signature_bytes[i];
                                                        }
                                                        let signature = Signature{0:sig_bytes};
                                                        let content_peers = ret.iter().map(|x| format!("{}:{}", x.0.to_string(), x.1)).collect::<Vec<String>>();
                                                        match serialize_bootstrap_peers(&content_peers) {
                                                            Ok(content) => {
                                                                if public_key.verify(content.as_bytes(),&signature) {
                                                                    Ok( ret )
                                                                } else {
                                                                    Err("Signature invalid")
                                                                }
                                                            },
                                                            Err(_) => Err("Couldn't reverse encode content")
                                                        }
                                                    } else {
                                                        Err("Not correct length for signature")
                                                    }
                                                },
                                                Err(_) => Err("Could not read signature")
                                            }
                                        }
                                        Err(_) => Err("Incorrect number of nodes"),
                                    }
                                }
                                Err(_) => Err("Incorrect public key"),
                            }
                        } else {
                            Err("Incorrect size of data")
                        }
                    }
                    Err(_) => Err("Invalid data"),
                }
            }
            Err(_) => Err("Invalid data"),
        }
    } else {
        Err("Missing bytes")
    }
}

pub fn generate_ed25519_key() -> [u8; 32] {
    let mut private_key = SecretKey { 0: [0; 32] };
    let mut public_key = PublicKey { 0: [0; 32] };
    keypair(OsRng::new().unwrap(), &mut private_key, &mut public_key);
    private_key.0
}

#[cfg(test)]
mod tests {
    use hacl_star::ed25519::SecretKey;
    use utils::*;

    const PRIVATE_TEST_KEY: [u8; 32] = [0xbe, 0xd2, 0x3a, 0xdd, 0x4d, 0x34, 0xab, 0x7a, 0x12,
                                        0xa9, 0xa6, 0xab, 0x2b, 0xaf, 0x97, 0x06, 0xb0, 0xf7,
                                        0x22, 0x57, 0xa9, 0x82, 0xd4, 0x19, 0x9f, 0x58, 0x44,
                                        0xa7, 0x8f, 0x3b, 0xe4, 0x70];

    #[test]
    pub fn test_generate_key_test() {
        let _key: [u8; 32] = generate_ed25519_key();
    }

    #[test]
    pub fn test_generate_public_key() {
        const EXPECTED: &str = "FC5D9F06051570D9DA9DFD1B3D9B7353E22D764244DCF6B9C665CC21F63DF8F2";
        let secret_key = SecretKey { 0: PRIVATE_TEST_KEY, };
        assert_eq!(EXPECTED, to_hex_string(&secret_key.get_public().0));
    }

    #[test]
    pub fn test_sign_verify() {
        const INPUT: &str = "00002IP401001001001008888IP6deadbeaf00000000000000000000000009999";
        let secret_key = SecretKey { 0: PRIVATE_TEST_KEY, };
        let signature = secret_key.signature(INPUT.as_bytes());
        let signature_hex = hex::encode(signature.0.to_vec());
        let signature_unhexed = hex::decode(&signature_hex).unwrap();
        let mut decoded_signature: [u8; 64] = [0; 64];
        for i in 0..64 {
            decoded_signature[i] = signature_unhexed[i];
        }
        assert!(secret_key.get_public()
                          .verify(INPUT.as_bytes(), &Signature { 0: decoded_signature }));
    }

    #[test]
    pub fn test_dns_generated() {
        let peers: Vec<String> = vec!["10.10.10.10:8888".to_string(),
                                      "dead:beaf:::9999".to_string()];
        let secret_key = SecretKey{0: PRIVATE_TEST_KEY};
        let public_hex_key = to_hex_string(&secret_key.get_public().0);
        match generate_bootstrap_dns(PRIVATE_TEST_KEY, 240, &peers) {
            Ok(res) => match read_peers_from_dns_entries(res, &public_hex_key) {
                Ok(peers) => {
                    assert_eq!(peers.len(), 2);
                    assert!(peers.iter()
                                 .find(|&x| x.0 == IpAddr::from_str("10.10.10.10").unwrap()
                                            && x.1 == 8888)
                                 .is_some());
                    assert!(peers.iter()
                                 .find(|&x| x.0 == IpAddr::from_str("dead:beaf::").unwrap()
                                            && x.1 == 9999)
                                 .is_some());
                }
                Err(e) => panic!("Can't read peers from generated records {}", e),
            },
            Err(e) => panic!("Can't generate DNS records {}", e),
        }
    }

}
