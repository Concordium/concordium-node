use base64::{encode};
use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use ::dns::dns;
use failure::{Fallible};
use hacl_star::ed25519::{keypair, PublicKey, SecretKey, Signature};
use hacl_star::sha2;
use hex;
use crate::crypto;
use openssl::asn1::Asn1Time;
use openssl::bn::{BigNum, MsbOption};
use openssl::hash::MessageDigest;
use openssl::ec::{EcKey, EcGroup};
use openssl::pkey::{PKey, Private};
use openssl::x509::extension::SubjectAlternativeName;
use openssl::x509::{X509Builder, X509NameBuilder, X509};
use rand::rngs::OsRng;
#[cfg(not(target_os = "windows"))]
use resolv_conf::Config as ResolverConfig;
#[cfg(not(target_os = "windows"))]
use std::fs::File;
use std::io::Cursor;
#[cfg(not(target_os = "windows"))]
use std::io::Read;
use std::net::IpAddr;
use std::str;
use std::str::FromStr;
use std::fs;

pub fn sha256(input: &str) -> [u8; 32] {
    sha256_bytes(input.as_bytes())
}

pub fn sha256_bytes(input: &[u8]) -> [u8; 32] {
    let mut output = [0; 32];
    sha2::Sha256::hash(&mut output, input);
    output
}

pub fn to_hex_string(bytes: &[u8]) -> String {
    bytes.iter()
         .map(|b| format!("{:02X}", b))
         .collect()
}

pub struct Cert {
    pub x509: X509,
    pub private_key: openssl::pkey::PKey<Private>,
}

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
/// - 04 // start of octet string (OpenSSL does it this way, repeating OctetString tag)
/// - 20 // length of octet string in # of bytes (32)
/// - private key (in hex encoding)
///
/// 3. It ends with the footer "-----END EC PRIVATE KEY-----"
///
pub fn crypto_key_to_pem(input: &crypto::KeyPair) -> Vec<u8> {
    let pemheader = b"-----BEGIN EC PRIVATE KEY-----\n";

    let mut pemcontent = hex::decode("302e020100300506032b656e04220420").unwrap(); // static, should never fail
    pemcontent.append(&mut input.private_key.to_vec());
    let pemcontent = encode(&pemcontent);

    let pemfooter = b"\n-----END EC PRIVATE KEY-----";

    [pemheader, pemcontent.as_bytes(), pemfooter].concat()
}

pub fn generate_certificate(id: String) -> Fallible<Cert> {
    // let ec_kp = crypto_sys::KeyPair::new();
    //
    // We can generate a KeyPair using our crypto_sys crate and we have functions to sign with it
    // but as we are using the openssl crate, an openssl::x509::X509 certificate has to be build with
    // the openssl::x509::X509Builder and in order to use the sign method in it, the function for signing
    // with our curve would need to be of type ENV_MD
    //
    // Currently, openssl supports ed25519 but the openssl crate doesn't map such function so
    // there is no way to use it through the crate.
    //
    // A way to sign x509 certificates with it on our own or a wrapper over openssl crate (and openssl-sys crate)
    // is needed in order to use it for tls connections.
    let group = EcGroup::from_curve_name(openssl::nid::Nid::SECP384R1)?;
    let ec_kp = EcKey::generate(&group)?;
    let kp_as_pk = PKey::from_ec_key(ec_kp)?;

    // these are static or well known values for the x509 cert so it should not fail
    let mut builder =  X509Builder::new()?;
    let mut name_builder = X509NameBuilder::new()?;

    name_builder.append_entry_by_text("C", "EU")?;
    name_builder.append_entry_by_text("O", "Concordium")?;
    name_builder.append_entry_by_text("CN", &id)?;

    let name = name_builder.build();
    builder.set_subject_name(&name)?;
    builder.set_issuer_name(&name)?;
    builder.set_pubkey(&kp_as_pk)?;
    builder.set_version(2)?;

    builder.set_not_before(&Asn1Time::days_from_now(0).unwrap())?;

    builder.set_not_after(&Asn1Time::days_from_now(365).unwrap())?;

    let mut serial = BigNum::new()?;
    serial.rand(128, MsbOption::MAYBE_ZERO, false)?;
    builder.set_serial_number(&serial.to_asn1_integer().unwrap())?;

    let subject_alternative_name =
        SubjectAlternativeName::new().dns(&format!("{}.node.concordium.com", id))
        .build(&builder.x509v3_context(None, None))?;
    builder.append_extension(subject_alternative_name)?;

    builder.sign(&kp_as_pk, MessageDigest::sha384())?;

    Ok(Cert { x509: builder.build(),
              private_key: kp_as_pk,
    })
}

pub fn parse_ip_port(input: &str) -> Option<(IpAddr, u16)> {
    if let Some(n) = input.rfind(":") {
        let (ip, port) = input.split_at(n);

        if let Ok(ip) = IpAddr::from_str(&ip) {
            if let Ok(port) = port[1..].parse::<u16>() {
                return Some((ip, port));
            }
        }
    }

    None
}

#[cfg(not(target_os = "windows"))]
pub fn get_resolvers(resolv_conf: &str, resolvers: &[String]) -> Vec<String> {
    if !resolvers.is_empty() {
        resolvers.to_owned()
    } else {
        let mut buf = Vec::with_capacity(4096);

        match File::open(resolv_conf) {
            Ok(mut f) => {
                match f.read_to_end(&mut buf) {
                    Ok(_) => {
                        match ResolverConfig::parse(&buf) {
                            Ok(conf) => {
                                let ret = conf.nameservers
                                              .iter()
                                              .map(|x| x.to_string())
                                              .collect::<Vec<_>>();
                                if !ret.is_empty() {
                                    ret
                                } else {
                                    panic!("No DNS resolvers found in {}", resolv_conf);
                                }
                            }
                            _ => panic!("Error reading file {}", resolv_conf),
                        }
                    }
                    _ => panic!("Error reading file {}", resolv_conf),
                }
            }
            _ => panic!("Can't open {}", resolv_conf),
        }
    }
}

#[cfg(target_os = "windows")]
pub fn get_resolvers(_resolv_conf: &str, resolvers: &[String]) -> Vec<String> {
    if !resolvers.is_empty() {
        resolvers.to_owned()
    } else {
        let adapters = ipconfig::get_adapters().unwrap_or_else(
            || { panic!("Couldn't get adapters. Bailing out!"); }
        );
        let name_servers = adapters.iter()
                                   .flat_map(|adapter| adapter.dns_servers().iter())
                                   .map(|dns_server| dns_server.to_string());

        if name_servers.is_empty() {
            panic!("Could not read dns servers!");
        }
        name_servers
    }
}

pub fn parse_host_port(input: &str,
                       resolvers: &[String],
                       dnssec_fail: bool)
                       -> Option<(IpAddr, u16)> {
    if let Some(n) = input.rfind(":") {
        let (ip, port) = input.split_at(n);
        let port = &port[1..];

        if let Ok(ip) = IpAddr::from_str(&ip) {
            if let Ok(port) = port.parse::<u16>() {
                Some((ip, port))
            } else {
                None
            }
        } else {
            match port.parse::<u16>() {
                Err(_) => None, // couldn't parse port
                Ok(port) => {
                    let resolver_addresses = resolvers.iter()
                        .map(|x| IpAddr::from_str(x))
                        .flatten()
                        .collect::<Vec<_>>();

                    if resolver_addresses.len() != 0 {
                        if let Ok(res) = dns::resolve_dns_a_record(
                            &ip,
                            &resolver_addresses,
                            dnssec_fail
                        ) { // resolved by A records
                            if !res.is_empty() {
                                match IpAddr::from_str(&res[0]) {
                                    Err(_) => { return None; },
                                    Ok(ip) => { return Some((ip, port)); }
                                };
                            }
                        }

                        if let Ok(res) = dns::resolve_dns_aaaa_record(
                            &ip,
                            &resolver_addresses,
                            dnssec_fail
                        ) { // resolved by AAAA records
                            if !res.is_empty() {
                               match IpAddr::from_str(&res[0]) {
                                    Err(_) => None,
                                    Ok(ip) => Some((ip, port))
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            }
        }
    } else {
        None // no colon in the IP
    }
}

pub fn get_bootstrap_nodes(bootstrap_name: String,
                           resolvers: &[String],
                           dnssec_fail: bool,
                           bootstrap_nodes: &[String])
                           -> Result<Vec<(IpAddr, u16)>, &'static str> {
    if !bootstrap_nodes.is_empty() {
        debug!("Not using DNS for bootstrapping, we have nodes specified");
        let bootstrap_nodes = bootstrap_nodes.iter()
                                             .map(|x| parse_host_port(x, resolvers, dnssec_fail))
                                             .flatten()
                                             .collect();
        return Ok(bootstrap_nodes);
    } else {
        debug!("No bootstrap nodes given, so attempting DNS");
        let resolver_addresses = resolvers.iter()
                                          .map(|x| IpAddr::from_str(x))
                                          .flatten()
                                          .collect::<Vec<_>>();
        if resolver_addresses.is_empty() {
            return Err("No valid resolvers given");
        }
        match dns::resolve_dns_txt_record(&bootstrap_name, &resolver_addresses, dnssec_fail) {
            Ok(res) => read_peers_from_dns_entries(res, super::get_dns_public_key()),
            Err(_) => Err("Error looking up bootstrap nodes"),
        }
    }
}

pub fn serialize_bootstrap_peers(peers: &[String]) -> Result<String, &'static str> {
    let mut buffer = format!("{:05}", peers.len());

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
                        buffer.push_str(&format!("IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:05}",
                                                 ip6.octets()[0],
                                                 ip6.octets()[1],
                                                 ip6.octets()[2],
                                                 ip6.octets()[3],
                                                 ip6.octets()[4],
                                                 ip6.octets()[5],
                                                 ip6.octets()[6],
                                                 ip6.octets()[7],
                                                 ip6.octets()[8],
                                                 ip6.octets()[9],
                                                 ip6.octets()[10],
                                                 ip6.octets()[11],
                                                 ip6.octets()[12],
                                                 ip6.octets()[13],
                                                 ip6.octets()[14],
                                                 ip6.octets()[15],
                                                 port));
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
                              peers: &[String])
                              -> Result<Vec<String>, &'static str> {
    let secret_key = SecretKey(input_key);
    let public_key = secret_key.get_public();

    let mut return_buffer = hex::encode(&public_key.0);

    match serialize_bootstrap_peers(peers) {
        Ok(ref content) => {
            return_buffer.push_str(content);
            let signature = secret_key.signature(content.as_bytes());
            return_buffer.push_str(&hex::encode(&(signature.0)[..]));
        }
        Err(_) => {
            return Err("Couldn't parse peers given");
        }
    }

    let mut ret = String::new();

    let mut return_size = vec![];
    return_size.write_u16::<NetworkEndian>(return_buffer.len() as u16).unwrap();
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
          .collect())
}

pub fn read_peers_from_dns_entries(entries: Vec<String>,
                                   public_key_str: &str)
                                   -> Result<Vec<(IpAddr, u16)>, &'static str> {
    let mut internal_entries = entries;
    internal_entries.sort();
    let mut ret: Vec<(IpAddr, u16)> = vec![];
    let buffer: String = internal_entries.iter()
                                         .map(|x| if x.len() > 3 { &x[3..] } else { &x })
                                         .collect::<String>();
    if buffer.len() > 4 {
        match hex::decode(&buffer[..4]) {
            Ok(size_bytes) => {
                let mut bytes_buf = Cursor::new(size_bytes);;
                match bytes_buf.read_u16::<NetworkEndian>() {
                    Ok(size) => {
                        if size as usize == buffer.len() - 4 {
                            if buffer[4..68].to_lowercase() != public_key_str.to_lowercase() {
                                return Err("Invalid public key");
                            }
                            match hex::decode(&buffer[4..68]) {
                                Ok(input_pub_key_bytes) => {
                                    let mut pub_key_bytes: [u8; 32] = Default::default();
                                    pub_key_bytes.copy_from_slice(&input_pub_key_bytes[..32]);
                                    let public_key = PublicKey(pub_key_bytes);
                                    let mut bytes_taken_for_nodes = 0;

                                    match &buffer[68..73].parse::<u16>() {
                                        Ok(nodes_count) => {
                                            let inner_buffer = &buffer[73..];
                                            for _ in 0..*nodes_count {
                                                match &buffer[bytes_taken_for_nodes..][73..76] {
                                                    "IP4" => {
                                                        let ip = &inner_buffer[bytes_taken_for_nodes..][3..3 + 12];
                                                        let port = &inner_buffer[bytes_taken_for_nodes..]
                                                                                [3 + 12..3 + 12 + 5];
                                                        match IpAddr::from_str(&format!("{}.{}.{}.{}",
                                                                                        &ip[..3],
                                                                                        &ip[3..6],
                                                                                        &ip[6..9],
                                                                                        &ip[9..12])[..])
                                                        {
                                                            Ok(ip) => match port.parse::<u16>() {
                                                                Ok(port) => ret.push((ip, port)),
                                                                Err(_) => return Err("Could not parse port for node"),
                                                            },
                                                            Err(_) => return Err("Can't parse IP for node"),
                                                        }
                                                        bytes_taken_for_nodes += 3 + 12 + 5;
                                                    }
                                                    "IP6" => {
                                                        let ip = &inner_buffer[bytes_taken_for_nodes..][3..3 + 32];
                                                        let port = &inner_buffer[bytes_taken_for_nodes..]
                                                                                [3 + 32..3 + 32 + 5];
                                                        match IpAddr::from_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}",
                                                                                        &ip[..4],
                                                                                        &ip[4..8],
                                                                                        &ip[8..12],
                                                                                        &ip[12..16],
                                                                                        &ip[16..20],
                                                                                        &ip[20..24],
                                                                                        &ip[24..28],
                                                                                        &ip[28..32])[..])
                                                        {
                                                            Ok(ip) => match port.parse::<u16>() {
                                                                Ok(port) => ret.push((ip, port)),
                                                                Err(_) => return Err("Could not parse port for node"),
                                                            },
                                                            Err(_) => return Err("Can't parse IP for node"),
                                                        }
                                                        bytes_taken_for_nodes += 3 + 32 + 5;
                                                    }
                                                    _ => return Err("Invalid data for node"),
                                                }
                                            }
                                            match hex::decode(&buffer[(4 + 64 + 5 + bytes_taken_for_nodes)..]) {
                                                Ok(signature_bytes) => {
                                                    if signature_bytes.len() == 64 {
                                                        let mut sig_bytes: [u8; 64] = [0; 64];
                                                        for i in 0..64 {
                                                            sig_bytes[i] = signature_bytes[i];
                                                        }
                                                        let signature = Signature(sig_bytes);
                                                        let content_peers = ret.iter()
                                                            .map(|x| format!("{}:{}", x.0.to_string(), x.1))
                                                            .collect::<Vec<_>>();

                                                        match serialize_bootstrap_peers(&content_peers) {
                                                            Ok(content) => {
                                                                if public_key.verify(content.as_bytes(), &signature) {
                                                                    Ok(ret)
                                                                } else {
                                                                    Err("Signature invalid")
                                                                }
                                                            }
                                                            Err(_) => Err("Couldn't reverse encode content"),
                                                        }
                                                    } else {
                                                        Err("Not correct length for signature")
                                                    }
                                                }
                                                Err(_) => Err("Could not read signature"),
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
    (keypair(OsRng::new().unwrap()).0).0
}

pub fn get_tps_test_messages(path: Option<String>) -> Vec<Vec<u8>> {
    let mut ret = Vec::new();

    if let Some(ref _path) = path {
        info!("Trying path to find TPS test messages: {}", _path);
        if let Ok(files) = fs::read_dir(_path) {
            for file in files.filter_map(|f| f.ok()).filter(|f| !f.path().is_dir()) {
                let data = fs::read(file.path()).expect("Unable to read file!");
                ret.push(data);
            }
        }
    }

    ret
}

#[cfg(test)]
mod tests {
    use hacl_star::ed25519::SecretKey;
    use crate::utils::*;
    use crate::crypto::KeyPair;

    const PRIVATE_TEST_KEY: [u8; 32] = [0xbe, 0xd2, 0x3a, 0xdd, 0x4d, 0x34, 0xab, 0x7a, 0x12,
                                        0xa9, 0xa6, 0xab, 0x2b, 0xaf, 0x97, 0x06, 0xb0, 0xf7,
                                        0x22, 0x57, 0xa9, 0x82, 0xd4, 0x19, 0x9f, 0x58, 0x44,
                                        0xa7, 0x8f, 0x3b, 0xe4, 0x70];

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
        let secret_key = SecretKey { 0: PRIVATE_TEST_KEY, };
        let public_hex_key = to_hex_string(&secret_key.get_public().0);
        match generate_bootstrap_dns(PRIVATE_TEST_KEY, 240, &peers) {
            Ok(res) => {
                match read_peers_from_dns_entries(res, &public_hex_key) {
                    Ok(peers) => {
                        assert_eq!(peers.len(), 2);
                        assert!(peers.iter()
                                     .find(|&x| {
                                               x.0 == IpAddr::from_str("10.10.10.10").unwrap()
                                               && x.1 == 8888
                                           })
                                     .is_some());
                        assert!(peers.iter()
                                     .find(|&x| {
                                               x.0 == IpAddr::from_str("dead:beaf::").unwrap()
                                               && x.1 == 9999
                                           })
                                     .is_some());
                    }
                    Err(e) => panic!("Can't read peers from generated records {}", e),
                }
            }
            Err(e) => panic!("Can't generate DNS records {}", e),
        }
    }

    #[test]
    pub fn test_read_resolv_conf() {
        assert_eq!(get_resolvers("tests/resolv.conf-linux", &vec![]),
                   vec!["2001:4860:4860::8888",
                        "2001:4860:4860::8844",
                        "8.8.8.8",
                        "8.8.4.4"]);
    }

    #[test]
    pub fn test_read_resolv_conf_with_default() {
        assert_ne!(get_resolvers("tests/resolv.conf-linux", &vec!["9.9.9.9".to_string()]),
                   vec!["2001:4860:4860::8888",
                        "2001:4860:4860::8844",
                        "8.8.8.8",
                        "8.8.4.4"]);
    }

    #[test]
    pub fn test_keypair_import_openssl() {
        let kp = KeyPair::new();
        assert!(openssl::pkey::PKey::private_key_from_pem(&crypto_key_to_pem(&kp)).is_ok());
    }

}
