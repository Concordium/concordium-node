use crate::{
    self as p2p_client,
    common::{serialize_addr, P2PPeer},
    configuration,
    fails::{HostPortParseError, NoDNSResolversAvailable},
    p2p::{
        banned_nodes::{insert_ban, remove_ban, BannedNode},
        P2PNode,
    },
};
use base64;
use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use concordium_dns::dns;
use env_logger::{Builder, Env};
use failure::{Error, Fallible};
use hacl_star::{
    ed25519::{keypair, PublicKey, SecretKey, Signature},
    sha2,
};
use rand::rngs::OsRng;
use rkv::{Rkv, StoreOptions};
use snow::Keypair;
#[cfg(feature = "benchmark")]
use std::fs;
#[cfg(not(target_os = "windows"))]
use std::fs::File;
use std::{
    convert::TryFrom,
    io::Cursor,
    net::{IpAddr, SocketAddr},
    str::{self, FromStr},
    sync::RwLock,
};

pub fn sha256(input: &str) -> [u8; 32] { sha256_bytes(input.as_bytes()) }

pub fn sha256_bytes(input: &[u8]) -> [u8; 32] {
    let mut output = [0; 32];
    sha2::Sha256::hash(&mut output, input);
    output
}

pub fn to_hex_string(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

/// It transforms an hexadecimal string `hex` into binary data.
pub fn from_hex_string(hex: &str) -> Result<Vec<u8>, std::num::ParseIntError> {
    (0..hex.len())
        .step_by(2)
        .map(|idx| u8::from_str_radix(&hex[idx..idx + 2], 16))
        .collect::<Result<Vec<u8>, _>>()
}

pub fn parse_ip_port(input: &str) -> Option<SocketAddr> {
    if let Some(n) = input.rfind(':') {
        let (ip, port) = input.split_at(n);

        if let Ok(ip) = IpAddr::from_str(&ip) {
            if let Ok(port) = port[1..].parse::<u16>() {
                return Some(SocketAddr::new(ip, port));
            }
        }
    }

    None
}

#[cfg(not(target_os = "windows"))]
pub fn get_resolvers(resolv_conf: &str, resolvers: &[String]) -> Vec<String> {
    use std::io::{BufRead, BufReader};
    if !resolvers.is_empty() {
        resolvers.to_owned()
    } else {
        match File::open(resolv_conf) {
            Ok(f) => BufReader::new(f)
                .lines()
                .map(|line| match line {
                    Ok(line_valid) => {
                        if line_valid.starts_with("nameserver ") && line_valid.len() >= 12 {
                            let (_, rest) = line_valid.split_at(11);
                            let rest_trimmed = rest.trim();
                            if !rest_trimmed.is_empty() {
                                Some(rest_trimmed.to_owned())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    Err(_) => None,
                })
                .flatten()
                .collect::<Vec<String>>(),
            _ => panic!("Can't open {}", resolv_conf),
        }
    }
}

#[cfg(target_os = "windows")]
pub fn get_resolvers(_resolv_conf: &str, resolvers: &[String]) -> Vec<String> {
    if !resolvers.is_empty() {
        resolvers.to_owned()
    } else {
        let adapters = ipconfig::get_adapters().unwrap_or_else(|_| {
            panic!("Couldn't get adapters. Bailing out!");
        });
        let name_servers = adapters
            .iter()
            .flat_map(|adapter| adapter.dns_servers().iter())
            .map(|dns_server| dns_server.to_string())
            .collect::<Vec<String>>();

        if name_servers.is_empty() {
            panic!("Could not read dns servers!");
        }
        name_servers
    }
}

pub fn parse_host_port(
    input: &str,
    resolvers: &[String],
    dnssec_fail: bool,
) -> Fallible<Vec<SocketAddr>> {
    if let Some(n) = input.rfind(':') {
        let (ip, port) = input.split_at(n);
        let port = &port[1..];

        if let Ok(ip) = IpAddr::from_str(&ip) {
            if let Ok(port) = port.parse::<u16>() {
                Ok(vec![SocketAddr::new(ip, port)])
            } else {
                return Err(Error::from(HostPortParseError::new(input.to_owned())));
            }
        } else {
            match port.parse::<u16>() {
                Err(_) => Err(Error::from(HostPortParseError::new(input.to_owned()))), /* couldn't parse port */
                Ok(port) => {
                    let resolver_addresses = resolvers
                        .iter()
                        .map(|x| IpAddr::from_str(x))
                        .flatten()
                        .collect::<Vec<_>>();
                    if !resolver_addresses.is_empty() {
                        let a_record_resolver = if let Ok(res) =
                            dns::resolve_dns_a_record(&ip, &resolver_addresses, dnssec_fail)
                        {
                            res.into_iter()
                                .filter_map(|element| match IpAddr::from_str(&element) {
                                    Ok(ip) => Some(SocketAddr::new(ip, port).to_owned()),
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                        } else {
                            vec![]
                        };
                        let aaaa_record_resolver = if let Ok(res) =
                            dns::resolve_dns_aaaa_record(&ip, &resolver_addresses, dnssec_fail)
                        {
                            res.into_iter()
                                .filter_map(|element| IpAddr::from_str(&element).ok())
                                .map(|ip| SocketAddr::new(ip, port).to_owned())
                                .collect::<Vec<_>>()
                        } else {
                            vec![]
                        };
                        Ok(a_record_resolver
                            .iter()
                            .chain(aaaa_record_resolver.iter())
                            .map(ToOwned::to_owned)
                            .collect::<Vec<_>>())
                    } else {
                        Err(Error::from(NoDNSResolversAvailable))
                    }
                }
            }
        }
    } else {
        Err(Error::from(HostPortParseError::new(input.to_owned()))) // No colon in host:post
    }
}

pub fn get_bootstrap_nodes(
    bootstrap_name: String,
    resolvers: &[String],
    dnssec_fail: bool,
    bootstrap_nodes: &[String],
) -> Result<Vec<SocketAddr>, &'static str> {
    if !bootstrap_nodes.is_empty() {
        debug!("Not using DNS for bootstrapping, we have nodes specified");
        let bootstrap_nodes = bootstrap_nodes
            .iter()
            .filter_map(|ip_port| {
                parse_host_port(ip_port, resolvers, dnssec_fail)
                    .map_err(|err| error!("Invalid bootstrapping node received {}", err))
                    .ok()
            })
            .flatten()
            .collect::<Vec<_>>();
        Ok(bootstrap_nodes)
    } else {
        debug!("No bootstrap nodes given, so attempting DNS");
        let resolver_addresses = resolvers
            .iter()
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
            Some(addr) => buffer.push_str(&serialize_addr(addr)),
            _ => return Err("Invalid IP:port"),
        }
    }
    Ok(buffer)
}

pub fn generate_bootstrap_dns(
    input_key: [u8; 32],
    record_length: usize,
    peers: &[String],
) -> Result<Vec<String>, &'static str> {
    let secret_key = SecretKey(input_key);
    let public_key = secret_key.get_public();

    let mut return_buffer = base64::encode(&public_key.0);

    match serialize_bootstrap_peers(peers) {
        Ok(ref content) => {
            return_buffer.push_str(content);
            let signature = secret_key.signature(content.as_bytes());
            return_buffer.push_str(&base64::encode(&(signature.0)[..]));
        }
        Err(_) => {
            return Err("Couldn't parse peers given");
        }
    }

    let mut ret = String::new();

    let mut return_size = vec![];
    assert!(return_size
        .write_u16::<NetworkEndian>(return_buffer.len() as u16)
        .is_ok());
    ret.push_str(&base64::encode(&return_size));
    ret.push_str(&return_buffer);

    let mut element = 0;
    Ok(ret
        .as_bytes()
        .chunks(record_length)
        .filter_map(|buf| {
            if let Ok(val) = str::from_utf8(&buf) {
                let ret = format!("{:02} {}", element, val);
                element += 1;
                Some(ret)
            } else {
                None
            }
        })
        .collect())
}

pub fn read_peers_from_dns_entries(
    entries: Vec<String>,
    public_key_str: &str,
) -> Result<Vec<SocketAddr>, &'static str> {
    let mut internal_entries = entries;
    internal_entries.sort();
    let mut ret: Vec<SocketAddr> = vec![];
    let buffer: String = internal_entries
        .iter()
        .map(|x| if x.len() > 3 { &x[3..] } else { &x })
        .collect::<String>();
    if buffer.len() > 4 {
        match base64::decode(&buffer[..4]) {
            Ok(size_bytes) => {
                let mut bytes_buf = Cursor::new(size_bytes);;
                match bytes_buf.read_u16::<NetworkEndian>() {
                    Ok(size) => {
                        if size as usize == buffer.len() - 4 {
                            if buffer[4..48].to_lowercase() != public_key_str.to_lowercase() {
                                return Err("Invalid public key");
                            }
                            match base64::decode(&buffer[4..48]) {
                                Ok(input_pub_key_bytes) => {
                                    let mut pub_key_bytes: [u8; 32] = Default::default();
                                    pub_key_bytes.copy_from_slice(&input_pub_key_bytes[..32]);
                                    let public_key = PublicKey(pub_key_bytes);
                                    let mut bytes_taken_for_nodes = 0;

                                    match &buffer[49..53].parse::<u16>() {
                                        Ok(nodes_count) => {
                                            let inner_buffer = &buffer[53..];
                                            for _ in 0..*nodes_count {
                                                match &buffer[bytes_taken_for_nodes..][53..56] {
                                                    "IP4" => {
                                                        let ip = &inner_buffer
                                                            [bytes_taken_for_nodes..]
                                                            [3..3 + 12];
                                                        let port = &inner_buffer
                                                            [bytes_taken_for_nodes..]
                                                            [3 + 12..3 + 12 + 5];
                                                        match IpAddr::from_str(
                                                            &format!(
                                                                "{}.{}.{}.{}",
                                                                &ip[..3],
                                                                &ip[3..6],
                                                                &ip[6..9],
                                                                &ip[9..12]
                                                            )[..],
                                                        ) {
                                                            Ok(ip) => match port.parse::<u16>() {
                                                                Ok(port) => ret.push(
                                                                    SocketAddr::new(ip, port),
                                                                ),
                                                                Err(_) => {
                                                                    return Err("Could not parse \
                                                                                port for node")
                                                                }
                                                            },
                                                            Err(_) => {
                                                                return Err(
                                                                    "Can't parse IP for node"
                                                                )
                                                            }
                                                        }
                                                        bytes_taken_for_nodes += 3 + 12 + 5;
                                                    }
                                                    "IP6" => {
                                                        let ip = &inner_buffer
                                                            [bytes_taken_for_nodes..]
                                                            [3..3 + 32];
                                                        let port = &inner_buffer
                                                            [bytes_taken_for_nodes..]
                                                            [3 + 32..3 + 32 + 5];
                                                        match IpAddr::from_str(
                                                            &format!(
                                                                "{}:{}:{}:{}:{}:{}:{}:{}",
                                                                &ip[..4],
                                                                &ip[4..8],
                                                                &ip[8..12],
                                                                &ip[12..16],
                                                                &ip[16..20],
                                                                &ip[20..24],
                                                                &ip[24..28],
                                                                &ip[28..32]
                                                            )[..],
                                                        ) {
                                                            Ok(ip) => match port.parse::<u16>() {
                                                                Ok(port) => ret.push(
                                                                    SocketAddr::new(ip, port),
                                                                ),
                                                                Err(_) => {
                                                                    return Err("Could not parse \
                                                                                port for node")
                                                                }
                                                            },
                                                            Err(_) => {
                                                                return Err(
                                                                    "Can't parse IP for node"
                                                                )
                                                            }
                                                        }
                                                        bytes_taken_for_nodes += 3 + 32 + 5;
                                                    }
                                                    _ => return Err("Invalid data for node"),
                                                }
                                            }
                                            match base64::decode(
                                                &buffer[(4 + 44 + 5 + bytes_taken_for_nodes)..],
                                            ) {
                                                Ok(signature_bytes) => {
                                                    if signature_bytes.len() == 64 {
                                                        let mut sig_bytes: [u8; 64] = [0; 64];
                                                        sig_bytes[..64].clone_from_slice(
                                                            &signature_bytes[..64],
                                                        );
                                                        let signature = Signature(sig_bytes);
                                                        let content_peers = ret
                                                            .iter()
                                                            .map(|addr| {
                                                                format!(
                                                                    "{}:{}",
                                                                    addr.ip().to_string(),
                                                                    addr.port()
                                                                )
                                                            })
                                                            .collect::<Vec<_>>();
                                                        match serialize_bootstrap_peers(
                                                            &content_peers,
                                                        ) {
                                                            Ok(content) => {
                                                                if public_key.verify(
                                                                    content.as_bytes(),
                                                                    &signature,
                                                                ) {
                                                                    Ok(ret)
                                                                } else {
                                                                    Err("Signature invalid")
                                                                }
                                                            }
                                                            Err(_) => {
                                                                Err("Couldn't reverse encode \
                                                                     content")
                                                            }
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

pub fn generate_ed25519_key() -> [u8; 32] { (keypair(OsRng::new().unwrap()).0).0 }

#[cfg(feature = "benchmark")]
pub fn get_tps_test_messages(path: Option<String>) -> Vec<Vec<u8>> {
    let mut ret = Vec::new();

    if let Some(ref _path) = path {
        info!("Trying path to find TPS test messages: {}", _path);
        if let Ok(files) = fs::read_dir(_path) {
            for file in files.filter_map(Result::ok).filter(|f| !f.path().is_dir()) {
                let data = fs::read(file.path()).expect("Unable to read file!");
                ret.push(data);
            }
        }
    }

    ret
}

pub fn load_bans(node: &mut P2PNode, kvs_env: &RwLock<Rkv>) -> Fallible<()> {
    let ban_kvs_env = safe_read!(kvs_env)?;
    let ban_store = ban_kvs_env.open_single("bans", StoreOptions::create())?;

    {
        let ban_reader = ban_kvs_env.read()?;
        let ban_iter = ban_store.iter_start(&ban_reader)?;

        for entry in ban_iter {
            let (id_bytes, _expiry) = entry?;
            let node_to_ban = BannedNode::try_from(id_bytes)?;

            node.ban_node(node_to_ban);
        }
    }

    Ok(())
}

pub fn ban_node(
    node: &mut P2PNode,
    peer: &P2PPeer,
    to_ban: BannedNode,
    kvs_handle: &RwLock<Rkv>,
    no_trust_bans: bool,
) {
    info!("Ban node request for {:?} from {:?}", to_ban, peer);
    node.ban_node(to_ban);

    let store_key = to_ban.to_db_repr();
    if let Err(e) = insert_ban(&kvs_handle, &store_key) {
        error!("{}", e);
    }

    if !no_trust_bans {
        node.send_ban(to_ban);
    }
}

pub fn unban_node(
    node: &mut P2PNode,
    peer: &P2PPeer,
    to_unban: BannedNode,
    kvs_handle: &RwLock<Rkv>,
    no_trust_bans: bool,
) {
    info!("Unban node request for {:?} from {:?}", to_unban, peer);
    node.unban_node(to_unban);

    let store_key = to_unban.to_db_repr();
    if let Err(e) = remove_ban(&kvs_handle, &store_key) {
        error!("{}", e);
    }

    if !no_trust_bans {
        node.send_unban(to_unban);
    }
}

/// It clones `kp`. `snow::Keypair` does not derive `Clone` in current version.
pub fn clone_snow_keypair(kp: &Keypair) -> Keypair {
    Keypair {
        private: kp.private.clone(),
        public:  kp.public.clone(),
    }
}

pub fn get_config_and_logging_setup(
) -> Fallible<(configuration::Config, configuration::AppPreferences)> {
    // Get config and app preferences
    let conf = configuration::parse_config()?;
    let app_prefs = configuration::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );

    // Prepare the logger
    let env = if conf.common.trace {
        Env::default().filter_or("LOG_LEVEL", "trace")
    } else if conf.common.debug {
        Env::default().filter_or("LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.common.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();

    p2p_client::setup_panics();

    info!(
        "Starting up {} version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );
    info!(
        "Application data directory: {:?}",
        app_prefs.get_user_app_dir()
    );
    info!(
        "Application config directory: {:?}",
        app_prefs.get_user_config_dir()
    );

    Ok((conf, app_prefs))
}

#[cfg(test)]
mod tests {
    use crate::utils::*;
    use hacl_star::ed25519::SecretKey;

    const PRIVATE_TEST_KEY: [u8; 32] = [
        0xbe, 0xd2, 0x3a, 0xdd, 0x4d, 0x34, 0xab, 0x7a, 0x12, 0xa9, 0xa6, 0xab, 0x2b, 0xaf, 0x97,
        0x06, 0xb0, 0xf7, 0x22, 0x57, 0xa9, 0x82, 0xd4, 0x19, 0x9f, 0x58, 0x44, 0xa7, 0x8f, 0x3b,
        0xe4, 0x70,
    ];

    #[test]
    pub fn test_generate_public_key() {
        const EXPECTED: &str = "fc5d9f06051570d9da9dfd1b3d9b7353e22d764244dcf6b9c665cc21f63df8f2";
        let secret_key = SecretKey {
            0: PRIVATE_TEST_KEY,
        };
        assert_eq!(EXPECTED, to_hex_string(&secret_key.get_public().0));
    }

    #[test]
    pub fn test_sign_verify() {
        const INPUT: &str = "00002IP401001001001008888IP6deadbeaf00000000000000000000000009999";
        let secret_key = SecretKey {
            0: PRIVATE_TEST_KEY,
        };
        let signature = secret_key.signature(INPUT.as_bytes());
        let signature_hex = base64::encode(&signature.0.to_vec());
        let signature_unhexed = base64::decode(&signature_hex).unwrap();
        let mut decoded_signature: [u8; 64] = [0; 64];
        for i in 0..64 {
            decoded_signature[i] = signature_unhexed[i];
        }
        assert!(secret_key
            .get_public()
            .verify(INPUT.as_bytes(), &Signature {
                0: decoded_signature,
            }));
    }

    #[test]
    pub fn test_dns_generated() {
        let peers: Vec<String> = vec![
            "10.10.10.10:8888".to_string(),
            "dead:beaf:::9999".to_string(),
        ];
        let secret_key = SecretKey {
            0: PRIVATE_TEST_KEY,
        };
        let public_b64_key = base64::encode(&secret_key.get_public().0);
        match generate_bootstrap_dns(PRIVATE_TEST_KEY, 240, &peers) {
            Ok(res) => match read_peers_from_dns_entries(res, &public_b64_key) {
                Ok(peers) => {
                    assert_eq!(peers.len(), 2);
                    assert!(peers
                        .iter()
                        .find(|&x| x
                            == &SocketAddr::new(IpAddr::from_str("10.10.10.10").unwrap(), 8888))
                        .is_some());
                    assert!(peers
                        .iter()
                        .find(|&x| x
                            == &SocketAddr::new(IpAddr::from_str("dead:beaf::").unwrap(), 9999))
                        .is_some());
                }
                Err(e) => panic!("Can't read peers from generated records {}", e),
            },
            Err(e) => panic!("Can't generate DNS records {}", e),
        }
    }

    #[test]
    pub fn test_read_resolv_conf() {
        assert_eq!(get_resolvers("tests/resolv.conf-linux", &vec![]), vec![
            "2001:4860:4860::8888",
            "2001:4860:4860::8844",
            "8.8.8.8",
            "8.8.4.4"
        ]);
    }

    #[test]
    pub fn test_read_resolv_conf_with_default() {
        assert_ne!(
            get_resolvers("tests/resolv.conf-linux", &vec!["9.9.9.9".to_string()]),
            vec![
                "2001:4860:4860::8888",
                "2001:4860:4860::8844",
                "8.8.8.8",
                "8.8.4.4"
            ]
        );
    }
}
