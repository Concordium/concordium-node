//! Miscellaneous utilities.

use crate::{concordium_dns::dns, configuration as config};
use anyhow::{bail, ensure, Context};
use byteorder::{NetworkEndian, WriteBytesExt};
use ed25519_dalek::{Keypair, PublicKey, SecretKey, Signer};
#[cfg(not(target_os = "macos"))]
use env_logger::Builder;
use env_logger::Env;
use log::LevelFilter;
use rand::rngs::OsRng;
#[cfg(not(target_os = "windows"))]
use std::fs::File;
#[cfg(not(target_os = "macos"))]
use std::io::Write;
use std::{
    net::{IpAddr, SocketAddr},
    path::Path,
    str::{self, FromStr},
};

fn serialize_ip(ip: IpAddr) -> String {
    match ip {
        IpAddr::V4(ip4) => format!(
            "IP4{:03}{:03}{:03}{:03}",
            ip4.octets()[0],
            ip4.octets()[1],
            ip4.octets()[2],
            ip4.octets()[3],
        ),
        IpAddr::V6(ip6) => format!(
            "IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:\
             02x}{:02x}{:02x}",
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
        ),
    }
}

fn serialize_addr(addr: SocketAddr) -> String {
    format!("{}{:05}", serialize_ip(addr.ip()), addr.port())
}

pub fn to_hex_string(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

fn parse_ip_port(input: &str) -> Option<SocketAddr> {
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

#[cfg(not(target_os = "macos"))]
pub fn setup_logger_env(env: Env, no_log_timestamp: bool) {
    let mut log_builder = Builder::from_env(env);
    if no_log_timestamp {
        log_builder.format_timestamp(None);
    } else {
        log_builder.format(|buf, record| {
            writeln!(buf, "{}: {}: {}", buf.timestamp_nanos(), record.level(), record.args())
        });
    }
    log_builder.filter(Some(&"tokio_reactor"), LevelFilter::Error);
    log_builder.filter(Some(&"hyper"), LevelFilter::Error);
    log_builder.filter(Some(&"reqwest"), LevelFilter::Error);
    log_builder.filter(Some(&"gotham"), LevelFilter::Error);
    log_builder.filter(Some(&"h2"), LevelFilter::Error);
    log_builder.init();
}

#[cfg(target_os = "macos")]
pub fn setup_macos_logger(trace: bool, debug: bool) {
    let level_filter = if trace {
        LevelFilter::Trace
    } else if debug {
        LevelFilter::Debug
    } else {
        LevelFilter::Info
    };

    crate::macos_log::MacOsLogger::new("software.concordium.node")
        .level_filter(level_filter)
        .category_level_filter("tokio_reactor", LevelFilter::Error)
        .category_level_filter("hyper", LevelFilter::Error)
        .category_level_filter("reqwest", LevelFilter::Error)
        .category_level_filter("gotham", LevelFilter::Error)
        .category_level_filter("h2", LevelFilter::Error)
        .init()
        .expect("Failed to initialise MacOsLogger");
}

#[cfg(not(target_os = "windows"))]
pub fn get_resolvers(resolv_conf: &Path, resolvers: &[String]) -> Vec<String> {
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
            _ => panic!("Can't open {}", resolv_conf.to_string_lossy()),
        }
    }
}

#[cfg(target_os = "windows")]
pub fn get_resolvers(_resolv_conf: &Path, resolvers: &[String]) -> Vec<String> {
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
    require_dnssec: bool,
) -> anyhow::Result<Vec<SocketAddr>> {
    if let Some(n) = input.rfind(':') {
        let (ip, port) = input.split_at(n);
        let port = port[1..].parse::<u16>().context(format!(
            "Cannot parse <{}> as the host port. The value should be a 16-bit unsigned integer in \
             base 10.",
            port
        ))?;

        if let Ok(ip) = IpAddr::from_str(&ip) {
            Ok(vec![SocketAddr::new(ip, port)])
        } else {
            let resolver_addresses =
                resolvers.iter().map(|x| IpAddr::from_str(x)).flatten().collect::<Vec<_>>();
            ensure!(!resolver_addresses.is_empty(), "No DNS resolvers available");

            let a_record_resolver = if let Ok(res) =
                dns::resolve_dns_a_record(&ip, &resolver_addresses, require_dnssec)
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
                dns::resolve_dns_aaaa_record(&ip, &resolver_addresses, require_dnssec)
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
        }
    } else {
        bail!("Can't parse port from <{}>. The accepted format is address:port.", input.to_owned());
    }
}

pub fn get_bootstrap_nodes(
    resolvers: &[String],
    require_dnssec: bool,
    bootstrap_nodes: &[String],
) -> Result<Vec<SocketAddr>, String> {
    if !bootstrap_nodes.is_empty() {
        debug!("Not using DNS for bootstrapping, we have nodes specified");
        let bootstrap_nodes = bootstrap_nodes
            .iter()
            .filter_map(|ip_port| {
                parse_host_port(ip_port, resolvers, require_dnssec)
                    .map_err(|err| error!("Invalid bootstrapper node received: {}", err))
                    .ok()
            })
            .flatten()
            .collect::<Vec<_>>();
        Ok(bootstrap_nodes)
    } else {
        Err("No bootstrap nodes specified.".to_string())
    }
}

fn serialize_bootstrap_peers(peers: &[String]) -> Result<String, &'static str> {
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
    let secret_key = match SecretKey::from_bytes(&input_key) {
        Ok(sk) => sk,
        Err(_) => return Err("Invalid secret key."),
    };
    let public_key = PublicKey::from(&secret_key);
    let kp = Keypair {
        secret: secret_key,
        public: public_key,
    };

    let mut return_buffer = base64::encode(public_key.as_bytes());

    match serialize_bootstrap_peers(peers) {
        Ok(ref content) => {
            return_buffer.push_str(content);
            let signature = kp.sign(content.as_bytes());
            return_buffer.push_str(&base64::encode(&signature.to_bytes()[..]));
        }
        Err(_) => {
            return Err("Couldn't parse peers given");
        }
    }

    let mut ret = String::new();

    let mut return_size = vec![];
    assert!(return_size.write_u16::<NetworkEndian>(return_buffer.len() as u16).is_ok());
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

pub fn generate_ed25519_key() -> SecretKey { SecretKey::generate(&mut OsRng::default()) }

pub fn get_config_and_logging_setup() -> anyhow::Result<(config::Config, config::AppPreferences)> {
    // Get config and app preferences
    let conf = config::parse_config()?;
    let app_prefs = config::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );

    // Prepare the logger
    let (env, log_lvl) = if conf.common.trace {
        (Env::default().filter_or("LOG_LEVEL", "trace"), "trace")
    } else if conf.common.debug {
        (Env::default().filter_or("LOG_LEVEL", "debug"), "debug")
    } else {
        (Env::default().filter_or("LOG_LEVEL", "info"), "info")
    };

    #[cfg(not(target_os = "macos"))]
    setup_logger_env(env, conf.common.no_log_timestamp);

    #[cfg(target_os = "macos")]
    setup_macos_logger(conf.common.trace, conf.common.debug);

    if conf.common.print_config {
        info!("Config:{:?}\n", conf);
    }

    info!("Starting up {} version {}!", crate::APPNAME, crate::VERSION);
    info!("Application data directory: {}", app_prefs.get_user_app_dir().to_string_lossy());
    info!("Application config directory: {}", app_prefs.get_user_config_dir().to_string_lossy());
    info!(
        "Network: {}",
        if conf.cli.no_network {
            "disabled"
        } else {
            "enabled"
        }
    );
    info!("Log level: {}", log_lvl);

    Ok((conf, app_prefs))
}

#[cfg(test)]
mod tests {
    use crate::utils::*;
    use ed25519_dalek::{Signature, Verifier};
    use std::convert::TryFrom;

    const PRIVATE_TEST_KEY: [u8; 32] = [
        0xbe, 0xd2, 0x3a, 0xdd, 0x4d, 0x34, 0xab, 0x7a, 0x12, 0xa9, 0xa6, 0xab, 0x2b, 0xaf, 0x97,
        0x06, 0xb0, 0xf7, 0x22, 0x57, 0xa9, 0x82, 0xd4, 0x19, 0x9f, 0x58, 0x44, 0xa7, 0x8f, 0x3b,
        0xe4, 0x70,
    ];

    #[test]
    pub fn test_generate_public_key() {
        const EXPECTED: &str = "fc5d9f06051570d9da9dfd1b3d9b7353e22d764244dcf6b9c665cc21f63df8f2";
        let secret_key = SecretKey::from_bytes(&PRIVATE_TEST_KEY).unwrap();
        assert_eq!(EXPECTED, to_hex_string(PublicKey::from(&secret_key).as_bytes()));
    }

    #[test]
    pub fn test_sign_verify() {
        const INPUT: &str = "00002IP401001001001008888IP6deadbeaf00000000000000000000000009999";
        let secret_key = SecretKey::from_bytes(&PRIVATE_TEST_KEY).unwrap();
        let public = PublicKey::from(&secret_key);
        let kp = Keypair {
            secret: secret_key,
            public,
        };
        let signature = kp.sign(INPUT.as_bytes());
        let signature_hex = base64::encode(&signature.to_bytes()[..]);
        let signature_unhexed = base64::decode(&signature_hex).unwrap();
        let mut decoded_signature: [u8; 64] = [0; 64];
        for i in 0..64 {
            decoded_signature[i] = signature_unhexed[i];
        }
        assert!(kp
            .public
            .verify(INPUT.as_bytes(), &Signature::try_from(&decoded_signature[..]).unwrap())
            .is_ok());
    }
}
