#![feature(box_syntax, box_patterns, ip, unboxed_closures, fn_traits, integer_atomics)]
#![recursion_limit = "1024"]
#[macro_use]
extern crate error_chain;
extern crate structopt;
extern crate byteorder;
extern crate bytes;
#[cfg(not(windows))]
extern crate get_if_addrs;
extern crate hacl_star;
extern crate libc;
extern crate mio;
extern crate num_bigint;
extern crate num_traits;
extern crate rand;
extern crate ring;
extern crate rustls;
#[macro_use]
extern crate log;
extern crate app_dirs;
extern crate env_logger;
extern crate futures;
extern crate grpcio;
extern crate hex;
extern crate iron;
extern crate openssl;
extern crate preferences;
extern crate protobuf;
extern crate reqwest;
extern crate router;
extern crate rusqlite;
extern crate serde_json;
extern crate time;
extern crate untrusted;
extern crate webpki;
#[macro_use]
extern crate prometheus;
#[macro_use]
extern crate lazy_static;
extern crate atomic_counter;
#[cfg(not(debug_assertions))]
#[macro_use]
extern crate human_panic;
#[macro_use]
extern crate cfg_if;
extern crate dns;
#[cfg(windows)]
extern crate ipconfig;
extern crate resolv_conf;
extern crate semver;
extern crate vecio;

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &'static str = env!("CARGO_PKG_NAME");
const DEFAULT_DNS_PUBLIC_KEY: &'static str =
    "58C4FD93586B92A76BA89141667B1C205349C6C38CC8AB2F6613F7483EBFDAA3";
const ENV_DNS_PUBLIC_KEY: Option<&'static str> = option_env!("CORCORDIUM_PUBLIC_DNS_KEY");

pub fn get_dns_public_key() -> &'static str {
    ENV_DNS_PUBLIC_KEY.unwrap_or(DEFAULT_DNS_PUBLIC_KEY)
}

#[macro_use] pub mod common;
pub mod connection;
pub mod configuration;
pub mod db;
pub mod errors;
pub mod p2p;
pub mod prometheus_exporter;
pub mod proto;
pub mod rpc;
pub mod utils;
pub mod network;
pub mod stats_engine;

cfg_if! {
    if #[cfg(not(debug_assertions))] {
        pub fn setup_panics() { setup_panic!(); }
    } else  {
        pub fn setup_panics() {}
    }
}
