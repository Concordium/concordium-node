#![recursion_limit = "1024"]

#[macro_use]
extern crate log;
#[macro_use]
extern crate derive_builder;
#[cfg(not(target_os = "windows"))]
extern crate get_if_addrs;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
cfg_if! {
    if #[cfg(feature = "instrumentation")] {
    #[macro_use]
    extern crate prometheus;
    #[macro_use]
    extern crate gotham_derive;
    extern crate hyper;
    extern crate mime;
    }
}

#[macro_use]
extern crate lazy_static;
#[cfg(not(debug_assertions))]
#[macro_use]
extern crate human_panic;
#[macro_use]
extern crate cfg_if;
#[cfg(target_os = "windows")]
extern crate ipconfig;

#[macro_use]
extern crate failure;
extern crate tempfile;

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
#[cfg(feature = "s11n_serde")]
extern crate serde;

#[cfg(feature = "s11n_serde_cbor")]
#[macro_use]
extern crate serde_derive;

#[cfg(feature = "s11n_serde_cbor")]
extern crate serde_cbor;

#[cfg(any(feature = "s11n_serde_json", feature = "instrumentation"))]
extern crate serde_json;

#[cfg(feature = "s11n_capnp")]
extern crate capnp;

#[cfg(feature = "s11n_nom")]
#[macro_use]
extern crate nom;

#[cfg(test)]
#[macro_use]
extern crate static_assertions;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &str = env!("CARGO_PKG_NAME");
const DEFAULT_DNS_PUBLIC_KEY: &str =
    "58C4FD93586B92A76BA89141667B1C205349C6C38CC8AB2F6613F7483EBFDAA3";
const ENV_DNS_PUBLIC_KEY: Option<&str> = option_env!("CORCORDIUM_PUBLIC_DNS_KEY");
pub fn get_dns_public_key() -> &'static str { ENV_DNS_PUBLIC_KEY.unwrap_or(DEFAULT_DNS_PUBLIC_KEY) }

#[macro_use]
pub mod fails;
#[macro_use]
pub mod common;
pub mod configuration;
pub mod connection;
pub mod db;

pub mod client;
pub mod crypto;
#[macro_use]
pub mod network;
pub mod p2p;

pub mod stats_export_service;

pub mod proto;
pub mod rpc;
pub mod stats_engine;
pub mod utils;

#[cfg(feature = "s11n_capnp")]
pub mod p2p_capnp;

cfg_if! {
    if #[cfg(not(debug_assertions))] {
        pub fn setup_panics() { setup_panic!(); }
    } else  {
        pub fn setup_panics() {}
    }
}
