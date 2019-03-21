#![feature(box_syntax, box_patterns, ip, unboxed_closures, fn_traits, integer_atomics, custom_attribute)]
#![recursion_limit = "1024"]

#[macro_use] extern crate log;
#[macro_use]
extern crate derive_builder;
#[cfg(not(target_os = "windows"))]
extern crate get_if_addrs;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate prometheus;
#[macro_use]
extern crate lazy_static;
#[cfg(not(debug_assertions))]
#[macro_use]
extern crate human_panic;
#[macro_use]
extern crate cfg_if;
#[cfg(target_os = "windows")]
extern crate ipconfig;

use failure;

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &'static str = env!("CARGO_PKG_NAME");
const DEFAULT_DNS_PUBLIC_KEY: &'static str =
    "58C4FD93586B92A76BA89141667B1C205349C6C38CC8AB2F6613F7483EBFDAA3";
const ENV_DNS_PUBLIC_KEY: Option<&'static str> = option_env!("CORCORDIUM_PUBLIC_DNS_KEY");

pub fn get_dns_public_key() -> &'static str {
    ENV_DNS_PUBLIC_KEY.unwrap_or(DEFAULT_DNS_PUBLIC_KEY)
}

#[macro_use] pub mod fails;
#[macro_use] pub mod common;
pub mod connection;
pub mod configuration;
pub mod db;

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
