#[macro_use]
extern crate structopt;
extern crate byteorder;
extern crate bytes;
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
extern crate env_logger;
extern crate openssl;
extern crate time;
extern crate untrusted;
extern crate webpki;
#[macro_use]
extern crate nom;
extern crate app_dirs;
extern crate futures;
extern crate grpcio;
extern crate preferences;
extern crate protobuf;
extern crate rusqlite;

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &'static str = env!("CARGO_PKG_NAME");

pub mod common;
pub mod configuration;
pub mod db;
pub mod p2p;
pub mod proto;
pub mod rpc;
pub mod utils;
