#[macro_use] extern crate structopt;
extern crate libc;
extern crate hacl_star;
extern crate rand;
extern crate bytes;
extern crate mio;
extern crate ring;
extern crate get_if_addrs;
extern crate num_bigint;
extern crate num_traits;
extern crate byteorder;
extern crate rustls;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate time;
extern crate webpki;
extern crate untrusted;
extern crate openssl;
#[macro_use]
extern crate nom;
extern crate protobuf;
extern crate grpcio;
extern crate futures;

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &'static str = env!("CARGO_PKG_NAME");

pub mod configuration;
pub mod common;
pub mod p2p;
pub mod utils;
pub mod rpc;
pub mod proto;