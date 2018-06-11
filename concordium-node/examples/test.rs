extern crate p2p_client;
#[macro_use] extern crate structopt;
extern crate bytes;
extern crate mio;
extern crate hacl_star;
use p2p_client::utils::*;

fn main() {
    const TESTSTRING: &str = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";
    println!("Hashed {:?} to {:?}", TESTSTRING,to_hex_string( sha256(TESTSTRING)));
}