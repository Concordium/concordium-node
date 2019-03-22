#![recursion_limit = "1024"]
#[macro_use]
extern crate arrayref;

use p2p_client::utils::generate_bootstrap_dns;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use structopt::StructOpt;
use failure::Fallible;

#[derive(StructOpt, Debug)]
#[structopt(name = "DNS Record Generator")]
struct ConfigCli {
    #[structopt(raw(required = "true"),
                long = "add-peers",
                help = "Peer in format IP:PORT, multiple allowed")]
    peers: Vec<String>,
    #[structopt(help = "Read private key from file",
                long = "keyfile",
                raw(required = "true"))]
    keyfile: String,
    #[structopt(long = "record-length",
                help = "DNS record length",
                default_value = "250")]
    dns_record_length: usize,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();
    if !std::path::Path::new(&conf.keyfile).exists() {
        println!("Key {} doesn't exist, please specify valid file",
                 conf.keyfile);
        exit(1);
    }

    p2p_client::setup_panics();

    let mut private_key_bytes: Vec<u8> = vec![];
    match File::open(&conf.keyfile) {
        Ok(ref mut file) => {
            if let Err(e) = file.read_to_end(&mut private_key_bytes) {
                println!("Error while reading {} {}", &conf.keyfile, e);
                exit(1);
            }
        }
        Err(e) => {
            println!("Error while opening {} {}", &conf.keyfile, e);
            exit(1);
        }
    }

    println!("DNS records:");
    match generate_bootstrap_dns(array_ref!(private_key_bytes, 0, 32).clone(),
                                 conf.dns_record_length,
                                 &conf.peers)
    {
        Ok(entries) => {
            for entry in entries {
                println!("\tIN\tTXT\t{}", entry);
            }
        }
        Err(e) => {
            println!("Error while generating DNS records {}", e);
            exit(1);
        }
    }
    Ok(())
}
