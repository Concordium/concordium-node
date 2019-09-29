#![recursion_limit = "1024"]
use failure::Fallible;
use p2p_client::utils::generate_bootstrap_dns;
use std::{fs::File, io::Read, process::exit};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "DNS Record Generator")]
struct ConfigCli {
    #[structopt(
        raw(required = "true"),
        long = "add-peers",
        help = "Peer in format IP:PORT, multiple allowed"
    )]
    peers: Vec<String>,
    #[structopt(
        help = "Read private key from file",
        long = "keyfile",
        raw(required = "true")
    )]
    keyfile: String,
    #[structopt(
        long = "record-length",
        help = "DNS record length",
        default_value = "250"
    )]
    dns_record_length: usize,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    if conf.print_config {
        // Print out the configuration
        println!("{:?}", conf);
    }

    if !std::path::Path::new(&conf.keyfile).exists() {
        println!(
            "Key {} doesn't exist, please specify valid file",
            conf.keyfile
        );
        exit(1);
    }

    let mut private_key_bytes: [u8; 32] = [0; 32];
    match File::open(&conf.keyfile) {
        Ok(ref mut file) => {
            if let Err(e) = file.read_exact(&mut private_key_bytes) {
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
    match generate_bootstrap_dns(private_key_bytes, conf.dns_record_length, &conf.peers) {
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
