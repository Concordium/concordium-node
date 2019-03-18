#![recursion_limit = "1024"]
#[macro_use]
extern crate error_chain;

use hacl_star::ed25519::SecretKey;
use p2p_client::errors::*;
use p2p_client::utils::{generate_ed25519_key, to_hex_string};
use std::fs::OpenOptions;
use std::io::Write;
use std::process::exit;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "DNS Key Generator")]
struct ConfigCli {
    #[structopt(long = "keyfile", help = "Output key to file in binary")]
    keyfile: String,
    #[structopt(long = "print-key",
                short = "p",
                help = "Print key as HEX when done")]
    print_key: bool,
    #[structopt(long = "force-overwrite",
                help = "Force overwrite if file already exists")]
    force_overwrite: bool,
}

quick_main!(run);

pub fn run() -> ResultExtWrapper<()> {
    let conf = ConfigCli::from_args();
    p2p_client::setup_panics();
    if !std::path::Path::new(&conf.keyfile).exists() || conf.force_overwrite {
        match OpenOptions::new().read(true)
                                .write(true)
                                .create(true)
                                .open(&conf.keyfile)
        {
            Ok(mut file) => {
                let key: [u8; 32] = generate_ed25519_key();
                let secret_key = SecretKey { 0: key };
                let public_key = secret_key.get_public();
                match file.write_all(&key) {
                    Ok(_) => {
                        println!("Key written to {}", &conf.keyfile);
                        println!("Public key is {}", to_hex_string(&public_key.0));
                        if conf.print_key {
                            println!("Key written to file is {}", to_hex_string(&key));
                        }
                    }
                    Err(e) => {
                        println!("Couldn't write file {}", e);
                        exit(1);
                    }
                }
            }
            Err(e) => {
                println!("Can't open file {}", e);
                exit(1);
            }
        }
    } else {
        println!("Won't overwrite file unless given argument given to do so");
        exit(1);
    }
    Ok(())
}
