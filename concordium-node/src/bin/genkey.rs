#![recursion_limit = "1024"]

use concordium_node::utils::{generate_ed25519_key, to_hex_string};
use ed25519_dalek::{PublicKey, SecretKey};
use failure::Fallible;
use std::{fs::OpenOptions, io::Write, process::exit};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "DNS Key Generator")]
struct ConfigCli {
    #[structopt(long = "keyfile", help = "Output key to file in binary")]
    keyfile: String,
    #[structopt(long = "print-key", short = "p", help = "Print key as HEX when done")]
    print_key: bool,
    #[structopt(long = "force-overwrite", help = "Force overwrite if file already exists")]
    force_overwrite: bool,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    if conf.print_config {
        // Print out the configuration
        println!("{:?}", conf);
    }

    if !std::path::Path::new(&conf.keyfile).exists() || conf.force_overwrite {
        match OpenOptions::new().read(true).write(true).create(true).open(&conf.keyfile) {
            Ok(mut file) => {
                let secret_key: SecretKey = generate_ed25519_key();
                let public_key = PublicKey::from(&secret_key);
                match file.write_all(secret_key.as_bytes().as_ref()) {
                    Ok(_) => {
                        println!("Key written to {}", &conf.keyfile);
                        println!("Public key is {}", to_hex_string(public_key.as_bytes()));
                        if conf.print_key {
                            println!(
                                "Key written to file is {}",
                                to_hex_string(secret_key.as_bytes())
                            );
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
