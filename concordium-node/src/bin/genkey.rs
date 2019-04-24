#![recursion_limit = "1024"]

use failure::Fallible;
use hacl_star::ed25519::SecretKey;
use p2p_client::utils::{generate_ed25519_key, to_hex_string};
use std::{fs::OpenOptions, io::Write, process::exit};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "DNS Key Generator")]
struct ConfigCli {
    #[structopt(long = "keyfile", help = "Output key to file in binary")]
    keyfile: String,
    #[structopt(long = "print-key", short = "p", help = "Print key as HEX when done")]
    print_key: bool,
    #[structopt(
        long = "force-overwrite",
        help = "Force overwrite if file already exists"
    )]
    force_overwrite: bool,
    #[structopt(
        long = "print-config",
        help = "Print out config struct"
    )]
    pub print_config: bool,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    if conf.print_config {
        //Print out the configuration
        println!("{:?}", conf);
    }

    p2p_client::setup_panics();
    if !std::path::Path::new(&conf.keyfile).exists() || conf.force_overwrite {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&conf.keyfile)
        {
            Ok(mut file) => {
                let key: [u8; 32] = generate_ed25519_key();
                let secret_key = SecretKey(key);
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
