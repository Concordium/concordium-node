#![recursion_limit = "1024"]

use consensus_rust::{
    consensus::{ConsensusContainer, ConsensusLogLevel},
    ffi,
};
use failure::Fallible;
use std::{fs::OpenOptions, io::Read, process::exit};
use structopt::StructOpt;
use tempdir::TempDir;

#[derive(StructOpt, Debug)]
#[structopt(name = "Genesis tester tool")]
struct ConfigCli {
    #[structopt(long = "genesis-file", help = "Genesis file to open")]
    genesis_file: String,
    #[structopt(long = "private-key-file", help = "Private key file to open")]
    private_key_file: Option<String>,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    let genesis_data = match OpenOptions::new().read(true).open(&conf.genesis_file) {
        Ok(mut file) => {
            let mut read_data = vec![];
            match file.read_to_end(&mut read_data) {
                Ok(_) => read_data,
                Err(_) => {
                    println!("Could not read genesis data file {} properly", &conf.genesis_file);
                    exit(1);
                }
            }
        }
        Err(_) => {
            println!("Could not open genesis data file {}", &conf.genesis_file);
            exit(1);
        }
    };

    let private_data = if let Some(private_key_file) = conf.private_key_file {
        match OpenOptions::new().read(true).open(&private_key_file) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => Some(read_data),
                    Err(_) => {
                        println!("Could not read private key file {} properly", &private_key_file);
                        exit(1);
                    }
                }
            }
            Err(_) => {
                println!("Could not open private key file {}", &private_key_file);
                exit(1);
            }
        }
    } else {
        None
    };

    let tmp_dir = TempDir::new("genesis-tester")?;

    ffi::start_haskell(&[]);

    match ConsensusContainer::new(
        u64::max_value(),
        u64::max_value(),
        u64::max_value(),
        u64::max_value(),
        genesis_data,
        private_data.clone(),
        match private_data {
            Some(_) => Some(0),
            _ => None,
        },
        ConsensusLogLevel::Info,
        &tmp_dir.into_path(),
        &"",
    ) {
        Err(e) => {
            println!("Error while starting consensus {}", e);
            ffi::stop_haskell();
            exit(1);
        }
        Ok(consensus_container) => {
            println!("Opened genesis data and private key correctly");
            consensus_container.stop();
            ffi::stop_haskell();
            exit(0);
        }
    }
}
