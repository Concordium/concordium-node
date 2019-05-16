#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

use concordium_consensus::{
    consensus::ConsensusContainer,
    ffi::{start_haskell, stop_haskell},
};
use env_logger::{Builder, Env};
use failure::{bail, Fallible};
use std::{
    fs::{create_dir_all, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    process::exit,
};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "Baker data generator")]
struct ConfigCli {
    #[structopt(long = "output-dir", help = "Directory to output files to")]
    output_dir: String,
    #[structopt(long = "num-bakers", help = "Number of bakers in the network")]
    pub baker_num_bakers: u64,
    #[structopt(
        long = "baker-genesis",
        help = "Genesis time to build with",
        default_value = "0"
    )]
    pub baker_genesis: u64,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    let env = Env::default().filter_or("MY_LOG_LEVEL", "info");
    let mut log_builder = Builder::from_env(env);
    log_builder.init();

    if conf.print_config {
        // Print out the configuration
        println!("{:?}", conf);
    }

    let output_path = Path::new(&conf.output_dir);

    if output_path.exists() {
        bail!("Output path {} already exists!", conf.output_dir);
    }

    create_dir_all(output_path)?;

    start_haskell();

    let consensus_baked_data =
        ConsensusContainer::generate_data(conf.baker_genesis, conf.baker_num_bakers);

    stop_haskell();

    match consensus_baked_data {
        Ok((genesis_data, private_data_blobs)) => {
            let genesis_out_path: PathBuf =
                [&conf.output_dir, p2p_client::client::FILE_NAME_GENESIS_DATA]
                    .iter()
                    .collect();
            match OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(&genesis_out_path)
            {
                Ok(mut genesis_file) => match genesis_file.write_all(&genesis_data) {
                    Ok(_) => info!(
                        "Wrote out genesis data to {}",
                        genesis_out_path.to_str().unwrap()
                    ),
                    Err(err) => {
                        error!(
                            "Could not write genesis data to file {} due to {}",
                            genesis_out_path.to_str().unwrap(),
                            err
                        );
                        exit(1);
                    }
                },
                Err(err) => {
                    error!(
                        "Could not open genesis file {} for writing due to {}",
                        genesis_out_path.to_str().unwrap(),
                        err
                    );
                    exit(1);
                }
            }
            private_data_blobs
                .iter()
                .for_each(|(baker_id, private_data)| {
                    let private_data_out_path: PathBuf = [
                        &conf.output_dir,
                        &format!(
                            "{}{}{}",
                            p2p_client::client::FILE_NAME_PREFIX_BAKER_PRIVATE,
                            baker_id,
                            p2p_client::client::FILE_NAME_SUFFIX_BAKER_PRIVATE
                        ),
                    ]
                    .iter()
                    .collect();
                    match OpenOptions::new()
                        .read(true)
                        .write(true)
                        .create(true)
                        .open(&private_data_out_path)
                    {
                        Ok(mut baker_file) => match baker_file.write_all(&private_data) {
                            Ok(_) => info!(
                                "Wrote out private data to {}",
                                private_data_out_path.to_str().unwrap()
                            ),
                            Err(err) => {
                                error!(
                                    "Could not write private data to file {} due to {}",
                                    private_data_out_path.to_str().unwrap(),
                                    err
                                );
                                exit(1);
                            }
                        },
                        Err(err) => {
                            error!(
                                "Could not open private data file {} for writing due to {}",
                                private_data_out_path.to_str().unwrap(),
                                err
                            );
                            exit(1);
                        }
                    }
                });
        }
        Err(err) => bail!("Can't generate data {}", err),
    }
    Ok(())
}
