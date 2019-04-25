#![recursion_limit = "1024"]

use failure::{err_msg, Fallible};
use serde_json::Value;
use std::{fs::File, io::prelude::*};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "TestRunner Results Parser")]
struct ConfigCli {
    #[structopt(
        help = "Output to analyze either URL to Test Runner or file on disk",
        long = "analyze",
        short = "a"
    )]
    pub to_analyze: String,
    #[structopt(help = "Print results as a CSV file", long = "csv")]
    pub csv: bool,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
}

fn get_measurements(
    start_time: u64,
    measurements: &[serde_json::Value],
) -> Option<Vec<(String, u64, u64)>> {
    let mut res = vec![];
    for measurement in measurements {
        let m = measurement["node_id"].as_str().and_then(|node_id| {
            measurement["received_time"]
                .as_u64()
                .and_then(|received_time| {
                    Some((
                        node_id.to_string(),
                        received_time - start_time,
                        received_time,
                    ))
                })
        });
        if let Some(m) = m {
            res.push(m)
        }
    }
    Some(res)
}

fn process_measurement(
    start_time: u64,
    packet_size: u64,
    csv: bool,
    mut measurements: Vec<(String, u64, u64)>,
) {
    measurements.sort_by_key(|x| x.1);
    if csv {
        for ele in measurements {
            println!("{},{},{}", ele.0, ele.2, ele.1);
        }
    } else {
        println!("Analyzed output from test runner");
        println!("Start of test: {}", start_time);
        println!("Measurements count: {}", measurements.len());
        println!("Packet size used: {}", packet_size);
        for ele in measurements {
            println!(
                "- {} got it at {} with a transmission time of {}",
                ele.0, ele.2, ele.1
            );
        }
    }
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    if conf.print_config {
        // Print out the configuration
        println!("{:?}", conf);
    }

    p2p_client::setup_panics();
    let results =
        if conf.to_analyze.starts_with("https://") || conf.to_analyze.starts_with("http://") {
            match reqwest::get(&conf.to_analyze) {
                Ok(ref mut res) if res.status().is_success() => res
                    .text()
                    .unwrap_or_else(|_| panic!("Can't read file from URL")),
                _ => panic!("Can't read file from URL"),
            }
        } else {
            match File::open(conf.to_analyze.clone()) {
                Ok(mut file) => {
                    let mut contents = String::new();
                    match file.read_to_string(&mut contents) {
                        Ok(_) => contents,
                        _ => panic!("Error reading file"),
                    }
                }
                _ => panic!("Can't read file"),
            }
        };
    let json_value: Value = serde_json::from_str(&results)?;
    if json_value["service_name"]
        .as_str()
        .map_or(false, |val| val == "TestRunner")
        && json_value["service_version"]
            .as_str()
            .map_or(false, |val| val == p2p_client::VERSION)
    {
        if json_value["test_start_time"]
            .as_u64()
            .and_then(|start_time| {
                json_value["packet_size"].as_u64().and_then(|packet_size| {
                    json_value["measurements"]
                        .as_array()
                        .and_then(|m| get_measurements(start_time, m))
                        .map(|m| process_measurement(start_time, packet_size, conf.csv, m))
                })
            })
            .is_some()
        {
            return Ok(());
        }
    };
    Err(err_msg("invalid JSON format"))
}
