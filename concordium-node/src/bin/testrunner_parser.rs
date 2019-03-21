#![recursion_limit = "1024"]
#[macro_use] extern crate log;

use serde_json::{json, Value};
use std::cmp::Ordering;
use std::fs::File;
use std::io::prelude::*;
use structopt::StructOpt;
use failure::{ Fallible, Error, err_msg };
use p2p_client::failing_main;

#[derive(StructOpt, Debug)]
#[structopt(name = "TestRunner Results Parser")]
struct ConfigCli {
    #[structopt(help = "Output to analyze either URL to Test Runner or file on disk",
                long = "analyze",
                short = "a")]
    pub to_analyze: String,
    #[structopt(help = "Print results as a CSV file", long = "csv")]
    pub csv: bool,
}

failing_main!(run);

pub fn run() -> Fallible<()> {
    let conf = ConfigCli::from_args();
    p2p_client::setup_panics();
    let results =
        if conf.to_analyze.starts_with("https://") || conf.to_analyze.starts_with("http://") {
            match reqwest::get(&conf.to_analyze) {
                Ok(ref mut res) if res.status().is_success() => {
                    res.text().unwrap_or_else(|_| panic!("Can't read file from URL"))
                }
                _ => panic!("Can't read file from URL"),
            }
        } else {
            match File::open(conf.to_analyze) {
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
    if json_value["service_name"] != json!(null)
       && json_value["service_version"] != json!(null)
       && json_value["service_name"].as_str().unwrap() == "TestRunner"
       && json_value.get("service_version").unwrap().as_str().unwrap() == p2p_client::VERSION
    {
        if json_value["test_start_time"] != json!(null)
            && json_value["packet_size"] != json!(null)
            && json_value["measurements"] != json!(null) {
                let start_time = json_value["test_start_time"].as_u64().unwrap();
                let packet_size = json_value["packet_size"].as_u64().unwrap();
                let mut measurements =
                    json_value["measurements"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|ref v| {
                        if v["node_id"]  != json!(null)
                            && v["received_time"]  != json!(null) {
                                Ok((v["node_id"].as_str().unwrap().to_string(),
                                    v["received_time"].as_u64().unwrap() - start_time,
                                    v["received_time"].as_u64().unwrap()))
                            } else {
                                Err(err_msg("invalid JSON format"))
                            }
                    })
                    .collect::<Result<Vec<(String, u64, u64)>, Error>>()?;
                measurements.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));
                if conf.csv {
                    for ele in measurements {
                        println!("{},{},{}", ele.0, ele.2, ele.1);
                    }
                } else {
                    println!("Analyzed output from test runner");
                    println!("Start of test: {}", start_time);
                    println!("Measurements count: {}", measurements.len());
                    println!("Packet size used: {}", packet_size);
                    for ele in measurements {
                        println!("- {} got it at {} with a transmission time of {}",
                                 ele.0, ele.2, ele.1);
                    }
                }
                Ok(())
            } else {
                Err(err_msg("invalid JSON format"))
            }
    } else {
        Err(err_msg("invalid JSON format"))
    }
}
