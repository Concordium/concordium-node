#![recursion_limit = "1024"]
extern crate structopt;
#[macro_use]
extern crate error_chain;
extern crate p2p_client;
extern crate reqwest;
extern crate serde;
extern crate serde_json;

use p2p_client::errors::*;
use serde_json::Value;
use std::cmp::Ordering;
use std::fs::File;
use std::io::prelude::*;
use structopt::StructOpt;

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

quick_main!(run);

pub fn run() -> ResultExtWrapper<()> {
    let conf = ConfigCli::from_args();
    p2p_client::setup_panics();
    let results =
        if conf.to_analyze.starts_with("https://") || conf.to_analyze.starts_with("http://") {
            match reqwest::get(&conf.to_analyze) {
                Ok(ref mut res) if res.status().is_success() => {
                    match res.text() {
                        Ok(text) => text,
                        _ => panic!("Can't read file from URL"),
                    }
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
    if json_value.get("service_name").is_some()
       && json_value.get("service_version").is_some()
       && json_value.get("service_name").unwrap().as_str().unwrap() == "TestRunner"
       && json_value.get("service_version").unwrap().as_str().unwrap() == p2p_client::VERSION
    {
        let start_time = json_value.get("test_start_time").unwrap().as_u64().unwrap();
        let packet_size = json_value.get("packet_size").unwrap().as_u64().unwrap();
        let mut measurements =
            json_value.get("measurements")
                      .unwrap()
                      .as_array()
                      .unwrap()
                      .iter()
                      .map(|ref v| {
                               (v.get("node_id").unwrap().as_str().unwrap().to_string(),
                                v.get("received_time").unwrap().as_u64().unwrap() - start_time,
                                v.get("received_time").unwrap().as_u64().unwrap())
                           })
                      .collect::<Vec<(String, u64, u64)>>();
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
        Err(ErrorKindWrapper::ParseError("Json not correct format".to_string()).into())
    }
}
