#![recursion_limit = "1024"]

// Force the system allocator on every platform
use std::{alloc::System, sync::Arc};
#[global_allocator]
static A: System = System;

use anyhow::{ensure, Context};
use concordium_base::hashes::BlockHash;
use concordium_node::{
    common::PeerType,
    consensus_ffi::consensus::Regenesis,
    p2p::maintenance::{attempt_bootstrap, spawn, P2PNode},
    stats_export_service::instantiate_stats_export_engine,
    utils::get_config_and_logging_setup,
};

use std::{env, process::Command, thread, time::Duration};

fn main() -> anyhow::Result<()> {
    let (mut conf, app_prefs) = get_config_and_logging_setup()?;
    let data_dir_path = app_prefs.get_data_dir();

    conf.connection.max_allowed_nodes = Some(0);
    conf.connection.thread_pool_size = 1;
    let pager_duty_token = env::var("PD_TOKEN")?;
    let pager_duty_email = env::var("PD_EMAIL")?;
    let pager_duty_svcid = env::var("PD_SVCID")?;

    let stats_export_service = instantiate_stats_export_engine(&conf.prometheus)?;
    let fname = conf
        .bootstrapper
        .regenesis_block_hashes
        .clone()
        .unwrap_or_else(|| data_dir_path.join(std::path::Path::new("genesis_hash")));
    let regenesis_blocks: Vec<BlockHash> = serde_json::from_slice(&std::fs::read(fname)?)?;
    let regenesis_arc: Arc<Regenesis> = Arc::new(Regenesis::from_blocks(regenesis_blocks));

    ensure!(
        regenesis_arc.blocks.read().unwrap().len() > 0,
        "Bootstrapper can't run without specifying genesis hashes."
    );

    let (node, server, poll) =
        P2PNode::new(conf.common.id, &conf, PeerType::Node, stats_export_service, regenesis_arc)
            .context("Failed to create the node.")?;

    spawn(&node, server, poll, None);

    if !conf.connection.no_bootstrap_dns {
        attempt_bootstrap(&node);
    }

    loop {
        thread::sleep(Duration::from_millis(500));
        if !(node.connections().read().unwrap()).is_empty() {
            println!("Success - I was able to bootstrap");
            return node.close_and_join();
        }
        if node.get_uptime() > 10_000 {
            Command::new("curl")
                .arg("-i")
                .arg("-X")
                .arg("POST")
                .arg("--header")
                .arg("Content-Type: application/json")
                .arg("--header")
                .arg("Accept: application/vnd.pagerduty+json;version=2")
                .arg("--header")
                .arg(format!("From: {}", pager_duty_email))
                .arg("--header")
                .arg(format!("Authorization: Token token={}", pager_duty_token))
                .arg("-d")
                .arg(format!(
                    "
                    {{
                      \"incident\": {{
                        \"type\": \"incident\",
                        \"title\": \"Bootstrapping failed!\",
                        \"service\": {{
                          \"id\": \"{}\",
                          \"type\": \"service_reference\"
                        }}
                      }}
                    }}
                ",
                    pager_duty_svcid
                ))
                .arg("https://api.pagerduty.com/incidents")
                .output()
                .expect("Couldn't send a bootstrap error notification");
            println!("Error - I was not able to bootstrap");
            return node.close_and_join();
        }
    }
}
