#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_common::PacketType;
use crypto_common::Serial;
use failure::{bail, Error};
use p2p_client::{
    common::PeerType,
    network::NetworkId,
    p2p::{
        connectivity::{connect, send_broadcast_message},
        maintenance::{spawn, P2PNode},
    },
    stats_export_service::instantiate_stats_export_engine,
    utils,
};
use std::{env, fs::File, io::prelude::*, sync::Arc, thread, time::Duration};

fn main() -> Result<(), Error> {
    let (mut conf, app_prefs) = utils::get_config_and_logging_setup()?;
    let data_dir_path = app_prefs.get_user_app_dir();

    conf.connection.thread_pool_size = 4;
    conf.connection.dnssec_disabled = true;
    conf.connection.no_bootstrap_dns = true;
    conf.connection.bootstrap_server = "foo:8888".to_string();
    conf.connection.desired_nodes = conf.connection.connect_to.len() as u16;

    let data_file = env::var("DATA_FILE")?;

    let delay_between_batches = 2000;
    let batch_sizes = 40;
    let skip_first = 0;

    let stats_export_service = instantiate_stats_export_engine(&conf)?;

    let (node, poll) = P2PNode::new(
        conf.common.id.clone(),
        &conf,
        PeerType::Node,
        stats_export_service,
        Some(data_dir_path),
    );

    spawn(&node, poll, None);

    conf.connection.connect_to.clone().iter().for_each(
        |host: &String| match utils::parse_host_port(
            &host,
            &node.config.dns_resolvers,
            conf.connection.dnssec_disabled,
        ) {
            Ok(addrs) => {
                for addr in addrs {
                    let _ = connect(&node, PeerType::Node, addr, None).map_err(|e| error!("{}", e));
                }
            }
            Err(err) => error!("Can't parse configured addresses to connect to: {}", err),
        },
    );

    thread::sleep(Duration::from_millis(10000));
    if !(node.connections().read().unwrap()).is_empty() {
        info!("Connected to network");

        if let Ok(mut file) = File::open(&data_file) {
            let mut blocks_len_buffer = [0; 8];
            if let Ok(_) = file.read_exact(&mut blocks_len_buffer) {
                let block_count = u64::from_be_bytes(blocks_len_buffer);
                info!("Going to read {} blocks from file {}", block_count, data_file);
                for i in 0..block_count {
                    let mut block_len_buffer = [0; 8];
                    if let Ok(_) = file.read_exact(&mut block_len_buffer) {
                        let block_size = u64::from_be_bytes(block_len_buffer);
                        info!(
                            "Going to {} read {} bytes for block from file {}",
                            i, block_size, data_file
                        );
                        let mut blocks_data_buffer = vec![0; block_size as usize];
                        if let Ok(_) = file.read_exact(&mut blocks_data_buffer[..]) {
                            if i < skip_first {
                                info!("- skipping as already sent");
                                continue;
                            }
                            let mut data_out = vec![0; 0];
                            (PacketType::Block as u8).serial(&mut data_out);
                            data_out.extend(blocks_data_buffer);
                            info!(
                                "- Sent {} byte(s)",
                                send_broadcast_message(
                                    &node,
                                    vec![],
                                    NetworkId::from(1000),
                                    Arc::from(data_out),
                                )
                            );
                        } else {
                            bail!("Error reading block!");
                        }
                    }
                    if i != 0 && i % batch_sizes == 0 {
                        info!("Will stall for {} ms", delay_between_batches);
                        thread::sleep(Duration::from_millis(delay_between_batches));
                    }
                }
            }
        }
    }

    return node.close_and_join();
}
