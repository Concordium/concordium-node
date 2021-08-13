#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

use anyhow::{bail, Context};
use concordium_node::{
    common::PeerType,
    consensus_ffi::helpers::PacketType,
    network::NetworkId,
    p2p::{
        connectivity::{connect, send_broadcast_message},
        maintenance::{spawn, P2PNode},
    },
    stats_export_service::instantiate_stats_export_engine,
    utils,
};
use crypto_common::Serial;
use std::{
    fs::File,
    io::prelude::*,
    sync::{Arc, RwLock},
    thread,
    time::Duration,
};

fn main() -> anyhow::Result<()> {
    let (mut conf, _app_prefs) = utils::get_config_and_logging_setup()?;

    conf.connection.require_dnssec = false;
    conf.connection.no_bootstrap_dns = true;
    conf.connection.desired_nodes = conf.connection.connect_to.len() as u16;

    let stats_export_service = instantiate_stats_export_engine(&conf)?;

    let (node, poll) = P2PNode::new(
        conf.common.id,
        &conf,
        PeerType::Node,
        stats_export_service,
        Arc::new(RwLock::new(vec![])),
    )
    .context("Failed to create the node")?;

    spawn(&node, poll, None);

    conf.connection.connect_to.iter().for_each(|host: &String| {
        match utils::parse_host_port(
            &host,
            &node.config.dns_resolvers,
            conf.connection.require_dnssec,
        ) {
            Ok(addrs) => {
                for addr in addrs {
                    let _ = connect(&node, PeerType::Node, addr, None, false)
                        .map_err(|e| error!("{}", e));
                }
            }
            Err(err) => error!("Can't parse configured addresses to connect to: {}", err),
        }
    });

    info!("Sleeping to let network connections settle");
    thread::sleep(Duration::from_millis(10000));
    if !(node.connections().read().unwrap()).is_empty() {
        info!("Connected to network");

        if let Ok(mut file) = File::open(&conf.database_emitter.import_file) {
            let mut counter = 0;
            loop {
                let mut block_len_buffer = [0; 8];
                if let Ok(read_bytes) = file.read(&mut block_len_buffer) {
                    if read_bytes != block_len_buffer.len() {
                        if read_bytes == 0 {
                            info!("No more blocks to be read from file");
                        } else {
                            error!("No enough bytes to read");
                        }
                        break;
                    }
                    let block_size = u64::from_be_bytes(block_len_buffer);
                    info!(
                        "Block#{} - will read {} bytes for block from file {}",
                        counter, block_size, &conf.database_emitter.import_file
                    );
                    let mut blocks_data_buffer = vec![0; block_size as usize];
                    if let Ok(blocks_bytes_read) = file.read(&mut blocks_data_buffer[..]) {
                        if blocks_bytes_read != block_size as usize {
                            error!(
                                "The file didn't contain all the {} byte(s) needed to properly \
                                 read the block!",
                                block_size
                            );
                            break;
                        }
                        if counter < conf.database_emitter.skip_first {
                            info!("- skipping as per request");
                            counter += 1;
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
                                NetworkId::from(conf.common.network_ids.clone()[0]),
                                Arc::from(data_out),
                            )
                        );
                    } else {
                        bail!("Error reading block!");
                    }
                } else {
                    bail!("Can't read size of next block from file!");
                }
                if counter != 0 && counter % conf.database_emitter.batch_sizes == 0 {
                    info!("Will stall for {} ms", &conf.database_emitter.delay_between_batches);
                    thread::sleep(Duration::from_millis(
                        conf.database_emitter.delay_between_batches,
                    ));
                }
                counter += 1;
            }
        }
    }

    node.close_and_join()
}
