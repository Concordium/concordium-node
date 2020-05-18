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
use std::{fs::File, io::prelude::*, sync::Arc, thread, time::Duration};

fn main() -> Result<(), Error> {
    let (mut conf, app_prefs) = utils::get_config_and_logging_setup()?;
    let data_dir_path = app_prefs.get_user_app_dir();

    conf.connection.thread_pool_size = 4;
    conf.connection.dnssec_disabled = true;
    conf.connection.no_bootstrap_dns = true;
    conf.connection.bootstrap_server = "foo:8888".to_string();
    conf.connection.desired_nodes = conf.connection.connect_to.len() as u16;

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

        if let Ok(mut file) = File::open(&conf.database_emitter.import_file) {
            let mut blocks_len_buffer = [0; 8];
            if file.read_exact(&mut blocks_len_buffer).is_ok() {
                let block_count = u64::from_be_bytes(blocks_len_buffer);
                info!(
                    "Going to read {} blocks from file {}",
                    block_count, &conf.database_emitter.import_file
                );
                for i in 0..block_count {
                    let mut block_len_buffer = [0; 8];
                    if file.read_exact(&mut block_len_buffer).is_ok() {
                        let block_size = u64::from_be_bytes(block_len_buffer);
                        info!(
                            "Going to {} read {} bytes for block from file {}",
                            i, block_size, &conf.database_emitter.import_file
                        );
                        let mut blocks_data_buffer = vec![0; block_size as usize];
                        if file.read_exact(&mut blocks_data_buffer[..]).is_ok() {
                            if i < conf.database_emitter.skip_first {
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
                                    NetworkId::from(conf.common.network_ids.clone()[1]),
                                    Arc::from(data_out),
                                )
                            );
                        } else {
                            bail!("Error reading block!");
                        }
                    }
                    if i != 0 && i % conf.database_emitter.batch_sizes == 0 {
                        info!("Will stall for {} ms", &conf.database_emitter.delay_between_batches);
                        thread::sleep(Duration::from_millis(
                            conf.database_emitter.delay_between_batches,
                        ));
                    }
                }
            }
        }
    }

    node.close_and_join()
}
