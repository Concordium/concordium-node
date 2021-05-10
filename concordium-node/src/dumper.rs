//! Handles the `network_dump` feature.

cfg_if! {
    if #[cfg(feature = "network_dump")] {
        use crate::common::P2PNodeId;
        use crossbeam_channel::{self, Receiver};
        use std::io::Write;
    }
}
use crate::{network::NetworkMessage, spawn_or_die};
use anyhow::bail;
use chrono::prelude::{DateTime, Utc};

use std::{fmt, net::IpAddr, sync::Arc};

/// A structure containing network data to be dumped to the disk.
pub struct DumpItem {
    timestamp:   DateTime<Utc>,
    inbound:     bool,
    remote_addr: IpAddr,
    msg:         Arc<[u8]>,
}

impl DumpItem {
    /// Creates a new dump item object.
    pub fn new(inbound: bool, remote_addr: IpAddr, msg: Arc<[u8]>) -> Self {
        DumpItem {
            timestamp: Utc::now(),
            inbound,
            remote_addr,
            msg,
        }
    }
}

impl fmt::Display for DumpItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} - {} - {} - {:?} - {:?}",
            self.timestamp,
            if self.inbound {
                "IN"
            } else {
                "OUT"
            },
            self.remote_addr,
            self.msg,
            NetworkMessage::deserialize(&self.msg)
                .map(|m| format!("{:?}", m))
                .unwrap_or_else(|_| "couldn't deserialize".to_string())
        )
    }
}

/// Creates the thread responsible for intercepting and dumping network data.
#[cfg(feature = "network_dump")]
pub fn create_dump_thread(
    ip: IpAddr,
    id: P2PNodeId,
    rx: Receiver<DumpItem>,
    act_rx: Receiver<(std::path::PathBuf, bool)>,
    base_dir: std::path::PathBuf,
) {
    spawn_or_die!("network dump", move || -> anyhow::Result<()> {
        let mut dir: Option<std::path::PathBuf> = None;
        let mut pretty_dump: Option<std::fs::File> = None;
        let mut raw_dump: Option<std::fs::File> = None;
        let mut count = 0;
        loop {
            if let Ok((new_path, raw)) = act_rx.try_recv() {
                if new_path.components().next().is_none() {
                    info!("Dump process stopped");
                    break;
                }
                let new_path = base_dir.join(&new_path);
                // Create directory
                let _ = std::fs::create_dir(&new_path.clone());

                // Create and start pretty dump file
                let mut pretty_dump_file = std::fs::File::create(
                    base_dir
                        .join(new_path.join(std::path::Path::new(&format!("{}-pretty.log", id)))),
                )
                .map_err(|e| {
                    error!("Aborting dump due to error: {}", e);
                    e
                })?;
                pretty_dump_file
                    .write_fmt(format_args!(
                        "Dumping started at: {}\nLocal IP is: {}\nLocal ID is: {}\n\n",
                        Utc::now(),
                        ip,
                        id
                    ))
                    .map_err(|e| {
                        error!("Aborting dump due to error: {}", e);
                        e
                    })?;
                pretty_dump.replace(pretty_dump_file);

                // Activate raw dump
                if raw {
                    count = 1;
                };
                info!("Starting dump in: {:?}", &new_path);
                dir = Some(new_path);
            };
            if let Some(ref dir) = dir {
                let msg = rx.recv()?;
                // Raw dump
                if count > 0 {
                    // Create file
                    let file = std::fs::File::create(
                        dir.join(std::path::Path::new(&format!("{}-{}", id, count))),
                    )
                    .map_err(|e| {
                        error!("Aborting dump due to error: {}", e);
                        e
                    })?;
                    raw_dump.replace(file);
                    // Write message
                    if let Some(ref mut rd) = raw_dump {
                        let _ = rd.write(&msg.msg).map_err(|e| {
                            error!("Aborting dump due to error: {}", e);
                            e
                        })?;
                        count += 1;
                    }
                };

                // Pretty dump
                if let Some(ref mut pd) = pretty_dump {
                    pd.write_fmt(format_args!("{}\n\n", msg)).map_err(|e| {
                        error!("Aborting dump due to error: {}", e);
                        e
                    })?;
                };
            };
            std::thread::yield_now();
        }
        Ok(())
    });
}
