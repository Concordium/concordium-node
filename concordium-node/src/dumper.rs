#[cfg(feature = "network_dump")]
use crate::common::P2PNodeId;
use crate::common::{
    serialization::{deserializable::Deserializable, read_archive_adapter::ReadArchiveAdapter},
    RemotePeer,
};
#[cfg(feature = "network_dump")]
use crate::configuration::APP_INFO;
#[cfg(feature = "network_dump")]
use app_dirs2::{get_app_root, AppDataType};
use chrono::prelude::{DateTime, Utc};
use concordium_common::hybrid_buf::HybridBuf;
#[cfg(feature = "network_dump")]
use failure::Fallible;
#[cfg(feature = "network_dump")]
use std::io::Write;
use std::net::IpAddr;
#[cfg(feature = "network_dump")]
use std::sync::mpsc::Receiver;

pub struct DumpItem {
    timestamp:   DateTime<Utc>,
    inbound:     bool,
    remote_peer: RemotePeer,
    remote_addr: IpAddr,
    msg:         HybridBuf,
}

impl DumpItem {
    pub fn new(
        timestamp: DateTime<Utc>,
        inbound: bool,
        remote_peer: RemotePeer,
        remote_addr: IpAddr,
        msg: HybridBuf,
    ) -> Self {
        DumpItem {
            timestamp,
            inbound,
            remote_peer,
            remote_addr,
            msg,
        }
    }

    pub fn into_pretty_dump(self) -> String {
        let mut archive = ReadArchiveAdapter::new(self.msg.clone(), self.remote_peer);

        format!(
            "{} - {} - {} - {:?} - {:?}",
            self.timestamp,
            if self.inbound { "IN" } else { "OUT" },
            self.remote_addr,
            if let Ok(cv) = self.msg.into_vec() {
                cv
            } else {
                vec![]
            },
            crate::network::NetworkMessage::deserialize(&mut archive)
        )
    }
}

#[cfg(feature = "network_dump")]
pub fn create_dump_thread(
    ip: IpAddr,
    id: P2PNodeId,
    rx: Receiver<DumpItem>,
    act_rx: Receiver<(std::path::PathBuf, bool)>,
    base_dir: &Option<String>,
) {
    let base_dir = if let Some(dir) = base_dir {
        std::path::PathBuf::from(dir)
    } else {
        get_app_root(AppDataType::UserData, &APP_INFO)
            .map_err(|e| panic!("Filesystem error encountered when creating app_root: {}", e))
            .unwrap()
    };
    spawn_or_die!("Dump thread", move || -> Fallible<()> {
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
                let mut msg = rx.recv()?;
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
                        let _ = rd
                            .write(
                                if let Ok(ref container) = msg.msg.read_all_into_view() {
                                    container.as_slice()
                                } else {
                                    &[]
                                },
                            )
                            .map_err(|e| {
                                error!("Aborting dump due to error: {}", e);
                                e
                            })?;
                        count += 1;
                    }
                };

                // Pretty dump
                if let Some(ref mut pd) = pretty_dump {
                    pd.write_fmt(format_args!("{}\n\n", msg.into_pretty_dump()))
                        .map_err(|e| {
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
