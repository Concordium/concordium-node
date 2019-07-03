use crate::p2p::tls_server::TlsServer;

use concordium_common::UCursor;

use mio::Token;
use std::sync::{mpsc::Receiver, Arc, RwLock};

/// This data type is used to queue a request from any thread (like tests, RPC,
/// Cli, etc.), into a node. Please note that any access to internal `socket`
/// *must be executed* inside MIO poll-loop thread.
pub struct NetworkRawRequest {
    pub token: Token, // It identifies the connection.
    pub data:  UCursor,
}

/// It extracts and sends each queued request.
///
/// # Mio poll-loop thread
///
/// This function *HAS TO BE called* from inside that MIO poll-loop thread
/// because `connection` object (and its `socket` descriptor) is designed to be
/// accessed from that single thread. Read process is executed inside MIO
/// poll-loop thread, and any write is queued to be processed later in that
/// poll-loop.
pub fn process_network_requests(
    tls_server: &Arc<RwLock<TlsServer>>,
    network_request_receiver: &mut Receiver<NetworkRawRequest>,
) {
    network_request_receiver
        .try_iter()
        .for_each(|network_request| {
            trace!(
                "Processing network raw request ({} bytes) in connection {}",
                network_request.data.len(),
                usize::from(network_request.token)
            );

            let tls_server_locked = read_or_die!(tls_server);
            let rc_conn_opt = tls_server_locked.find_connection_by_token(network_request.token);
            match rc_conn_opt {
                Some(ref rc_conn) => {
                    if let Err(err) =
                        write_or_die!(rc_conn).async_send_from_poll_loop(network_request.data)
                    {
                        error!(
                            "Network raw request error on connection {}: {}",
                            usize::from(network_request.token),
                            err
                        );
                    }
                }
                None => error!(
                    "Network raw request cannot be sent due to a missing connection {}",
                    usize::from(network_request.token)
                ),
            }
        });
}
