use crate::{
    connection::MessageSendingPriority,
    p2p::{P2PNode, Receivers},
};

use concordium_common::hybrid_buf::HybridBuf;

use mio::Token;

/// This data type is used to queue a request from any thread (like tests, RPC,
/// Cli, etc.), into a node. Please note that any access to internal `socket`
/// *must be executed* inside MIO poll-loop thread.
pub struct NetworkRawRequest {
    pub token:    Token, // It identifies the connection.
    pub data:     HybridBuf,
    pub priority: MessageSendingPriority,
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
pub fn process_network_requests(p2p_node: &P2PNode, receivers: &Receivers) {
    receivers
        .network_requests
        .try_iter()
        .for_each(|network_request| {
            trace!(
                "Processing network raw request ({} bytes) in connection {}",
                network_request.data.len().unwrap_or(0),
                usize::from(network_request.token)
            );

            let conn_opt = p2p_node.find_connection_by_token(network_request.token);
            match conn_opt {
                Some(ref conn) => {
                    if !conn.is_closed() {
                        if let Err(err) = conn.async_send_from_poll_loop(
                            network_request.data,
                            network_request.priority,
                        ) {
                            conn.close();
                            debug!(
                                "Network raw request error in connection {}: {}; the connection \
                                 will be closed.",
                                usize::from(network_request.token),
                                err
                            );
                        }
                    } else {
                        trace!("Attempted to write to an already closed connection");
                    }
                }
                None => debug!(
                    "Network raw request cannot be sent due to a missing connection {}",
                    usize::from(network_request.token)
                ),
            }
        });
}
