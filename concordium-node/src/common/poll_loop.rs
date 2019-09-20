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
    for request in receivers.network_requests.try_iter() {
        trace!(
            "Processing network raw request ({} bytes) in connection {}",
            request.data.len().unwrap_or(0),
            usize::from(request.token)
        );

        if let Some(ref conn) = p2p_node.find_connection_by_token(request.token) {
            if let Err(err) = conn.async_send_from_poll_loop(request.data, request.priority) {
                error!(
                    "Network raw request error in connection {}/{}: {}",
                    usize::from(request.token),
                    conn.remote_addr(),
                    err
                );
                conn.handler().remove_connection(conn.token);
            }
        } else {
            debug!(
                "Network raw request cannot be sent due to a missing connection {}",
                usize::from(request.token)
            );
            return;
        }
    }
}
