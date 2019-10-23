use crate::connection::low_level::create_frame;

use concordium_common::hybrid_buf::HybridBuf;

use std::collections::VecDeque;

/// It implements a Handshake stream/sink for Noise *IKpks2*.
/// It should be shared by the encrypt sink and the decrypt stream.
pub struct HandshakeStreamSink {
    // Sink
    pub send_queue: VecDeque<HybridBuf>,
}

impl HandshakeStreamSink {
    pub fn new(is_initiator: bool) -> Self {
        let mut send_queue = VecDeque::new();

        if is_initiator {
            // Start with an empty payload.
            send_queue.push_back(create_frame(&[]).unwrap());
        }

        HandshakeStreamSink { send_queue }
    }
}
