pub mod network_handler;

mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

pub mod connection;
mod connection_default_handlers;
mod connection_handshake_handlers;
mod connection_private;
pub mod fails;
mod handler_utils;

use rustls::Session;

/// It is a common trait for `rustls::ClientSession` and `rustls::ServerSession`
pub trait CommonSession: Session + std::io::Write + std::io::Read {}

impl<T> CommonSession for T where T: Session + std::io::Write + std::io::Read {}

pub use self::{
    connection::Connection,
    network_handler::{
        EmptyCW, MessageHandler, MessageManager, NetworkPacketCW, NetworkRequestCW,
        NetworkResponseCW, PacketHandler, RequestHandler, ResponseHandler,
    },
    p2p_event::P2PEvent,
    seen_messages_list::SeenMessagesList,
};
