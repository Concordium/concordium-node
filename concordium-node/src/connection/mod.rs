pub mod network_handler;

mod p2p_node_mode;
mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

mod connection_default_handlers;
mod connection_private;
pub mod connection;

use rustls::Session;

/// It is a common trait for `rustls::ClientSession` and `rustls::ServerSession`
pub trait CommonSession : Session + std::io::Write + std::io::Read
{}

impl<T> CommonSession for T
    where T: Session + std::io::Write + std::io::Read
{}

pub use self::connection::Connection;
pub use self::p2p_node_mode::P2PNodeMode;
pub use self::p2p_event::P2PEvent;
pub use self::seen_messages_list::SeenMessagesList;
pub use self::network_handler::{
    RequestHandler, PacketHandler, ResponseHandler, MessageHandler,
    NetworkRequestCW, NetworkResponseCW, NetworkPacketCW, EmptyCW,
    MessageManager };

