use network::{ NetworkRequest, NetworkResponse, NetworkPacket };
pub use self::parse_handler::{ ParseCallbackResult, ParseCallback, ParseCallbackWrapper, ParseHandler };

pub type NetworkRequestSafeFn = ParseCallbackWrapper<NetworkRequest>;
pub type NetworkResponseSafeFn = ParseCallbackWrapper<NetworkResponse>;
pub type NetworkPacketSafeFn = ParseCallbackWrapper<NetworkPacket>;

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_callback {
    ($callback:expr) => {
        Arc::new ( Mutex::new ( Box::new( $callback )))
    }
}

macro_rules! impl_all_fns {
    ($type:ident, $msg:ident) => {

        impl FnOnce<(&$msg,)> for $type {
            type Output = ParseCallbackResult;

            extern "rust-call" fn call_once(self, args: (&$msg,)) -> ParseCallbackResult
            {
                self.process_message( args.0)
            }
        }

        impl FnMut<(&$msg,)> for $type{
            extern "rust-call" fn call_mut(&mut self, args: (&$msg,)) -> ParseCallbackResult
            {
                self.process_message( args.0)
            }
        }

        impl Fn<(&$msg,)> for $type{
            extern "rust-call" fn call(&self, args: (&$msg,)) -> ParseCallbackResult
            {
                self.process_message( args.0)
            }
        }
    }
}

pub mod parse_handler;
pub mod packet_handler;
pub mod request_handler;
pub mod response_handler;
pub mod message_handler;

mod p2p_node_mode;
mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

mod connection_default_handlers;
mod connection_private;
pub mod connection;

pub use self::connection::Connection;
pub use self::p2p_node_mode::P2PNodeMode;
pub use self::p2p_event::P2PEvent;
pub use self::seen_messages_list::SeenMessagesList;
pub use self::message_handler::{ MessageHandler, MessageManager };
pub use self::request_handler::{ RequestHandler };
pub use self::response_handler::{ ResponseHandler };
pub use self::packet_handler::{ PacketHandler };

