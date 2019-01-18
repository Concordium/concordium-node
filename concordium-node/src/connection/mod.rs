use std::sync::{ Arc, RwLock };
use rustls::{ ServerSession, ClientSession, Session };

use network::{ NetworkRequest };
use connection::parse_handler::{ ParseCallbackWrapper };

type NetworkRequestSafeFn = ParseCallbackWrapper<NetworkRequest>;
type ConnServerSession = Option< Arc< RwLock< ServerSession > > >;
type ConnClientSession = Option< Arc< RwLock< ClientSession > > >;
type ConnSession = Option< Arc< RwLock<dyn Session > > >;

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

macro_rules! update_atomic_stamp {
    ($mode:ident, $stamp:ident) => {
        if $mode != P2PNodeMode::BootstrapperMode && $mode != P2PNodeMode::BootstrapperPrivateMode {
            $stamp.store( get_current_stamp(), Ordering::Relaxed);
        }
    }
}

macro_rules! sessionAs {
    ($mself:ident, $t:ty) => {
        if $mself.initiated_by_me {
            if let Some(ref cli_session) = $mself.tls_client_session {
                Some( Arc::clone(&cli_session) as Arc< RwLock< $t > >)
            } else {
                None
            }
        } else {
            if let Some(ref srv_session) = $mself.tls_server_session {
                Some( Arc::clone(&srv_session) as Arc< RwLock< $t > >)
            } else {
                None
            }
        }
    }
}

pub mod parse_handler;
pub mod packet_handler;
pub mod request_handler;
pub mod message_handler;

mod p2p_node_mode;
mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

mod connection_default_handlers;
pub mod connection;

pub use self::connection::Connection;
pub use self::p2p_node_mode::P2PNodeMode;
pub use self::p2p_event::P2PEvent;
pub use self::seen_messages_list::SeenMessagesList;
