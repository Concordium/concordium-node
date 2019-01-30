/// Utility macro to implement all `Fn*` functions for specific `type`.
macro_rules! impl_all_fns {
    ($type:ident, $msg:ident) => {

        impl FnOnce<(&$msg,)> for $type {
            type Output = FunctorResult;

            extern "rust-call" fn call_once(self, args: (&$msg,)) -> FunctorResult
            {
                self.process_message( args.0)
            }
        }

        impl FnMut<(&$msg,)> for $type{
            extern "rust-call" fn call_mut(&mut self, args: (&$msg,)) -> FunctorResult
            {
                self.process_message( args.0)
            }
        }

        impl Fn<(&$msg,)> for $type{
            extern "rust-call" fn call(&self, args: (&$msg,)) -> FunctorResult
            {
                self.process_message( args.0)
            }
        }
    }
}

pub mod message_handler;
pub mod packet_handler;
pub mod request_handler;
pub mod response_handler;

pub use self::message_handler::{ MessageHandler, MessageManager,
    NetworkPacketCW, NetworkRequestCW, NetworkResponseCW, EmptyCW };
pub use self::packet_handler::{ PacketHandler };
pub use self::request_handler::{ RequestHandler };
pub use self::response_handler::{ ResponseHandler };

