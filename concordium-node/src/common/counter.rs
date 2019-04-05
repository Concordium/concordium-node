use std::sync::atomic::AtomicUsize;

lazy_static! {
    pub static ref TOTAL_MESSAGES_RECEIVED_COUNTER: AtomicUsize = { AtomicUsize::new(0) };
    pub static ref TOTAL_MESSAGES_SENT_COUNTER: AtomicUsize = { AtomicUsize::new(0) };
}
