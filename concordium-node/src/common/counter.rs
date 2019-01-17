use atomic_counter::{ RelaxedCounter };

lazy_static! {
    pub static ref TOTAL_MESSAGES_RECEIVED_COUNTER: RelaxedCounter = { RelaxedCounter::new(0) };
    pub static ref TOTAL_MESSAGES_SENT_COUNTER: RelaxedCounter = { RelaxedCounter::new(0) };
}


