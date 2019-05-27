use crate::network::packet::MessageId;

use std::{
    collections::VecDeque,
    sync::{Arc, RwLock},
};

#[derive(Default, Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs:            Arc<RwLock<VecDeque<MessageId>>>,
    message_ids_retained: usize,
}

impl SeenMessagesList {
    pub fn new(message_ids_retained: usize) -> Self {
        SeenMessagesList {
            seen_msgs: Arc::new(RwLock::new(VecDeque::with_capacity(
                message_ids_retained / 10,
            ))),
            message_ids_retained,
        }
    }

    pub fn contains(&self, msgid: &MessageId) -> bool {
        if let Ok(ref mut list) = safe_read!(self.seen_msgs) {
            return list.contains(msgid);
        }
        false
    }

    pub fn append(&self, msgid: &MessageId) -> bool {
        if let Ok(ref mut list) = safe_write!(self.seen_msgs) {
            if !list.contains(msgid) {
                if list.len() >= self.message_ids_retained {
                    list.pop_front();
                }
                list.push_back(msgid.to_owned());
            }
            true
        } else {
            false
        }
    }
}
