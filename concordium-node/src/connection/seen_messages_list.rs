use chrono::{DateTime, Utc};

use crate::network::packet::MessageId;

use std::{
    cmp::Ordering,
    collections::HashSet,
    hash::{Hash, Hasher},
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone)]
pub struct SeenMessage {
    id:        MessageId,
    timestamp: DateTime<Utc>,
}

impl SeenMessage {
    pub fn new(id: MessageId) -> Self {
        Self {
            id,
            timestamp: Utc::now(),
        }
    }
}

impl PartialEq for SeenMessage {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl Eq for SeenMessage {}

impl Hash for SeenMessage {
    fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl PartialOrd for SeenMessage {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.timestamp.cmp(&other.timestamp))
    }
}

impl Ord for SeenMessage {
    fn cmp(&self, other: &Self) -> Ordering {
        self.timestamp.partial_cmp(&other.timestamp).unwrap() // infallible
    }
}

#[derive(Default, Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs:            Arc<RwLock<HashSet<SeenMessage>>>,
    message_ids_retained: usize,
}

impl SeenMessagesList {
    pub fn new(message_ids_retained: usize) -> Self {
        SeenMessagesList {
            seen_msgs: Arc::new(RwLock::new(HashSet::with_capacity(
                message_ids_retained / 2,
            ))),
            message_ids_retained,
        }
    }

    pub fn contains(&self, msgid: &MessageId) -> bool {
        if let Ok(ref mut list) = safe_read!(self.seen_msgs) {
            return list.contains(&SeenMessage::new(msgid.to_owned()));
        }
        false
    }

    pub fn append(&self, msgid: &MessageId) -> bool {
        if let Ok(mut list) = safe_write!(self.seen_msgs) {
            let msg = SeenMessage::new(msgid.to_owned());

            if list.replace(msg).is_none() && list.len() == self.message_ids_retained {
                let oldest = list.iter().min().cloned().unwrap(); // safe (non-empty)
                list.remove(&oldest);
            }
            true
        } else {
            false
        }
    }
}
