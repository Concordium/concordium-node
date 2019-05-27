use crate::network::packet::MessageId;

use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    hash::{Hash, Hasher},
    mem,
    sync::{Arc, RwLock},
    time::Instant,
};

#[derive(Debug)]
pub struct SeenMessage {
    id: MessageId,
    timestamp: Instant,
}

impl SeenMessage {
    pub fn new(id: MessageId) -> Self {
        Self {
            id,
            timestamp: Instant::now(),
        }
    }
}

impl PartialEq for SeenMessage {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for SeenMessage {}

impl Hash for SeenMessage {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
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
                message_ids_retained / 10,
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

            if list.insert(msg) {
                if list.len() == self.message_ids_retained {
                    // remove the oldest 50% of the messages
                    let old_list = mem::replace(&mut *list, HashSet::new());
                    let mut new_list = old_list.into_iter().collect::<BinaryHeap<_>>().into_sorted_vec();
                    new_list.truncate(self.message_ids_retained / 2);
                    *list = new_list.into_iter().collect::<HashSet<_>>();
                }
            }
            true
        } else {
            false
        }
    }
}
