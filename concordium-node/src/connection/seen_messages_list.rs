use chrono::{DateTime, Utc};
use hash_hasher::{HashBuildHasher, HashedSet};

use crate::network::packet::MessageId;

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::{
        atomic::{AtomicI64, Ordering as AtomicOrdering},
        Arc, RwLock,
    },
};

#[derive(Debug, Clone)]
pub struct SeenMessage {
    pub id:        MessageId,
    pub timestamp: DateTime<Utc>,
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
    seen_msgs:        Arc<RwLock<HashedSet<SeenMessage>>>,
    oldest_timestamp: Arc<AtomicI64>,
}

impl SeenMessagesList {
    pub fn new(message_ids_retained: usize) -> Self {
        SeenMessagesList {
            seen_msgs:        Arc::new(RwLock::new(HashedSet::with_capacity_and_hasher(
                message_ids_retained,
                HashBuildHasher::default(),
            ))),
            oldest_timestamp: Arc::new(AtomicI64::new(0)),
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
            let current_stamp = msg.timestamp.timestamp();

            if list.len() == list.capacity() {
                let remove_older_than =
                    (current_stamp + self.oldest_timestamp.load(AtomicOrdering::SeqCst)) / 2;
                list.retain(|element| element.timestamp.timestamp() > remove_older_than);
                self.oldest_timestamp
                    .store(remove_older_than, AtomicOrdering::SeqCst);
            }

            let replace_op = list.replace(msg);

            if replace_op.is_none() && self.oldest_timestamp.load(AtomicOrdering::SeqCst) == 0 {
                self.oldest_timestamp
                    .store(current_stamp, AtomicOrdering::SeqCst);
            }

            replace_op.is_none()
        } else {
            false
        }
    }
}
