use std::{
    collections::VecDeque,
    sync::{Arc, RwLock},
};

#[derive(Default, Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs: Arc<RwLock<VecDeque<String>>>,
}

const MAX_MESSAGE_SEEN_LIST: u16 = 20000;

impl SeenMessagesList {
    pub fn new() -> Self {
        SeenMessagesList {
            seen_msgs: Arc::new(RwLock::new(VecDeque::new())),
        }
    }

    pub fn contains(&self, msgid: &str) -> bool {
        if let Ok(ref mut list) = safe_read!(self.seen_msgs) {
            return list.contains(&msgid.to_owned());
        }
        false
    }

    pub fn append(&self, msgid: &str) -> bool {
        if let Ok(ref mut list) = safe_write!(self.seen_msgs) {
            if !list.contains(&msgid.to_owned()) {
                list.push_back(msgid.to_owned());
                if list.len() >= MAX_MESSAGE_SEEN_LIST as usize {
                    list.pop_front();
                }
            }
            true
        } else {
            false
        }
    }
}
