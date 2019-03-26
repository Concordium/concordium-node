use std::sync::{ Arc, RwLock };

#[derive(Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs: Arc<RwLock<Vec<String>>>,
}

impl SeenMessagesList {
    pub fn new() -> Self {
        SeenMessagesList { seen_msgs: Arc::new(RwLock::new(Vec::new())), }
    }

    pub fn contains(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = safe_read!(self.seen_msgs) {
            return list.contains(msgid);
        }
        false
    }

    pub fn append(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = safe_write!(self.seen_msgs) {
            if !list.contains(msgid) {
                if list.len() >= 1000 {
                    list.remove(0);
                    list.push(msgid.to_owned());
                } else {
                    list.push(msgid.to_owned());
                }
            }
            true
        } else {
            false
        }
    }
}


