use std::sync::{ Arc, Mutex };

#[derive(Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs: Arc<Mutex<Vec<String>>>,
}

impl SeenMessagesList {
    pub fn new() -> Self {
        SeenMessagesList { seen_msgs: Arc::new(Mutex::new(Vec::new())), }
    }

    pub fn contains(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = self.seen_msgs.lock() {
            return list.contains(msgid);
        }
        false
    }

    pub fn append(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = self.seen_msgs.lock() {
            if !list.contains(msgid) {
                if list.len() >= 1000 {
                    list.remove(0);
                    list.push(msgid.clone().to_owned());
                } else {
                    list.push(msgid.clone().to_owned());
                }
            }
            true
        } else {
            false
        }
    }
}


