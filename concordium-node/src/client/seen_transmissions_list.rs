use super::fails;
use concordium_common::UCursor;
use failure::{bail, Fallible};
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug)]
pub struct SeenTransmission {
    pub seen_in_message_id: String,
    pub seen_at:            u64,
    pub payload:            Arc<Mutex<UCursor>>,
}

impl SeenTransmission {
    pub fn new(seen_in_message_id: String, seen_at: u64, payload: UCursor) -> Self {
        SeenTransmission {
            seen_in_message_id,
            seen_at,
            payload: Arc::new(Mutex::new(payload)),
        }
    }

    pub fn read_payload(&mut self) -> Fallible<Vec<u8>> {
        Ok(safe_lock!(self.payload)?.read_all_into_view()?.as_slice()[..].to_vec())
    }
}

impl PartialEq for SeenTransmission {
    fn eq(&self, other: &SeenTransmission) -> bool {
        self.seen_in_message_id == other.seen_in_message_id
    }
}

#[derive(Debug, Clone)]
pub struct SeenTransmissionsList {
    seen_transmissions:         Arc<RwLock<Vec<SeenTransmission>>>,
    max_elements:               u64,
    max_size_bytes_per_element: u64,
}

impl SeenTransmissionsList {
    pub fn new(max_elements: u64, max_size_bytes_per_element: u64) -> Self {
        SeenTransmissionsList {
            seen_transmissions: Arc::new(RwLock::new(Vec::with_capacity(max_elements as usize))),
            max_elements,
            max_size_bytes_per_element,
        }
    }

    fn contains_seen_message_id(list: &[SeenTransmission], message_id: &str) -> bool {
        list.iter().any(|msg| msg.seen_in_message_id == message_id)
    }

    fn append(&self, list: &mut Vec<SeenTransmission>, seen_transmission: SeenTransmission) {
        if !list.contains(&seen_transmission) {
            if self.max_elements != 0 && list.len() >= self.max_elements as usize {
                list.remove(0);
                list.push(seen_transmission);
            } else {
                list.push(seen_transmission);
            }
        }
    }

    pub fn add_transmission(
        &self,
        seen_in_message_id: String,
        seen_at: u64,
        payload: &[u8],
    ) -> Fallible<()> {
        let mut inner_list = safe_write!(self.seen_transmissions)?;
        if !Self::contains_seen_message_id(&inner_list, &seen_in_message_id) {
            let mut cursor = UCursor::from(payload.to_owned());
            if cursor.len() > self.max_size_bytes_per_element {
                cursor.swap_to_file()?;
            }
            self.append(
                &mut inner_list,
                SeenTransmission::new(seen_in_message_id, seen_at, cursor),
            );
            Ok(())
        } else {
            bail!(fails::DuplicateSeenTransmissionElementAttempted::new(
                seen_in_message_id
            ))
        }
    }

    pub fn get_transmissions_since(
        &self,
        since_timestamp: u64,
    ) -> Fallible<Vec<(String, Vec<u8>)>> {
        if since_timestamp == 0 {
            Ok(safe_write!(self.seen_transmissions)?
                .iter_mut()
                .filter_map(|element| {
                    if element.seen_at > since_timestamp {
                        element
                            .read_payload()
                            .map(|bytes| (element.seen_in_message_id.to_owned(), bytes))
                            .ok()
                    } else {
                        None
                    }
                })
                .collect())
        } else {
            Ok(safe_write!(self.seen_transmissions)?
                .iter_mut()
                .filter_map(|element| {
                    if element.seen_at > since_timestamp {
                        element
                            .read_payload()
                            .map(|bytes| (element.seen_in_message_id.to_owned(), bytes))
                            .ok()
                    } else {
                        None
                    }
                })
                .collect())
        }
    }
}
