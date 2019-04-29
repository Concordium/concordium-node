use failure::Fail;

#[derive(Debug, Fail)]
#[fail(display = "Can't add already cached message id <{}>", message_id)]
pub struct DuplicateSeenTransmissionElementAttempted {
    message_id: String,
}

impl DuplicateSeenTransmissionElementAttempted {
    pub fn new(message_id: String) -> DuplicateSeenTransmissionElementAttempted {
        DuplicateSeenTransmissionElementAttempted { message_id }
    }
}
