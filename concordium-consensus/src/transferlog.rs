use concordium_common::{
    into_err, RelayOrStopReceiver, RelayOrStopSenderHelper, RelayOrStopSyncSender,
};
use failure::Fallible;

use std::sync::{mpsc, Mutex};

const TRANSACTION_LOG_QUEUE_DEPTH: usize = 4096;

pub struct TransactionLogQueue {
    pub receiver: Mutex<RelayOrStopReceiver<TransactionLogMessage>>,
    pub sender:   RelayOrStopSyncSender<TransactionLogMessage>,
}

impl Default for TransactionLogQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::sync_channel(TRANSACTION_LOG_QUEUE_DEPTH);
        Self {
            receiver: Mutex::new(receiver),
            sender,
        }
    }
}

impl TransactionLogQueue {
    pub fn send_message(&self, message: TransactionLogMessage) -> Fallible<()> {
        into_err!(self.sender.send_msg(message))
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(self.sender.send_stop())?;
        Ok(())
    }
}

lazy_static! {
    pub static ref TRANSACTION_LOG_QUEUE: TransactionLogQueue = { TransactionLogQueue::default() };
}

#[derive(Debug)]
pub enum TransactionLogMessage {
    Unknown(u8),
}
