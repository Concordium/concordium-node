use crate::{
    connection::{
        network_handler::{message_handler::EmptyFunction, MessageHandler},
        Connection,
    },
    network::message::NetworkMessage,
};
use concordium_common::{
    functor::{UnitFunction, UnitFunctor},
    hybrid_buf::HybridBuf,
};
use failure::Fallible;
use std::sync::RwLock;

/// Models the result of processing a message through the `MessageProcessor`
#[derive(Debug)]
pub enum ProcessResult {
    Drop,
    Done,
}

pub fn collapse_process_result(
    conn: &Connection,
    data: Vec<HybridBuf>,
) -> Result<ProcessResult, Vec<Result<ProcessResult, failure::Error>>> {
    let mut found_drop = false;
    let mut errors = vec![];

    for elem in data {
        let res = conn.process_message(elem);
        if res.is_err() {
            errors.push(res);
        } else if let Ok(ProcessResult::Drop) = res {
            found_drop = true;
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    if found_drop {
        Ok(ProcessResult::Drop)
    } else {
        Ok(ProcessResult::Done)
    }
}

/// Handles a `NetworkMessage`
///
/// First, the message is passed to filters sorted in descending priority. Only
/// of all the filters are passed, then the execution part begins. All the
/// actions are executed (the inner `MessageHandler` dispatchs to several
/// functors depending on the variant of the `NetworkMessage`) and in the end
/// the `notifications` receive the message in case they want to react to the
/// processing of the message finishing.
#[derive(Clone)]
pub struct MessageProcessor {
    actions:       MessageHandler,
    notifications: UnitFunctor<NetworkMessage>,
}

impl Default for MessageProcessor {
    fn default() -> Self { MessageProcessor::new() }
}

impl MessageProcessor {
    pub fn new() -> Self {
        MessageProcessor {
            actions:       MessageHandler::new(),
            notifications: UnitFunctor::new(),
        }
    }

    pub fn add_action(&self, callback: UnitFunction<NetworkMessage>) -> &Self {
        self.actions.add_callback(callback);
        self
    }

    pub fn set_invalid_handler(&self, func: EmptyFunction) -> &Self {
        self.actions.set_invalid_handler(func);
        self
    }

    pub fn actions(&self) -> &MessageHandler { &self.actions }

    pub fn add_notification(&self, func: UnitFunction<NetworkMessage>) -> &Self {
        self.notifications.add_callback(func);
        self
    }

    pub fn notifications(&self) -> &RwLock<Vec<UnitFunction<NetworkMessage>>> {
        &self.notifications.callbacks()
    }

    pub fn process_message(&self, message: &NetworkMessage) -> Fallible<ProcessResult> {
        self.actions.process_message(message)?;
        self.notifications.run_callbacks(message)?;
        Ok(ProcessResult::Done)
    }

    pub fn add(&self, other: &MessageProcessor) -> &Self {
        self.actions.add(&other.actions);

        for cb in read_or_die!(other.notifications()).iter() {
            self.add_notification(cb.clone());
        }

        self
    }
}

pub trait MessageManager {
    fn message_processor(&self) -> MessageProcessor;
}
