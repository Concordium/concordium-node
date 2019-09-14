use concordium_consensus::transferlog::TransactionLogMessage;

#[allow(unused_variables)]
pub fn log_transfer_event(enabled: bool, host: &str, port: u16, msg: TransactionLogMessage) {
    info!("Send to ES");
}
