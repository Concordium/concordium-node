use super::seen_transmissions_list::SeenTransmissionsList;
use failure::Fallible;

pub enum SeenTransmissionType {
    Block,
    Finalization,
    FinalizationRecord,
}

lazy_static! {
    static ref SEEN_TRANSMISSIONS_LIST_BLOCKS: SeenTransmissionsList =
        { SeenTransmissionsList::new(0, 5_000u64) };
    static ref SEEN_TRANSMISSIONS_LIST_FINALIZATIONS: SeenTransmissionsList =
        { SeenTransmissionsList::new(1000, 5_000u64) };
    static ref SEEN_TRANSMISSIONS_LIST_FINALIZATIONRECORDS: SeenTransmissionsList =
        { SeenTransmissionsList::new(0, 5_000u64) };
}

pub fn add_transmission_to_seenlist(
    transmission_type: SeenTransmissionType,
    seen_in_message_id: String,
    seen_at: u64,
    payload: &[u8],
) -> Fallible<()> {
    match transmission_type {
        SeenTransmissionType::Block => {
            SEEN_TRANSMISSIONS_LIST_BLOCKS.add_transmission(seen_in_message_id, seen_at, payload)
        }
        SeenTransmissionType::Finalization => SEEN_TRANSMISSIONS_LIST_FINALIZATIONS
            .add_transmission(seen_in_message_id, seen_at, payload),
        SeenTransmissionType::FinalizationRecord => SEEN_TRANSMISSIONS_LIST_FINALIZATIONRECORDS
            .add_transmission(seen_in_message_id, seen_at, payload),
    }
}

pub fn get_transmissions_since_from_seenlist(
    transmission_type: SeenTransmissionType,
    since_stamp: u64,
) -> Fallible<Vec<Vec<u8>>> {
    match transmission_type {
        SeenTransmissionType::Block => {
            SEEN_TRANSMISSIONS_LIST_BLOCKS.get_transmissions_since(since_stamp)
        }
        SeenTransmissionType::Finalization => {
            SEEN_TRANSMISSIONS_LIST_FINALIZATIONS.get_transmissions_since(since_stamp)
        }
        SeenTransmissionType::FinalizationRecord => {
            SEEN_TRANSMISSIONS_LIST_FINALIZATIONRECORDS.get_transmissions_since(since_stamp)
        }
    }
}
