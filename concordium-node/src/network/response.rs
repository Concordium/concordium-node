use crate::common::P2PPeer;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong,
    PeerList(Vec<P2PPeer>),
}
