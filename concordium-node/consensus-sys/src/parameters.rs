// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Parameters.hs

use std::collections::HashMap;

use crate::common::*;

pub type BakerId = u64;

pub type LeadershipElectionNonce = Box<[u8]>;
pub type BakerSignVerifyKey = VerifyKey;
pub type BakerSignPrivateKey = sig::Keypair;
pub type BakerElectionVerifyKey = vrf::PublicKey;
pub type BakerElectionPrivateKey = vrf::Keypair;
pub type LotteryPower = f64;
pub type ElectionDifficulty = f64;

pub type VoterId = u64;
pub type VoterVerificationKey = VerifyKey;
pub type VoterVRFPublicKey = vrf::PublicKey;
pub type VoterSignKey = SignKey;
pub type VoterPower = u64;

pub struct BakerInfo {
    election_verify_key: BakerElectionVerifyKey,
    signature_verify_key: BakerSignVerifyKey,
    lottery_power: LotteryPower,
}

pub struct BirkParameters {
    leadership_election_nonce: LeadershipElectionNonce,
    election_difficulty: ElectionDifficulty,
    bakers: HashMap<BakerId, BakerInfo>,
}

pub struct VoterInfo {
    verification_key: VoterVerificationKey,
    public_key: VoterVRFPublicKey,
    voting_power: VoterPower,
}

pub type FinalizationParameters = Box<[VoterInfo]>;

pub type Timestamp = u64;

pub type Duration = u64;

pub struct GenesisData {
    creation_time: Timestamp,
    slot_duration: Duration,
    birk_parameters: BirkParameters,
    finalization_parameters: FinalizationParameters,
}
