// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Parameters.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};

use failure::{ensure, Fallible};

use std::{
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{block::BakerId, common::*};

pub type BakerSignVerifyKey = ByteString;
pub type BakerSignPrivateKey = Encoded;
pub type BakerElectionVerifyKey = Encoded;
pub type BakerElectionPrivateKey = Encoded;
pub type LotteryPower = f64;
pub type ElectionDifficulty = f64;

pub type VoterId = u64;
pub type VoterVerificationKey = ByteString;
pub type VoterVRFPublicKey = Encoded;
pub type VoterSignKey = Encoded;
pub type VoterPower = u64;

const BAKER_VRF_KEY: u8 = 32;
const BAKER_SIGN_KEY: u8 = 8 + 32; // unnecessary 8B prefix
const BAKER_INFO: u8 = BAKER_VRF_KEY
    + BAKER_SIGN_KEY
    + size_of::<LotteryPower>() as u8
    + size_of::<AccountAddress>() as u8;

const VOTER_SIGN_KEY: u8 = 8 + 32; // unnecessary 8B prefix
const VOTER_VRF_KEY: u8 = 32;
const VOTER_INFO: u8 = VOTER_SIGN_KEY + VOTER_VRF_KEY + size_of::<VoterPower>() as u8;

#[derive(Debug)]
pub struct BirkParameters {
    election_nonce:      ByteString,
    election_difficulty: ElectionDifficulty,
    pub bakers:          Vec<(BakerId, BakerInfo)>,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for BirkParameters {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let election_nonce = Encoded::new(&read_bytestring(cursor)?);
        let election_difficulty = NetworkEndian::read_f64(&read_const_sized!(cursor, 8));

        let baker_count = NetworkEndian::read_u64(&read_const_sized!(cursor, 8)) as usize;
        ensure!(baker_count <= ALLOCATION_LIMIT, "The baker count ({}) exceeds the safety limit!", baker_count);
        let mut bakers = Vec::with_capacity(baker_count);
        for _ in 0..baker_count {
            let id = NetworkEndian::read_u64(&read_const_sized!(cursor, 8));
            let info = BakerInfo::deserialize(&read_const_sized!(cursor, BAKER_INFO))?;

            bakers.push((id, info));
        }

        let params = BirkParameters {
            election_nonce,
            election_difficulty,
            bakers,
        };

        // serialization is not checked here due to the parameters being of an unknown
        // size; it is instead done while deserializing the parent object -
        // GenesisData

        Ok(params)
    }

    fn serialize(&self) -> Box<[u8]> {
        let baker_info_size = 8 + self.bakers.len() * (size_of::<BakerId>() + BAKER_INFO as usize);
        let mut baker_cursor = create_serialization_cursor(baker_info_size);

        let _ = baker_cursor.write_u64::<NetworkEndian>(self.bakers.len() as u64);

        for (id, info) in self.bakers.iter() {
            let _ = baker_cursor.write_u64::<NetworkEndian>(*id);
            let _ = baker_cursor.write_all(&info.serialize());
        }

        debug_assert_eq!(baker_cursor.position(), baker_cursor.get_ref().len() as u64);

        let size = self.election_nonce.len()
            + size_of::<ElectionDifficulty>()
            + baker_cursor.get_ref().len();
        let mut cursor = create_serialization_cursor(size);

        let _ = cursor.write_all(&self.election_nonce);
        let _ = cursor.write_f64::<NetworkEndian>(self.election_difficulty);
        let _ = cursor.write_all(baker_cursor.get_ref());

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct BakerInfo {
    election_verify_key:  BakerElectionVerifyKey,
    signature_verify_key: BakerSignVerifyKey,
    lottery_power:        LotteryPower,
    account_address:      AccountAddress,
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for BakerInfo {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let election_verify_key = Encoded::new(&read_const_sized!(&mut cursor, BAKER_VRF_KEY));
        let signature_verify_key = ByteString::new(&read_const_sized!(&mut cursor, BAKER_SIGN_KEY));
        let lottery_power = NetworkEndian::read_f64(&read_const_sized!(cursor, 8));
        let account_address =
            AccountAddress(read_const_sized!(cursor, size_of::<AccountAddress>()));

        let info = BakerInfo {
            election_verify_key,
            signature_verify_key,
            lottery_power,
            account_address,
        };

        check_serialization!(info, cursor);

        Ok(info)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(BAKER_INFO as usize);

        let _ = cursor.write_all(&self.election_verify_key);
        let _ = cursor.write_all(&self.signature_verify_key);
        let _ = cursor.write_f64::<NetworkEndian>(self.lottery_power);
        let _ = cursor.write_all(&self.account_address.0);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct FinalizationParameters(Box<[VoterInfo]>);

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for FinalizationParameters {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let param_count = NetworkEndian::read_u64(&read_const_sized!(cursor, 8)) as usize;
        ensure!(param_count <= ALLOCATION_LIMIT, "The finalization parameter count ({}) exceeds the safety limit!", param_count);
        let mut params = Vec::with_capacity(param_count);

        for _ in 0..param_count {
            params.push(VoterInfo::deserialize(&read_const_sized!(
                cursor, VOTER_INFO
            ))?);
        }

        let params = FinalizationParameters(params.into_boxed_slice());

        // serialization is not checked here due to the parameters being of an unknown
        // size it is instead done while deserializing the parent object -
        // GenesisData

        Ok(params)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(8 + self.0.len() * VOTER_INFO as usize);

        let _ = cursor.write_u64::<NetworkEndian>(self.0.len() as u64);

        for info in self.0.iter() {
            let _ = cursor.write_all(&info.serialize());
        }

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct VoterInfo {
    signature_verify_key: VoterVerificationKey,
    election_verify_key:  VoterVRFPublicKey,
    voting_power:         VoterPower,
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for VoterInfo {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let signature_verify_key = ByteString::new(&read_const_sized!(&mut cursor, VOTER_SIGN_KEY));
        let election_verify_key = Encoded::new(&read_const_sized!(&mut cursor, VOTER_VRF_KEY));
        let voting_power = NetworkEndian::read_u64(&read_const_sized!(cursor, 8));

        let info = VoterInfo {
            signature_verify_key,
            election_verify_key,
            voting_power,
        };

        check_serialization!(info, cursor);

        Ok(info)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(VOTER_INFO as usize);

        let _ = cursor.write_all(&self.signature_verify_key);
        let _ = cursor.write_all(&self.election_verify_key);
        let _ = cursor.write_u64::<NetworkEndian>(self.voting_power);

        cursor.into_inner()
    }
}
