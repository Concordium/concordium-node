// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Parameters.hs

use byteorder::{ByteOrder, ReadBytesExt, WriteBytesExt};

use failure::Fallible;

use std::{collections::HashMap, mem::size_of};

use concordium_common::{
    blockchain_types::BakerId,
    serial::{NoParam, Serial},
};

use crate::common::*;

pub type BakerSignVerifyKey = ByteString;
pub type BakerSignPrivateKey = Encoded;
pub type BakerElectionVerifyKey = Encoded;
pub type BakerElectionPrivateKey = Encoded;
pub type ElectionDifficulty = f64;

pub type VoterId = u64;
pub type VoterVerificationKey = ByteString;
pub type VoterVRFPublicKey = Encoded;
pub type VoterSignKey = Encoded;
pub type VoterPower = u64;

pub const BAKER_VRF_KEY: u8 = 32;

const VOTER_SIGN_KEY: u8 = 2 + 32;
const VOTER_VRF_KEY: u8 = 32;
pub const VOTER_INFO: u8 = VOTER_SIGN_KEY + VOTER_VRF_KEY + size_of::<VoterPower>() as u8;
const ELGAMAL_GENERATOR: u8 = 48;
const MAX_BAKER_ALLOC: usize = 512;

#[derive(Debug)]
pub struct Bakers {
    baker_map:         HashMap<BakerId, BakerInfo>,
    bakers_by_key:     HashMap<(BakerSignVerifyKey, BakerElectionVerifyKey), Box<[BakerId]>>,
    baker_total_stake: Amount,
    next_baker_id:     BakerId,
}

impl Serial for Bakers {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let baker_map = read_hashmap!(
            source,
            (BakerId::deserial(source)?, BakerInfo::deserial(source)?),
            8,
            MAX_BAKER_ALLOC
        );
        let bakers_by_key = read_hashmap!(
            source,
            (
                (
                    read_bytestring_short_length(source)?,
                    Encoded::new(&read_const_sized!(source, BAKER_VRF_KEY))
                ),
                read_multiple!(source, BakerId::deserial(source)?, 8, MAX_BAKER_ALLOC)
            ),
            8,
            MAX_BAKER_ALLOC
        );

        let baker_total_stake = Amount::deserial(source)?;
        let next_baker_id = BakerId::deserial(source)?;

        let params = Bakers {
            baker_map,
            bakers_by_key,
            baker_total_stake,
            next_baker_id,
        };

        Ok(params)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u64::<Endianness>(self.baker_map.len() as u64)?;
        for (id, info) in self.baker_map.iter() {
            id.serial(target)?;
            info.serial(target)?;
        }

        target.write_u64::<Endianness>(self.bakers_by_key.len() as u64)?;
        for ((bsk, bvk), bakerids) in self.bakers_by_key.iter() {
            write_bytestring_short_length(target, bsk)?;
            target.write_all(bvk)?;
            target.write_u64::<Endianness>(bakerids.len() as u64)?;
            for id in bakerids.iter() {
                id.serial(target)?;
            }
        }

        self.baker_total_stake.serial(target)?;
        self.next_baker_id.serial(target)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct BirkParameters {
    election_nonce:      ByteString,
    election_difficulty: ElectionDifficulty,
    pub bakers:          Bakers,
}

impl Serial for BirkParameters {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let election_nonce = read_bytestring(source)?;
        let election_difficulty = Endianness::read_f64(&read_ty!(source, ElectionDifficulty));
        let bakers = Bakers::deserial(source)?;

        let params = BirkParameters {
            election_nonce,
            election_difficulty,
            bakers,
        };

        Ok(params)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        write_bytestring(target, &self.election_nonce)?;
        target.write_f64::<Endianness>(self.election_difficulty)?;
        self.bakers.serial(target)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct CryptographicParameters {
    pub elgamal_generator:        ByteString,
    pub attribute_commitment_key: ByteString,
}

impl Serial for CryptographicParameters {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let elgamal_generator = Encoded::new(&read_const_sized!(source, ELGAMAL_GENERATOR));
        let attribute_commitment_key = read_bytestring_medium(source)?;

        let crypto_params = CryptographicParameters {
            elgamal_generator,
            attribute_commitment_key,
        };

        Ok(crypto_params)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_all(&self.elgamal_generator)?;
        target.write_u32::<Endianness>(self.attribute_commitment_key.len() as u32)?;
        target.write_all(&self.attribute_commitment_key)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct BakerInfo {
    election_verify_key:  BakerElectionVerifyKey,
    signature_verify_key: BakerSignVerifyKey,
    stake:                Amount,
    account_address:      AccountAddress,
}

impl Serial for BakerInfo {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let election_verify_key = Encoded::new(&read_const_sized!(source, BAKER_VRF_KEY));
        let signature_verify_key = read_bytestring_short_length(source)?;
        let stake = Amount::deserial(source)?;
        let account_address = AccountAddress(read_ty!(source, AccountAddress));

        let info = BakerInfo {
            election_verify_key,
            signature_verify_key,
            stake,
            account_address,
        };

        Ok(info)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_all(&self.election_verify_key)?;
        write_bytestring_short_length(target, &self.signature_verify_key)?;
        self.stake.serial(target)?;
        target.write_all(&self.account_address.0)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct VoterInfo {
    pub signature_verify_key: VoterVerificationKey,
    election_verify_key:      VoterVRFPublicKey,
    voting_power:             VoterPower,
}

impl Serial for VoterInfo {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let signature_verify_key = read_bytestring_short_length(source)?;
        let election_verify_key = Encoded::new(&read_const_sized!(source, VOTER_VRF_KEY));
        let voting_power = VoterPower::deserial(source)?;

        let info = VoterInfo {
            signature_verify_key,
            election_verify_key,
            voting_power,
        };

        Ok(info)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        write_bytestring_short_length(target, &self.signature_verify_key)?;
        target.write_all(&self.election_verify_key)?;
        self.voting_power.serial(target)?;

        Ok(())
    }
}
