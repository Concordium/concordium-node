// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::{ensure, format_err, Fallible};
use hash_hasher::HashedMap;

use std::{convert::TryFrom, io::Cursor, mem::size_of};

use concordium_common::indexed_vec::IndexedVec;

use crate::{
    common::*,
    parameters::{BakerElectionVerifyKey, BakerSignVerifyKey, BAKER_VRF_KEY},
};

// const PAYLOAD_MAX_LEN: u32 = 512 * 1024 * 1024; // 512MB

#[derive(Debug)]
pub struct TransactionHeader {
    pub scheme_id:      SchemeId,
    pub sender_key:     ByteString,
    pub nonce:          Nonce,
    pub gas_amount:     Energy,
    pub payload_size:   u32,
    pub sender_account: AccountAddress,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for TransactionHeader {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let scheme_id = SchemeId::try_from(u8::deserial(source)?)?;
        let sender_key = read_bytestring_short_length(source)?;

        let nonce = Nonce::try_from(u64::deserial(source)?)?;

        let gas_amount = Energy::deserial(source)?;
        let payload_size = source.read_u32::<Endianness>()?;
        let sender_account = AccountAddress::from((&*sender_key, scheme_id));

        let transaction_header = TransactionHeader {
            scheme_id,
            sender_key,
            nonce,
            gas_amount,
            payload_size,
            sender_account,
        };

        Ok(transaction_header)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        u8::serial(&(self.scheme_id as u8), target)?;
        target.write_u16::<NetworkEndian>(self.sender_key.len() as u16)?;
        target.write_all(&self.sender_key)?;
        u64::serial(&self.nonce.0, target)?;
        Energy::serial(&self.gas_amount, target)?;
        target.write_u32::<NetworkEndian>(self.payload_size)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct BareTransaction {
    signature:   ByteString,
    pub header:  TransactionHeader,
    pub payload: TransactionPayload,
    pub hash:    TransactionHash,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for BareTransaction {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let mut full_tx = Vec::new();
        source.read_to_end(&mut full_tx)?;
        let hash = sha256(&full_tx);

        let source = &mut Cursor::new(full_tx);
        let signature = read_bytestring_short_length(source)?;
        let header = TransactionHeader::deserial(source)?;
        let payload = TransactionPayload::deserial_with_param(source, header.payload_size)?;

        let transaction = BareTransaction {
            signature,
            header,
            payload,
            hash,
        };

        Ok(transaction)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u16::<NetworkEndian>(self.signature.len() as u16)?;
        target.write_all(&self.signature)?;
        self.header.serial(target)?;
        self.payload.serial(target)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct FullTransaction {
    pub bare_transaction: BareTransaction,
    pub arrival:          u64,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for FullTransaction {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let bare_transaction = BareTransaction::deserial(source)?;

        let arrival = source.read_u64::<Endianness>()?;
        let transaction = FullTransaction {
            bare_transaction,
            arrival,
        };

        Ok(transaction)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.bare_transaction.serial(target)?;
        target.write_u64::<NetworkEndian>(self.arrival as u64)?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TransactionType {
    DeployModule = 0,
    InitContract,
    Update,
    Transfer,
    DeployCredential,
    DeployEncryptionKey,
    AddBaker,
    RemoveBaker,
    UpdateBakerAccount,
    UpdateBakerSignKey,
    DelegateStake,
    UndelegateStake,
}

impl TryFrom<u8> for TransactionType {
    type Error = failure::Error;

    fn try_from(id: u8) -> Fallible<Self> {
        match id {
            0 => Ok(TransactionType::DeployModule),
            1 => Ok(TransactionType::InitContract),
            2 => Ok(TransactionType::Update),
            3 => Ok(TransactionType::Transfer),
            4 => Ok(TransactionType::DeployCredential),
            5 => Ok(TransactionType::DeployEncryptionKey),
            6 => Ok(TransactionType::AddBaker),
            7 => Ok(TransactionType::RemoveBaker),
            8 => Ok(TransactionType::UpdateBakerAccount),
            9 => Ok(TransactionType::UpdateBakerSignKey),
            10 => Ok(TransactionType::DelegateStake),
            11 => Ok(TransactionType::UndelegateStake),
            n => Err(format_err!("Unsupported TransactionType ({})!", n)),
        }
    }
}

pub type TyName = u32;

type Proof = ByteString;

#[derive(Debug)]
pub enum TransactionPayload {
    DeployModule(Encoded),
    InitContract {
        amount:   Amount,
        module:   HashBytes,
        contract: TyName,
        param:    Encoded,
    },
    Update {
        amount:  Amount,
        address: ContractAddress,
        message: Encoded,
    },
    Transfer {
        target_scheme:  SchemeId,
        target_address: AccountAddress,
        amount:         Amount,
    },
    DeployCredential(Encoded),
    DeployEncryptionKey(ByteString),
    AddBaker {
        election_verify_key:  BakerElectionVerifyKey,
        signature_verify_key: BakerSignVerifyKey,
        account_address:      AccountAddress,
        proof:                Proof,
    },
    RemoveBaker {
        id:    BakerId,
        proof: Proof,
    },
    UpdateBakerAccount {
        id:              BakerId,
        account_address: AccountAddress,
        proof:           Proof,
    },
    UpdateBakerSignKey {
        id:                   BakerId,
        signature_verify_key: BakerSignVerifyKey,
        proof:                Proof,
    },
    DelegateStake(BakerId),
    UndelegateStake,
}

impl TransactionPayload {
    pub fn transaction_type(&self) -> TransactionType {
        use TransactionPayload::*;

        match self {
            DeployModule(_) => TransactionType::DeployModule,
            InitContract { .. } => TransactionType::InitContract,
            Update { .. } => TransactionType::Update,
            Transfer { .. } => TransactionType::Transfer,
            DeployCredential(_) => TransactionType::DeployCredential,
            DeployEncryptionKey(_) => TransactionType::DeployEncryptionKey,
            AddBaker { .. } => TransactionType::AddBaker,
            RemoveBaker { .. } => TransactionType::RemoveBaker,
            UpdateBakerAccount { .. } => TransactionType::UpdateBakerAccount,
            UpdateBakerSignKey { .. } => TransactionType::UpdateBakerSignKey,
            DelegateStake(_) => TransactionType::DelegateStake,
            UndelegateStake => TransactionType::UndelegateStake,
        }
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for TransactionPayload {
    type Param = u32;

    fn deserial_with_param<R: ReadBytesExt>(source: &mut R, len: Self::Param) -> Fallible<Self> {
        let variant = TransactionType::try_from(source.read_u8()?)?;

        match variant {
            TransactionType::DeployModule => {
                let module = Encoded::new(&read_sized!(source, len - 1));
                Ok(TransactionPayload::DeployModule(module))
            }
            TransactionType::InitContract => {
                let amount = Amount::deserial(source)?;
                let module = HashBytes::from(read_ty!(source, HashBytes));
                let contract = TyName::deserial(source)?;

                let non_param_len = sum_ty_lens!(TransactionType, Amount, HashBytes, TyName);
                ensure!(
                    len as usize >= non_param_len,
                    "malformed transaction param!"
                );
                let param_size = len as usize - non_param_len;
                let param = Encoded::new(&read_sized!(source, param_size));

                Ok(TransactionPayload::InitContract {
                    amount,
                    module,
                    contract,
                    param,
                })
            }
            TransactionType::Update => {
                let amount = Amount::deserial(source)?;
                let address = ContractAddress::deserial(source)?;

                let non_message_len = sum_ty_lens!(TransactionType, Amount, ContractAddress);
                ensure!(
                    len as usize >= non_message_len,
                    "malformed transaction message!"
                );
                let msg_size = len as usize - non_message_len;
                let message = Encoded::new(&read_sized!(source, msg_size));

                Ok(TransactionPayload::Update {
                    amount,
                    address,
                    message,
                })
            }
            TransactionType::Transfer => {
                let target_scheme = SchemeId::try_from(source.read_u8()?)?;
                let target_address = AccountAddress(read_ty!(source, AccountAddress));
                let amount = Amount::deserial(source)?;

                Ok(TransactionPayload::Transfer {
                    target_scheme,
                    target_address,
                    amount,
                })
            }
            TransactionType::DeployCredential => {
                let credential = Encoded::new(&read_sized!(source, len - 1));

                Ok(TransactionPayload::DeployCredential(credential))
            }
            TransactionType::DeployEncryptionKey => {
                let ek = read_bytestring_short_length(source)?;

                Ok(TransactionPayload::DeployEncryptionKey(ek))
            }
            TransactionType::AddBaker => {
                let election_verify_key = Encoded::new(&read_const_sized!(source, BAKER_VRF_KEY));
                let signature_verify_key = read_bytestring_short_length(source)?;
                let account_address = AccountAddress(read_ty!(source, AccountAddress));
                let proof = read_bytestring(source)?;

                Ok(TransactionPayload::AddBaker {
                    election_verify_key,
                    signature_verify_key,
                    account_address,
                    proof,
                })
            }
            TransactionType::RemoveBaker => {
                let id = BakerId::deserial(source)?;
                let proof = read_bytestring(source)?;

                Ok(TransactionPayload::RemoveBaker { id, proof })
            }
            TransactionType::UpdateBakerAccount => {
                let id = BakerId::deserial(source)?;
                let account_address = AccountAddress(read_ty!(source, AccountAddress));
                let proof = read_bytestring(source)?;

                Ok(TransactionPayload::UpdateBakerAccount {
                    id,
                    account_address,
                    proof,
                })
            }
            TransactionType::UpdateBakerSignKey => {
                let id = BakerId::deserial(source)?;
                let signature_verify_key = read_bytestring_short_length(source)?;
                let proof = read_bytestring(source)?;

                Ok(TransactionPayload::UpdateBakerSignKey {
                    id,
                    signature_verify_key,
                    proof,
                })
            }
            TransactionType::DelegateStake => {
                let id = BakerId::deserial(source)?;

                Ok(TransactionPayload::DelegateStake(id))
            }
            TransactionType::UndelegateStake => Ok(TransactionPayload::UndelegateStake),
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(self.transaction_type() as u8)?;

        match self {
            TransactionPayload::DeployModule(module) => {
                target.write_all(&module)?;
            }
            TransactionPayload::InitContract {
                amount,
                module,
                contract,
                param,
            } => {
                target.write_u64::<NetworkEndian>(*amount)?;
                target.write_all(&*module)?;
                target.write_u32::<NetworkEndian>(*contract)?;
                target.write_all(&*param)?;
            }
            TransactionPayload::Update {
                amount,
                address,
                message,
            } => {
                target.write_u64::<NetworkEndian>(*amount)?;
                address.serial(target)?;
                target.write_all(&*message)?;
            }
            TransactionPayload::Transfer {
                target_scheme,
                target_address,
                amount,
            } => {
                target.write_u8(*target_scheme as u8)?;
                target.write_all(&target_address.0)?;
                target.write_u64::<NetworkEndian>(*amount)?;
            }
            TransactionPayload::DeployCredential(credential) => {
                target.write_all(&credential)?;
            }
            TransactionPayload::DeployEncryptionKey(ek) => {
                target.write_u16::<NetworkEndian>(ek.len() as u16)?;
                target.write_all(&ek)?;
            }
            TransactionPayload::AddBaker {
                election_verify_key,
                signature_verify_key,
                account_address,
                proof,
            } => {
                target.write_all(&election_verify_key)?;
                write_bytestring_short_length(target, &signature_verify_key)?;
                target.write_all(&account_address.0)?;
                write_bytestring(target, &proof)?;
            }
            TransactionPayload::RemoveBaker { id, proof } => {
                target.write_u64::<NetworkEndian>(*id)?;
                write_bytestring(target, &proof)?;
            }
            TransactionPayload::UpdateBakerAccount {
                id,
                account_address,
                proof,
            } => {
                target.write_u64::<NetworkEndian>(*id)?;
                target.write_all(&account_address.0)?;
                write_bytestring(target, &proof)?;
            }
            TransactionPayload::UpdateBakerSignKey {
                id,
                signature_verify_key,
                proof,
            } => {
                target.write_u64::<NetworkEndian>(*id)?;
                write_bytestring_short_length(target, &signature_verify_key)?;
                write_bytestring(target, &proof)?;
            }
            TransactionPayload::DelegateStake(id) => {
                target.write_u64::<NetworkEndian>(*id)?;
            }
            TransactionPayload::UndelegateStake => {
                target.write_u8(TransactionType::UndelegateStake as u8)?;
            }
        }

        Ok(())
    }
}

pub struct AccountNonFinalizedTransactions {
    map:        IndexedVec<Vec<BareTransaction>>, // indexed by Nonce
    next_nonce: Nonce,
}

impl Default for AccountNonFinalizedTransactions {
    fn default() -> Self {
        Self {
            map:        Default::default(),
            next_nonce: Nonce::try_from(1).unwrap(), // safe
        }
    }
}

#[derive(Default)]
pub struct TransactionTable {
    #[allow(dead_code)]
    map: HashedMap<TransactionHash, (BareTransaction, Slot)>,
    pub(super) non_finalized_transactions:
        HashedMap<AccountAddress, AccountNonFinalizedTransactions>,
}

impl TransactionTable {
    pub fn insert(&mut self, transaction: BareTransaction, finalized: bool) {
        if !finalized {
            let account_transactions = self
                .non_finalized_transactions
                .entry(transaction.header.sender_account.clone())
                .or_default();
            // WARNING: the map is indexed by Nonce - 1, since a nonce is non-zero
            let index = transaction.header.nonce.0 as usize - 1;

            if let Some(ref mut transactions) = account_transactions.map.get_mut(index) {
                transactions.push(transaction);
            } else {
                account_transactions.map.insert(index, vec![transaction])
            }
            account_transactions.next_nonce.0 += 1;
        } else {
            unimplemented!()
        }
    }
}

pub type PendingTransactionTable = HashedMap<AccountAddress, (Nonce, Nonce)>;
