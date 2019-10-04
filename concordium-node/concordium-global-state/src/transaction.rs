// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::{ensure, format_err, Fallible};
use hash_hasher::HashedMap;

use std::{
    convert::TryFrom,
    io::{Cursor, Read},
    mem::size_of,
};

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
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let scheme_id = SchemeId::try_from(read_const_sized!(cursor, 1)[0])?;
        let sender_key = read_bytestring_short_length(cursor, "sender key's length")?;

        let nonce_raw = NetworkEndian::read_u64(&read_ty!(cursor, Nonce));
        let nonce = Nonce::try_from(nonce_raw)?;

        let gas_amount = NetworkEndian::read_u64(&read_ty!(cursor, Energy));
        let payload_size = NetworkEndian::read_u32(&read_const_sized!(cursor, 4));
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
        target.write_u8(self.scheme_id as u8)?;
        target.write_u16::<NetworkEndian>(self.sender_key.len() as u16)?;
        target.write_all(&self.sender_key)?;
        target.write_u64::<NetworkEndian>(self.nonce.0)?;
        target.write_u64::<NetworkEndian>(self.gas_amount)?;
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
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let initial_pos = cursor.position() as usize;
        let signature = read_bytestring_short_length(cursor, "transaction signature")?;
        let header = TransactionHeader::deserialize(cursor)?;
        let payload = TransactionPayload::deserialize((cursor, header.payload_size))?;

        let hash = sha256(&cursor.get_ref()[initial_pos..cursor.position() as usize]);
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
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let bare_transaction = BareTransaction::deserialize(cursor)?;

        let arrival = NetworkEndian::read_u64(&read_const_sized!(cursor, 8));
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
    type Source = (&'a mut Cursor<&'b [u8]>, u32);

    fn deserialize((cursor, len): Self::Source) -> Fallible<Self> {
        let variant = TransactionType::try_from(read_ty!(cursor, TransactionType)[0])?;

        match variant {
            TransactionType::DeployModule => {
                let module = Encoded::new(&read_sized!(cursor, len - 1));
                Ok(TransactionPayload::DeployModule(module))
            }
            TransactionType::InitContract => {
                let amount = NetworkEndian::read_u64(&read_ty!(cursor, Amount));
                let module = HashBytes::from(read_ty!(cursor, HashBytes));
                let contract = NetworkEndian::read_u32(&read_ty!(cursor, TyName));

                let non_param_len = sum_ty_lens!(TransactionType, Amount, HashBytes, TyName);
                ensure!(
                    len as usize >= non_param_len,
                    "malformed transaction param!"
                );
                let param_size = len as usize - non_param_len;
                let param = Encoded::new(&read_sized!(cursor, param_size));

                Ok(TransactionPayload::InitContract {
                    amount,
                    module,
                    contract,
                    param,
                })
            }
            TransactionType::Update => {
                let amount = NetworkEndian::read_u64(&read_ty!(cursor, Amount));
                let address = ContractAddress::deserialize(cursor)?;

                let non_message_len = sum_ty_lens!(TransactionType, Amount, ContractAddress);
                ensure!(
                    len as usize >= non_message_len,
                    "malformed transaction message!"
                );
                let msg_size = len as usize - non_message_len;
                let message = Encoded::new(&read_sized!(cursor, msg_size));

                Ok(TransactionPayload::Update {
                    amount,
                    address,
                    message,
                })
            }
            TransactionType::Transfer => {
                let target_scheme = SchemeId::try_from(read_ty!(cursor, SchemeId)[0])?;
                let target_address = AccountAddress(read_ty!(cursor, AccountAddress));
                let amount = NetworkEndian::read_u64(&read_ty!(cursor, Amount));

                Ok(TransactionPayload::Transfer {
                    target_scheme,
                    target_address,
                    amount,
                })
            }
            TransactionType::DeployCredential => {
                let credential = Encoded::new(&read_sized!(cursor, len - 1));

                Ok(TransactionPayload::DeployCredential(credential))
            }
            TransactionType::DeployEncryptionKey => {
                let ek = read_bytestring_short_length(cursor, "encryption key to deploy")?;

                Ok(TransactionPayload::DeployEncryptionKey(ek))
            }
            TransactionType::AddBaker => {
                let election_verify_key = Encoded::new(&read_const_sized!(cursor, BAKER_VRF_KEY));
                let signature_verify_key =
                    read_bytestring_short_length(cursor, "baker sign verify key")?;
                let account_address = AccountAddress(read_ty!(cursor, AccountAddress));
                let proof = read_bytestring(cursor, "baker addition proof")?;

                Ok(TransactionPayload::AddBaker {
                    election_verify_key,
                    signature_verify_key,
                    account_address,
                    proof,
                })
            }
            TransactionType::RemoveBaker => {
                let id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));
                let proof = read_bytestring(cursor, "baker removal proof")?;

                Ok(TransactionPayload::RemoveBaker { id, proof })
            }
            TransactionType::UpdateBakerAccount => {
                let id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));
                let account_address = AccountAddress(read_ty!(cursor, AccountAddress));
                let proof = read_bytestring(cursor, "baker update proof")?;

                Ok(TransactionPayload::UpdateBakerAccount {
                    id,
                    account_address,
                    proof,
                })
            }
            TransactionType::UpdateBakerSignKey => {
                let id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));
                let signature_verify_key =
                    read_bytestring_short_length(cursor, "baker sign verify key")?;
                let proof = read_bytestring(cursor, "baker update proof")?;

                Ok(TransactionPayload::UpdateBakerSignKey {
                    id,
                    signature_verify_key,
                    proof,
                })
            }
            TransactionType::DelegateStake => {
                let id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));

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
