// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::{format_err, Fallible};
use hash_hasher::HashedMap;

use std::{convert::TryFrom, mem::size_of};

use concordium_common::indexed_vec::IndexedVec;

use crate::common::*;

// const PAYLOAD_MAX_LEN: u32 = 512 * 1024 * 1024; // 512MB

#[derive(Debug)]
pub struct SignatureVerifyKey {
    pub scheme_id:  SchemeId,
    pub verify_key: ByteString,
}

impl Serial for SignatureVerifyKey {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let scheme_id = SchemeId::try_from(u8::deserial(source)?)?;
        let verify_key = Encoded::new(&read_sized!(source, scheme_id.verify_key_length()));

        let signature_verify_key = SignatureVerifyKey {
            scheme_id,
            verify_key,
        };

        Ok(signature_verify_key)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        u8::serial(&(self.scheme_id as u8), target)?;
        target.write_all(&self.verify_key)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct TransactionHeader {
    pub sender_key:     SignatureVerifyKey,
    pub nonce:          Nonce,
    pub gas_amount:     Energy,
    pub payload_size:   u32,
    pub sender_account: AccountAddress,
}

impl Serial for TransactionHeader {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let sender_key = SignatureVerifyKey::deserial(source)?;

        let nonce = Nonce::try_from(u64::deserial(source)?)?;

        let gas_amount = Energy::deserial(source)?;
        let payload_size = source.read_u32::<Endianness>()?;
        let sender_account = AccountAddress::from((&*sender_key.verify_key, sender_key.scheme_id));

        let transaction_header = TransactionHeader {
            sender_key,
            nonce,
            gas_amount,
            payload_size,
            sender_account,
        };

        Ok(transaction_header)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        SignatureVerifyKey::serial(&self.sender_key, target)?;
        u64::serial(&self.nonce.0, target)?;
        Energy::serial(&self.gas_amount, target)?;
        target.write_u32::<Endianness>(self.payload_size)?;

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

impl Serial for BareTransaction {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        use std::io::Write;
        // TODO avoid serializing the transaction again to get the hash
        // maybe requiring `+ Seek` or sth similar
        let signature = read_bytestring_short_length(source)?;
        let header = TransactionHeader::deserial(source)?;
        let payload = TransactionPayload::deserial_with_param(source, header.payload_size)?;
        let hash = sha256(&{
            let mut header_ser = Vec::new();
            header.serial(&mut header_ser)?;
            let mut payload_ser = Vec::new();
            payload.serial(&mut payload_ser)?;
            let mut target = create_serialization_cursor(
                size_of::<u16>() + signature.len() + header_ser.len() + payload_ser.len(),
            );
            target.write_u16::<Endianness>(signature.len() as u16)?;
            target.write_all(&signature)?;
            target.write_all(&header_ser)?;
            target.write_all(&payload_ser)?;
            target.into_inner()
        });
        let transaction = BareTransaction {
            signature,
            header,
            payload,
            hash,
        };

        Ok(transaction)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u16::<Endianness>(self.signature.len() as u16)?;
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

impl Serial for FullTransaction {
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
        target.write_u64::<Endianness>(self.arrival as u64)?;

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

pub type TransactionPayload = Encoded;

impl TransactionPayload {
    pub fn transaction_type(&self) -> TransactionType {
        match &self[0] {
            0 => TransactionType::DeployModule,
            1 => TransactionType::InitContract,
            2 => TransactionType::Update,
            3 => TransactionType::Transfer,
            4 => TransactionType::DeployCredential,
            5 => TransactionType::DeployEncryptionKey,
            6 => TransactionType::AddBaker,
            7 => TransactionType::RemoveBaker,
            8 => TransactionType::UpdateBakerAccount,
            9 => TransactionType::UpdateBakerSignKey,
            10 => TransactionType::DelegateStake,
            _ => TransactionType::UndelegateStake,
        }
    }
}

impl Serial for TransactionPayload {
    type Param = u32;

    fn deserial_with_param<R: ReadBytesExt>(source: &mut R, len: Self::Param) -> Fallible<Self> {
        Ok(Encoded::new(&read_sized!(source, len)))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_all(self)?;
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
