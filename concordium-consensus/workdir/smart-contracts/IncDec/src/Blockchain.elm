module Blockchain exposing (Account, Address, Amount, Caller, Chain, Contract, MaxAllowedGasCost, Reply(..), Tx(..){-, Version, accountAddress, accountNew, accountPublicBalance, accountToCaller, callerAddress, contract, emptyChain, getCurrentBlockHash-})

--import Credentials
import Dict exposing (Dict)
--import Random
import Set exposing (Set)


type alias Contract flags msg state =
    { version : Int
    , init : flags -> Chain -> ( Caller, Amount ) -> state
    , receive : Chain -> state -> ( Caller, Amount, Maybe msg ) -> Reply state
    }


type alias DeployedContract flags msg state =
    { contract : Contract flags msg state
    , address : Address
    , balance : Amount
    , owner : Address -- Contract inherits credentials from Owner
    , state : state
    }


contract :
    Int -- Version
    -> (flags -> Chain -> ( Caller, Amount ) -> state) -- Init
    -> (Chain -> state -> ( Caller, Amount, Maybe msg ) -> Reply state) -- Receive
    -> Contract flags msg state
contract version init receive =
    Contract version init receive


type alias Amount =
    Float


type TxBalance
    = PublicAmount Amount
    | PrivateAmount



-- type
--   Chain
--   -- state of the blockchain as of some block id
--   -- Nothing = newest block a.k.a. current block
--   = Chain (Maybe BlockId)


type Chain
    = Chain (List BlockId)


emptyChain =
    Chain []


type alias BlockId =
    Int



-- getParent : Chain -> BlockId -> (Maybe Chain -> msg) -> Tx.Callback msg


type alias Version =
    Int


type Account
    = Account
        { address : Address
        , publicAmount : Amount
        , credentials : () --Credentials TODO Reinstate credentials
        , receive : Chain -> ( Caller, Amount ) -> Tx
        }


accountNew : Address -> Account
accountNew address =
    Account
        { address = address
        , publicAmount = 0
        , credentials = () --Credentials.none TODO Reinstate credentials
        , receive = accountDefaultReceive
        }


accountAddress : Account -> Address
accountAddress (Account { address }) =
    address


accountPublicBalance : Account -> Amount
accountPublicBalance (Account { publicAmount }) =
    publicAmount


{-| Does this mean received amounts are implicitly added to account.publicAmount elsewhere?
-}
accountDefaultReceive : Chain -> ( Caller, Amount ) -> Tx
accountDefaultReceive chain ( caller, amount ) =
    None



-- @TODO We can't use this because we still want comparable... what do?
-- type
--   Address
--   -- non-exported constructor
--   = Address String


type alias Address =
    -- @TODO once we have a real implementation and the sasndbox doesn't need to fake values, this needs to become an opaque type
    String


type Caller
    = CalledAccount Account
    | CallerContract Address


callerAddress : Caller -> Address
callerAddress caller =
    case caller of
        CalledAccount (Account { address }) ->
            address

        CallerContract address ->
            address


{-| @TODO remove me, call triggers should be handled more naturally via library?
Subsequent note: can't remember the context for this original message. Need to carefully re-evaluate Main.elm & see if we can simplify the sandbox's `update` fn.
-}
accountToCaller : Account -> Caller
accountToCaller account =
    CalledAccount account


type Error
    = Error


getBalance : Chain -> Address -> Result Error Amount
getBalance blockchain address =
    -- Dict.get address blockchain
    --     |> Result.fromMaybe Error
    --     -- ("Address not found: " ++ address)
    --     |> Result.map (\(BlockHeader blockHeader) -> blockHeader.amount)
    Debug.todo "getBalance"


type alias Credentials =
    Set Credential


type
    Credential
    -- Opaque
    = Credential
        { identityIssuer : Address
        , anonymityRevokers : Set Address
        , timeout : Timestamp -- seconds since some epoch
        , predicates : Predicates
        }


type alias InvalidCredentials =
    Set InvalidCredential


type alias InvalidCredential =
    { credential : Credential
    , invalidationReason : String -- @TODO this will be some custom type; timeout, blacklist, issuer blacklist, revoker blacklist, governance revoked
    }


type Timestamp
    = Timestamp Int


type alias Predicates =
    Set Predicate


type alias Predicate =
    -- temporary placeholder type; will be some large record type later
    String


{-| Returns valid Credentials only
-}
getCredentials : Chain -> Address -> Result Error Credentials
getCredentials blockchain address =
    Ok Set.empty


getInvalidatedCredentials : Chain -> Address -> Result Error InvalidCredentials
getInvalidatedCredentials blockchain address =
    Ok Set.empty


{-| getContractState is used for fetching the state of another contract. For example, you could have a contract that stores the current price of rice in its state, and this way you wouldn't have to pay for a txn + callback to read it.
-}
getContractState : Chain -> Address -> Result Error state
getContractState blockchain contractAddress =
    Debug.todo "getContractState"


getContractOwner : Chain -> Address -> Result Error Address
getContractOwner blockchain contractAddress =
    Debug.todo "getContractOwner"



-- What is this for? Wouldn't we just solve this with compilation type-safety? Reference contract explicitly and assume it's there. If it's not... doesn't compile...
-- isMsgDefinedAt : Chain -> Address -> msg -> Bool
-- isMsgDefinedAt
--
--
-- TODO: need to make sure we're type safe when we send responses; we don't know where we get the response back, or from who.
-- MR edit: not sure this is accurate. Type-safety gives us exactly this...?


{-| BlockInfo will be used to get an idea of the real world. I can see relative time, perceived time, if one person authored many blocks, etc.
-}
type BlockHeader
    = BlockHeader
        { blockNumber : BlockId -- Will always be positive & incrementing
        , hash : String
        , createdAt : Timestamp -- estimated time
        }


getCurrentBlockHash : Chain -> String
getCurrentBlockHash blockchain =
    case getCurrentBlock blockchain of
        BlockHeader { hash } ->
            hash


getCurrentBlock : Chain -> BlockHeader
getCurrentBlock blockchain =
    -- @TODO STUB
    fakeBlockHeader


getBlockAt : Chain -> BlockId -> Maybe BlockHeader
getBlockAt blockchain blockId =
    -- @TODO STUB
    Nothing


--{-| random seed based on the hash of the latest block, which makes it somewhat cryptographically secure, even though it's known to everyone.
---}
--randomFromBlockHash : Chain -> Random.Seed
--randomFromBlockHash blockchain =
--    -- mock implementation, something like;
--    -- getCurrentBlock blockchain |> getBlockHash |> Random.initialSeed
--    -- @TODO STUB
--    Random.initialSeed 123


{-| get a reference to the blockchain at some (old) blockid. You can use this new blockchain object to get e.g. account balance at that specific point in time.
-}
getChain : Chain -> BlockId -> Maybe Chain
getChain blockchain blockId =
    -- @TODO STUB
    Nothing



---- Implementation stuff in Sandbox


fakeBlockHeader : BlockHeader
fakeBlockHeader =
    BlockHeader
        { blockNumber = 123
        , hash = "123"
        , createdAt = Timestamp 123
        }


{-| Moved from Tx.elm to prevent circular dep... unravel later
-}
type Tx
    = None
    | Transfer Address Amount MaxAllowedGasCost
    | Send Address String Amount MaxAllowedGasCost
    | Batch (Set Tx)


type Reply state
    = Reject -- Rejects sent Amount, no state changes or transactions are made
    | Accept state Tx -- Accepts sent Amount, any state changes are made and any transactions are scheduled




type alias MaxAllowedGasCost =
    Int
