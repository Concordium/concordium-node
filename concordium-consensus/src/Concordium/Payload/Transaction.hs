{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# Language OverloadedStrings #-}
module Concordium.Payload.Transaction where

import GHC.Generics
import Data.Word
import Data.ByteString.Char8(ByteString, pack)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Serialize
import Data.Hashable
import Data.Bits
import Numeric

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Interpreter.CallContract as I
import Data.Functor((<&>))

import Control.Monad(foldM)
import Control.Monad.IO.Class

import Data.String(fromString)

-- ONLY FOR locking in tryTransaction
import Control.Concurrent.MVar
import System.IO.Unsafe

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

data TransactionNonce = TransactionNonce !Word64 !Word64 !Word64 !Word64
    deriving (Eq, Ord, Generic)

instance Hashable TransactionNonce where
    hashWithSalt salt (TransactionNonce a _ _ _) = fromIntegral a `xor` salt
    hash (TransactionNonce a _ _ _) = fromIntegral a

instance Show TransactionNonce where
    show (TransactionNonce a b c d) =
        LBS.unpack (toLazyByteString $ word64HexFixed a <> word64HexFixed b <> word64HexFixed c <> word64HexFixed d)

instance Serialize TransactionNonce

data Transaction = Transaction {
    transactionNonce :: TransactionNonce,
    transactionMetadata :: Metadata,
    transactionPayload :: Payload
} deriving (Generic)

instance Serialize Transaction

instance Show Transaction where
    showsPrec l (Transaction nonce meta payload) = showsPrec l nonce . (':':) . showsPrec l meta . (':':) . showsPrec l payload

toTransactions :: ByteString -> Maybe [Transaction]
toTransactions bs = case decode bs of
        Left _ -> Nothing
        Right r -> Just r

fromTransactions :: [Transaction] -> ByteString
fromTransactions = encode

type Address = ByteString

data Metadata = Metadata { sender :: Address }
  deriving(Show, Generic)

instance Serialize Metadata

type Amount = Word

data Message = Increment | Decrement
    deriving (Show, Generic)

instance Serialize Message

data Payload = Update Address Message
  deriving (Show, Generic)

instance Serialize Payload

data GlobalState = GlobalState { instances :: Map Address I.State }

data Event = Updated Address Message
           | Rejected
  deriving (Show)

data Failure a b = Failure a b
    deriving (Show)

encodeTransactions :: [Transaction] -> ByteString
encodeTransactions = encode

encodeTransaction :: Transaction -> ByteString
encodeTransaction = encode

messageToMsg Increment = I.Msg . fromString $ "[\"Increment\"]"
messageToMsg Decrement = I.Msg . fromString $ "[\"Decrement\"]"

-- tryTransaction :: MonadIO m => GlobalState -> Transaction -> m (Either String (Event, GlobalState))
tryTransaction ctx gs@(GlobalState {instances = instances}) (Transaction _ meta payload) =
  let Metadata {sender = sender} = meta
      Update addr msg = payload
      cstate = Map.lookup addr instances
  in case cstate of
       Nothing -> return . Left $ "Non-existent contract instance: " ++ show addr
       Just addrState -> do
         txres <- liftIO $ I.callContract ctx (I.Address "<stubbed address>") (Just (messageToMsg msg)) addrState undefined I.Caller
         case txres of
           I.Reject -> return . Right $ (Rejected, gs)
           I.Accept newState I.None -> return . Right $ (Updated addr msg, GlobalState (Map.insert addr newState instances))
           I.Accept newState (I.Transfer _ _ _) -> return . Right $ (Updated addr msg, GlobalState (Map.insert addr newState instances)) -- TODO Do the transfer
           I.Accept newState (I.Send _ _ _ _) -> return . Right $ (Updated addr msg, GlobalState (Map.insert addr newState instances)) -- TODO Do the sending
           I.Accept newState (I.Batch _) -> return . Right $ (Updated addr msg, GlobalState (Map.insert addr newState instances)) -- TODO Unwrap and handle the batch

executeBlock :: MonadIO m => GlobalState -> [Transaction] -> m (Either String ([Event], GlobalState))
executeBlock gs msgs = do
  () <- liftIO (takeMVar lock)
  let hardcodedSmartContract = I.Code "workdir/smart-contracts/IncDec/elm.json" ["workdir/smart-contracts/IncDec/src/IncDec.elm","workdir/smart-contracts/IncDec/src/Evergreen.elm","workdir/smart-contracts/IncDec/src/Blockchain.elm","workdir/smart-contracts/IncDec/src/Tx.elm","workdir/smart-contracts/IncDec/src/Reply.elm"] "IncDec"
  ctx <- liftIO $ I.makeContext hardcodedSmartContract
  r <- foldM (\case (Left s) -> \_ -> return $ Left s
                    (Right (es, gs')) -> \msg -> do
                      res <- tryTransaction ctx gs' msg
                      return $ res <&> (\(e, gs'') -> (e:es, gs'')))
       (Right ([], gs)) msgs
  liftIO (putMVar lock ())
  return $ r <&> (\(es, gs) -> (reverse es, gs))

makeBlock :: (MonadIO m) => GlobalState -> [Transaction] -> m ([(Transaction, Event)], [Failure String Transaction], GlobalState)
makeBlock gs msgs = do
  () <- liftIO (takeMVar lock)
  let hardcodedSmartContract = I.Code "workdir/smart-contracts/IncDec/elm.json" ["workdir/smart-contracts/IncDec/src/IncDec.elm","workdir/smart-contracts/IncDec/src/Evergreen.elm","workdir/smart-contracts/IncDec/src/Blockchain.elm","workdir/smart-contracts/IncDec/src/Tx.elm","workdir/smart-contracts/IncDec/src/Reply.elm"] "IncDec"
  ctx <- liftIO $ I.makeContext hardcodedSmartContract
  (sucs, fails, gs') <- foldM (\(suc, fails, gs') msg -> do
                                  res <- tryTransaction ctx gs' msg
                                  case res of
                                    Left err -> return (suc, Failure err msg : fails, gs')
                                    Right (e, gs'') -> return ((msg, e) : suc, fails, gs''))
                        ([], [], gs) msgs
  liftIO (putMVar lock ())
  return (reverse sucs, reverse fails, gs')

mkState :: Int -> I.State
mkState i = I.State $ fromString $ "[" ++ show i ++ "]" -- Wire encoding used by Oak encodes Ints as single-element JSON arrays that contain a Number

initState :: Int -> GlobalState
initState n = GlobalState { instances = Map.fromList . map (\i -> (pack (show i), mkState 0)) $ enumFromTo 0 (n-1)
                          }
