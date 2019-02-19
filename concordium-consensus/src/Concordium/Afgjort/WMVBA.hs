{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}
module Concordium.Afgjort.WMVBA (
    WMVBAMessage(..),
    putWMVBAMessage,
    getWMVBAMessage,
    WMVBAState,
    initialWMVBAState,
    WMVBAInstance(WMVBAInstance),
    WMVBAMonad,
    WMVBAOutputEvent(..),
    WMVBA,
    runWMVBA,
    justifyWMVBAInput,
    receiveWMVBAMessage,
    startWMVBA
) where

import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.List as List
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Serialize.Get

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.ABBA

data WMVBAMessage val party
    = WMVBAFreezeMessage (FreezeMessage val)
    | WMVBAABBAMessage (ABBAMessage party)
    | WMVBAWitnessCreatorMessage val

instance (Show val, Show party) => Show (WMVBAMessage val party) where
    show (WMVBAFreezeMessage (Proposal val)) = "Propose " ++ show val
    show (WMVBAFreezeMessage (Vote v)) = "Vote " ++ show v
    show (WMVBAABBAMessage (Justified phase b ticket)) = "Justified@" ++ show phase ++ ": " ++ show b
    show (WMVBAABBAMessage (CSSSeen phase party b)) = "Seen@" ++ show phase ++ ": " ++ show party ++ "->" ++ show b
    show (WMVBAABBAMessage (CSSDoneReporting phase choices)) = "DoneReporting@" ++ show phase
    show (WMVBAABBAMessage (WeAreDone b)) = "WeAreDone: " ++ show b
    show (WMVBAWitnessCreatorMessage v) = "Witness: " ++ show v

putWMVBAMessage :: Putter val -> Putter party -> Putter (WMVBAMessage val party)
putWMVBAMessage putVal putParty = enc
    where
        enc (WMVBAFreezeMessage (Proposal val)) = putWord8 0 >> putVal val
        enc (WMVBAFreezeMessage (Vote Nothing)) = putWord8 1
        enc (WMVBAFreezeMessage (Vote (Just val))) = putWord8 2 >> putVal val
        enc (WMVBAABBAMessage (Justified phase False ticket)) = putWord8 3 >> putWord32be phase >> S.put ticket
        enc (WMVBAABBAMessage (Justified phase True ticket)) = putWord8 4 >> putWord32be phase >> S.put ticket
        enc (WMVBAABBAMessage (CSSSeen phase party False)) = putWord8 5 >> putWord32be phase >> putParty party
        enc (WMVBAABBAMessage (CSSSeen phase party True)) = putWord8 6 >> putWord32be phase >> putParty party
        enc (WMVBAABBAMessage (CSSDoneReporting phase choices)) = putWord8 7 >> putWord32be phase >> putListOf putParty (fst <$> choseFalse) >> putListOf putParty (fst <$> choseTrue)
            where
                (choseTrue, choseFalse) = List.partition snd $ Map.toAscList choices
        enc (WMVBAABBAMessage (WeAreDone False)) = putWord8 8
        enc (WMVBAABBAMessage (WeAreDone True)) = putWord8 9
        enc (WMVBAWitnessCreatorMessage val) = putWord8 10 >> putVal val

getWMVBAMessage :: (Ord party) => Get val -> Get party -> Get (WMVBAMessage val party)
getWMVBAMessage getVal getParty = getWord8 >>= \case
        0 -> WMVBAFreezeMessage . Proposal <$> getVal
        1 -> return $ WMVBAFreezeMessage (Vote Nothing)
        2 -> WMVBAFreezeMessage . Vote . Just <$> getVal
        3 -> do
            phase <- getWord32be
            ticket <- S.get
            return $ WMVBAABBAMessage (Justified phase False ticket)
        4 -> do
            phase <- getWord32be
            ticket <- S.get
            return $ WMVBAABBAMessage (Justified phase True ticket)
        5 -> do
            phase <- getWord32be
            party <- getParty
            return $ WMVBAABBAMessage (CSSSeen phase party False)
        6 -> do
            phase <- getWord32be
            party <- getParty
            return $ WMVBAABBAMessage (CSSSeen phase party True)
        7 -> do
            phase <- getWord32be
            choseFalse <- getListOf getParty
            choseTrue <- getListOf getParty
            return $ WMVBAABBAMessage $ CSSDoneReporting phase $ Map.fromList $ ((,False) <$> choseFalse) ++ ((,True) <$> choseTrue)
        8 -> return (WMVBAABBAMessage (WeAreDone False))
        9 -> return (WMVBAABBAMessage (WeAreDone True))
        10 -> WMVBAWitnessCreatorMessage <$> getVal
        _ -> fail "Incorrect message type"

data OutcomeState val = OSAwaiting | OSFrozen val | OSABBASuccess | OSDone

data WMVBAState val party sig = WMVBAState {
    _freezeState :: FreezeState val party,
    _abbaState :: ABBAState party,
    _justifiedDecision :: OutcomeState val,
    _justifications :: Map val (Int, Map party sig)
}
makeLenses ''WMVBAState

initialWMVBAState :: WMVBAState val party sig
initialWMVBAState = WMVBAState {
    _freezeState = initialFreezeState,
    _abbaState = initialABBAState,
    _justifiedDecision = OSAwaiting,
    _justifications = Map.empty
}

data WMVBAInstance val party sig = WMVBAInstance {
    baid :: BS.ByteString,
    totalWeight :: Int,
    corruptWeight :: Int,
    partyWeight :: party -> Int,
    publicKeys :: party -> VRF.PublicKey,
    me :: party,
    privateKey :: VRF.KeyPair
}

toFreezeInstance :: WMVBAInstance val party sig -> FreezeInstance party
toFreezeInstance (WMVBAInstance _ totalWeight corruptWeight partyWeight _ me _) = FreezeInstance totalWeight corruptWeight partyWeight me

toABBAInstance :: (ABBAMonad party m, Ord party) => WMVBAInstance val party sig -> ABBAInstance party m
toABBAInstance (WMVBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey) = newABBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey

class (MonadState (WMVBAState val party sig) m, MonadReader (WMVBAInstance val party sig) m) => WMVBAMonad val party sig m where
    sendWMVBAMessage :: WMVBAMessage val party -> m ()
    wmvbaComplete :: Maybe (val, [(party, sig)]) -> m ()

data WMVBAOutputEvent val party sig
    = SendWMVBAMessage (WMVBAMessage val party)
    | WMVBAComplete (Maybe (val, [(party, sig)]))

newtype WMVBA val party sig a = WMVBA {
    runWMVBA' :: RWS (WMVBAInstance val party sig) (Endo [WMVBAOutputEvent val party sig]) (WMVBAState val party sig) a
} deriving (Functor, Applicative, Monad)

runWMVBA :: WMVBA val party sig a -> WMVBAInstance val party sig -> WMVBAState val party sig -> (a, WMVBAState val party sig, [WMVBAOutputEvent val party sig])
runWMVBA z i s = runRWS (runWMVBA' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadReader (WMVBAInstance val party sig) (WMVBA val party sig) where
    ask = WMVBA ask
    reader = WMVBA . reader
    local f = WMVBA . local f . runWMVBA'

instance MonadState (WMVBAState val party sig) (WMVBA val party sig) where
    get = WMVBA get
    put = WMVBA . put
    state = WMVBA . state

instance WMVBAMonad val party sig (WMVBA val party sig) where
    sendWMVBAMessage = WMVBA . tell . Endo . (:) . SendWMVBAMessage
    wmvbaComplete = WMVBA . tell . Endo . (:) . WMVBAComplete

liftFreeze :: (WMVBAMonad val party sig m, Ord party) => Freeze val party a -> m a
liftFreeze a = do
        freezestate <- use freezeState
        freezecontext <- toFreezeInstance <$> ask
        let (r, freezestate', evs) = runFreeze a freezecontext freezestate
        freezeState .= freezestate'
        handleEvents evs
        return r
    where
        handleEvents [] = return ()
        handleEvents (SendFreezeMessage msg : r) = do
            sendWMVBAMessage (WMVBAFreezeMessage msg)
            handleEvents r
        handleEvents (Frozen c : r) = do
            inst <- ask
            liftABBA (beginABBA (toABBAInstance inst) (isJust c))
            handleEvents r
        handleEvents (DecisionJustified c : r) = do
            inst <- ask
            liftABBA (justifyABBAChoice (toABBAInstance inst) (isJust c))
            forM_ c $ \v ->
                use justifiedDecision >>= \case
                    OSAwaiting -> justifiedDecision .= OSFrozen v
                    OSABBASuccess -> do
                        justifiedDecision .= OSDone
                        sendWMVBAMessage (WMVBAWitnessCreatorMessage v)
                    _ -> return ()
            handleEvents r

liftABBA :: (WMVBAMonad val party sig m) => ABBA party a -> m a
liftABBA a = do
        aBBAState <- use abbaState
        let (r, aBBAState', evs) = runABBA a aBBAState
        abbaState .= aBBAState'
        handleEvents evs
        return r
    where
        handleEvents [] = return ()
        handleEvents (SendABBAMessage msg : r) = do
            sendWMVBAMessage (WMVBAABBAMessage msg)
            handleEvents r
        handleEvents (ABBAComplete c : r) = do
            if c then
                use justifiedDecision >>= \case
                    OSAwaiting -> justifiedDecision .= OSABBASuccess
                    OSFrozen v -> do
                        justifiedDecision .= OSDone
                        sendWMVBAMessage (WMVBAWitnessCreatorMessage v)
                    _ -> return ()
            else
                wmvbaComplete Nothing
            handleEvents r

-- |Record that an input is justified.
justifyWMVBAInput :: forall val party sig m. (WMVBAMonad val party sig m, Ord val, Ord party) => val -> m ()
justifyWMVBAInput val = liftFreeze $ justifyCandidate val

-- |Handle an incoming 'WMVBAMessage'.
receiveWMVBAMessage :: (WMVBAMonad val party sig m, Ord val, Ord party, Eq sig) => party -> sig -> WMVBAMessage val party -> m ()
receiveWMVBAMessage src _ (WMVBAFreezeMessage msg) = liftFreeze $ receiveFreezeMessage src msg
receiveWMVBAMessage src _ (WMVBAABBAMessage msg) = do
        inst <- toABBAInstance <$> ask
        liftABBA $ receiveABBAMessage inst src msg
receiveWMVBAMessage src sig (WMVBAWitnessCreatorMessage v) = do
        WMVBAInstance{..} <- ask
        (wt, m) <- use (justifications . at v . non (0, Map.empty))
        when (isNothing $ Map.lookup src m) $ do
            Just (newWeight, newMap) <- justifications . at v <.= Just (wt + partyWeight src, Map.insert src sig m)
            when (newWeight > corruptWeight) $
                wmvbaComplete (Just (v, Map.toList newMap))

-- |Start the WMVBA for us with a given input.  This should only be called once
-- per instance, and the input should already be justified.
startWMVBA :: (WMVBAMonad val party sig m) => val -> m ()
startWMVBA val = sendWMVBAMessage (WMVBAFreezeMessage (Proposal val))