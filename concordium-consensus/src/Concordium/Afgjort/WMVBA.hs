{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}
module Concordium.Afgjort.WMVBA (
    WMVBAMessage(..),
    messageValues,
    messageParties,
    WMVBAState,
    initialWMVBAState,
    WMVBAInstance(WMVBAInstance),
    WMVBAMonad(..),
    WMVBAOutputEvent(..),
    WMVBA,
    runWMVBA,
    justifyWMVBAInput,
    receiveWMVBAMessage,
    startWMVBA,
    -- * For testing
    _freezeState
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
import Concordium.Afgjort.Types
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.ABBA

data WMVBAMessage
    = WMVBAFreezeMessage FreezeMessage
    | WMVBAABBAMessage ABBAMessage
    | WMVBAWitnessCreatorMessage Val
    deriving (Eq, Ord)

messageValues :: WMVBAMessage -> [Val]
messageValues (WMVBAFreezeMessage (Proposal val)) = [val]
messageValues (WMVBAFreezeMessage (Vote (Just val))) = [val]
messageValues (WMVBAFreezeMessage (Vote Nothing)) = []
messageValues (WMVBAABBAMessage _) = []
messageValues (WMVBAWitnessCreatorMessage val) = [val]

messageParties :: WMVBAMessage -> [Party]
messageParties (WMVBAFreezeMessage _) = []
messageParties (WMVBAABBAMessage (CSSSeen _ party _)) = [party]
messageParties (WMVBAABBAMessage (CSSDoneReporting _ choices)) = Map.keys choices
messageParties (WMVBAABBAMessage _) = []
messageParties (WMVBAWitnessCreatorMessage _) = []

instance Show WMVBAMessage where
    show (WMVBAFreezeMessage (Proposal val)) = "Propose " ++ show val
    show (WMVBAFreezeMessage (Vote v)) = "Vote " ++ show v
    show (WMVBAABBAMessage (Justified phase b _ticket)) = "Justified@" ++ show phase ++ ": " ++ show b
    show (WMVBAABBAMessage (CSSSeen phase party b)) = "Seen@" ++ show phase ++ ": " ++ show party ++ "->" ++ show b
    show (WMVBAABBAMessage (CSSDoneReporting phase _choices)) = "DoneReporting@" ++ show phase
    show (WMVBAABBAMessage (WeAreDone b)) = "WeAreDone: " ++ show b
    show (WMVBAWitnessCreatorMessage v) = "Witness: " ++ show v

instance S.Serialize WMVBAMessage where
    put (WMVBAFreezeMessage (Proposal val)) = putWord8 0 >> putVal val
    put (WMVBAFreezeMessage (Vote Nothing)) = putWord8 1
    put (WMVBAFreezeMessage (Vote (Just val))) = putWord8 2 >> putVal val
    put (WMVBAABBAMessage (Justified phase False ticket)) = putWord8 3 >> putWord32be phase >> S.put ticket
    put (WMVBAABBAMessage (Justified phase True ticket)) = putWord8 4 >> putWord32be phase >> S.put ticket
    put (WMVBAABBAMessage (CSSSeen phase party False)) = putWord8 5 >> putWord32be phase >> putParty party
    put (WMVBAABBAMessage (CSSSeen phase party True)) = putWord8 6 >> putWord32be phase >> putParty party
    put (WMVBAABBAMessage (CSSDoneReporting phase choices)) = putWord8 7 >> putWord32be phase >> putListOf putParty (fst <$> choseFalse) >> putListOf putParty (fst <$> choseTrue)
        where
            (choseTrue, choseFalse) = List.partition snd $ Map.toAscList choices
    put (WMVBAABBAMessage (WeAreDone False)) = putWord8 8
    put (WMVBAABBAMessage (WeAreDone True)) = putWord8 9
    put (WMVBAWitnessCreatorMessage val) = putWord8 10 >> putVal val

    get = getWord8 >>= \case
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

data OutcomeState val = OSAwaiting | OSFrozen val | OSABBASuccess | OSDone deriving (Show)

data WMVBAState sig = WMVBAState {
    _freezeState :: FreezeState,
    _abbaState :: ABBAState,
    _justifiedDecision :: OutcomeState Val,
    _justifications :: Map Val (Int, Map Party sig)
} deriving (Show)
makeLenses ''WMVBAState

initialWMVBAState :: WMVBAState sig
initialWMVBAState = WMVBAState {
    _freezeState = initialFreezeState,
    _abbaState = initialABBAState,
    _justifiedDecision = OSAwaiting,
    _justifications = Map.empty
}

data WMVBAInstance sig = WMVBAInstance {
    baid :: BS.ByteString,
    totalWeight :: Int,
    corruptWeight :: Int,
    partyWeight :: Party -> Int,
    publicKeys :: Party -> VRF.PublicKey,
    me :: Party,
    privateKey :: VRF.KeyPair
}

toFreezeInstance :: WMVBAInstance sig -> FreezeInstance
toFreezeInstance (WMVBAInstance _ totalWeight corruptWeight partyWeight _ me _) = FreezeInstance totalWeight corruptWeight partyWeight me

toABBAInstance :: WMVBAInstance sig -> ABBAInstance
toABBAInstance (WMVBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey) = ABBAInstance baid totalWeight corruptWeight partyWeight pubKeys me privateKey

class (MonadState (WMVBAState sig) m, MonadReader (WMVBAInstance sig) m, MonadIO m) => WMVBAMonad sig m where
    sendWMVBAMessage :: WMVBAMessage -> m ()
    wmvbaComplete :: Maybe (Val, [(Party, sig)]) -> m ()

data WMVBAOutputEvent sig
    = SendWMVBAMessage WMVBAMessage
    | WMVBAComplete (Maybe (Val, [(Party, sig)]))

newtype WMVBA sig a = WMVBA {
    runWMVBA' :: RWST (WMVBAInstance sig) (Endo [WMVBAOutputEvent sig]) (WMVBAState sig) IO a
} deriving (Functor, Applicative, Monad, MonadIO)

runWMVBA :: WMVBA sig a -> WMVBAInstance sig -> WMVBAState sig -> IO (a, WMVBAState sig, [WMVBAOutputEvent sig])
runWMVBA z i s = runRWST (runWMVBA' z) i s <&> _3 %~ (\(Endo f) -> f [])

instance MonadReader (WMVBAInstance sig) (WMVBA sig) where
    ask = WMVBA ask
    reader = WMVBA . reader
    local f = WMVBA . local f . runWMVBA'

instance MonadState (WMVBAState sig) (WMVBA sig) where
    get = WMVBA get
    put = WMVBA . put
    state = WMVBA . state

instance WMVBAMonad sig (WMVBA sig) where
    sendWMVBAMessage = WMVBA . tell . Endo . (:) . SendWMVBAMessage
    wmvbaComplete = WMVBA . tell . Endo . (:) . WMVBAComplete

liftFreeze :: (WMVBAMonad sig m) => Freeze a -> m a
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
            liftABBA (beginABBA (isJust c))
            handleEvents r
        handleEvents (DecisionJustified c : r) = do
            liftABBA (justifyABBAChoice (isJust c))
            forM_ c $ \v ->
                use justifiedDecision >>= \case
                    OSAwaiting -> justifiedDecision .= OSFrozen v
                    OSABBASuccess -> do
                        justifiedDecision .= OSDone
                        sendWMVBAMessage (WMVBAWitnessCreatorMessage v)
                    _ -> return ()
            handleEvents r

liftABBA :: (WMVBAMonad sig m) => ABBA a -> m a
liftABBA a = do
        aBBAInstance <- asks toABBAInstance
        aBBAState <- use abbaState
        (r, aBBAState', evs) <- liftIO $ runABBA a aBBAInstance aBBAState
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
justifyWMVBAInput :: forall sig m. (WMVBAMonad sig m) => Val -> m ()
justifyWMVBAInput val = liftFreeze $ justifyCandidate val

-- |Handle an incoming 'WMVBAMessage'.
receiveWMVBAMessage :: (WMVBAMonad sig m, Eq sig) => Party -> sig -> WMVBAMessage -> m ()
receiveWMVBAMessage src _ (WMVBAFreezeMessage msg) = liftFreeze $ receiveFreezeMessage src msg
receiveWMVBAMessage src _ (WMVBAABBAMessage msg) = do
        liftABBA $ receiveABBAMessage src msg
receiveWMVBAMessage src sig (WMVBAWitnessCreatorMessage v) = do
        WMVBAInstance{..} <- ask
        (wt, m) <- use (justifications . at v . non (0, Map.empty))
        when (isNothing $ Map.lookup src m) $ do
            let
                newWeight = wt + partyWeight src
                newMap = Map.insert src sig m
            justifications . at v .= Just (newWeight, newMap)
            when (newWeight > corruptWeight) $
                wmvbaComplete (Just (v, Map.toList newMap))

-- |Start the WMVBA for us with a given input.  This should only be called once
-- per instance, and the input should already be justified.
startWMVBA :: (WMVBAMonad sig m) => Val -> m ()
startWMVBA val = sendWMVBAMessage (WMVBAFreezeMessage (Proposal val))