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
    isJustifiedWMVBAInput,
    receiveWMVBAMessage,
    startWMVBA,
    putWMVBAMessageBody,
    WMVBASummary(..),
    wmvbaSummary,
    putWMVBASummary,
    getWMVBASummary,
    processWMVBASummary,
    wmvbaFailedSummary,
    wmvbaWADBot,
    wmvbaWADBotMessage,
    -- * For testing
    _freezeState
) where

import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Serialize as S
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Bits
import Data.Word

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.CSS.NominationSet
import Concordium.Afgjort.PartyMap (PartyMap)
import qualified Concordium.Afgjort.PartyMap as PM

data WMVBAMessage
    = WMVBAFreezeMessage !FreezeMessage
    | WMVBAABBAMessage !ABBAMessage
    | WMVBAWitnessCreatorMessage !Val
    deriving (Eq, Ord)

messageValues :: WMVBAMessage -> [Val]
messageValues (WMVBAFreezeMessage (Proposal val)) = [val]
messageValues (WMVBAFreezeMessage (Vote (Just val))) = [val]
messageValues (WMVBAFreezeMessage (Vote Nothing)) = []
messageValues (WMVBAABBAMessage _) = []
messageValues (WMVBAABBAMessage _) = []
messageValues (WMVBAWitnessCreatorMessage val) = [val]

messageParties :: WMVBAMessage -> [Party]
messageParties (WMVBAFreezeMessage _) = []
messageParties (WMVBAABBAMessage (CSSSeen _ ns)) = fst <$> nominationSetToList ns
messageParties (WMVBAABBAMessage (CSSDoneReporting _ choices)) = fst <$> nominationSetToList choices
messageParties (WMVBAABBAMessage _) = []
messageParties (WMVBAWitnessCreatorMessage _) = []

instance Show WMVBAMessage where
    show (WMVBAFreezeMessage (Proposal val)) = "Propose " ++ show val
    show (WMVBAFreezeMessage (Vote v)) = "Vote " ++ show v
    show (WMVBAABBAMessage (Justified phase b _ticket)) = "Justified@" ++ show phase ++ ": " ++ show b
    show (WMVBAABBAMessage (CSSSeen phase ns)) = "Seen@" ++ show phase ++ ": " ++ show ns
    show (WMVBAABBAMessage (CSSDoneReporting phase _choices)) = "DoneReporting@" ++ show phase
    show (WMVBAABBAMessage (WeAreDone b)) = "WeAreDone: " ++ show b
    show (WMVBAWitnessCreatorMessage v) = "Witness: " ++ show v

-- |Serialize the part of a 'WMVBAMessage' that should be signed.
-- (This is everything except the ticket in a 'Justified' message.)
putWMVBAMessageBody :: WMVBAMessage -> Put
putWMVBAMessageBody (WMVBAFreezeMessage (Proposal val)) = putWord8 0 >> putVal val
putWMVBAMessageBody (WMVBAFreezeMessage (Vote Nothing)) = putWord8 1
putWMVBAMessageBody (WMVBAFreezeMessage (Vote (Just val))) = putWord8 2 >> putVal val
putWMVBAMessageBody (WMVBAABBAMessage (Justified phase False _)) = putWord8 3 >> putWord32be phase
putWMVBAMessageBody (WMVBAABBAMessage (Justified phase True _)) = putWord8 4 >> putWord32be phase
putWMVBAMessageBody (WMVBAABBAMessage (CSSSeen phase ns)) = putWord8 tag >> putWord32be phase >> putUntaggedNominationSet ns
    where
        tag = case nomTag ns of
            NSEmpty -> error "Empty set for Seen message"      -- Should not be possible
            NSTop -> 5
            NSBot -> 6
            NSBoth -> 7
putWMVBAMessageBody (WMVBAABBAMessage (CSSDoneReporting phase choices)) = putWord8 tag >> putWord32be phase >> putUntaggedNominationSet choices
    where
        tag = case nomTag choices of
            NSEmpty -> error "Empty set for DoneReporting message"      -- Should not be possible
            NSTop -> 8
            NSBot -> 9
            NSBoth -> 10
putWMVBAMessageBody (WMVBAABBAMessage (WeAreDone False)) = putWord8 11
putWMVBAMessageBody (WMVBAABBAMessage (WeAreDone True)) = putWord8 12
putWMVBAMessageBody (WMVBAWitnessCreatorMessage val) = putWord8 13 >> putVal val

instance S.Serialize WMVBAMessage where
    put m@(WMVBAABBAMessage (Justified _ _ ticket)) = putWMVBAMessageBody m >> S.put ticket
    put m = putWMVBAMessageBody m

    get = getWord8 >>= \case
        0 -> WMVBAFreezeMessage . Proposal <$> getVal
        1 -> return $ WMVBAFreezeMessage (Vote Nothing)
        2 -> WMVBAFreezeMessage . Vote . Just <$> getVal
        3 -> do
            phase <- getWord32be
            ticket <- S.get
            return $! WMVBAABBAMessage (Justified phase False ticket)
        4 -> do
            phase <- getWord32be
            ticket <- S.get
            return $! WMVBAABBAMessage (Justified phase True ticket)
        5 -> do
            phase <- getWord32be
            ns <- getUntaggedNominationSet NSTop
            return $! WMVBAABBAMessage (CSSSeen phase ns)
        6 -> do
            phase <- getWord32be
            ns <- getUntaggedNominationSet NSBot
            return $! WMVBAABBAMessage (CSSSeen phase ns)
        7 -> do
            phase <- getWord32be
            ns <- getUntaggedNominationSet NSBoth
            return $! WMVBAABBAMessage (CSSSeen phase ns)
        8 -> do
            phase <- getWord32be
            choices <- getUntaggedNominationSet NSTop
            return $! WMVBAABBAMessage $ CSSDoneReporting phase choices
        9 -> do
            phase <- getWord32be
            choices <- getUntaggedNominationSet NSBot
            return $! WMVBAABBAMessage $ CSSDoneReporting phase choices
        10 -> do
            phase <- getWord32be
            choices <- getUntaggedNominationSet NSBoth
            return $! WMVBAABBAMessage $ CSSDoneReporting phase choices
        11 -> return (WMVBAABBAMessage (WeAreDone False))
        12 -> return (WMVBAABBAMessage (WeAreDone True))
        13 -> WMVBAWitnessCreatorMessage <$> getVal
        _ -> fail "Incorrect message type"

data OutcomeState = OSAwaiting | OSFrozen Val | OSABBASuccess | OSDone Val deriving (Show)

outcomeVal :: OutcomeState -> Maybe Val
outcomeVal (OSFrozen v) = Just v
outcomeVal (OSDone v) = Just v
outcomeVal _ = Nothing

data WMVBAState sig = WMVBAState {
    _freezeState :: FreezeState sig,
    _abbaState :: ABBAState sig,
    _justifiedDecision :: OutcomeState,
    _justifications :: Map Val (PartyMap sig) -- TODO: change to also hold BlsSig
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
    totalWeight :: VoterPower,
    corruptWeight :: VoterPower,
    partyWeight :: Party -> VoterPower,
    maxParty :: Party,
    publicKeys :: Party -> VRF.PublicKey,
    me :: Party,
    privateKey :: VRF.KeyPair
    -- TODO: add Blskeys
}

toFreezeInstance :: WMVBAInstance sig -> FreezeInstance
toFreezeInstance (WMVBAInstance _ totalWeight corruptWeight partyWeight _ _ me _) = FreezeInstance totalWeight corruptWeight partyWeight me

toABBAInstance :: WMVBAInstance sig -> ABBAInstance
toABBAInstance (WMVBAInstance baid totalWeight corruptWeight partyWeight maxParty pubKeys me privateKey) = ABBAInstance baid totalWeight corruptWeight partyWeight maxParty pubKeys me privateKey

class (MonadState (WMVBAState sig) m, MonadReader (WMVBAInstance sig) m, MonadIO m) => WMVBAMonad sig m where
    sendWMVBAMessage :: WMVBAMessage -> m ()
    wmvbaComplete :: Maybe (Val, [(Party, sig)]) -> m () -- TODO: change to new finalproof

data WMVBAOutputEvent sig
    = SendWMVBAMessage WMVBAMessage
    | WMVBAComplete (Maybe (Val, [(Party, sig)])) -- TODO: change to new finalproof

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

liftFreeze :: (WMVBAMonad sig m) => Freeze sig a -> m a -- add BLS sig here (maybe)
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
                        justifiedDecision .= OSDone v
                        sendWMVBAMessage (WMVBAWitnessCreatorMessage v)
                    _ -> return ()
            handleEvents r

liftABBA :: (WMVBAMonad sig m) => ABBA sig a -> m a -- add BLS sign here
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
                        justifiedDecision .= OSDone v
                        sendWMVBAMessage (WMVBAWitnessCreatorMessage v)
                    _ -> return ()
            else
                wmvbaComplete Nothing
            handleEvents r

-- |Record that an input is justified.
justifyWMVBAInput :: forall sig m. (WMVBAMonad sig m) => Val -> m ()
justifyWMVBAInput val = liftFreeze $ justifyCandidate val

-- |Determine if an input is justified.
isJustifiedWMVBAInput :: forall sig m. (WMVBAMonad sig m) => Val -> m Bool
isJustifiedWMVBAInput val = liftFreeze $ isProposalJustified val

-- |Handle an incoming 'WMVBAMessage'.
receiveWMVBAMessage :: (WMVBAMonad sig m, Eq sig) => Party -> sig -> WMVBAMessage -> m ()
receiveWMVBAMessage src sig (WMVBAFreezeMessage msg) = liftFreeze $ receiveFreezeMessage src msg sig
receiveWMVBAMessage src sig (WMVBAABBAMessage msg) = do
        liftABBA $ receiveABBAMessage src msg sig
receiveWMVBAMessage src sig (WMVBAWitnessCreatorMessage v) = do -- receive BLS sig here
        WMVBAInstance{..} <- ask
        newJV <- justifications . at v . non PM.empty <%= PM.insert src (partyWeight src) sig -- add BLS sig to justifications
        when (PM.weight newJV > corruptWeight) $
            wmvbaComplete (Just (v, PM.toList newJV))

-- |Start the WMVBA for us with a given input.  This should only be called once
-- per instance, and the input should already be justified.
startWMVBA :: (WMVBAMonad sig m) => Val -> m ()
startWMVBA val = sendWMVBAMessage (WMVBAFreezeMessage (Proposal val))

data WMVBASummary sig = WMVBASummary {
    summaryFreeze :: Maybe (FreezeSummary sig),
    summaryABBA :: Maybe (ABBASummary sig),
    -- |If freeze has completed and we have witness creation signatures,
    -- then this records them.
    summaryWitnessCreation :: Maybe (Val, Map Party sig)
}

putWMVBASummary :: (S.Serialize sig) => Party -> WMVBASummary sig -> Put
putWMVBASummary maxParty WMVBASummary{..} = do
        putWord8 descByte
        forM_ summaryFreeze $ putFreezeSummary maxParty
        forM_ summaryABBA $ putABBASummary maxParty
        forM_ summaryWitnessCreation $ \(v, m) -> do
            putVal v
            putPartyMap maxParty m
    where
        descByte = (if isJust summaryFreeze then flip setBit 0 else id) $
                    (if isJust summaryABBA then flip setBit 1 else id) $
                    (if isJust summaryWitnessCreation then flip setBit 2 else id)
                    (0 :: Word8)

getWMVBASummary :: (S.Serialize sig) => Party -> Get (WMVBASummary sig)
getWMVBASummary maxParty = do
        descByte <- getWord8
        summaryFreeze <- if testBit descByte 0 then
                Just <$> getFreezeSummary maxParty
            else
                return Nothing
        summaryABBA <- if testBit descByte 1 then
                Just <$> getABBASummary maxParty
            else
                return Nothing
        summaryWitnessCreation <- if testBit descByte 2 then do
                v <- getVal
                m <- getPartyMap maxParty
                return (Just (v, m))
            else
                return Nothing
        return WMVBASummary{..}

wmvbaSummary :: SimpleGetter (WMVBAState sig) (WMVBASummary sig)
wmvbaSummary = to ws
    where
        ws WMVBAState{..} = WMVBASummary{..}
            where
                summaryFreeze = if _abbaState ^. abbaOutcome == Just False then Nothing else Just (_freezeState ^. freezeSummary)
                summaryABBA = if _freezeState ^. freezeCompleted then Just (_abbaState ^. abbaSummary) else Nothing
                summaryWitnessCreation = outcomeVal _justifiedDecision >>= wc
                wc v = (v,) . PM.partyMap <$> (_justifications ^? ix v)

processWMVBASummary :: (WMVBAMonad sig m, Eq sig) => WMVBASummary sig -> (Party -> WMVBAMessage -> sig -> Bool) -> m CatchUpResult
processWMVBASummary WMVBASummary{..} checkSig = do
        -- Process the Freeze summary
        freezeCUR <- forM summaryFreeze $ \fs -> liftFreeze (processFreezeSummary fs checkFreezeSig)
        -- Process the ABBA summary
        abbaBehind <- forM summaryABBA $ \asum -> liftABBA (processABBASummary asum checkABBASig)
        -- Process the Witness Creation messages
        WMVBASummary{summaryWitnessCreation=myWC} <- use wmvbaSummary
        wcBehind <- case (summaryWitnessCreation, myWC) of
            (Just (v, m), Nothing) -> do
                forM_ (Map.toList . Map.filterWithKey (checkWCSig v) $ m) $ \(p, s) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage v)
                return False
            (Just (v, m), Just (v', m')) ->
                if v == v' then do
                    forM_ (Map.toList . Map.filterWithKey (checkWCSig v) . (`Map.difference` m') $ m) $
                        \(p, s) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage v)
                    -- If we have some signatures they don't, then they're behind
                    return $ not $ null $ Map.differenceWithKey (\party x y -> if x == y || checkWCSig v party y then Nothing else Just x) m' m
                else do
                    forM_ (Map.toList . Map.filterWithKey (checkWCSig v) $ m) $ \(p, s) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage v)
                    return True
            (Nothing, Just _) -> return True
            _ -> return False
        WMVBASummary{summaryFreeze=myFreeze,summaryABBA=myABBA} <- use wmvbaSummary
        -- The result is behind if:
        -- 1. Freeze is behind, unless ABBA completed with result Bottom; or
        -- 2. ABBA is behind unless the round is completed (this is not exactly what we check); or
        -- 3. Witness Creation is behind
        return $! CatchUpResult {
            curBehind = wcBehind || maybe (isJust myFreeze) curBehind freezeCUR || fromMaybe (isJust myABBA) abbaBehind,
            curSkovCatchUp = maybe False curSkovCatchUp freezeCUR
        }
    where
        checkFreezeSig p fm sig = checkSig p (WMVBAFreezeMessage fm) sig
        checkABBASig p am sig = checkSig p (WMVBAABBAMessage am) sig
        checkWCSig v p sig = checkSig p (WMVBAWitnessCreatorMessage v) sig

-- |Create a 'WMVBASummary' from a collection of signatures on @WeAreDone False@.
wmvbaFailedSummary :: Map Party sig -> WMVBASummary sig
wmvbaFailedSummary wadBotSigs = WMVBASummary Nothing (Just (ABBASummary [] Map.empty wadBotSigs)) Nothing

-- |Get the collection of signatures on @WeAreDone False@.
wmvbaWADBot :: WMVBAState sig -> Map Party sig
wmvbaWADBot = PM.partyMap . _botWeAreDone . _abbaState

wmvbaWADBotMessage :: WMVBAMessage
wmvbaWADBotMessage = WMVBAABBAMessage (WeAreDone False)
