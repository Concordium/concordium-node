{-# LANGUAGE
    TemplateHaskell,
    ScopedTypeVariables,
    GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
module Concordium.Afgjort.WMVBA (
    WMVBAMessage(..),
    messageValues,
    messageParties,
    WMVBAState,
    initialWMVBAState,
    WMVBAInstance(WMVBAInstance),
    WMVBAMonad(..),
    DelayedABBAAction,
    WMVBAOutputEvent(..),
    WMVBA,
    runWMVBA,
    justifyWMVBAInput,
    isJustifiedWMVBAInput,
    receiveWMVBAMessage,
    startWMVBA,
    triggerWMVBAAction,
    putWMVBAMessageBody,
    WMVBASummary(..),
    wmvbaSummary,
    putWMVBASummary,
    getWMVBASummary,
    processWMVBASummary,
    wmvbaFailedSummary,
    wmvbaWADBot,
    wmvbaWADBotMessage,
    witnessMessage,
    createAggregateSig,
    OutputWitnesses(..),
    getOutputWitnesses,
    uncheckedOutputWitnesses,
    -- * Passive
    WMVBAPassiveState(..),
    initialWMVBAPassiveState,
    passiveReceiveWMVBAMessage,
    passiveReceiveWMVBASignatures,
    passiveGetOutputWitnesses,
    -- * For testing
    _freezeState,
    findCulprits,
    makeWMVBAWitnessCreatorMessage,
    WMVBAState(WMVBAState)
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
import Data.Time.Clock

import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Afgjort.Types
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import Concordium.Afgjort.PartyMap (PartyMap)
import Concordium.Afgjort.PartySet (PartySet)
import qualified Concordium.Afgjort.PartyMap as PM
import qualified Concordium.Afgjort.PartySet as PS

-- |Baseline estimate of network delay.
-- TODO: Do not hard code this.
deltaABBA :: NominalDiffTime
deltaABBA = 0.1

data WMVBAMessage
    = WMVBAFreezeMessage !FreezeMessage
    | WMVBAABBAMessage !ABBAMessage
    | WMVBAWitnessCreatorMessage !(Val, Bls.Signature)
    deriving (Eq, Ord)

messageValues :: WMVBAMessage -> Maybe Val
messageValues (WMVBAFreezeMessage (Proposal val)) = Just val
messageValues (WMVBAFreezeMessage (Vote (Just val))) = Just val
messageValues (WMVBAFreezeMessage (Vote Nothing)) = Nothing
messageValues (WMVBAABBAMessage _) = Nothing
messageValues (WMVBAWitnessCreatorMessage (val, _)) = Just val

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
putWMVBAMessageBody (WMVBAWitnessCreatorMessage (val, blssig)) = putWord8 13 >> putVal val >> S.put blssig

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
        13 -> WMVBAWitnessCreatorMessage <$> (getTwoOf getVal S.get)
        _ -> fail "Incorrect message type"

data OutcomeState = OSAwaiting | OSFrozen Val | OSABBASuccess | OSDone Val deriving (Show)

outcomeVal :: OutcomeState -> Maybe Val
outcomeVal (OSFrozen v) = Just v
outcomeVal (OSDone v) = Just v
outcomeVal _ = Nothing

-- |The collection of signatures gathered by finalization.
--
-- INVARIANT: `knownGoodSigs` and `unknownSigs` must be disjoint
-- (in their domains), and both should also be disjoint from
-- `knownBadSigs`.
data OutputWitnesses = OutputWitnesses {
    -- |The signatures that are known to be valid.
    knownGoodSigs :: !(Map Party Bls.Signature),
    -- |The signatures that have not been checked.
    unknownSigs :: !(Map Party Bls.Signature),
    -- |The parties that are known to have sent bad signatures.
    knownBadSigs :: !BitSet.BitSet
}

uncheckedOutputWitnesses :: Map Party Bls.Signature -> OutputWitnesses
uncheckedOutputWitnesses unknownSigs = OutputWitnesses{
        knownGoodSigs = Map.empty,
        knownBadSigs = BitSet.empty,
        ..
    }

data WMVBAState sig = WMVBAState {
    _freezeState :: FreezeState sig,
    _abbaState :: ABBAState sig,
    _justifiedDecision :: OutcomeState,
    -- TODO: separate out: known good, known bad, and unchecked signatures.
    -- Possibly, we just store justifications with a flag indicating whether
    -- the BLS signature has been checked.
    _justifications :: Map Val (PartyMap (sig, Bls.Signature)),
    _badJustifications :: Map Val (PartySet)
} deriving (Show)
makeLenses ''WMVBAState

initialWMVBAState :: WMVBAState sig
initialWMVBAState = WMVBAState {
    _freezeState = initialFreezeState,
    _abbaState = initialABBAState,
    _justifiedDecision = OSAwaiting,
    _justifications = Map.empty,
    _badJustifications = Map.empty
}

data WMVBAInstance = WMVBAInstance {
    baid :: BS.ByteString,
    totalWeight :: VoterPower,
    corruptWeight :: VoterPower,
    partyWeight :: Party -> VoterPower,
    maxParty :: Party,
    publicKeys :: Party -> VRF.PublicKey,
    me :: Party,
    privateKey :: VRF.KeyPair,
    publicBlsKeys :: Party -> Bls.PublicKey,
    privateBlsKey :: Bls.SecretKey
}

toFreezeInstance :: WMVBAInstance -> FreezeInstance
toFreezeInstance (WMVBAInstance _ totalWeight corruptWeight partyWeight _ _ me _ _ _) = FreezeInstance totalWeight corruptWeight partyWeight me

toABBAInstance :: WMVBAInstance -> ABBAInstance
toABBAInstance (WMVBAInstance baid totalWeight corruptWeight partyWeight maxParty pubKeys me privateKey _ _) =
  ABBAInstance baid totalWeight corruptWeight partyWeight maxParty pubKeys me privateKey

class (MonadState (WMVBAState sig) m, MonadReader WMVBAInstance m, MonadIO m) => WMVBAMonad sig m where
    sendWMVBAMessage :: WMVBAMessage -> m ()
    wmvbaComplete :: Maybe (Val, ([Party], Bls.Signature)) -> m ()
    wmvbaDelay :: NominalDiffTime -> DelayedABBAAction -> m ()

data WMVBAOutputEvent sig
    = SendWMVBAMessage WMVBAMessage
    | WMVBAComplete (Maybe (Val, ([Party], Bls.Signature)))
    | WMVBADelay NominalDiffTime DelayedABBAAction
    deriving (Show)

newtype WMVBA sig a = WMVBA {
    runWMVBA' :: RWST WMVBAInstance (Endo [WMVBAOutputEvent sig]) (WMVBAState sig) IO a
} deriving (Functor, Applicative, Monad, MonadIO)

runWMVBA :: WMVBA sig a -> WMVBAInstance -> WMVBAState sig -> IO (a, WMVBAState sig, [WMVBAOutputEvent sig])
runWMVBA z i s = runRWST (runWMVBA' z) i s <&> _3 %~ (\(Endo f) -> f [])

instance MonadReader WMVBAInstance (WMVBA sig) where
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
    wmvbaDelay delay action = WMVBA $ tell $ Endo (WMVBADelay delay action :)

liftFreeze :: (WMVBAMonad sig m) => Freeze sig a -> m a
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
                        WMVBAInstance{..} <- ask
                        sendWMVBAMessage (makeWMVBAWitnessCreatorMessage baid v privateBlsKey)
                    _ -> return ()
            handleEvents r

liftABBA :: (WMVBAMonad sig m) => ABBA sig a -> m a
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
                        WMVBAInstance{..} <- ask
                        sendWMVBAMessage (makeWMVBAWitnessCreatorMessage baid v privateBlsKey)
                    _ -> return ()
            else
                wmvbaComplete Nothing
            handleEvents r
        handleEvents (ABBADelay ticks action : r) = do
            wmvbaDelay (deltaABBA * fromIntegral ticks) action
            handleEvents r

witnessMessage :: BS.ByteString -> Val -> BS.ByteString
witnessMessage baid v = baid <> S.encode v

makeWMVBAWitnessCreatorMessage :: BS.ByteString -> Val -> Bls.SecretKey -> WMVBAMessage
makeWMVBAWitnessCreatorMessage baid v privateBlsKey = WMVBAWitnessCreatorMessage (v, Bls.sign (witnessMessage baid v) privateBlsKey)

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
receiveWMVBAMessage src sig (WMVBAWitnessCreatorMessage (v, blssig)) = do
        wi@WMVBAInstance{..} <- ask
        -- add the (v, (sig, blssig)) to the list of justifications under key src
        newJV <- justifications . at v . non PM.empty <%= PM.insert src (partyWeight src) (sig, blssig)
        badJV <- use $ badJustifications . at v . non PS.empty
        -- When justifications combined weight minus the weight of the bad justifications exceeds corruptWeight,
        -- we can attempt to create the bls aggregate signature
        when ((PM.weight newJV) - (PS.weight badJV) > corruptWeight) $ do
          -- TODO: optimize, this is just a first draft
          let (proofData, newBadJV) = createAggregateSig wi v (snd <$> newJV) badJV
          badJustifications . at v . non PS.empty .= newBadJV
          forM_ proofData $ \proof -> wmvbaComplete (Just (v, proof))
          -- TODO: check if finalization can still finish. If enough bad justifications has been seen,
          -- finalization may not be able to finish.

-- |Construct an aggregate signature. This aggregates all of the
-- valid signatures (except those already marked as bad). It returns
-- the aggregated signature and an updated set of bad parties. No signature
-- is returned if enough bad justifications are found that the good ones no
-- longer exceed the corruption threshold
createAggregateSig ::
    WMVBAInstance
    -> Val
    -- ^Value chosen
    -> PartyMap Bls.Signature
    -- ^Parties' BLS signatures
    -> PartySet
    -- ^Parties with known bad signatures
    -> (Maybe ([Party], Bls.Signature), PartySet)
createAggregateSig WMVBAInstance{..} v allJV badJV
        | Bls.verifyAggregate toSign keys aggSig =
            (Just (fst <$> goodJustifications, aggSig), badJV)
        -- Assuming the function was called while the goodjustifications weight
        -- did indeed exceed the corrupted threshold, there will be at least one
        -- bad signer. After finding the bad signers, check that we can still
        -- produce a proof with enough weight.
        | PM.weight allJV - PS.weight newBadJV > corruptWeight =
            (Just (fst <$> newGoodJustifications, newAggSig), newBadJV)
        | otherwise = (Nothing, newBadJV)
    where
        toSign = witnessMessage baid v
        goodJustifications
            | PS.null badJV = PM.toList allJV
            | otherwise = filter (\(p,_) -> not (PS.member p badJV)) (PM.toList allJV)
        keys = (publicBlsKeys . fst) <$> goodJustifications
        aggSig = Bls.aggregateMany (snd <$> goodJustifications)
        culprits = findCulprits goodJustifications toSign publicBlsKeys
        newBadJV = foldr (\c -> PS.insert c (partyWeight c)) badJV culprits
        newGoodJustifications = filter (\(p,_) -> not (PS.member p newBadJV)) goodJustifications
        newAggSig = Bls.aggregateMany (snd <$> newGoodJustifications)

-- TODO: optimize, this is just a first draft
-- Internal function, this is only exported for testing purposes
--
-- First argument is a list of parties and their signatures
-- Second argument is the bytestring that each party supposedly signed
-- The third argument is a lookup function from parties to their Bls publickey
-- Returns the list of parties whose signature did not verify under their key.
findCulprits :: [(Party, Bls.Signature)] -> BS.ByteString -> (Party -> Bls.PublicKey) -> [Party]
findCulprits lst toSign keys = culprits lst1 <> culprits lst2
    where
        splitup = foldr (\x ~(l, r) -> (x:r, l)) ([], [])
        (lst1, lst2) = splitup lst
        culprits [] = []
        culprits [(p, blss)] = [p | not (Bls.verify toSign (keys p) blss)]
        culprits lst'
            | Bls.verifyAggregate toSign  (keys . fst <$> lst') (Bls.aggregateMany (snd <$> lst')) = []
            | otherwise = findCulprits lst' toSign keys

-- |Start the WMVBA for us with a given input.  This should only be called once
-- per instance, and the input should already be justified.
startWMVBA :: (WMVBAMonad sig m) => Val -> m ()
startWMVBA val = sendWMVBAMessage (WMVBAFreezeMessage (Proposal val))

-- |Trigger a delayed action.
triggerWMVBAAction :: (WMVBAMonad sig m) => DelayedABBAAction -> m ()
triggerWMVBAAction a = liftABBA (triggerABBAAction a)

data WMVBASummary sig = WMVBASummary {
    summaryFreeze :: Maybe (FreezeSummary sig),
    summaryABBA :: Maybe (ABBASummary sig),
    -- |If freeze has completed and we have witness creation signatures,
    -- then this records them.
    summaryWitnessCreation :: Maybe (Val, Map Party (sig, Bls.Signature))
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
                forM_ (Map.toList . Map.filterWithKey (checkWCSig v) $ m) $
                  \(p, (s, blssig)) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage (v, blssig))
                return False
            (Just (v, m), Just (v', m')) ->
                if v == v' then do
                    forM_ (Map.toList . Map.filterWithKey (checkWCSig v) . (`Map.difference` m') $ m) $
                        \(p, (s, blssig)) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage (v, blssig))
                    -- If we have some signatures they don't, then they're behind
                    return $ not $ null $ Map.differenceWithKey (\party x y -> if x == y || checkWCSig v party y then Nothing else Just x) m' m
                else do
                    forM_ (Map.toList . Map.filterWithKey (checkWCSig v) $ m) $ \(p, (s, blssig)) -> receiveWMVBAMessage p s (WMVBAWitnessCreatorMessage (v, blssig))
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
        checkWCSig v p (sig, blssig) = checkSig p (WMVBAWitnessCreatorMessage (v, blssig)) sig

-- |Create a 'WMVBASummary' from a collection of signatures on @WeAreDone False@.
wmvbaFailedSummary :: Map Party sig -> WMVBASummary sig
wmvbaFailedSummary wadBotSigs = WMVBASummary Nothing (Just (ABBASummary [] Map.empty wadBotSigs)) Nothing

-- |Get the collection of signatures on @WeAreDone False@.
wmvbaWADBot :: WMVBAState sig -> Map Party sig
wmvbaWADBot = PM.partyMap . _botWeAreDone . _abbaState

wmvbaWADBotMessage :: WMVBAMessage
wmvbaWADBotMessage = WMVBAABBAMessage (WeAreDone False)

-- |Get the finalization witness signatures received. 
getOutputWitnesses :: Val -> WMVBAState sig -> OutputWitnesses
getOutputWitnesses v WMVBAState{..} = OutputWitnesses{..}
    where
        justs = snd <$> Map.findWithDefault PM.empty v _justifications
        badJusts = Map.findWithDefault PS.empty v _badJustifications
        knownGoodSigs = Map.empty
        unknownSigs = Map.filterWithKey (\p _ -> not (PS.member p badJusts)) (PM.partyMap justs)
        knownBadSigs = PS.parties badJusts

-- |The 'WMVBAPassiveState' collects WitnessCreator signatures for generating a
-- finalization proof, without participating in the WMVBA protocol.
data WMVBAPassiveState = WMVBAPassiveState {
        _passiveWitnesses :: !(Map Val (PartyMap Bls.Signature, PartySet))
    } deriving (Show, Eq)
makeLenses ''WMVBAPassiveState

initialWMVBAPassiveState :: WMVBAPassiveState
initialWMVBAPassiveState = WMVBAPassiveState Map.empty

passiveReceiveWMVBASignatures :: (MonadState WMVBAPassiveState m)
    => WMVBAInstance
    -> Val
    -> PM.PartyMap Bls.Signature
    -> (Party -> VoterPower)
    -> m (Maybe (Val, ([Party], Bls.Signature)))
passiveReceiveWMVBASignatures wi@WMVBAInstance{..} v partyMap voterPower = do
        (newJV, badJV) <- passiveWitnesses . at v . non (PM.empty, PS.empty) <%= (_1 %~ PM.union voterPower partyMap)
        -- When justifications combined weight minus the weight of the bad justifications exceeds corruptWeight,
        -- we can attempt to create the bls aggregate signature
        if PM.weight newJV - PS.weight badJV > corruptWeight then do
            let (proofData, newBadJV) = createAggregateSig wi v newJV badJV
            passiveWitnesses . at v . non (PM.empty, PS.empty) . _2 .= newBadJV
            return $ (v,) <$> proofData
        else
            return Nothing

passiveReceiveWMVBAMessage :: (MonadState WMVBAPassiveState m)
    => WMVBAInstance
    -> Party
    -- ^Message sender
    -> WMVBAMessage
    -- ^Message
    -> m (Maybe (Val, ([Party], Bls.Signature)))
passiveReceiveWMVBAMessage wi@WMVBAInstance{..} src (WMVBAWitnessCreatorMessage (v, blssig)) =
    passiveReceiveWMVBASignatures wi v (PM.singleton src (partyWeight src) blssig) partyWeight
passiveReceiveWMVBAMessage _ _ _ = return Nothing

passiveGetOutputWitnesses :: Val -> WMVBAPassiveState -> OutputWitnesses
passiveGetOutputWitnesses v WMVBAPassiveState{..} = OutputWitnesses{..}
    where
        (justs, badJusts) = Map.findWithDefault (PM.empty, PS.empty) v _passiveWitnesses
        knownGoodSigs = Map.empty
        unknownSigs = Map.filterWithKey (\p _ -> not (PS.member p badJusts)) (PM.partyMap justs)
        knownBadSigs = PS.parties badJusts

