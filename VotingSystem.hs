{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-------------------------------------------------------------------------------
-- | Decentralized Voting System Smart Contract
-- | Features:
-- | 1. Multi-phase voting with admin controls
-- | 2. Multi-signature admin support
-- | 3. Time-bound voting sessions
-- | 4. Different voting systems (single/multiple choice)
-- | 5. Voter delegation support
-- | 6. Real-time vote tallying
-- | 7. Quorum requirements
-------------------------------------------------------------------------------

module Main where

-- Standard Haskell imports
import Prelude (IO, String, FilePath, putStrLn, Maybe(..), ($), print)
import qualified Prelude as P

-- Plutus imports for smart contract development
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (splitAt, replicate, (<$>), (>>=), (>>), return, pure, (<*), (*>))
import qualified PlutusTx.Builtins as Builtins

-- Import Interval functions for time checking
import Plutus.V1.Ledger.Interval as Interval

-- Serialization and encoding imports
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Cardano API imports for address generation
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------------------------------------
-- DATA TYPE DEFINITIONS
-------------------------------------------------------------------------------

-- | Voter roles in the system
-- | Admin: Can manage the voting session
-- | PrimaryVoter: Special voters with additional weight
-- | GeneralVoter: Regular voters
-- | Pending: Newly registered voters awaiting approval
data VoterRole = Admin | PrimaryVoter | GeneralVoter | Pending

PlutusTx.unstableMakeIsData ''VoterRole

-- | State of a voter in the system
-- | Registered: Voter has registered but not approved
-- | Approved: Voter can cast votes
-- | Rejected: Voter has been rejected by admin
-- | Suspended: Voter temporarily cannot vote
-- | Voted: Voter has already cast their vote
data VoterState = Registered | Approved | Rejected | Suspended | Voted

PlutusTx.unstableMakeIsData ''VoterState

-- | State of the voting session
-- | Setup: Session being configured, no voting allowed
-- | Active: Voting is in progress
-- | Tallying: Voting closed, results being tallied
-- | Closed: Session completely closed
data SessionState = Setup | Active | Tallying | Closed

PlutusTx.unstableMakeIsData ''SessionState

-- | Types of voting systems supported
-- | SingleChoice: Voter selects exactly one option
-- | MultipleChoice: Voter selects multiple options
-- | RankedChoice: Voter ranks all options
data VotingSystem = SingleChoice | MultipleChoice | RankedChoice

PlutusTx.unstableMakeIsData ''VotingSystem

-- | Record for individual voter information
-- | Stores all metadata about a voter in the system
data VoterRecord = VoterRecord
    { vrPubKeyHash :: PubKeyHash       -- ^ Unique identifier for the voter
    , vrRole       :: VoterRole        -- ^ Role/privilege level
    , vrState      :: VoterState       -- ^ Current state in the system
    , vrHasVoted   :: Bool             -- ^ Whether voter has cast a vote
    , vrDelegate   :: Maybe PubKeyHash -- ^ Optional delegation to another voter
    , vrVoteWeight :: Integer          -- ^ Weight of vote (e.g., token-based)
    }

PlutusTx.unstableMakeIsData ''VoterRecord

-- | Main datum stored in the script UTxO
-- | Contains all state information for the voting session
data VotingDatum = VotingDatum
    { vdAdmins       :: [PubKeyHash]        -- ^ List of admin public key hashes
    , vdRequiredSigs :: Integer             -- ^ Minimum admin signatures required
    , vdVoters       :: [VoterRecord]       -- ^ List of all registered voters
    , vdSessionState :: SessionState        -- ^ Current session state
    , vdVotingSystem :: VotingSystem        -- ^ Type of voting system
    , vdOptions      :: [BuiltinByteString] -- ^ Available voting options
    , vdVoteCounts   :: [Integer]           -- ^ Running tally of votes per option
    , vdStartTime    :: POSIXTime           -- ^ Start time of voting period
    , vdEndTime      :: POSIXTime           -- ^ End time of voting period
    , vdMinVotes     :: Integer             -- ^ Minimum votes required for quorum
    , vdMaxChoices   :: Integer             -- ^ Maximum choices per voter
    }

PlutusTx.unstableMakeIsData ''VotingDatum

-- | Actions that can be performed on the contract
-- | Each action corresponds to a different phase/operation
data VotingAction = 
    CreateSession                      -- ^ Initialize a new voting session
    | RegisterVoter                    -- ^ Self-registration by a voter
    | ApproveVoter PubKeyHash          -- ^ Admin approves a specific voter
    | RejectVoter PubKeyHash           -- ^ Admin rejects a specific voter
    | CastVote [Integer]               -- ^ Voter casts vote with selection indices
    | CloseVoting                      -- ^ Admin closes the voting phase
    | TallyVotes                       -- ^ Admin tallies final results
    | UpdateAdmin PubKeyHash Bool      -- ^ Add/remove admin (PubKeyHash, True=add, False=remove)

PlutusTx.unstableMakeIsData ''VotingAction

-------------------------------------------------------------------------------
-- HELPER FUNCTIONS (INLINABLE FOR PLUTUSTX COMPILATION)
-------------------------------------------------------------------------------

-- | Extract the single signer from a transaction context
-- | Fails if there are zero or multiple signers
{-# INLINABLE getOnlySigner #-}
getOnlySigner :: ScriptContext -> PubKeyHash
getOnlySigner ctx = case txInfoSignatories (scriptContextTxInfo ctx) of
    [pkh] -> pkh
    _     -> traceError "expected exactly one signer"

-- | Check if a PubKeyHash is in a list
{-# INLINABLE elemPkh #-}
elemPkh :: PubKeyHash -> [PubKeyHash] -> Bool
elemPkh _ [] = False
elemPkh x (y:ys) = if x == y then True else elemPkh x ys

-- | Check if multiple admin signatures are present and valid
-- | @admins: List of admin public key hashes
-- | @required: Minimum number of signatures needed
-- | @ctx: Transaction context
{-# INLINABLE checkAdminSignatures #-}
checkAdminSignatures :: [PubKeyHash] -> Integer -> ScriptContext -> Bool
checkAdminSignatures admins required ctx =
    let signers = txInfoSignatories (scriptContextTxInfo ctx)
        -- Filter only signers who are admins
        adminSigners = filter (\pkh -> pkh `elemPkh` admins) signers
    in length adminSigners >= required

-- | Check if transaction occurs within voting period
-- | Uses Plutus TimeRange to validate against start/end times
{-# INLINABLE isWithinVotingPeriod #-}
isWithinVotingPeriod :: ScriptContext -> POSIXTime -> POSIXTime -> Bool
isWithinVotingPeriod ctx startTime endTime =
    let txInfo = scriptContextTxInfo ctx
        validRange = txInfoValidRange txInfo
        votingPeriod = interval startTime endTime
    in Interval.contains votingPeriod validRange

-- | Find a voter by their public key hash in the voter list
-- | Returns Nothing if voter not found
{-# INLINABLE findVoter #-}
findVoter :: PubKeyHash -> [VoterRecord] -> Maybe VoterRecord
findVoter _ [] = Nothing
findVoter pkh (v:vs)
    | vrPubKeyHash v == pkh = Just v
    | otherwise             = findVoter pkh vs

-- | Check if voter state is Approved
{-# INLINABLE isApproved #-}
isApproved :: VoterState -> Bool
isApproved Approved = True
isApproved _ = False

-- | Check if voter state is Rejected
{-# INLINABLE isRejected #-}
isRejected :: VoterState -> Bool
isRejected Rejected = True
isRejected _ = False

-- | Check if minimum quorum of votes has been reached
-- | @voters: List of all voters
-- | @minVotes: Minimum number of votes required
{-# INLINABLE checkQuorum #-}
checkQuorum :: [VoterRecord] -> Integer -> Bool
checkQuorum voters minVotes =
    let approvedVoters = filter (\v -> isApproved (vrState v)) voters
        votedCount = length (filter vrHasVoted approvedVoters)
    in votedCount >= minVotes

-- | Validate vote selection based on voting system rules
{-# INLINABLE validateVoteSelection #-}
validateVoteSelection :: VotingSystem -> [Integer] -> Integer -> Integer -> Bool
validateVoteSelection SingleChoice selection numOptions _ =
    length selection == 1 && 
    head selection >= 0 && 
    head selection < numOptions
    
validateVoteSelection MultipleChoice selection numOptions maxChoices =
    let selLength = length selection
    in selLength > 0 && 
       selLength <= maxChoices &&
       all (\i -> i >= 0 && i < numOptions) selection
    
validateVoteSelection RankedChoice selection numOptions _ =
    let selLength = length selection
    in selLength == numOptions &&  -- Must rank all options
       all (\i -> i >= 0 && i < numOptions) selection

-- | Check if a SessionState is in a list
-- | Uses pattern matching for comparison
{-# INLINABLE elemSessionState #-}
elemSessionState :: SessionState -> [SessionState] -> Bool
elemSessionState _ [] = False
elemSessionState x (y:ys) = 
    case x of
        Setup -> case y of
            Setup -> True
            _ -> elemSessionState x ys
        Active -> case y of
            Active -> True
            _ -> elemSessionState x ys
        Tallying -> case y of
            Tallying -> True
            _ -> elemSessionState x ys
        Closed -> case y of
            Closed -> True
            _ -> elemSessionState x ys

-- | Custom replicate function for PlutusTx
{-# INLINABLE replicate' #-}
replicate' :: Integer -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

-- | Compare two SessionState values for equality
{-# INLINABLE sessionStateEq #-}
sessionStateEq :: SessionState -> SessionState -> Bool
sessionStateEq Setup Setup = True
sessionStateEq Active Active = True
sessionStateEq Tallying Tallying = True
sessionStateEq Closed Closed = True
sessionStateEq _ _ = False

-------------------------------------------------------------------------------
-- MAIN VALIDATOR LOGIC
-------------------------------------------------------------------------------

-- | Core validator function implementing all business logic
{-# INLINABLE mkVotingValidator #-}
mkVotingValidator :: VotingDatum -> VotingAction -> ScriptContext -> Bool
mkVotingValidator dat action ctx =
    case action of
        -- Phase 1: Session Creation
        CreateSession ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be in setup state" 
                (sessionStateEq (vdSessionState dat) Setup)

        -- Phase 1: Voter Registration
        RegisterVoter ->
            let signer = getOnlySigner ctx
            in traceIfFalse "session must be in setup or active state" 
                (vdSessionState dat `elemSessionState` [Setup, Active]) &&
               traceIfFalse "voter already registered" 
                (isNothing (findVoter signer (vdVoters dat)))

        -- Phase 2: Approve Voter
        ApproveVoter pkh ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be in setup or active state" 
                (vdSessionState dat `elemSessionState` [Setup, Active]) &&
            let maybeVoter = findVoter pkh (vdVoters dat)
            in traceIfFalse "voter must exist" (isJust maybeVoter) &&
               case maybeVoter of
                   Just voter -> not (isApproved (vrState voter))
                   Nothing    -> False

        -- Phase 2: Reject Voter
        RejectVoter pkh ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be in setup or active state" 
                (vdSessionState dat `elemSessionState` [Setup, Active]) &&
            let maybeVoter = findVoter pkh (vdVoters dat)
            in traceIfFalse "voter must exist" (isJust maybeVoter) &&
               case maybeVoter of
                   Just voter -> not (isRejected (vrState voter))
                   Nothing    -> False

        -- Phase 3: Cast Vote
        CastVote selection ->
            let signer = getOnlySigner ctx
            in case findVoter signer (vdVoters dat) of
                Nothing -> traceError "voter not registered"
                Just voter ->
                    let -- First check if voter has delegated
                        actualVoterPkh = case vrDelegate voter of
                            Just delegatePkh -> 
                                case findVoter delegatePkh (vdVoters dat) of
                                    Just delegate -> 
                                        if isApproved (vrState delegate) && not (vrHasVoted delegate)
                                        then delegatePkh
                                        else signer
                                    Nothing -> signer
                            Nothing -> signer
                        
                        -- Get the actual voter record
                        actualVoter = if actualVoterPkh == signer 
                                     then voter 
                                     else case findVoter actualVoterPkh (vdVoters dat) of
                                            Just v -> v
                                            Nothing -> traceError "delegate not found"
                        
                        voterApproved = isApproved (vrState actualVoter)
                        inVotingPeriod = isWithinVotingPeriod ctx 
                            (vdStartTime dat) (vdEndTime dat)
                        validSelection = validateVoteSelection 
                            (vdVotingSystem dat) selection 
                            (length (vdOptions dat)) (vdMaxChoices dat)
                    in
                        traceIfFalse "session must be active" 
                            (sessionStateEq (vdSessionState dat) Active) &&
                        traceIfFalse "voter must be approved" voterApproved &&
                        traceIfFalse "already voted" (not (vrHasVoted actualVoter)) &&
                        traceIfFalse "not within voting period" inVotingPeriod &&
                        traceIfFalse "invalid selection" validSelection

        -- Phase 4: Close Voting
        CloseVoting ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be active" 
                (sessionStateEq (vdSessionState dat) Active) &&
            traceIfFalse "quorum not met" 
                (checkQuorum (vdVoters dat) (vdMinVotes dat))

        -- Phase 4: Tally Votes
        TallyVotes ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be in tallying state" 
                (sessionStateEq (vdSessionState dat) Tallying)

        -- Admin Management: Add or remove admins
        UpdateAdmin pkh addAdmin ->
            checkAdminSignatures (vdAdmins dat) (vdRequiredSigs dat) ctx &&
            traceIfFalse "session must be in setup state" 
                (sessionStateEq (vdSessionState dat) Setup) &&
            if addAdmin
                then not (pkh `elemPkh` vdAdmins dat)
                else pkh `elemPkh` vdAdmins dat

-------------------------------------------------------------------------------
-- UNTYPED VALIDATOR WRAPPER
-------------------------------------------------------------------------------

-- | Untyped wrapper for the validator
{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkVotingValidator
        (PlutusTx.unsafeFromBuiltinData d)
        (PlutusTx.unsafeFromBuiltinData r)
        (PlutusTx.unsafeFromBuiltinData c)
    then ()
    else error ()

-- | Compiled validator script ready for deployment
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------------------------------------
-- ADDRESS & HASH FUNCTIONS
-------------------------------------------------------------------------------

-- | Calculate the validator hash from serialized validator
plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
    in ValidatorHash (toBuiltin (SBS.fromShort short))

-- | Create script address from validator
plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-- | Convert validator to Bech32 address for Cardano network
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort (LBS.toStrict (Serialise.serialise val))
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        addr :: CS.AddressInEra CS.BabbageEra
        addr = CS.makeShelleyAddressInEra
                network
                (CS.PaymentCredentialByScript scriptHash)
                CS.NoStakeAddress
    in T.unpack (CS.serialiseAddress addr)

-------------------------------------------------------------------------------
-- UTILITY FUNCTIONS FOR DEPLOYMENT
-------------------------------------------------------------------------------

-- | Create initial datum for a new voting session
createInitialDatum :: PubKeyHash -> [BuiltinByteString] -> POSIXTime -> POSIXTime -> VotingDatum
createInitialDatum admin options startTime endTime = VotingDatum
    { vdAdmins = [admin]
    , vdRequiredSigs = 1
    , vdVoters = []
    , vdSessionState = Setup
    , vdVotingSystem = SingleChoice
    , vdOptions = options
    , vdVoteCounts = replicate' (length options) 0
    , vdStartTime = startTime
    , vdEndTime = endTime
    , vdMinVotes = 1
    , vdMaxChoices = 1
    }

-- | Create a voter record for registration
createVoterRecord :: PubKeyHash -> VoterRole -> Integer -> VoterRecord
createVoterRecord pkh role weight = VoterRecord
    { vrPubKeyHash = pkh
    , vrRole = role
    , vrState = Registered
    , vrHasVoted = False
    , vrDelegate = Nothing
    , vrVoteWeight = weight
    }

-------------------------------------------------------------------------------
-- FILE WRITING FUNCTIONS
-------------------------------------------------------------------------------

-- | Write serialized validator to file
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn ("Validator written to: " P.++ path)

-- | Write CBOR hex representation of validator to file
writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn ("CBOR hex written to: " P.++ path)

-------------------------------------------------------------------------------
-- MAIN ENTRY POINT
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Use Testnet with network magic 1 (preprod testnet)
    let network = C.Testnet (C.NetworkMagic 1)
    
    -- Write validator files for deployment
    writeValidator "voting_system.plutus" validator
    writeCBOR      "voting_system.cbor"   validator

    -- Generate and display Bech32 address
    let bech32 = toBech32ScriptAddress network validator

    -- Display deployment information
    putStrLn "\n================================================================"
    putStrLn "DECENTRALIZED VOTING SYSTEM SMART CONTRACT"
    putStrLn "================================================================"
    putStrLn ""
    putStrLn "Bech32 Address:"
    putStrLn bech32
    putStrLn ""
    putStrLn "FEATURES:"
    putStrLn "• Multi-signature admin governance"
    putStrLn "• Time-bound voting sessions"
    putStrLn "• Multiple voting systems (single/multiple/ranked choice)"
    putStrLn "• Voter delegation support"
    putStrLn "• Real-time vote tallying"
    putStrLn "• Quorum requirements"
    putStrLn "• Weighted voting based on roles"
    putStrLn ""
    putStrLn "VOTING PHASES:"
    putStrLn "1. Setup: Admin creates session, voters register"
    putStrLn "2. Approval: Admin approves/rejects specific voters"
    putStrLn "3. Voting: Approved voters cast votes within time window"
    putStrLn "4. Tallying: Admin closes voting and tallies results"
    putStrLn ""
    putStrLn "GENERATED FILES:"
    putStrLn "• voting_system.plutus - Serialized validator"
    putStrLn "• voting_system.cbor   - CBOR hex representation"
    putStrLn "================================================================"
    putStrLn ""