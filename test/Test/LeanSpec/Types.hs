-- | Bridge types for parsing leanSpec JSON fixtures into domain types.
--
-- leanSpec uses different field names and structures (camelCase JSON).
-- These intermediate types handle deserialization and conversion.
module Test.LeanSpec.Types
  ( -- * SSZ fixtures
    SSZFixtureEnvelope (..)
  , SSZFixture (..)
    -- * State transition fixtures
  , STFixtureEnvelope (..)
  , STFixture (..)
  , STPost (..)
  , FixtureState (..)
  , FixtureBlock (..)
  , FixtureBlockBody (..)
  , FixtureBlockHeader (..)
  , FixtureCheckpoint (..)
  , FixtureConfig (..)
  , FixtureValidator (..)
  , FixtureAggregatedAttestation (..)
  , FixtureAggregatedAttestations (..)
    -- * Fork choice fixtures
  , FCFixtureEnvelope (..)
  , FCFixture (..)
  , FCStep (..)
  , FCStepChecks (..)
    -- * Conversions
  , fixtureBlockToDomain
  , fixtureBlockHeaderToDomain
  , fixtureCheckpointToDomain
  ) where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Consensus.Constants (Slot, ValidatorIndex, Root)
import Consensus.Types
  ( BeaconBlock (..)
  , BeaconBlockBody (..)
  , BeaconBlockHeader (..)
  , Checkpoint (..)
  )
import SSZ.Common (mkBytesN)
import SSZ.List (mkSszList)

-- ---------------------------------------------------------------------------
-- Hex parsing utilities
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- SSZ fixture types
-- ---------------------------------------------------------------------------

-- | Top-level SSZ fixture envelope: maps testId -> fixture
newtype SSZFixtureEnvelope = SSZFixtureEnvelope
  { sszFixtures :: Map.Map Text SSZFixture }

instance FromJSON SSZFixtureEnvelope where
  parseJSON = withObject "SSZFixtureEnvelope" $ \o ->
    SSZFixtureEnvelope <$> parseJSON (Object o)

data SSZFixture = SSZFixture
  { ssfTypeName   :: !Text
  , ssfValue      :: !Value
  , ssfSerialized :: !Text
  } deriving stock (Show)

instance FromJSON SSZFixture where
  parseJSON = withObject "SSZFixture" $ \o -> SSZFixture
    <$> o .: "typeName"
    <*> o .: "value"
    <*> o .: "serialized"

-- ---------------------------------------------------------------------------
-- State transition fixture types
-- ---------------------------------------------------------------------------

newtype STFixtureEnvelope = STFixtureEnvelope
  { stFixtures :: Map.Map Text STFixture }

instance FromJSON STFixtureEnvelope where
  parseJSON = withObject "STFixtureEnvelope" $ \o ->
    STFixtureEnvelope <$> parseJSON (Object o)

data STFixture = STFixture
  { stfPre             :: !FixtureState
  , stfBlocks          :: ![FixtureBlock]
  , stfPost            :: !(Maybe STPost)
  , stfExpectException :: !(Maybe Text)
  } deriving stock (Show)

instance FromJSON STFixture where
  parseJSON = withObject "STFixture" $ \o -> STFixture
    <$> o .: "pre"
    <*> o .: "blocks"
    <*> o .:? "post"
    <*> o .:? "expectException"

data STPost = STPost
  { stpSlot                       :: !(Maybe Slot)
  , stpLatestBlockHeaderSlot      :: !(Maybe Slot)
  , stpLatestBlockHeaderStateRoot :: !(Maybe Text)
  , stpHistoricalBlockHashesCount :: !(Maybe Int)
  } deriving stock (Show)

instance FromJSON STPost where
  parseJSON = withObject "STPost" $ \o -> STPost
    <$> o .:? "slot"
    <*> o .:? "latestBlockHeaderSlot"
    <*> o .:? "latestBlockHeaderStateRoot"
    <*> o .:? "historicalBlockHashesCount"

-- ---------------------------------------------------------------------------
-- Fork choice fixture types
-- ---------------------------------------------------------------------------

newtype FCFixtureEnvelope = FCFixtureEnvelope
  { fcFixtures :: Map.Map Text FCFixture }

instance FromJSON FCFixtureEnvelope where
  parseJSON = withObject "FCFixtureEnvelope" $ \o ->
    FCFixtureEnvelope <$> parseJSON (Object o)

data FCFixture = FCFixture
  { fcfAnchorState :: !FixtureState
  , fcfAnchorBlock :: !FixtureBlock
  , fcfSteps       :: ![FCStep]
  } deriving stock (Show)

instance FromJSON FCFixture where
  parseJSON = withObject "FCFixture" $ \o -> FCFixture
    <$> o .: "anchorState"
    <*> o .: "anchorBlock"
    <*> o .: "steps"

data FCStep = FCStep
  { fcsValid    :: !Bool
  , fcsStepType :: !Text
  , fcsBlock    :: !(Maybe FixtureBlock)
  , fcsTime     :: !(Maybe Word64)
  , fcsChecks   :: !(Maybe FCStepChecks)
  } deriving stock (Show)

instance FromJSON FCStep where
  parseJSON = withObject "FCStep" $ \o -> FCStep
    <$> o .:? "valid" .!= True
    <*> o .: "stepType"
    <*> o .:? "block"
    <*> o .:? "time"
    <*> o .:? "checks"

data FCStepChecks = FCStepChecks
  { fccHeadSlot :: !(Maybe Slot)
  , fccHeadRoot :: !(Maybe Text)
  } deriving stock (Show)

instance FromJSON FCStepChecks where
  parseJSON = withObject "FCStepChecks" $ \o -> FCStepChecks
    <$> o .:? "headSlot"
    <*> o .:? "headRoot"

-- ---------------------------------------------------------------------------
-- Shared fixture sub-types (leanSpec JSON structure)
-- ---------------------------------------------------------------------------

data FixtureConfig = FixtureConfig
  { fcGenesisTime :: !Word64
  } deriving stock (Show)

instance FromJSON FixtureConfig where
  parseJSON = withObject "FixtureConfig" $ \o -> FixtureConfig
    <$> o .: "genesisTime"

data FixtureCheckpoint = FixtureCheckpoint
  { fckRoot :: !Text
  , fckSlot :: !Slot
  } deriving stock (Show)

instance FromJSON FixtureCheckpoint where
  parseJSON = withObject "FixtureCheckpoint" $ \o -> FixtureCheckpoint
    <$> o .: "root"
    <*> o .: "slot"

data FixtureBlockHeader = FixtureBlockHeader
  { fbhSlot          :: !Slot
  , fbhProposerIndex :: !ValidatorIndex
  , fbhParentRoot    :: !Text
  , fbhStateRoot     :: !Text
  , fbhBodyRoot      :: !Text
  } deriving stock (Show)

instance FromJSON FixtureBlockHeader where
  parseJSON = withObject "FixtureBlockHeader" $ \o -> FixtureBlockHeader
    <$> o .: "slot"
    <*> o .: "proposerIndex"
    <*> o .: "parentRoot"
    <*> o .: "stateRoot"
    <*> o .: "bodyRoot"

data FixtureValidator = FixtureValidator
  { fvAttestationPubkey :: !Text
  , fvProposalPubkey    :: !Text
  , fvIndex             :: !Word64
  } deriving stock (Show)

instance FromJSON FixtureValidator where
  parseJSON = withObject "FixtureValidator" $ \o -> FixtureValidator
    <$> o .: "attestationPubkey"
    <*> o .: "proposalPubkey"
    <*> o .: "index"

newtype FixtureAggregatedAttestations = FixtureAggregatedAttestations
  { faaData :: [FixtureAggregatedAttestation]
  } deriving stock (Show)

instance FromJSON FixtureAggregatedAttestations where
  parseJSON = withObject "FixtureAggregatedAttestations" $ \o ->
    FixtureAggregatedAttestations <$> o .: "data"

data FixtureAggregatedAttestation = FixtureAggregatedAttestation
  { faaAttData        :: !Value
  , faaAggregationBits :: !Value
  } deriving stock (Show)

instance FromJSON FixtureAggregatedAttestation where
  parseJSON = withObject "FixtureAggregatedAttestation" $ \o -> FixtureAggregatedAttestation
    <$> o .: "data"
    <*> o .: "aggregationBits"

data FixtureBlockBody = FixtureBlockBody
  { fbbAttestations :: !FixtureAggregatedAttestations
  } deriving stock (Show)

instance FromJSON FixtureBlockBody where
  parseJSON = withObject "FixtureBlockBody" $ \o -> FixtureBlockBody
    <$> o .: "attestations"

data FixtureBlock = FixtureBlock
  { fbSlot          :: !Slot
  , fbProposerIndex :: !ValidatorIndex
  , fbParentRoot    :: !Text
  , fbStateRoot     :: !Text
  , fbBody          :: !FixtureBlockBody
  } deriving stock (Show)

instance FromJSON FixtureBlock where
  parseJSON = withObject "FixtureBlock" $ \o -> FixtureBlock
    <$> o .: "slot"
    <*> o .: "proposerIndex"
    <*> o .: "parentRoot"
    <*> o .: "stateRoot"
    <*> o .: "body"

data FixtureState = FixtureState
  { fsConfig                    :: !FixtureConfig
  , fsSlot                      :: !Slot
  , fsLatestBlockHeader         :: !FixtureBlockHeader
  , fsLatestJustified           :: !FixtureCheckpoint
  , fsLatestFinalized           :: !FixtureCheckpoint
  , fsValidators                :: !Value  -- parsed as raw JSON (type divergence)
  , fsHistoricalBlockHashes     :: !Value
  , fsJustifiedSlots            :: !Value
  , fsJustificationsRoots       :: !Value
  , fsJustificationsValidators  :: !Value
  } deriving stock (Show)

instance FromJSON FixtureState where
  parseJSON = withObject "FixtureState" $ \o -> FixtureState
    <$> o .: "config"
    <*> o .: "slot"
    <*> o .: "latestBlockHeader"
    <*> o .: "latestJustified"
    <*> o .: "latestFinalized"
    <*> o .: "validators"
    <*> o .: "historicalBlockHashes"
    <*> o .: "justifiedSlots"
    <*> o .: "justificationsRoots"
    <*> o .: "justificationsValidators"

-- ---------------------------------------------------------------------------
-- Conversions to domain types
-- ---------------------------------------------------------------------------

fixtureCheckpointToDomain :: FixtureCheckpoint -> Either String Checkpoint
fixtureCheckpointToDomain fc = do
  root <- parseHexRoot (fckRoot fc)
  pure Checkpoint { cpSlot = fckSlot fc, cpRoot = root }

fixtureBlockHeaderToDomain :: FixtureBlockHeader -> Either String BeaconBlockHeader
fixtureBlockHeaderToDomain fh = do
  parentRoot <- parseHexRoot (fbhParentRoot fh)
  stateRoot  <- parseHexRoot (fbhStateRoot fh)
  bodyRoot   <- parseHexRoot (fbhBodyRoot fh)
  pure BeaconBlockHeader
    { bbhSlot          = fbhSlot fh
    , bbhProposerIndex = fbhProposerIndex fh
    , bbhParentRoot    = parentRoot
    , bbhStateRoot     = stateRoot
    , bbhBodyRoot      = bodyRoot
    }

fixtureBlockToDomain :: FixtureBlock -> Either String BeaconBlock
fixtureBlockToDomain fb = do
  parentRoot <- parseHexRoot (fbParentRoot fb)
  stateRoot  <- parseHexRoot (fbStateRoot fb)
  pure BeaconBlock
    { bbSlot          = fbSlot fb
    , bbProposerIndex = fbProposerIndex fb
    , bbParentRoot    = parentRoot
    , bbStateRoot     = stateRoot
    , bbBody          = BeaconBlockBody
        { bbbAttestations = case mkSszList [] of
            Right l -> l
            Left _  -> error "impossible: empty list"
        }
    }

parseHexRoot :: Text -> Either String Root
parseHexRoot t = case B16.decode (TE.encodeUtf8 (T.drop 2 t)) of
  Right bs -> case mkBytesN bs of
    Right r  -> Right r
    Left err -> Left ("invalid root bytes: " <> show err)
  Left err -> Left ("invalid hex: " <> err)
