-- | Genesis configuration parsing and state initialization.
module Genesis
  ( GenesisValidator (..)
  , GenesisConfig (..)
  , parseGenesisConfig
  , initializeGenesisState
  , mkGenesisBlock
  , initializeGenesis
  ) where

import Data.Aeson ((.:), FromJSON (..), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Word (Word64)
import Consensus.Constants
import Consensus.ForkChoice (initStore)
import Consensus.Types
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (zeroN)
import SSZ.List (mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import SSZ.Common (mkBytesN)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data GenesisValidator = GenesisValidator
  { gvPubkey  :: !XmssPubkey
  , gvBalance :: !Gwei
  } deriving stock (Eq, Show)

data GenesisConfig = GenesisConfig
  { gcGenesisTime :: !UTCTime
  , gcValidators  :: ![GenesisValidator]
  , gcForkVersion :: !Version
  , gcChainId     :: !Word64
  } deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- JSON parsing
-- ---------------------------------------------------------------------------

instance FromJSON GenesisValidator where
  parseJSON = withObject "GenesisValidator" $ \o -> do
    pubkeyHex <- o .: "pubkey"
    balance   <- o .: "balance"
    pubkeyBs  <- parseHexField "pubkey" pubkeyHex
    case mkXmssPubkey pubkeyBs of
      Left err -> fail $ "Invalid pubkey: " <> show err
      Right pk -> pure $ GenesisValidator pk balance

instance FromJSON GenesisConfig where
  parseJSON = withObject "GenesisConfig" $ \o -> do
    timeStr     <- o .: "genesis_time"
    validators  <- o .: "validators"
    versionHex  <- o .: "fork_version"
    chainId     <- o .: "chain_id"
    time <- case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" timeStr of
      Nothing -> fail $ "Invalid genesis_time format: " <> timeStr
      Just t  -> pure t
    versionBs <- parseHexField "fork_version" versionHex
    case mkBytesN @4 versionBs of
      Left err -> fail $ "Invalid fork_version: " <> show err
      Right v  -> pure $ GenesisConfig time validators v chainId

parseHexField :: String -> String -> Aeson.Parser BS.ByteString
parseHexField fieldName hexStr = do
  let stripped = case hexStr of
        ('0':'x':rest) -> rest
        other          -> other
  case Base16.decode (encodeUtf8 (T.pack stripped)) of
    Left err -> fail $ "Invalid hex in " <> fieldName <> ": " <> err
    Right bs -> pure bs

-- ---------------------------------------------------------------------------
-- State initialization
-- ---------------------------------------------------------------------------

-- | Build the genesis BeaconState from configuration.
initializeGenesisState :: GenesisConfig -> BeaconState
initializeGenesisState gc =
  let validators     = map toValidator (gcValidators gc)
      genesisTimeSec = floor (utcTimeToPOSIXSeconds (gcGenesisTime gc)) :: Word64
      config         = Config { cfGenesisTime = genesisTimeSec }
      valList        = forceRight $ mkSszList @VALIDATORS_LIMIT validators
      emptyHashes    = forceRight $ mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT []
      emptyJSlots    = forceRight $ mkBitlist @JUSTIFIED_SLOTS_LIMIT []
      emptyJRoots    = forceRight $ mkSszList @JUSTIFICATIONS_ROOTS_LIMIT []
      emptyJVals     = forceRight $ mkBitlist @JUSTIFICATIONS_VALIDATORS_LIMIT []
      bodyRoot       = toRoot mkEmptyBody
  in  BeaconState
    { bsConfig                   = config
    , bsSlot                     = 0
    , bsLatestBlockHeader        = BeaconBlockHeader 0 0 zeroRoot zeroRoot bodyRoot
    , bsLatestJustified          = zeroCheckpoint
    , bsLatestFinalized          = zeroCheckpoint
    , bsHistoricalBlockHashes    = emptyHashes
    , bsJustifiedSlots           = emptyJSlots
    , bsValidators               = valList
    , bsJustificationsRoots      = emptyJRoots
    , bsJustificationsValidators = emptyJVals
    }

-- | The genesis block (slot 0, zero parent root).
mkGenesisBlock :: BeaconBlock
mkGenesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody

-- | Initialize both genesis state and fork choice store.
initializeGenesis :: GenesisConfig -> (BeaconState, Store)
initializeGenesis gc =
  let gs    = initializeGenesisState gc
      gb    = mkGenesisBlock
      store = initStore gs gb
  in  (gs, store)

-- ---------------------------------------------------------------------------
-- Parsing helpers
-- ---------------------------------------------------------------------------

-- | Parse a genesis config from a lazy JSON ByteString.
parseGenesisConfig :: ByteString -> Either String GenesisConfig
parseGenesisConfig = Aeson.eitherDecode

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

toValidator :: GenesisValidator -> Validator
toValidator gv = Validator
  { vPubkey           = gvPubkey gv
  , vEffectiveBalance = gvBalance gv
  , vSlashed          = False
  , vActivationSlot   = 0
  , vExitSlot         = maxBound
  , vWithdrawableSlot = maxBound
  }

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }

zeroRoot :: Root
zeroRoot = zeroN @32

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint 0 zeroRoot

toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"
