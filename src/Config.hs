-- | Library-level node configuration, separate from CLI parsing.
module Config
  ( NodeConfig (..)
  , LogLevel (..)
  , defaultNodeConfig
  ) where

-- | Log verbosity level.
data LogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | Core configuration for a lean-consensus node.
data NodeConfig = NodeConfig
  { ncDataDir         :: !FilePath
  , ncListenPort      :: !Int
  , ncBootnodes       :: ![String]
  , ncGenesisFile     :: !FilePath
  , ncKeyPath         :: !FilePath
  , ncIsAggregator    :: !Bool
  , ncRpcPort         :: !(Maybe Int)
  , ncMetricsPort     :: !(Maybe Int)
  , ncValidatorKeyDir :: !(Maybe FilePath)
  , ncLogLevel        :: !LogLevel
  } deriving stock (Eq, Show)

-- | Sensible defaults for local development.
defaultNodeConfig :: NodeConfig
defaultNodeConfig = NodeConfig
  { ncDataDir         = "data"
  , ncListenPort      = 9000
  , ncBootnodes       = []
  , ncGenesisFile     = "genesis.json"
  , ncKeyPath         = "key.dat"
  , ncIsAggregator    = False
  , ncRpcPort         = Nothing
  , ncMetricsPort     = Nothing
  , ncValidatorKeyDir = Nothing
  , ncLogLevel        = Info
  }
