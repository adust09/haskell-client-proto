-- | CLI option parsing for lean-consensus.
module Options
  ( Options (..)
  , parseOptions
  , toNodeConfig
  ) where

import Options.Applicative

import Config (LogLevel (..), NodeConfig (..))

data Options = Options
  { optDataDir         :: !FilePath
  , optListenPort      :: !Int
  , optBootnodes       :: ![String]
  , optIsAggregator    :: !Bool
  , optKeyPath         :: !FilePath
  , optGenesisFile     :: !FilePath
  , optRpcPort         :: !(Maybe Int)
  , optMetricsPort     :: !(Maybe Int)
  , optValidatorKeyDir :: !(Maybe FilePath)
  , optP2PSocket       :: !(Maybe FilePath)
  , optLogLevel        :: !LogLevel
  } deriving stock (Show)

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> header "lean-consensus — Haskell Lean Consensus client for pq-devnet-3"
      )

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "data-dir"
     <> metavar "DIR"
     <> value "data"
     <> showDefault
     <> help "Data directory for storage"
      )
  <*> option auto
      ( long "listen-port"
     <> metavar "PORT"
     <> value 9000
     <> showDefault
     <> help "P2P listen port"
      )
  <*> many (strOption
      ( long "bootnode"
     <> metavar "MULTIADDR"
     <> help "Bootnode multiaddr (repeatable)"
      ))
  <*> switch
      ( long "is-aggregator"
     <> help "Run as aggregator node"
      )
  <*> strOption
      ( long "key-path"
     <> metavar "PATH"
     <> value "key.dat"
     <> showDefault
     <> help "Path to XMSS key file"
      )
  <*> strOption
      ( long "genesis"
     <> metavar "FILE"
     <> value "genesis.json"
     <> showDefault
     <> help "Path to genesis config JSON"
      )
  <*> optional (option auto
      ( long "rpc-port"
     <> metavar "PORT"
     <> help "HTTP RPC port (disabled if not set)"
      ))
  <*> optional (option auto
      ( long "metrics-port"
     <> metavar "PORT"
     <> help "Prometheus metrics port (disabled if not set)"
      ))
  <*> optional (strOption
      ( long "validator-keys"
     <> metavar "DIR"
     <> help "Validator key directory (enables validator mode)"
      ))
  <*> optional (strOption
      ( long "p2p-socket"
     <> metavar "PATH"
     <> help "Unix socket path for P2P IPC sidecar"
      ))
  <*> option auto
      ( long "log-level"
     <> metavar "LEVEL"
     <> value Info
     <> showDefault
     <> help "Log level: Debug, Info, Warn, Error"
      )

-- | Convert CLI options to library-level NodeConfig.
toNodeConfig :: Options -> NodeConfig
toNodeConfig o = NodeConfig
  { ncDataDir         = optDataDir o
  , ncListenPort      = optListenPort o
  , ncBootnodes       = optBootnodes o
  , ncGenesisFile     = optGenesisFile o
  , ncKeyPath         = optKeyPath o
  , ncIsAggregator    = optIsAggregator o
  , ncRpcPort         = optRpcPort o
  , ncMetricsPort     = optMetricsPort o
  , ncValidatorKeyDir = optValidatorKeyDir o
  , ncP2PSocket       = optP2PSocket o
  , ncLogLevel        = optLogLevel o
  }
