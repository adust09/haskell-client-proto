-- | CLI option parsing for lean-consensus.
module Options
  ( Options (..)
  , parseOptions
  ) where

import Options.Applicative

data Options = Options
  { optListenPort   :: !Int
  , optBootnodes    :: ![String]
  , optIsAggregator :: !Bool
  , optKeyPath      :: !FilePath
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
  <$> option auto
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
