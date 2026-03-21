module Test.Genesis (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BL

import Config (LogLevel (..), NodeConfig (..), defaultNodeConfig)
import Consensus.Types (BeaconState (..), BeaconBlock (..))
import Genesis

tests :: TestTree
tests = testGroup "Genesis"
  [ testGroup "JSON parsing"
      [ testCase "should parse valid genesis config" $ do
          let json = sampleGenesisJson
          case parseGenesisConfig json of
            Left err -> assertFailure $ "Failed to parse: " <> err
            Right gc -> do
              length (gcValidators gc) @?= 2
              gcChainId gc @?= 1337

      , testCase "should reject invalid hex in pubkey" $ do
          let json = BL.pack "{\"genesis_time\":\"2026-01-01T00:00:00Z\",\"validators\":[{\"pubkey\":\"0xZZZZ\",\"balance\":32000000000}],\"fork_version\":\"0x00000001\",\"chain_id\":1}"
          case parseGenesisConfig json of
            Left _  -> pure ()
            Right _ -> assertFailure "Expected parse failure for invalid hex"

      , testCase "should reject invalid genesis_time format" $ do
          let json = BL.pack "{\"genesis_time\":\"not-a-date\",\"validators\":[],\"fork_version\":\"0x00000001\",\"chain_id\":1}"
          case parseGenesisConfig json of
            Left _  -> pure ()
            Right _ -> assertFailure "Expected parse failure for invalid time"
      ]

  , testGroup "State initialization"
      [ testCase "should create state at slot 0 with correct validator count" $ do
          case parseGenesisConfig sampleGenesisJson of
            Left err -> assertFailure $ "Failed to parse: " <> err
            Right gc -> do
              let st = initializeGenesisState gc
              bsSlot st @?= 0
              length (gcValidators gc) @?= 2

      , testCase "should create genesis block at slot 0" $ do
          let gb = mkGenesisBlock
          bbSlot gb @?= 0
          bbProposerIndex gb @?= 0
      ]

  , testGroup "Config"
      [ testCase "should have sensible defaults" $ do
          ncDataDir defaultNodeConfig @?= "data"
          ncListenPort defaultNodeConfig @?= 9000
          ncLogLevel defaultNodeConfig @?= Info
          ncValidatorKeyDir defaultNodeConfig @?= Nothing
      ]
  ]

-- | Sample genesis JSON with 2 validators (52-byte pubkeys).
sampleGenesisJson :: BL.ByteString
sampleGenesisJson = BL.pack $ concat
  [ "{\"genesis_time\":\"2026-01-01T00:00:00Z\""
  , ",\"validators\":["
  , "{\"pubkey\":\"0x" <> replicate 104 '0' <> "\",\"balance\":32000000000}"
  , ",{\"pubkey\":\"0x" <> replicate 104 'a' <> "\",\"balance\":32000000000}"
  , "]"
  , ",\"fork_version\":\"0x00000001\""
  , ",\"chain_id\":1337"
  , "}"
  ]
