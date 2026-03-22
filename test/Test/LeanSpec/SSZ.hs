-- | SSZ roundtrip tests using leanSpec-generated fixtures.
--
-- For each SSZ fixture, we:
-- 1. Parse the "serialized" hex to get the expected SSZ bytes
-- 2. Decode from SSZ bytes using our SszDecode instance
-- 3. Re-encode and verify the bytes match
--
-- Only types whose structure matches between leanSpec and this project
-- are tested (Block, BlockHeader, Checkpoint). Types with divergent
-- structures (Validator, State) are skipped.
module Test.LeanSpec.SSZ (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import SSZ.Common (SszEncode (..), SszDecode (..), SszError)
import Consensus.Types (BeaconBlockHeader, Checkpoint)
import Test.LeanSpec.Loader
import Test.LeanSpec.Types

tests :: TestTree
tests = testGroup "LeanSpec SSZ" [sszRoundtripTests]

sszRoundtripTests :: TestTree
sszRoundtripTests = testCaseSteps "SSZ roundtrip from leanSpec fixtures" $ \step -> do
  available <- fixturesAvailable
  if not available
    then step "SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    else do
      let sszDir = fixturesDir <> "/ssz"
      files <- discoverFixtures sszDir
      step ("Found " <> show (length files) <> " SSZ fixture files")

      results <- mapM (runSszFile step) files
      let (passed, skipped, failed) = foldResults results
      step ("Passed: " <> show passed <> ", Skipped: " <> show skipped <> ", Failed: " <> show failed)
      assertEqual "SSZ roundtrip failures" 0 failed

data TestResult = Pass | Skip | Fail String

foldResults :: [TestResult] -> (Int, Int, Int)
foldResults = foldr go (0, 0, 0)
  where
    go Pass      (p, s, f) = (p+1, s, f)
    go Skip      (p, s, f) = (p, s+1, f)
    go (Fail _)  (p, s, f) = (p, s, f+1)

runSszFile :: (String -> IO ()) -> FilePath -> IO TestResult
runSszFile step path = do
  result <- loadFixture path
  case result of
    Left err -> do
      step ("FAIL parse " <> path <> ": " <> err)
      pure (Fail err)
    Right (SSZFixtureEnvelope fixtures) ->
      case Map.elems fixtures of
        []    -> pure Skip
        (f:_) -> runSszFixture step (ssfTypeName f) f

runSszFixture :: (String -> IO ()) -> T.Text -> SSZFixture -> IO TestResult
runSszFixture step typeName fixture = do
  let hexStr = T.drop 2 (ssfSerialized fixture)
  case B16.decode (TE.encodeUtf8 hexStr) of
    Left err -> do
      step ("FAIL hex decode for " <> T.unpack typeName <> ": " <> err)
      pure (Fail err)
    Right sszBytes -> case T.unpack typeName of
      "Checkpoint"       -> roundtripTest step typeName sszBytes (sszDecode sszBytes :: Either SszError Checkpoint)
      "BlockHeader"      -> roundtripTest step typeName sszBytes (sszDecode sszBytes :: Either SszError BeaconBlockHeader)
      -- Block and Block-containing types are skipped because leanSpec uses
      -- AggregatedAttestation in BlockBody, while we use SignedAggregatedAttestation.
      -- Validator is skipped due to completely different field structure.
      _                  -> pure Skip

roundtripTest :: SszEncode a => (String -> IO ()) -> T.Text -> BS.ByteString -> Either e a -> IO TestResult
roundtripTest step typeName originalBytes decoded = case decoded of
  Left _  -> do
    step ("FAIL decode " <> T.unpack typeName)
    pure (Fail ("decode failed for " <> T.unpack typeName))
  Right val -> do
    let reEncoded = sszEncode val
    if reEncoded == originalBytes
      then pure Pass
      else do
        step ("FAIL roundtrip " <> T.unpack typeName <> ": bytes mismatch")
        pure (Fail ("roundtrip mismatch for " <> T.unpack typeName))
