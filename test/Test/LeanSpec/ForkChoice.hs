-- | Fork choice tests using leanSpec-generated fixtures.
--
-- Due to type divergence in State (leanSpec State vs our BeaconState),
-- we currently test:
-- 1. Fixture parsing and deserialization
-- 2. Anchor block conversion to domain type
-- 3. Step structure validation (all steps parse correctly)
-- 4. Block steps have valid block conversions
--
-- Full fork choice replay requires bridging the State type gap.
module Test.LeanSpec.ForkChoice (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Test.LeanSpec.Loader
import Test.LeanSpec.Types

tests :: TestTree
tests = testGroup "LeanSpec ForkChoice" [fcTests]

fcTests :: TestTree
fcTests = testCaseSteps "Fork choice fixtures" $ \step -> do
  available <- fixturesAvailable
  if not available
    then step "SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    else do
      let fcDir = fixturesDir <> "/fork_choice"
      files <- discoverFixtures fcDir
      step ("Found " <> show (length files) <> " fork choice fixture files")

      results <- mapM (runFCFile step) files
      let (passed, skipped, failed) = foldResults results
      step ("Passed: " <> show passed <> ", Skipped: " <> show skipped <> ", Failed: " <> show failed)
      assertEqual "Fork choice fixture failures" 0 failed

data TestResult = Pass | Skip | Fail String

foldResults :: [TestResult] -> (Int, Int, Int)
foldResults = foldr go (0, 0, 0)
  where
    go Pass      (p, s, f) = (p+1, s, f)
    go Skip      (p, s, f) = (p, s+1, f)
    go (Fail _)  (p, s, f) = (p, s, f+1)

runFCFile :: (String -> IO ()) -> FilePath -> IO TestResult
runFCFile step path = do
  result <- loadFixture path
  case result of
    Left err -> do
      step ("FAIL parse " <> path <> ": " <> err)
      pure (Fail err)
    Right (FCFixtureEnvelope fixtures) ->
      case Map.elems fixtures of
        []    -> pure Skip
        (f:_) -> runFCFixture step path f

runFCFixture :: (String -> IO ()) -> FilePath -> FCFixture -> IO TestResult
runFCFixture step _path fixture = do
  -- Test 1: Anchor block converts to domain type
  case fixtureBlockToDomain (fcfAnchorBlock fixture) of
    Left err -> do
      step ("FAIL anchor block conversion: " <> err)
      pure (Fail err)
    Right _anchorBlock -> do
      -- Test 2: All block steps convert to domain type
      let blockSteps = [fb | FCStep _ stype (Just fb) _ _ <- fcfSteps fixture, stype == "block"]
      let blockResults = map fixtureBlockToDomain blockSteps
      case sequence blockResults of
        Left err -> do
          step ("FAIL step block conversion: " <> err)
          pure (Fail err)
        Right _blocks -> do
          -- Test 3: Anchor state header and checkpoints parse
          let anchorState = fcfAnchorState fixture
          case fixtureBlockHeaderToDomain (fsLatestBlockHeader anchorState) of
            Left err -> do
              step ("FAIL anchor header: " <> err)
              pure (Fail err)
            Right _ -> pure Pass
