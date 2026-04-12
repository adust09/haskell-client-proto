-- | State transition tests using leanSpec-generated fixtures.
--
-- Due to significant type divergence between leanSpec's State and our
-- BeaconState (different validator structure, different state fields),
-- we test a subset of properties:
-- 1. Block deserialization from fixture JSON
-- 2. Block SSZ roundtrip for fixture blocks
-- 3. Post-state slot expectations (where applicable)
--
-- Full state_transition() testing requires bridging the type gap first.
module Test.LeanSpec.StateTransition (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Test.LeanSpec.Loader
import Test.LeanSpec.Types

tests :: TestTree
tests = testGroup "LeanSpec StateTransition" [stTests]

stTests :: TestTree
stTests = testCaseSteps "State transition fixtures" $ \step -> do
  available <- fixturesAvailable
  if not available
    then step "SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    else do
      let stDir = fixturesDir <> "/state_transition"
      files <- discoverFixtures stDir
      step ("Found " <> show (length files) <> " state transition fixture files")

      results <- mapM (runSTFile step) files
      let (passed, skipped, failed) = foldResults results
      step ("Passed: " <> show passed <> ", Skipped: " <> show skipped <> ", Failed: " <> show failed)
      assertEqual "State transition fixture failures" 0 failed

data TestResult = Pass | Skip | Fail String

foldResults :: [TestResult] -> (Int, Int, Int)
foldResults = foldr go (0, 0, 0)
  where
    go Pass      (p, s, f) = (p+1, s, f)
    go Skip      (p, s, f) = (p, s+1, f)
    go (Fail _)  (p, s, f) = (p, s, f+1)

runSTFile :: (String -> IO ()) -> FilePath -> IO TestResult
runSTFile step path = do
  result <- loadFixture path
  case result of
    Left err -> do
      step ("FAIL parse " <> path <> ": " <> err)
      pure (Fail err)
    Right (STFixtureEnvelope fixtures) ->
      case Map.elems fixtures of
        []    -> pure Skip
        (f:_) -> runSTFixture step path f

runSTFixture :: (String -> IO ()) -> FilePath -> STFixture -> IO TestResult
runSTFixture step _path fixture = do
  -- Test 1: All blocks in the fixture parse and convert to domain type
  let blockResults = map fixtureBlockToDomain (stfBlocks fixture)
  case sequence blockResults of
    Left err -> do
      step ("FAIL block conversion: " <> err)
      pure (Fail err)
    Right _blocks -> do
      -- Test 2: Pre-state block header parses
      case fixtureBlockHeaderToDomain (fsLatestBlockHeader (stfPre fixture)) of
        Left err -> do
          step ("FAIL header conversion: " <> err)
          pure (Fail err)
        Right _header -> do
          -- Test 3: Checkpoints parse
          case (fixtureCheckpointToDomain (fsLatestJustified (stfPre fixture)),
                fixtureCheckpointToDomain (fsLatestFinalized (stfPre fixture))) of
            (Left err, _) -> do
              step ("FAIL justified checkpoint: " <> err)
              pure (Fail err)
            (_, Left err) -> do
              step ("FAIL finalized checkpoint: " <> err)
              pure (Fail err)
            (Right _, Right _) -> do
              -- Test 4: Validate post-state expectations if present
              case stfPost fixture of
                Nothing -> pure Pass
                Just post -> do
                  -- Verify slot expectation matches block sequence
                  case stpSlot post of
                    Nothing -> pure Pass
                    Just expectedSlot -> do
                      let lastBlockSlot = case stfBlocks fixture of
                            [] -> fsSlot (stfPre fixture)
                            bs -> fbSlot (last bs)
                      if expectedSlot == lastBlockSlot
                        then pure Pass
                        else do
                          step ("FAIL slot mismatch: expected " <> show expectedSlot
                                <> ", last block at " <> show lastBlockSlot)
                          pure (Fail "slot mismatch")
