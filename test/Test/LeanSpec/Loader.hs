-- | Fixture discovery and loading for leanSpec test vectors.
module Test.LeanSpec.Loader
  ( fixturesDir
  , discoverFixtures
  , loadFixture
  , fixturesAvailable
  ) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

-- | Path to the leanSpec fixtures directory, relative to the project root.
fixturesDir :: FilePath
fixturesDir = "leanSpec/fixtures/consensus"

-- | Check whether fixtures have been generated.
fixturesAvailable :: IO Bool
fixturesAvailable = doesDirectoryExist fixturesDir

-- | Recursively discover all JSON fixture files under a directory.
discoverFixtures :: FilePath -> IO [FilePath]
discoverFixtures dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else go dir
  where
    go path = do
      entries <- listDirectory path
      concat <$> mapM (processEntry path) entries

    processEntry parent name = do
      let full = parent </> name
      isDir <- doesDirectoryExist full
      if isDir
        then go full
        else pure [full | takeExtension name == ".json"]

-- | Load and parse a single JSON fixture file.
loadFixture :: FromJSON a => FilePath -> IO (Either String a)
loadFixture path = do
  result <- eitherDecodeFileStrict' path
  pure $ case result of
    Left err -> Left ("Failed to parse " <> path <> ": " <> err)
    Right v  -> Right v
