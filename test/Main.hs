module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import LeanConsensus (version)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "lean-consensus"
  [ testCase "version is not empty" $
      assertBool "version should not be empty" (not $ null version)
  ]
