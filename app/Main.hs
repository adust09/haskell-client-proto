module Main (main) where

import LeanConsensus (version)

main :: IO ()
main = putStrLn $ "lean-consensus v" <> version
