-- | Shared error type for cryptographic operations.
module Crypto.Error
  ( CryptoError (..)
  ) where

import Data.Word (Word32)

data CryptoError
  = KeyExhausted
  | InvalidSignature
  | InvalidKeyFormat
  | SigningFailed String
  | AggregationFailed String
  | InvalidTreeHeight Word32
  deriving (Show, Eq)
