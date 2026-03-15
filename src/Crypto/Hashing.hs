-- | Canonical SHA-256 hashing functions.
-- This is the single source of truth for SHA-256 in the project.
-- Named 'Crypto.Hashing' to avoid collision with crypton's 'Crypto.Hash'.
module Crypto.Hashing
  ( sha256
  , sha256Pair
  ) where

import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)

-- | SHA-256 hash via crypton.
sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (CH.hash bs :: CH.Digest CH.SHA256)

-- | SHA-256 of two concatenated ByteStrings.
sha256Pair :: ByteString -> ByteString -> ByteString
sha256Pair a b = sha256 (a <> b)
