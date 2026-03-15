-- | Signing root and domain computation following the Ethereum consensus spec.
module Crypto.SigningRoot
  ( computeSigningRoot
  , computeDomain
  ) where

import qualified Data.ByteString as BS
import Consensus.Constants (Domain, DomainType, Root, Version)
import Crypto.Hashing (sha256Pair)
import SSZ.Common (Bytes32, mkBytesN, unBytesN)
import SSZ.Merkleization (SszHashTreeRoot (..), merkleize)

-- | Compute the signing root for an SSZ object and domain.
-- Follows the Ethereum consensus spec 'SigningData' container:
-- @signing_root = merkleize([hash_tree_root(obj), domain])@
computeSigningRoot :: SszHashTreeRoot a => a -> Domain -> Bytes32
computeSigningRoot obj domain =
  let objRoot = hashTreeRoot obj
      domainBytes = unBytesN domain
      root = sha256Pair objRoot domainBytes
  in  case mkBytesN @32 root of
        Right b  -> b
        Left _   -> error "computeSigningRoot: SHA-256 produced non-32-byte output"

-- | Compute a domain value from domain type, fork version, and genesis validators root.
-- Formula: @domain = domainType(4 bytes) ++ forkDataRoot[0..27](28 bytes)@
computeDomain :: DomainType -> Version -> Root -> Domain
computeDomain domainType forkVersion genesisValidatorsRoot =
  let forkDataRoot = merkleize [zeroPadTo32 (unBytesN forkVersion), unBytesN genesisValidatorsRoot] 2
      domainBytes = unBytesN domainType <> BS.take 28 forkDataRoot
  in  case mkBytesN @32 domainBytes of
        Right b  -> b
        Left _   -> error "computeDomain: unexpected length"
  where
    zeroPadTo32 bs
      | BS.length bs >= 32 = BS.take 32 bs
      | otherwise = bs <> BS.replicate (32 - BS.length bs) 0
