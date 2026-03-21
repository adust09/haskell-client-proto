{-# LANGUAGE PackageImports #-}

-- | XMSS signature stub using Ed25519 (from crypton).
-- Produces 3112-byte signatures and 32-byte public keys.
-- Will be swapped for real FFI when C library becomes available.
module Crypto.LeanSig
  ( PrivateKey  -- opaque, no constructor exported
  , generateKeyPair
  , sign
  , verify
  , serializePrivateKey
  , deserializePrivateKey
  , privateKeyTreeHeight
  , publicKeyFromPrivate
  ) where

import "crypton" Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import Consensus.Constants (xmssPubkeySize, xmssSignatureSize)
import Consensus.Types (XmssPubkey (..), XmssSignature (..))
import Crypto.Error (CryptoError (..))
import Crypto.Hashing (sha256)

-- | Opaque private key. Wraps an Ed25519 secret key + tree height.
data PrivateKey = PrivateKey
  { pkSecretKey  :: !Ed25519.SecretKey
  , pkPublicKey  :: !Ed25519.PublicKey
  , pkTreeHeight :: !Word32
  } deriving (Show)

-- | Get the tree height of a private key.
privateKeyTreeHeight :: PrivateKey -> Word32
privateKeyTreeHeight = pkTreeHeight

-- | Extract the public key from a private key.
publicKeyFromPrivate :: PrivateKey -> XmssPubkey
publicKeyFromPrivate pk = XmssPubkey (BA.convert (pkPublicKey pk) :: ByteString)

-- | Generate a keypair from a tree height and seed.
-- Tree height must be in [1, 31].
generateKeyPair :: Word32 -> ByteString -> Either CryptoError (PrivateKey, XmssPubkey)
generateKeyPair treeHeight seed
  | treeHeight < 1 || treeHeight > 31 = Left (InvalidTreeHeight treeHeight)
  | otherwise =
      let seedHash = sha256 seed
          sk = case Ed25519.secretKey (BS.take 32 seedHash) of
            CryptoPassed k -> k
            CryptoFailed _ -> error "generateKeyPair: Ed25519.secretKey failed on 32-byte input"
          pk = Ed25519.toPublic sk
          ed25519Bytes = BA.convert pk :: ByteString
          -- Pad Ed25519 pubkey (32 bytes) to xmssPubkeySize (52 bytes) for mock XMSS
          pubBytes = ed25519Bytes <> BS.replicate (xmssPubkeySize - BS.length ed25519Bytes) 0
      in  Right (PrivateKey sk pk treeHeight, XmssPubkey pubBytes)

-- | Sign a message with a given leaf index.
-- The leaf index is embedded in the signature for verification.
sign :: PrivateKey -> ByteString -> Word32 -> Either CryptoError XmssSignature
sign pk message leafIndex =
  let actualMessage = message <> encodeLE32 leafIndex
      ed25519Sig = Ed25519.sign (pkSecretKey pk) (pkPublicKey pk) actualMessage
      sigBytes = BA.convert ed25519Sig :: ByteString  -- 64 bytes
      leafBytes = encodeLE32 leafIndex                -- 4 bytes
      -- Deterministic padding to fill to xmssSignatureSize
      paddingNeeded = xmssSignatureSize - 64 - 4  -- 3044 bytes
      padding = generatePadding sigBytes paddingNeeded
      fullSig = sigBytes <> leafBytes <> padding
  in  Right (XmssSignature fullSig)

-- | Verify a signature against a public key and message.
verify :: XmssPubkey -> ByteString -> XmssSignature -> Either CryptoError Bool
verify (XmssPubkey pubBytes) message (XmssSignature sigBytes)
  | BS.length sigBytes /= xmssSignatureSize = Left InvalidSignature
  | otherwise =
      -- Extract Ed25519 portion (first 32 bytes) from XMSS pubkey
      case Ed25519.publicKey (BS.take 32 pubBytes) of
        CryptoFailed _ -> Left InvalidKeyFormat
        CryptoPassed pk ->
          let ed25519SigBytes = BS.take 64 sigBytes
              leafBytes = BS.take 4 (BS.drop 64 sigBytes)
              leafIndex = decodeLE32 leafBytes
              actualMessage = message <> encodeLE32 leafIndex
              paddingBytes = BS.drop 68 sigBytes
          in  case Ed25519.signature ed25519SigBytes of
                CryptoFailed _ -> Right False
                CryptoPassed sig ->
                  let sigValid = Ed25519.verify pk actualMessage sig
                      expectedPadding = generatePadding ed25519SigBytes (xmssSignatureSize - 68)
                      paddingValid = paddingBytes == expectedPadding
                  in  Right (sigValid && paddingValid)

-- | Serialize a private key for persistence.
serializePrivateKey :: PrivateKey -> ByteString
serializePrivateKey pk =
  let skBytes = BA.convert (pkSecretKey pk) :: ByteString
      heightBytes = encodeLE32 (pkTreeHeight pk)
  in  heightBytes <> skBytes

-- | Deserialize a private key from bytes.
deserializePrivateKey :: ByteString -> Either CryptoError PrivateKey
deserializePrivateKey bs
  | BS.length bs /= 36 = Left InvalidKeyFormat  -- 4 (height) + 32 (secret key)
  | otherwise =
      let heightBytes = BS.take 4 bs
          skBytes = BS.drop 4 bs
          treeHeight = decodeLE32 heightBytes
      in  if treeHeight < 1 || treeHeight > 31
          then Left (InvalidTreeHeight treeHeight)
          else case Ed25519.secretKey skBytes of
            CryptoFailed _ -> Left InvalidKeyFormat
            CryptoPassed sk ->
              let pk = Ed25519.toPublic sk
              in  Right (PrivateKey sk pk treeHeight)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Encode a Word32 as 4 little-endian bytes.
encodeLE32 :: Word32 -> ByteString
encodeLE32 w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `div` 256)
  , fromIntegral (w `div` 65536)
  , fromIntegral (w `div` 16777216)
  ]

-- | Decode 4 little-endian bytes to a Word32.
decodeLE32 :: ByteString -> Word32
decodeLE32 bs =
  fromIntegral (BS.index bs 0)
  + fromIntegral (BS.index bs 1) * 256
  + fromIntegral (BS.index bs 2) * 65536
  + fromIntegral (BS.index bs 3) * 16777216

-- | Generate deterministic padding bytes from a signature.
generatePadding :: ByteString -> Int -> ByteString
generatePadding sigBytes needed =
  BS.take needed $ BS.concat
    [ sha256 (sigBytes <> encodeLE32 i) | i <- [0 .. fromIntegral (needed `div` 32 + 1)] ]
