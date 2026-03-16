-- | Stateful XMSS key management with crash-safe leaf index persistence.
-- Leaf indices are monotonically increasing and never reused.
module Crypto.KeyManager
  ( ManagedKey
  , KeyState (..)
  , newManagedKey
  , loadManagedKey
  , managedSign
  , managedPublicKey
  , remainingLeaves
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import System.Directory (doesFileExist, renameFile)

import Consensus.Types (XmssPubkey, XmssSignature)
import Crypto.Error (CryptoError (..))
import Crypto.LeanSig
  ( PrivateKey
  , sign
  , serializePrivateKey
  , deserializePrivateKey
  , privateKeyTreeHeight
  , publicKeyFromPrivate
  )

-- | Internal state for a managed key.
data KeyState = KeyState
  { ksPrivateKey :: !PrivateKey
  , ksPublicKey  :: !XmssPubkey
  , ksLeafIndex  :: !Word32
  , ksMaxLeaves  :: !Word64  -- Word64 to avoid 2^32 overflow
  } deriving (Show)

-- | Thread-safe managed key handle.
newtype ManagedKey = ManagedKey (MVar KeyState)

-- | Magic bytes for persistence format.
magic :: ByteString
magic = BS.pack [0x4B, 0x45, 0x59, 0x53]  -- "KEYS"

-- | Format version.
formatVersion :: Word32
formatVersion = 1

-- | Create a new managed key from a private key and public key.
newManagedKey :: PrivateKey -> XmssPubkey -> IO ManagedKey
newManagedKey privKey pubKey = do
  let h = privateKeyTreeHeight privKey
      maxLeaves = 2 ^ (fromIntegral h :: Int) :: Word64
  ManagedKey <$> newMVar (KeyState privKey pubKey 0 maxLeaves)

-- | Load a managed key from a persistence file.
loadManagedKey :: FilePath -> IO (Either CryptoError ManagedKey)
loadManagedKey path = do
  exists <- doesFileExist path
  tmpExists <- doesFileExist (path <> ".tmp")
  if not exists && tmpExists
    then pure (Left (SigningFailed "only .tmp file exists — manual intervention required"))
    else if not exists
      then pure (Left InvalidKeyFormat)
      else do
        bs <- BS.readFile path
        case parseKeyFile bs of
          Left e -> pure (Left e)
          Right ks -> Right . ManagedKey <$> newMVar ks

-- | Sign a message, atomically advancing the leaf index.
-- Persist-before-sign: the new leaf index is committed to disk BEFORE signing.
managedSign :: ManagedKey -> FilePath -> ByteString -> IO (Either CryptoError XmssSignature)
managedSign (ManagedKey mvar) persistPath message =
  modifyMVar mvar $ \ks ->
    if fromIntegral (ksLeafIndex ks) >= ksMaxLeaves ks
      then pure (ks, Left KeyExhausted)
      else do
        -- Step 1: Persist the NEXT leaf index before signing
        let nextIndex = ksLeafIndex ks + 1
            ks' = ks { ksLeafIndex = nextIndex }
        persistKeyState persistPath ks'
        -- Step 2: Sign with the CURRENT (pre-increment) index
        let result = sign (ksPrivateKey ks) message (ksLeafIndex ks)
        pure (ks', result)

-- | Get the public key from a managed key.
managedPublicKey :: ManagedKey -> IO XmssPubkey
managedPublicKey (ManagedKey mvar) = ksPublicKey <$> readMVar mvar

-- | Get the number of remaining leaves.
remainingLeaves :: ManagedKey -> IO Word64
remainingLeaves (ManagedKey mvar) = do
  ks <- readMVar mvar
  pure (ksMaxLeaves ks - fromIntegral (ksLeafIndex ks))

-- ---------------------------------------------------------------------------
-- Persistence
-- ---------------------------------------------------------------------------

-- | Persist key state atomically: write to .tmp, then rename.
persistKeyState :: FilePath -> KeyState -> IO ()
persistKeyState path ks = do
  let serialized = serializeKeyState ks
      tmpPath = path <> ".tmp"
  BS.writeFile tmpPath serialized
  renameFile tmpPath path

-- | Serialize key state to bytes.
-- Format: magic(4) + version(4) + treeHeight(4) + leafIndex(4) + serializedPrivateKey(N)
serializeKeyState :: KeyState -> ByteString
serializeKeyState ks =
  magic
  <> encodeLE32 formatVersion
  <> encodeLE32 (privateKeyTreeHeight (ksPrivateKey ks))
  <> encodeLE32 (ksLeafIndex ks)
  <> serializePrivateKey (ksPrivateKey ks)

-- | Parse a key file into a KeyState.
parseKeyFile :: ByteString -> Either CryptoError KeyState
parseKeyFile bs
  | BS.length bs < 16 = Left InvalidKeyFormat
  | BS.take 4 bs /= magic = Left InvalidKeyFormat
  | decodeLE32 (BS.take 4 (BS.drop 4 bs)) /= formatVersion = Left InvalidKeyFormat
  | otherwise = do
      let treeHeight = decodeLE32 (BS.take 4 (BS.drop 8 bs))
          leafIndex = decodeLE32 (BS.take 4 (BS.drop 12 bs))
          skBytes = BS.drop 16 bs
      privKey <- deserializePrivateKey skBytes
      let pubKey = publicKeyFromPrivate privKey
          maxLeaves = 2 ^ (fromIntegral treeHeight :: Int) :: Word64
      Right (KeyState privKey pubKey leafIndex maxLeaves)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

encodeLE32 :: Word32 -> ByteString
encodeLE32 w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `div` 256)
  , fromIntegral (w `div` 65536)
  , fromIntegral (w `div` 16777216)
  ]

decodeLE32 :: ByteString -> Word32
decodeLE32 bs =
  fromIntegral (BS.index bs 0)
  + fromIntegral (BS.index bs 1) * 256
  + fromIntegral (BS.index bs 2) * 65536
  + fromIntegral (BS.index bs 3) * 16777216
