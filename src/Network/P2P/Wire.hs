-- | Wire format helpers: SSZ + compression for gossipsub messages.
-- Uses zlib raw deflate as a fallback since the snappy C library is not
-- available.  The interface is kept stable so switching back to Snappy later
-- is a one-line change.
module Network.P2P.Wire
  ( encodeWire
  , decodeWire
  , compressWire
  , decompressWire
  ) where

import qualified Codec.Compression.Zlib.Raw as Zlib
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import SSZ.Common (SszEncode (..), SszDecode (..), SszError (..))

-- | Encode a value as SSZ then compress.
encodeWire :: SszEncode a => a -> ByteString
encodeWire = compressWire . sszEncode

-- | Decompress then SSZ-decode.
decodeWire :: SszDecode a => ByteString -> Either SszError a
decodeWire bs =
  let decompressed = decompressWire bs
  in  if BS.null decompressed && not (BS.null bs)
        then Left (CustomError "decompression failed")
        else sszDecode decompressed

-- | Compress a 'ByteString' with raw deflate.
compressWire :: ByteString -> ByteString
compressWire = LBS.toStrict . Zlib.compress . LBS.fromStrict

-- | Decompress a raw-deflate-compressed 'ByteString'.
decompressWire :: ByteString -> ByteString
decompressWire = LBS.toStrict . Zlib.decompress . LBS.fromStrict
