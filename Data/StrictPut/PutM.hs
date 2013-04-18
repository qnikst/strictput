{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
-- | 
-- Module: Data.StrictPut
-- Author: Andreas Voellmy <andreas.voellmy@yale.edu>
--         Alexander Vershilov <alexander.vershilov@gmail.com>
-- License: BSD-3
-- Stability: unstable
--
module Data.StrictPut.PutM
  ( putWord8
  , putWord16be
  , putWord32be
  , putWord64be
  , putByteString
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.ByteString.Builder.Prim
import Data.ByteString.Builder.Prim.Internal
import GHC.Word
import Foreign
import GHC.Exts
import Data.StrictPut.Types

putWord8 :: Word8 -> Put
putWord8 = encodeWithF word8
{-# INLINE putWord8 #-}

putWord16be :: Word16 -> Put
putWord16be = encodeWithF word16BE
{-# INLINE putWord16be #-}

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Put
putWord32be = encodeWithF word32BE
{-# INLINE putWord32be #-}

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Put
putWord64be = encodeWithF word64BE
{-# INLINE putWord64be #-}

putByteString :: S.ByteString -> Put
putByteString !bs = PutM f
  where f !ptr = case S.toForeignPtr bs of
                     (fp, offset, len) -> do
                         withForeignPtr fp $ \bsptr ->
                           S.memcpy ptr (bsptr `plusPtr` offset)
                                        (fromIntegral len)
                         return ((),ptr `plusPtr`  len)
{-# INLINE putByteString #-}

encodeWithF :: FixedPrim a -> a -> Put
encodeWithF fixedPrim w = PutM $ \(!ptr) -> runF fixedPrim w ptr >> return ((), ptr `plusPtr` (size fixedPrim))
