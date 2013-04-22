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
  , putZeros
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import GHC.Word
import Foreign
import GHC.Exts
import Data.StrictPut.Types

putWord8 :: Word8 -> Put
putWord8 !w = PutM (\(!ptr) -> do { poke ptr w; return ((), ptr `plusPtr` 1) })
{-# INLINE putWord8 #-}

putWord16be :: Word16 -> Put
putWord16be !w = PutM f
  where f !ptr =
          do poke ptr (fromIntegral (shiftr_w16 w 8) :: Word8)
             poke (ptr `plusPtr` 1) (fromIntegral (w) :: Word8)
             return ((), ptr `plusPtr` 2)
{-# INLINE putWord16be #-}

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Put
putWord32be !w = PutM f
  where f !p =
          do poke p (fromIntegral (shiftr_w32 w 24) :: Word8)
             poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
             poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 8) :: Word8)
             poke (p `plusPtr` 3) (fromIntegral (w) :: Word8)
             return ((), p `plusPtr` 4)
{-# INLINE putWord32be #-}

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Put
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
putWord64be !w =
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w :: Word32
    in PutM $ \(!p) -> do
      poke p (fromIntegral (shiftr_w32 a 24) :: Word8)
      poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
      poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 8) :: Word8)
      poke (p `plusPtr` 3) (fromIntegral (a) :: Word8)
      poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
      poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
      poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 8) :: Word8)
      poke (p `plusPtr` 7) (fromIntegral (b) :: Word8)
      return ((), p `plusPtr` 8)
#else
putWord64be !w = PutM $ \(!p) -> do
  poke p (fromIntegral (shiftr_w64 w 56) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
  poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
  poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
  poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 8) :: Word8)
  poke (p `plusPtr` 7) (fromIntegral (w) :: Word8)
  return ((), p `plusPtr` 8)
#endif
{-# INLINE putWord64be #-}

putByteString :: S.ByteString -> Put
putByteString !bs = PutM f
  where f !ptr =
          let (fp, offset, len) = S.toForeignPtr bs
          in do withForeignPtr fp $ \bsptr -> S.memcpy ptr (bsptr `plusPtr` offset) (fromIntegral len)
                return ((), ptr `plusPtr` len)
{-# INLINE putByteString #-}

putZeros :: Int -> Put
putZeros !i = PutM $ \p -> S.memset p 0 (fromIntegral i) >> return ((), p `plusPtr` i)
{-# INLINE putZeros #-}

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64


#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#` i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#` i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)
#endif
#endif
