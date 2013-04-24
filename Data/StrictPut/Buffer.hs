{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | 
-- Module: Data.StrictPut
-- Author: Alexander Vershilov <alexander.vershilov@gmail.com>
-- License: BSD-3
-- Stability: unstable
--
module Data.StrictPut.Buffer
  ( Buffer
  , mkBuffer
  , bufferSize
  , reuse
  , extract
  , runPutToBuffer
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S

import Foreign hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

import Data.StrictPut.Types

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)   -- pinned array
                     {-# UNPACK #-} !(Ptr Word8)          -- current position
                     {-# UNPACK #-} !Int                  -- current size

-- | Get current size of data in buffer
bufferSize :: Buffer -> Int
bufferSize (Buffer _ _ i) = i

-- | Reuse buffer O(1)
reuse :: Buffer -> Buffer
reuse (Buffer f _ _) = Buffer f (unsafeForeignPtrToPtr f) 0

-- | Extract bytestring from buffer O(1), if buffer will be reused
-- bytestring will be mangled
extract :: Buffer -> S.ByteString
extract (Buffer f _ c) = S.fromForeignPtr f 0 c

-- | Create new buffer
mkBuffer :: Int -> IO Buffer
mkBuffer sz = do
    fpbuf <- S.mallocByteString sz
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf 0 


runPutToBuffer :: Buffer -> Put -> IO Buffer
runPutToBuffer (Buffer f p x) put = runPut p put >>= \i -> return (Buffer f (p `plusPtr`i)  (x+i))
