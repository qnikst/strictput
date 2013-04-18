{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | 
-- Module: Data.StrictPut
-- Author: Andreas Voellmy <andreas.voellmy@yale.edu>
--         Alexander Vershilov <alexander.vershilov@gmail.com>
-- License: BSD-3
-- Stability: unstable
--
-- This module is improved version of Nettle.OpenFlow.StrictPut that
-- supports additional features like Markers and delayed input.
--
-- This module provides a monad for serializing data into byte strings.
-- It provides mostly the same interface that Data.Binary.Put does.
-- However, the implementation is different. It allows for the data to be
-- serialized into an existing array of Word8 values. This differs from the Data.Binary.Put
-- data type, which allocates a Word8 array every time a value is serialized.
-- This module's implementation is useful if you want to reuse the Word8 array for many serializations.
-- In the case of an OpenFlow server, we can reuse a buffer to send messages, since we have no use
-- for the the Word8 array, except to pass it to an IO procedure to write the data to a socket or file.
module Data.StrictPut (
  runPutToByteString,
  -- * Markers
  Marker,
  marker,
  toAddr,
  distance,
  shrink,
  -- * Buffer
  runPutToBuffer,
  Buffer,
  mkBuffer,
  extract,
  bufferSize,
  reuse,
  module Data.StrictPut.DelayedInput,
  module Data.StrictPut.Types,
  module Data.StrictPut.PutM
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import GHC.Word
import Foreign hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import GHC.Exts
import System.IO.Unsafe
import Data.StrictPut.DelayedInput
import Data.StrictPut.Types
import Data.StrictPut.PutM


-- | Allocates a new byte string, and runs the Put writer with that byte string.
-- The first argument is an upper bound on the size of the array needed to do the serialization.
runPutToByteString :: Int -> Put -> S.ByteString
runPutToByteString maxSize put =
  unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> runPut ptr put))
  -- unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> (`minusPtr` ptr) <$> runPut ptr put ))
  
-- | get current address

-- | Mark currect address
newtype Marker = Marker (Ptr Word8)

-- | Create new marker at current position/
marker :: PutM Marker
marker = PutM $ \x -> return (Marker x, x)
{-# INLINE marker #-}

-- | Find difference in current position and marker.
distance :: Marker 
         -> PutM Int
distance (Marker x) = PutM $ \x' -> return (x' `minusPtr` x, x')
{-# INLINE distance #-}

shrink :: Marker
        -> Put
shrink (Marker x) = PutM $ \_ -> return  ((),x)
{-# INLINE shrink #-}

-- | Get real address
toAddr :: Marker -> Addr#
toAddr (Marker (Ptr a)) = a
{-# INLINE toAddr #-}

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)   -- pinned array
                     {-# UNPACK #-} !(Ptr Word8)          -- current position
                     {-# UNPACK #-} !Int                  -- current size

bufferSize :: Buffer -> Int
bufferSize (Buffer _ _ i) = i

reuse :: Buffer -> Buffer
reuse (Buffer f _ _) = Buffer f (unsafeForeignPtrToPtr f) 0

extract :: Buffer -> S.ByteString
extract (Buffer f _ c) = S.fromForeignPtr f 0 c

mkBuffer :: Int -> IO Buffer
mkBuffer sz = do
    fpbuf <- S.mallocByteString sz
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf 0 


runPutToBuffer :: Buffer -> Put -> IO Buffer
runPutToBuffer (Buffer f p x) put = runPut p put >>= \i -> return (Buffer f (p `plusPtr`i)  (x+i))
