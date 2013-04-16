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
  PutM,
  Put,
  runPut,
  runPutToByteString,
  putWord8,
  putWord16be,
  putWord32be,
  putWord64be,
  putByteString,
  -- * Markers
  Marker,
  marker,
  toAddr,
  distance,
  shrink,
  -- * Delay
  DelayedPut,
  undelay,
  contramap,
  delayedWord8,
  delayedWord16be,
  -- ** Another approach
  DelayedPut',
  DPut(..),
  Word16be(..),
  delayedWord8',
  delayedWord16be',
  undelay',
  -- * Buffer
  runPutToBuffer,
  Buffer,
  mkBuffer,
  extract,
  bufferSize,
  reuse
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.ByteString.Builder.Prim
import Data.ByteString.Builder.Prim.Internal
import GHC.Word
import Foreign hiding ( unsafeForeignPtrToPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import GHC.Exts
import System.IO.Unsafe

-- A state monad with state being the pointer to write location.
newtype PutM a = PutM { unPut :: Ptr Word8 -> IO (a, Ptr Word8) }

type Put = PutM ()

-- | Runs the Put writer with write position given
-- by the first pointer argument. Returns the number
-- of words written.
runPut :: Ptr Word8 -> Put -> IO Int
runPut ptr (PutM f) =
     do (_, ptr') <- f ptr
        return (ptr' `minusPtr` ptr)

-- | Allocates a new byte string, and runs the Put writer with that byte string.
-- The first argument is an upper bound on the size of the array needed to do the serialization.
runPutToByteString :: Int -> Put -> S.ByteString
runPutToByteString maxSize put =
  unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> runPut ptr put))
  -- unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> (`minusPtr` ptr) <$> runPut ptr put ))
  
instance Monad PutM where
  return x = PutM (\ptr -> return (x, ptr))
  {-# INLINE return #-}
  (PutM m) >>= f = PutM (\(!ptr) -> do
      (a, ptr') <- m ptr
      case f a of 
        PutM !g -> g ptr')
  {-# INLINE (>>=) #-}
  
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

-- | Delayed action.
newtype DelayedPut a = DelayedPut (a -> IO Int)

contramap :: (a -> b) -> (DelayedPut b) -> (DelayedPut a)
contramap f (DelayedPut g) = DelayedPut (g.f)

undelay :: DelayedPut a 
        -> a 
        -> PutM ()
undelay (DelayedPut f) !x = PutM $ \p -> f x >> return ((),p)
{-# INLINE undelay #-}

delayedWord8 :: PutM (DelayedPut Word8)
delayedWord8 = PutM $ \p -> poke p (0::Word8) >> 
                            return (DelayedPut $ runPut p . putWord8, p `plusPtr` 1)
{-# INLINE delayedWord8 #-}

delayedWord16be :: PutM (DelayedPut Word16)
delayedWord16be = PutM $ \p -> poke (castPtr p) (0::Word16) >>
                               return (DelayedPut $ runPut p . putWord16be, p `plusPtr` 2)
{-# INLINE delayedWord16be #-}

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

encodeWithF :: FixedPrim a -> a -> Put
encodeWithF fixedPrim w = PutM $ \(!ptr) -> runF fixedPrim w ptr >> return ((), ptr `plusPtr` (size fixedPrim))

newtype DelayedPut' a = DelayedPut' (Ptr a)

class DPut a where
  dput :: Ptr a -> a -> IO ()

instance DPut Word8 where
  dput = poke
  {-# INLINE dput #-}

instance DPut Word16be where
  dput p (Word16be x) = void (runPut (castPtr p) (putWord16be x))
  {-# INLINE dput #-}

delayedWord8' :: PutM (DelayedPut' Word8)
delayedWord8' = PutM $ \p -> poke p (0::Word8) >> 
                             return (DelayedPut' p, p `plusPtr` 1)
{-# INLINE delayedWord8' #-}

delayedWord16be' :: PutM (DelayedPut' Word16be)
delayedWord16be' = PutM $ \p -> poke (castPtr p) (0::Word16) >>
                                return (DelayedPut' (castPtr p), p `plusPtr` 2)
{-# INLINE delayedWord16be' #-}

newtype Word16be = Word16be Word16

undelay' :: (DPut a)
         => DelayedPut' a
         -> a
         -> PutM ()
undelay' (DelayedPut' a) !x = PutM $ \p -> dput a x >> return ((),p)
{-# INLINE undelay' #-}
