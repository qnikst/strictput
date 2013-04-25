{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- Module: Data.StrictPut
-- Author: Alexander Vershilov <alexander.vershilov@gmail.com>
-- License: BSD-3
-- Stability: unstable
module Data.StrictPut.LookBehind
  ( lookByteString
  -- lookVector ?
  , withAddr#
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.StrictPut.Marker
import Data.StrictPut.Types
import GHC.Exts
import System.IO.Unsafe

-- | Temporary extract bytestring from current putter
lookByteString :: Marker -> PutM ByteString
lookByteString m = do
        len <- distance m
        return . unsafePerformIO $ unsafePackAddressLen len (toAddr# m)

-- | Perform a function on unpacket bytes
withAddr# :: Marker -> ( Addr# -> Int# -> a) -> PutM a
withAddr# m f = do 
  I# len# <- distance m
  return $ f (toAddr# m) len#
