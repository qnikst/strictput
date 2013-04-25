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
  module Data.StrictPut.DelayedInput,
  module Data.StrictPut.Marker,
  module Data.StrictPut.Types,
  module Data.StrictPut.PutM,
  module Data.StrictPut.Buffer,
  module Data.StrictPut.LookBehind
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import System.IO.Unsafe
import Data.StrictPut.DelayedInput
import Data.StrictPut.Types
import Data.StrictPut.Marker
import Data.StrictPut.PutM
import Data.StrictPut.Buffer
import Data.StrictPut.LookBehind

-- | Allocates a new byte string, and runs the Put writer with that byte string.
-- The first argument is an upper bound on the size of the array needed to do the serialization.
runPutToByteString :: Int -> Put -> S.ByteString
runPutToByteString maxSize put =
  unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> runPut ptr put))
  -- unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> (`minusPtr` ptr) <$> runPut ptr put ))
  
