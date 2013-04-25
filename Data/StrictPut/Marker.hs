{-# LANGUAGE MagicHash #-}
-- Module:    Data.StrictPut
-- Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
-- License:   BSD-3
-- Stability: unstable

--- | Module provides delayed input functionality,
-- this basically means that you can input delayed
-- action (at the moment it will be filled with zeros)
-- and then write correct value. 
--
-- This functionality is usefull for inserting length 
-- prefixed data in case if you don't know the length 
-- at begining of put.
module Data.StrictPut.Marker
  ( Marker
  , marker
  , toAddr#
  , toPtr
  -- * utility functions
  , distance
  , shrink
  ) where

import Data.Word
import Foreign.Ptr
import GHC.Exts
import Data.StrictPut.Types

-- | Mark currect address
newtype Marker = Marker {unMarker :: Ptr Word8}

-- | Get real address
toAddr# :: Marker -> Addr#
toAddr# (Marker (Ptr a)) = a
{-# INLINE toAddr# #-}

toPtr :: Marker -> Ptr a
toPtr = castPtr . unMarker

-- | Create new marker at current position/
marker :: PutM Marker
marker = PutM $ \x -> return (Marker x, x)
{-# INLINE marker #-}

-- | Find difference in current position and marker.
distance :: Marker 
         -> PutM Int
distance (Marker x) = PutM $ \x' -> return (x' `minusPtr` x, x')
{-# INLINE distance #-}

-- | Shrink data to marker
shrink :: Marker
        -> Put
shrink (Marker x) = PutM $ \_ -> return  ((),x)
{-# INLINE shrink #-}

