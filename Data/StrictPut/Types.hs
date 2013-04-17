{-# LANGUAGE BangPatterns, EmptyDataDecls #-}
-- | 
-- Module: Data.StrictPut
-- Author: Andreas Voellmy <andreas.voellmy@yale.edu>
--         Alexander Vershilov <alexander.vershilov@gmail.com>
-- License: BSD-3
-- Stability: unstable
-- 
module Data.StrictPut.Types
  ( PutM(..)
  , Put
  , runPut
  , Word16be
  ) where

import Foreign
import Data.Word

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
  
instance Monad PutM where
  return x = PutM (\ptr -> return (x, ptr))
  {-# INLINE return #-}
  (PutM m) >>= f = PutM (\(!ptr) -> do
      (a, ptr') <- m ptr
      case f a of 
        PutM !g -> g ptr')
  {-# INLINE (>>=) #-}

data Word16be
