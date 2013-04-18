{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}
-- | Module provides delayed input functionality,
-- this basically means that you can input delayed
-- action (at the moment it will be filled with zeros)
-- and then write correct value. 
--
-- This functionality is usefull for inserting length 
-- prefixed data in case if you don't know the length 
-- at begining of put.
module Data.StrictPut.DelayedInput
  ( -- * datatypes and classes
    DelayedPut(..)
  , DelayedClosure(..)
  , Delayed(..)
  -- * methods
  , delayedWord8
  , delayedWord16be
  -- * reexports
  , Contravariant(..)
  ) where

import Control.Monad (void)
import Data.Functor.Contravariant
import Foreign hiding (void)
import GHC.Word

import Data.StrictPut.Types
import Data.StrictPut.PutM

-- | Wrapper for delayed variable
newtype DelayedPut a b = DelayedPut (Ptr Word8)

class Delayed a b c where
  undelay :: a b c -> c -> Put

instance Delayed DelayedPut Word8 Word8 where
  undelay (DelayedPut a) !x = PutM $ \p -> poke a x >> return ((),p)
  {-# INLINE undelay #-}

instance Delayed DelayedPut Word16be Word16 where
  undelay (DelayedPut a) !x = PutM $ \p -> runPut a (putWord16be x) >> return ((),p)
  {-# INLINE undelay #-}

delayedWord8 :: PutM (DelayedPut Word8 Word8)
delayedWord8 = PutM $ \p -> poke p (0::Word8) >> return (DelayedPut p,p `plusPtr` 1)
{-# INLINE delayedWord8 #-}

delayedWord16be :: PutM (DelayedPut Word16be Word16)
delayedWord16be = PutM $ \p -> poke (castPtr p) (0::Word16) >> return (DelayedPut p,p `plusPtr` 2)
{-# INLINE delayedWord16be #-}

-- | Wrapper for delayed function, this approach is slitly less efficient
-- hovewer it supports 'contramap'
newtype DelayedClosure a b = DelayedClosure (b -> IO Int)

instance Contravariant (DelayedClosure a) where
  contramap f (DelayedClosure g) = DelayedClosure (g.f)
  {-# INLINE contramap #-}

instance Delayed DelayedClosure a b where
  undelay (DelayedClosure f) !x = PutM $ \p -> f x >> return ((),p)
  {-# INLINE undelay #-}
