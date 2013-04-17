{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
-- | Module provides delayed input functionality,
-- this basically means that you can input delayed
-- action (at the moment it will be filled with zeros)
-- and then write correct value. 
--
-- This functionality is usefull for inserting length 
-- prefixed data in case if you don't know the length 
-- at begining of put.
module Data.StrictPut.DelayedInput
  ( -- * value
    DelayedPut'(..)
  , DPut(..)
  , Word16be(..)
  , delayedWord8'
  , delayedWord16be'
  , undelay'
    -- * closure
  , DelayedPut
  , delayedWord8
  , delayedWord16be
  , undelay
  -- * multiparam typeclass
  , DelayedPut''(..)
  , Delayed(..)
  , delayedWord8''
  , delayedWord16be''
  -- * reexports
  , Contravariant(..)
  ) where

import Control.Monad (void)
import Data.Functor.Contravariant
import Foreign hiding (void)
import GHC.Word

import Data.StrictPut.Types
import Data.StrictPut.PutM

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

-- | Delayed action.
newtype DelayedPut a = DelayedPut (a -> IO Int)

instance Contravariant DelayedPut where
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

newtype DelayedPut'' a b = DelayedPut'' (Ptr Word8)

class Delayed a b c where
  undelay'' :: a b c -> c -> Put

instance Delayed DelayedPut'' Word8 Word8 where
  undelay'' (DelayedPut'' a) !x = PutM $ \p -> poke a x >> return ((),p)
  {-# INLINE undelay'' #-}

instance Delayed DelayedPut'' Word16be Word16 where
  undelay'' (DelayedPut'' a) !x = PutM $ \p -> runPut a (putWord16be x) >> return ((),p)
  {-# INLINE undelay'' #-}

delayedWord8'' :: PutM (DelayedPut'' Word8 Word8)
delayedWord8'' = PutM $ \p -> poke p (0::Word8) >> return (DelayedPut'' p,p `plusPtr` 1)
{-# INLINE delayedWord8'' #-}

delayedWord16be'' :: PutM (DelayedPut'' Word16be Word16)
delayedWord16be'' = PutM $ \p -> poke (castPtr p) (0::Word16) >> return (DelayedPut'' p,p `plusPtr` 2)
{-# INLINE delayedWord16be'' #-}
