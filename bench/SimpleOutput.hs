{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as BS
import System.IO
import Control.Monad
import Data.Bits
import Data.Word
import Data.Monoid
import Data.StrictPut
import Data.ByteString.Lazy.Builder  -- requires bytestring-0.10.x
import Criterion.Main
import System.IO.Silently

bs = BS.replicate 64 0

builder = mconcat (replicate 1000000 (byteString bs))

buildPL = mconcat (replicate 64 (word8 0))
{-# INLINE buildPL #-}

buildMAC :: Word64 -> Builder
buildMAC i = word16BE (fromIntegral $ i `shiftR` 32) <> word32BE (fromIntegral $ i .&. 0xFFFFFFFF)
{-# INLINE buildMAC #-}

putMAC :: Word64 -> Put
putMAC i = putWord16be (fromIntegral $ i `shiftR` 32) >> putWord32be (fromIntegral $ i .&. 0xFFFFFFFF)
{-# INLINE putMAC #-}

buildEth :: (Word64,Word64) -> Builder
buildEth (i,j) = mconcat [ buildMAC i, buildMAC j, word32BE 0, word16BE 0 -- Ethernet Frame header
                       , word32BE 0, word32BE 0, word32BE 0, word32BE 0 -- IP Header
                       , word32BE 0, word32BE 0, word32BE 0, word32BE 0 -- TCP Header
                       , byteString bs                                  -- Payload
                       , word32BE 0                                     -- CRC32
                       ]
{-# INLINE buildEth #-}

buildEth2 :: (Word64,Word64) -> Builder
buildEth2 (i,j) = mconcat [ buildMAC i, buildMAC j, word32BE 0, word16BE 0 -- Ethernet Frame header
                          , lazyByteString ipHeader                        -- IP Header
                          , lazyByteString tcpHeader                       -- TCP Header
                          , byteString bs                                  -- Payload
                          , word32BE 0                                     -- CRC32
                          ]
  where ipHeader  = toLazyByteString  $ mconcat [word32BE 0, word32BE 0, word32BE 0, word32BE 0]
        tcpHeader = toLazyByteString  $ mconcat [word32BE 0, word32BE 0, word32BE 0, word32BE 0]
{-# NOINLINE buildEth2 #-}

putEth2 :: (Word64,Word64) -> Put
putEth2 (i,j) = do
        putMAC i 
        putMAC j
        putWord32be 0
        putWord16be 0
        putIpHeader
        putTcpHeader
        putByteString bs
        putWord32be 0
  where putIpHeader = do putWord32be 0 >> putWord32be 0 >> putWord32be 0 >> putWord32be 0
        putTcpHeader = do putWord32be 0 >> putWord32be 0 >> putWord32be 0 >> putWord32be 0
{-# NOINLINE putEth2 #-}

buildAll = mconcat (map buildEth2 [(i,j)|i <- [1..100], j <- [1..100]])
{-# NOINLINE buildAll #-}

-- StrictPut w/o buffering
main1 = do
  mapM_ (\x -> BS.hPutStr stdout (runPutToByteString 4096 (putEth2 x))) [(i,j) | i <- [1..100], j <- [1..100]]

-- StrictPut with buffering w/o reusing
main2 = do
  b <- mkBuffer 32768 
  let go b' [] = BS.hPutStr stdout (extract b')
      go b' i@(x:xs) = 
         if bufferSize b' > 16346
            then BS.hPutStr stdout (extract b') >> mkBuffer 32768 >>= \b'' -> go  b'' i
            else do
                b'' <- runPutToBuffer b' (putEth2 x)
                go b'' xs
  go b [(i,j) | i <- [1..100], j <- [1..100]]

-- StrictPut with buffering with reusing
main3 = do
  b <- mkBuffer 32768 
  let go b' [] = BS.hPutStr stdout (extract b')
      go b' i@(x:xs) = 
         if bufferSize b' > 16346
            then BS.hPutStr stdout (extract b') >> go  (reuse b') i
            else do
                b'' <- runPutToBuffer b' (putEth2 x)
                go b'' xs
  go b [(i,j) | i <- [1..100], j <- [1..100]]
 
-- bytestring
main4 = do
  hPutBuilder stdout buildAll 

main = defaultMain 
  [ bgroup "only generation" 
      [ bench "strict put w/o buffering" $ nf (map (\x -> runPutToByteString 4096 (putEth2 x))) [(i,j) | i <- [1..100], j <- [1..100]]
      , bench "bytestring builder" $ nf (map (\x -> toLazyByteString (buildEth x))) [(i,j) | i <- [1..100], j <- [1..100]]
      ]
  , bgroup "stdout output under silence"
      [ bench "strict put w/buffering w/o reusing" $ silence main2
      , bench "strict put w/buffering w/reusing" $ silence main3
      , bench "bytestring-builder" $ silence main4
      ]
  ]
