import Data.StrictPut
import Criterion.Main

test0 w = runPutToByteString 32768 $ do
    x <- delayedWord8
    undelay x w 

test1 w =  runPutToByteString 32768 $ do
    x2 <- delayedWord16be
    undelay x2 w

test2 w = runPutToByteString 32768 $ do
    x <- delayedWord16be
    x1 <- delayedWord8
    undelay x1 (fromIntegral w)
    undelay x w


test0' w = runPutToByteString 32768 $ do
    x <- delayedWord8'
    undelay' x w 

test1' w =  runPutToByteString 32768 $ do
    x2 <- delayedWord16be'
    undelay' x2 (Word16be w)

test2' w = runPutToByteString 32768 $ do
    x <- delayedWord16be'
    x1 <- delayedWord8'
    undelay' x1 (fromIntegral w)
    undelay' x (Word16be w)

main = defaultMain 
  [ bgroup "word8"
      [ bench "closure"  $ nf (map test0) [1..256]
      , bench "pointer"  $ nf (map test0') [1..256]
      ]
  , bgroup "word16"
      [ bench "closure" $ nf (map test1) [1..256]
      , bench "pointer" $ nf (map test1') [1..256]
      ]
  , bgroup "word8+word16"
      [ bench "delayed word8+word16" $ nf (map test2) [1..256]
      , bench "delayed word8+word16" $ nf (map test2') [1..256]
      ]
  ]
