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


main = defaultMain 
  [ bgroup "delayed"
      [ bench "delayed word8"  $ nf (map test0) [1..256]
      , bench "delayed word16" $ nf (map test1) [1..256]
      , bench "delayed word8+word16" $ nf (map test2) [1..256]
      ]
  ]
