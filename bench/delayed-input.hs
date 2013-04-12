import Control.Monad
import Data.StrictPut
import Criterion.Main

test0 w = runPutToByteString 32768 $ do
    mapM_ (\x -> delayedWord8 >>= flip undelay x) w

test1 w =  runPutToByteString 32768 $ do
    mapM_ (\x -> delayedWord16be >>= flip undelay x) w

test2 w = runPutToByteString 32768 $ do
    mapM_ (\x -> do xs1 <- delayedWord16be
                    xs2 <- delayedWord8
                    undelay xs1 (fromIntegral x)
                    undelay xs2 x) w

test0' w = runPutToByteString 32768 $ do
    mapM_ (\x -> delayedWord8' >>= flip undelay' x) w

test1' w =  runPutToByteString 32768 $ do
    mapM_ (\x -> delayedWord16be' >>= flip undelay' (Word16be x)) w

test2' w = runPutToByteString 32768 $ do
    mapM_ (\x -> do xs1 <- delayedWord16be'
                    xs2 <- delayedWord8'
                    undelay' xs1 (Word16be (fromIntegral x))
                    undelay' xs2 x) w

main = defaultMain 
  [ bgroup "word8"
      [ bench "closure"  $ whnf test0 [1..2560]
      , bench "pointer"  $ whnf test0' [1..2560]
      ]
  , bgroup "word16"
      [ bench "closure" $ whnf test1 [1..2560]
      , bench "pointer" $ whnf test1' [1..2560]
      ]
  , bgroup "word8+word16"
      [ bench "closure" $ whnf test2 [1..2560]
      , bench "pointer" $ whnf test2' [1..2560]
      ]
  ]
