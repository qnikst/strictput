import Control.Monad
import Data.StrictPut
import Criterion.Main
import qualified Data.ByteString as BS


test0'' w = runPutToByteString 2560 $ do
    mapM_ (\x -> delayedWord8 >>= flip undelay x) w

test1'' w =  runPutToByteString (2560*2) $ do
    mapM_ (\x -> delayedWord16be >>= flip undelay x) w

test2'' w = runPutToByteString (2560*3) $ do
    mapM_ (\x -> do xs1 <- delayedWord16be
                    xs2 <- delayedWord8
                    undelay xs1 (fromIntegral x)
                    undelay xs2 x) w

test3'' w = runPutToByteString (2560*3) $ do
    mapM_ (\x -> do xs1 <- delayedWord16be
                    xs2 <- delayedWord8
                    undelay xs1 x
                    undelay xs2 (fromIntegral x)) w

test_bs w = runPutToByteString 256 $ do
              xs1 <- delayedWord16be
              putByteString $ BS.replicate 42 42
              undelay xs1 131


main = defaultMain 
  [ bgroup "word8"
      [ bench "typeclass" $ nf test0'' [1..2560] ]
  , bgroup "word16"
      [ bench "typeclass" $ nf test1'' [1..2560]
      ]
  , bgroup "word8+word16"
      [ bench "typeclass" $ nf test2'' [1..2560]
      ]
  , bgroup "word8+word16-2"
      [ bench "typeclass" $ nf test3'' [1..2560]
      ]
  ]
