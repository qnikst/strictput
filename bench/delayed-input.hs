import Control.Monad
import Data.StrictPut
import Criterion.Main

test0 w = runPutToByteString 32768 $ do
    xs <- replicateM (length w) delayedWord8
    mapM_ (uncurry undelay) $ zip xs w

test1 w =  runPutToByteString 32768 $ do
    xs <- replicateM (length w) delayedWord16be
    mapM_ (uncurry undelay) $ zip xs w

test2 w = runPutToByteString 32768 $ do
    xs1 <- replicateM (length w) delayedWord16be
    xs2 <- replicateM (length w) delayedWord8
    mapM_ (uncurry undelay) $ zip xs1 (map fromIntegral w)
    mapM_ (uncurry undelay) $ zip xs2 w


test0' w = runPutToByteString 32768 $ do
    xs <- replicateM (length w) delayedWord8'
    mapM_ (uncurry undelay') $ zip xs w

test1' w =  runPutToByteString 32768 $ do
    xs <- replicateM (length w) delayedWord16be'
    mapM_ (uncurry undelay') $ zip xs (map Word16be w)

test2' w = runPutToByteString 32768 $ do
    xs1 <- replicateM (length w) delayedWord16be'
    xs2 <- replicateM (length w) delayedWord8'
    mapM_ (uncurry undelay') $ zip xs1 (map (Word16be . fromIntegral) w)
    mapM_ (uncurry undelay') $ zip xs2 w

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
