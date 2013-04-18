import Control.Applicative 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Builder
import Data.List
import Data.Monoid
import Data.Word

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Test.QuickCheck hiding (Result)

import Data.StrictPut

test_dputWord8    :: [Word8] -> Result
test_dputWord8   ws  = (runPutToByteString 32768 $ mapM_ (\w -> delayedWord8 >>= \x -> undelay x w) ws) ?== BS.pack ws
test_dputWord16be :: [Word16] -> Result
test_dputWord16be ws = (runPutToByteString 32768 $ mapM_ (\w -> delayedWord16be >>= \x -> undelay x w) ws) 
    ?== (BS.concat . LBS.toChunks . toLazyByteString $ foldl' (\x y -> x <> word16BE y) mempty ws)
test_contravariant :: Word8 -> Result
test_contravariant w = l ==? BS.singleton (w+1)
    where 
      l = runPutToByteString 32768 $ do
            x <- contramap (+1) . closure <$> delayedWord8
            undelay x w


main = hspec $ do
    describe "minial functionality" $ do
        prop "word8"  $ (test_dputWord8 :: [Word8] -> Result)
        prop "word16" $ (test_dputWord16be :: [Word16] -> Result)
        prop "contra" $ (test_contravariant :: Word8 -> Result)
        
