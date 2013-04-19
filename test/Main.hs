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

test_putWord8 :: [Word8] -> Result
test_putWord8 ws = (runPutToByteString 32768 $ mapM_ putWord8 ws) ?== BS.pack ws
test_putWord16be :: [Word16] -> Result
test_putWord16be ws = (runPutToByteString 32768 $ mapM_ putWord16be ws)
    ?== (BS.concat . LBS.toChunks . toLazyByteString $ foldl' (\x y -> x <> word16BE y) mempty ws)
test_putWord32be :: [Word32] -> Result
test_putWord32be ws = (runPutToByteString 32768 $ mapM_ putWord32be ws)
    ?== (BS.concat . LBS.toChunks . toLazyByteString $ foldl' (\x y -> x <> word32BE y) mempty ws)
test_putWord64be :: [Word64] -> Result
test_putWord64be ws = (runPutToByteString 32768 $ mapM_ putWord64be ws)
    ?== (BS.concat . LBS.toChunks . toLazyByteString $ foldl' (\x y -> x <> word64BE y) mempty ws)
test_putByteString :: [Word8] -> Result
test_putByteString ws = (runPutToByteString 32768 $ putByteString bs) ?== bs
  where bs = BS.pack ws

main = hspec $ do
    describe "minial delayed put functionality" $ do
        prop "word8"  $ (test_dputWord8 :: [Word8] -> Result)
        prop "word16" $ (test_dputWord16be :: [Word16] -> Result)
        prop "contra" $ (test_contravariant :: Word8 -> Result)
    describe "minimal put functionality" $ do
        prop "bytestring" $ (test_putByteString :: [Word8]  -> Result)
        prop "word8"      $ (test_putWord8      :: [Word8]  -> Result)
        prop "word16be"   $ (test_putWord16be   :: [Word16] -> Result)
        prop "word32be"   $ (test_putWord32be   :: [Word32] -> Result)
        prop "word64be"   $ (test_putWord64be   :: [Word64] -> Result)

        
