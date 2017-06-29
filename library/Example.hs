{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Example where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import Data.Monoid
import Data.List

{- The test for appendAt' is as follows:

it "appendAt" $ do
      appendAt 3 "|" "xxxyyyy" `shouldBe` ("xxx|yyyy","yyyy")
-}
appendAt :: Int -> BS.ByteString -> BS.ByteString -> (BS.ByteString,BS.ByteString)
appendAt i appendBS bs = let (first, rest) = BS.splitAt i bs in (first <> appendBS <> rest,rest)

rows = 500
width = 3

initialBSVal1500 = (C8.replicate (500 * 3) 'x')

initialOffsets1500  :: [Int]
initialOffsets1500 = replicate 500 3

initialBSVal15000 = (C8.replicate (5000 * 3) 'x')
initialOffsets15000 :: [Int]
initialOffsets15000 = replicate 5000 3

{-# INLINE fixedWidthLine #-}
{-
it "fixedWidthLine" $ do
      fixedWidthLine [2,3,4] "xxyyyzzzz" `shouldBe` "xx|yyy|zzzz|"
-}
fixedWidthLine :: [Int] -> C8.ByteString -> C8.ByteString
fixedWidthLine offsets bs = fst $ go offsets ("" :: C8.ByteString,bs)
  where go :: [Int] -> (C8.ByteString,C8.ByteString) -> (C8.ByteString,C8.ByteString)
        go [] x = x
        go (x:xs) (acc,rest) = let (newAcc,newRest) = C8.splitAt x rest in go xs (acc <> (fst $ appendAt x "|" newAcc),newRest)


testLargeLine = C8.writeFile "testLargeLine.csv" (fixedWidthLine initialOffsets15000 initialBSVal15000)
