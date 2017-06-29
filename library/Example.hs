{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Example where

import Data.Monoid
-- -- import Data.List
import Pipes
-- import qualified Pipes.ByteString as P
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import System.IO
import Lens.Family
import Control.Foldl
import qualified Pipes.Group as Group
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

{- The test for appendAt' is as follows:

it "appendAt" $ do
      appendAt 3 "|" "xxxyyyy" `shouldBe` ("xxx|yyyy","yyyy")
-}
appendAt :: Int -> T.Text -> T.Text -> (T.Text,T.Text)
appendAt i appendBS bs = let (first, rest) = T.splitAt i bs in (first <> appendBS <> rest,rest)

rows = 500
width = 3

initialBSVal1500 = (T.replicate (500 * 3) "x")

initialOffsets1500  :: [Int]
initialOffsets1500 = replicate 500 3

initialBSVal15000 = (T.replicate (5000 * 3) "x")
initialOffsets15000 :: [Int]
initialOffsets15000 = replicate 5000 3

stripStart :: Monad m => Pipe T.Text T.Text m r
stripStart = do
    chunk <- await
    let text = T.stripStart chunk
    if T.null text
      then stripStart
      else do yield text
              cat

toDelimPipe :: Monad m => [Int] -> Pipe T.Text T.Text m r
toDelimPipe offsets = do
    chunk <- await
    let text = (fixedWidthLine offsets) chunk
    if T.null text
      then (toDelimPipe offsets)
      else do yield text
              cat

{-# INLINE fixedWidthLine #-}
{-
it "fixedWidthLine" $ do
      fixedWidthLine [2,3,4] "xxyyyzzzz" `shouldBe` "xx|yyy|zzzz|"
-}
fixedWidthLine :: [Int] -> T.Text -> T.Text
fixedWidthLine offsets bs = fst $ go offsets ("" :: T.Text,bs)
  where go :: [Int] -> (T.Text,T.Text) -> (T.Text,T.Text)
        go [] x = x
        go (x:xs) (acc,rest) = let (newAcc,newRest) = T.splitAt x rest in go xs (acc <> (fst $ appendAt x "|" newAcc),newRest)

-- stripLines =  view Text.unlines . Group.maps (>-> Text.stripStart) . view Text.lines

-- fileToFileTest = do
--   withFile "test.fixedwidth"  ReadMode  $ \hIn  ->
--     withFile "test.delimited" WriteMode $ \hOut ->
--     runEffect $ Text.fromHandle hIn & Text.lines %= maps (>-> Text.stripStart) >-> Text.toHandle hOut

testDelimToFixed = withFile "test.fixedwidth"  ReadMode  $ \hIn  -> runEffect $ (view Text.unlines . Group.maps (>-> (toDelimPipe [3,4,2]) ). view Text.lines) (Text.fromHandle hIn) >-> Text.stdout

takeLines n = view Text.unlines . Group.takes' n . view Text.lines
