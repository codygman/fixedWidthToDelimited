{-# LANGUAGE OverloadedStrings #-}
-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main
import Example

main :: IO ()
main = defaultMain [ bench "appendAt" (whnf (appendAt 3 "|") "xxxyyy")
                   , bench "fixedWidthLine 1500" (whnf (fixedWidthLine initialOffsets1500) initialBSVal1500)
                   , bench "fixedWidthLine 15000" (whnf (fixedWidthLine initialOffsets15000) initialBSVal15000)
                   ]
