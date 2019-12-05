module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List

getCounts = map length . group . sort

someFunc :: IO ()
someFunc = do
    (name:_) <- getArgs
    contents <- readFile name
    let words = filter (/= "") (lines contents)
    let counts = map getCounts words
    let twos = length $ filter (any (== 2)) counts
    let threes = length $ filter (any (== 3)) counts
    print $ twos * threes
