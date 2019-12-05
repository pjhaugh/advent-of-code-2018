module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List.Split

doParse :: [Char] -> Int
doParse ('+':cs) = read cs
doParse cs = read cs

someFunc :: IO ()
someFunc = do
    (dat:_) <- getArgs
    content <- readFile dat
    let words = filter ((/= 0) . length) (splitOn "\n" content)
    let ints = map doParse words
    print $ sum ints     
