module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List.Split

doParse :: [Char] -> Int
doParse ('+':cs) = read cs
doParse cs = read cs

findDup :: Eq a => [a] -> [a] -> a
findDup _ [] = error "Could not find dupe"
findDup seen (x:xs) = if elem x seen then x else findDup (x:seen) xs

someFunc :: IO ()
someFunc = do
    (dat:_) <- getArgs
    content <- readFile dat
    let words = filter ((/= 0) . length) (splitOn "\n" content)
    let ints = map doParse words
    let inf = cycle ints
    let freqs = scanl (+) 0 inf
    print $ findDup [] freqs
