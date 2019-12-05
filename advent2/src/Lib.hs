module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List

getCounts :: Ord a => [a] -> [Int]
getCounts = map length . group . sort

offByOne :: String -> String -> Bool
offByOne a b = (length $ filter (uncurry (/=)) $ zip a b) == 1

maybePair :: String -> Maybe String -> Maybe (String, String)
maybePair _ Nothing = Nothing
maybePair s (Just x) = Just (s, x)

isPair :: String -> [String] -> Maybe (String, String)
isPair s strs = let 
    matches = filter (offByOne s) strs in
    case matches of 
        (match:_) -> maybePair s (Just match)
        [] -> Nothing

getPair :: [String] -> Maybe (String, String)
getPair [] = Nothing
getPair [x] = Nothing
getPair (x:xs) = let
    p = isPair x xs in
    case p of 
        Nothing -> getPair xs
        _ -> p


someFunc :: IO ()
someFunc = do
    (name:_) <- getArgs
    contents <- readFile name
    let words = filter (/= "") (lines contents)
    case getPair words of 
        Just (a, b) -> print $ map fst $ filter (uncurry (==)) $ zip a b
