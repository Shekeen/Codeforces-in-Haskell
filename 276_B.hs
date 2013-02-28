module Main where

import Data.List

odds :: String -> Int
odds = (foldl (+) 0) . map ((`mod` 2) . length) . group . sort

solve :: String -> String
solve str = if odds str == 0 || (odds str) `mod` 2 == 1
               then "First"
               else "Second"

main = getLine >>= (putStrLn . solve)