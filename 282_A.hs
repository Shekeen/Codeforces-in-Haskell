module Main where

solve :: [String] -> Int
solve = solve' 0
        where
          solve' x [] = x
          solve' x (s:ss)
            | s!!1 == '+' = solve' (x+1) ss
            | s!!1 == '-' = solve' (x-1) ss
            | otherwise = solve' x ss

main = getLine >> getContents >>= (print . solve . lines)
