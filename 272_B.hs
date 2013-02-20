module Main where

import Data.List

c :: Integer -> Integer
c 1 = 0
c n = n * (n - 1) `div` 2

f :: Integer -> Integer
f = f' 0
    where
      f' acc 0 = acc
      f' acc x
        | x `mod` 2 == 0
          = f' acc (x `div` 2)
        | otherwise
          = f' (acc + 1) (x `div` 2)


solve :: [Integer] -> Integer
solve = sum . map (c.(genericLength :: [Integer] -> Integer)) . group . sort

main = getLine >> getLine >>= (print . solve . map (f.read) . words)