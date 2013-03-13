module Main where

import Data.List

qsort [] = []
qsort (x:xs) = qsort more ++ [x] ++ qsort less
      where
      (less, more) = partition (< x) xs

solve :: Int -> [Int] -> [Int]
solve k a = if last kFirst == head rest
              then [-1]
              else [last kFirst, 0]
                 where
                 (kFirst, rest) = splitAt k a

main = do
     nkStr <- getLine
     let [n, k] = map read $ words nkStr
     aStr <- getLine
     let a = qsort $ map read $ words aStr
     if k < n
       then putStrLn $ unwords $ map show $ solve k a
       else if k == n
              then putStrLn "0 0"
              else putStrLn "-1"