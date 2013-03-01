module Main where

import Data.List

main = do
     distances <- getLine >> getLine >>= (return . map read . words) :: IO [Int]
     [s1, s2] <- getLine >>= (return . sort . map read . words) :: IO [Int]
     let total = sum distances
         dist = sum $ drop (s1 - 1) $ take (s2 - 1) distances
     print $ min dist (total - dist)