module Main where

import Control.Monad
import Data.List

main = do
     distances <- liftM (map read . words) (getLine >> getLine) :: IO [Int]
     [s1, s2] <- liftM (sort . map read . words) getLine:: IO [Int]
     let total = sum distances
         dist = sum $ drop (s1 - 1) $ take (s2 - 1) distances
     print $ min dist (total - dist)