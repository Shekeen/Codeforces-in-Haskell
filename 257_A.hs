module Main where

import Data.List
import Control.Monad

qsort [] = []
qsort (x:xs) = qsort more ++ [x] ++ qsort less
      where
      (less, more) = partition (\ x' -> x' < x) xs

solveM :: Int -> Int -> [Int] -> Maybe Int
solveM 0 0 _ = Just 0
solveM _ 0 _ = Nothing
solveM m k [] = if m > k
                then Nothing
                else Just 0
solveM m k (x:xs) = if m > k
                    then (liftM2 (+)) (Just 1) (solveM m (k+x-1) xs)
                    else Just 0

solve :: Int -> Int -> [Int] -> Int
solve m k xs = case (solveM m k xs) of
                    Just a -> a
                    Nothing -> -1
                    
main = do
     nmkStr <- getLine
     let nmk = map (read :: String -> Int) $ words nmkStr
         m = nmk!!1
         k = nmk!!2
     electricStr <- getLine
     let electric = qsort $ map (read :: String -> Int) $ words electricStr
     print $ solve m k electric