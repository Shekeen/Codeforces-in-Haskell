module Main where

import Control.Monad.State
import Data.List
import Data.Int

type Ladder = [Int64]

makeTuple :: [Int64] -> (Int64, Int64)
makeTuple [x, y] = (x, y)

throwBoxes :: [(Int64, Int64)] -> State (Ladder, [Int64]) [Int64]
throwBoxes [] = do
           (_, heights) <- get
           return $ reverse heights
throwBoxes ((w,h):bs) = do
           (ladder, heights) <- get
           let oldHeight = maximum $ genericTake w ladder
               newHeight = oldHeight + h
               (part1, part2) = span (< newHeight) ladder
               newLadder = replicate (length part1) newHeight ++ part2
           put (newLadder, oldHeight:heights)
           throwBoxes bs

main = do
     ladder <- (getLine >> getLine >>= (return . map read . words)) :: IO Ladder
     boxes <- (getLine >> getContents >>= (return . map (makeTuple . map read . words) . lines)) :: IO [(Int64, Int64)]
     mapM_ print $ evalState (throwBoxes boxes) (ladder, [])