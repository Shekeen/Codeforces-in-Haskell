module Main where

import Control.Monad

shoot :: [Int] -> [Int] -> [Int]
shoot birds shot = let [wire, bird] = shot
                       birdsOnShotWire = birds!!(wire - 1)
                       birdsToMin = bird - 1
                       birdsToMax = birdsOnShotWire - birdsToMin - 1
                       totalWires = length birds
                       birdsDiff = (replicate (wire - 2) 0)
                                ++ (if wire == 1
                                   then [-birdsOnShotWire, birdsToMax]
                                   else
                                     if wire == totalWires
                                     then [birdsToMin, -birdsOnShotWire]
                                     else [birdsToMin, -birdsOnShotWire, birdsToMax])
                                ++ (replicate (totalWires - wire - 1) 0)
                       in
                    zipWith (+) birds birdsDiff

main = do
       birds <- liftM (map read . words) (getLine >> getLine)
       shots <- liftM (map (map read . words) . lines) (getLine >> getContents)
       mapM_ print $ foldl shoot birds shots