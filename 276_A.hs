module Main where

satisfaction :: Int -> [Int] -> Int
{- satisfaction TimeLimit RestaurantParams -}
satisfaction limit params = let [sat, time] = params
                            in
                            if time > limit then sat - time + limit else sat

main = do
  paramStr <- getLine
  restaurantsStr <- getContents
  let [_, timeLimit] = map read $ words paramStr
      restaurants = (map (map read . words)) $ lines restaurantsStr
  print $ maximum $ map (satisfaction timeLimit) restaurants
