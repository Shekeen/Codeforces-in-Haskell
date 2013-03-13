module Main where

data Muscle = Chest | Biceps | Back

max3 :: Int -> Int -> Int -> String
max3 a b c
  | a > b = if a > c then "chest" else "back"
  | b > c = "biceps"
  | otherwise = "back"

solve :: [Int] -> String
solve = solve' 0 0 0 Chest
      where
      solve' a b c _ [] = max3 a b c
      solve' a b c Chest (x:xs) = solve' (a+x) b c Biceps xs
      solve' a b c Biceps (x:xs) = solve' a (b+x) c Back xs
      solve' a b c Back (x:xs) = solve' a b (c+x) Chest xs

main = do
     getLine
     exercises <- fmap (map read . words) getLine
     putStrLn $ solve exercises
