module Main where

data Muscle = Chest | Biceps | Back

max3 :: Int -> Int -> Int -> String
max3 a b c = if a > b
             then if a > c
                  then "chest"
                  else "back"
             else if b > c
                  then "biceps"
                  else "back"

solve :: [Int] -> String
solve ex = solve' 0 0 0 Chest ex
      where
      solve' a b c _ [] = max3 a b c
      solve' a b c Chest (x:xs) = solve' (a+x) b c Biceps xs
      solve' a b c Biceps (x:xs) = solve' a (b+x) c Back xs
      solve' a b c Back (x:xs) = solve' a b (c+x) Chest xs

main = do
     getLine
     exercises <- fmap ((map (read :: String -> Int)) . words) getLine
     putStrLn $ solve exercises
