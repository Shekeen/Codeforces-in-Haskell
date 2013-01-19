module Main where

solve :: Int -> [Int] -> [Int] -> Int
solve 0 neg pos = sum neg + sum pos
solve k [] pos = (sum pos) - 2 * (head pos) * (k `mod` 2)
solve k neg []
      | length neg >= k = sum (drop k neg) - sum (take k neg)
      | otherwise = -(sum neg) + 2 * (last neg) * ((k - (length neg)) `mod` 2)
solve k neg pos
      | length neg >= k = sum (drop k neg) + sum pos - sum (take k neg)
      | otherwise = if (-(last neg)) > (head pos)
                    then (sum pos) - (sum neg) - 2 * (head pos) * ((k - (length neg)) `mod` 2)
                    else (sum pos) - (sum neg) + 2 * (last neg) * ((k - (length neg)) `mod` 2)
      
main = do
     nkStr <- getLine
     let nk = map (read :: String -> Int) $ words nkStr
         k = nk!!1
     revenuesStr <- getLine
     let revenues = map (read :: String -> Int) $ words revenuesStr
         (negative, positive) = span (<0) revenues
     print $ solve k negative positive