module Main where

distance :: (Int, Int) -> (Int, Int) -> Int
distance (a,b) (c,d) = abs (a-c) + abs (b-d)

find :: Int -> [Int] -> Int
find k xs = find' 0 k xs
     where
     find' _ _ [] = -1
     find' k x (x':xs) = if x == x'
                         then k
                         else find' (k+1) x xs

extractCoords :: [Int] -> (Int, Int)
extractCoords = extractCoords' 0
              where
              extractCoords' n ((-1):xs) = extractCoords' (n+1) xs
              extractCoords' n (x:xs) = (n, x)
     
getCoords :: [[Int]] -> (Int, Int)
getCoords mat = extractCoords $ map (find 1) mat

solve :: [[Int]] -> Int
solve mat = distance (2,2) $ getCoords mat

main = do
     matrixStr <- getContents
     let matrixChr = map words $ lines matrixStr
         matrix = map (map (read :: String -> Int)) matrixChr
     print $ solve matrix
