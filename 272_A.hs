module Main where

solve :: [Int] -> Int
solve xs = let l = length xs
               s = sum xs
           in foldl (\ acc x -> if x == 1 then acc else acc+1) 0 $ map ((`mod` (l+1)).(+s)) [1,2,3,4,5]

main = getLine >> getLine >>= (print . solve . map read . words)
