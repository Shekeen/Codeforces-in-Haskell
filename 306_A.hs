module Main where

solve :: [Int] -> [Int]
solve [n, m] = let q = n `quot` m
                   r = n `rem` m
                   first = take r $ repeat (q+1)
                   second = take (m-r) $ repeat q
                 in
                first ++ second

printSolution :: [Int] -> IO ()
printSolution = putStrLn . unwords . map show

main = getLine >>= (printSolution . solve . map read . words)
