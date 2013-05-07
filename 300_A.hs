module Main where

printSolution :: ([Int], [Int], [Int]) -> IO ()
printSolution (a, b, c) = do
  putStrLn (show (length a) ++ " " ++ unwords (map show a))
  putStrLn (show (length b) ++ " " ++ unwords (map show b))
  putStrLn (show (length c) ++ " " ++ unwords (map show c))

classify :: ([Int], [Int], [Int]) -> Int -> ([Int], [Int], [Int])
classify (a, b, c) n
  | n  < 0 = (n:a,   b,   c)
  | n  > 0 = (  a, n:b,   c)
  | n == 0 = (  a,   b, n:c)

fixPositive :: ([Int], [Int], [Int]) -> ([Int], [Int], [Int])
fixPositive (a1:a2:as, [], c) = (as, [a1, a2], c)
fixPositive t = t

fixNegative :: ([Int], [Int], [Int]) -> ([Int], [Int], [Int])
fixNegative t@(a,b,c) = if even (length a) then (tail a, b, head a : c) else t

solve :: [Int] -> ([Int], [Int], [Int])
solve = fixNegative . fixPositive . foldl classify ([], [], [])

main = getLine >> getLine >>= (printSolution . solve . map read . words)
