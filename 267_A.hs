module Main where

solve :: Int -> Int -> Int
solve 0 _ = 0
solve _ 0 = 0
solve a b
      | a >= b = (a `div` b) + solve b (a `mod` b)
      | otherwise = solve b a

printSolution :: IO ()
printSolution = do
              numsStr <- getLine
              let nums = map (read :: String -> Int) $ words numsStr
              print $ solve (nums!!0) (nums!!1)

printNSolutions :: Int -> IO ()
printNSolutions 1 = printSolution
printNSolutions n = do
                printSolution
                printNSolutions (n-1)
                
main = do
     nStr <- getLine
     printNSolutions $ (read nStr :: Int)
     
