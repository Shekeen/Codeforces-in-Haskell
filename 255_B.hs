module Main where

countDiff :: String -> Int
countDiff s = countDiff' s 0
        where
        countDiff' "" d = d
        countDiff' ('x':s) d = countDiff' s (d+1)
        countDiff' ('y':s) d = countDiff' s (d-1)

solve :: String -> String
solve s = if d > 0
          then replicate d 'x'
          else replicate (-d) 'y'
               where
               d = countDiff s

main = getLine >>= (putStrLn . solve)