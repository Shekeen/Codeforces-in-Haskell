module Main where

solve :: String -> String
solve "0" = ""
solve "1" = ""
solve ('1':ls) = '1' : solve ls
solve ('0':ls) = ls

main = getLine >>= (putStrLn . solve)
