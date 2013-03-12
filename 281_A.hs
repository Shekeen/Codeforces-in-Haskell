module Main where

import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (l:ls) = toUpper l : ls

main = getLine >>= (putStrLn . capitalize)
