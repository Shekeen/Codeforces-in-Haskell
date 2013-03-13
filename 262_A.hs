module Main where

checkHappyDigits :: String -> Int -> Bool
checkHappyDigits "" n
                 | n >= 0 = True
                 | otherwise = False
checkHappyDigits _ n | n < 0 = False
checkHappyDigits ('4':ls) n = checkHappyDigits ls (n-1)
checkHappyDigits ('7':ls) n = checkHappyDigits ls (n-1)
checkHappyDigits (_:ls) n = checkHappyDigits ls n

main = do
     nkStr <- getLine
     let [_, k] = map read $ words nkStr
     numsStr <- getLine
     let nums = words numsStr
     print $ foldl (\ a num -> if checkHappyDigits num k then a+1 else a) 0 nums