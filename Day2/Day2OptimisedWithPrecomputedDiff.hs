module Main where
import Control.Monad.Trans.Accum (mapAccum)

main::IO()
main = do
    fileData <- readFile "./day2inp.txt"
    print (checkValidTotal (interpritFile fileData))

getDiff :: [Int] -> [Int]
getDiff l = zipWith (-) l (drop 1 l)

interpritFile :: String -> [[Int]]
interpritFile cont =
    let inter = map words (lines cont)
    in map (map read) inter

checkValidTotal :: [[Int]] -> Int
checkValidTotal records = sum (map checkValidWithRemoval records)

checkValid :: [Int] -> Int
checkValid level = go level 1 (getDirection level)
    where
        go :: [Int] -> Int -> Int -> Int
        go (x : xs) valid upOrDown
          | valid == 0 = 0
          | checkValidPair (x : xs) upOrDown = 0
          | otherwise = go xs 1 upOrDown
        go _ valid _ = valid

checkValidPair :: [Int] -> Int -> Bool
checkValidPair [] _ = True
checkValidPair (x : _) upOrDown = (x * upOrDown) > 3 || (x * upOrDown) < 1

checkValidWithRemoval :: [Int] -> Int
checkValidWithRemoval level = go level 1 (getDirection level)
    where
        go :: [Int] -> Int -> Int -> Int
        go (x : xs) valid upOrDown
          | valid == 0 = 0
          | checkValidPair (x : xs) upOrDown = tryRemoval (x : xs) level
          | otherwise = go xs 1 upOrDown
        go _ valid _ = valid

tryRemoval :: [Int] -> [Int] -> Int
tryRemoval l level = go (length level - length l)
    where
        go :: Int -> Int
        go idx
            | checkValid (drop (idx - 1) level) == 1 = 1
            | checkValid (drop idx level) == 1 = 1
            | otherwise = 0

remove :: [Int] -> Int -> Int -> [Int]
remove l idx1 idx2 = 
    let sum_ = (l !! idx1) + (l !! idx2)
    in take idx1 l ++ sum_ : drop (idx2 + 1) l

getDirection :: [Int] -> Int
getDirection record = 
    let value = signum (sum (map signum record))
    in if value == 0
        then 1
        else value
