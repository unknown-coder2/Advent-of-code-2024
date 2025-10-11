module Main where

main::IO()
main = do
    fileData <- readFile "./day2inp.txt"
    print (checkValidTotal (interpritFile fileData))
    -- print (minimum (map length (interpritFile fileData)))

interpritFile :: String -> [[Int]]
interpritFile cont =
    let inter = map words (lines cont)
    in map (map read) inter

checkValidTotal :: [[Int]] -> Int
checkValidTotal records = sum (map checkValid records)

checkValid :: [Int] -> Int
checkValid level = go level 1 (getDirection level)
    where
        go :: [Int] -> Int -> Int -> Int
        go l valid upOrDown
          | valid == 0 = 0
          | null (drop 1 l) = valid
          | checkValidPair l upOrDown = 0
          | otherwise = go (tail l) 1 upOrDown

checkValidPair :: [Int] -> Int -> Bool
checkValidPair l upOrDown =
    let dif = (l !! 1) - head l
    in (dif * upOrDown) > 3 || (dif * upOrDown) < 1

getDirection :: [Int] -> Int
getDirection record =
    if uncurry (>) (go record (0, 0))
        then -1
    else 1
    where
        go :: [Int] -> (Int, Int) -> (Int, Int)
        go l prev
            | null (drop 1 l) = prev
            | getDirectionPair (head l) (l !! 1) == 1 = go (tail l) (fst prev, snd prev + 1)
            | getDirectionPair (head l) (l !! 1) == -1 = go (tail l) (fst prev + 1, snd prev)


getDirectionPair :: Int -> Int -> Int
getDirectionPair one two =
    if (one - two) < 0
        then 1
    else -1