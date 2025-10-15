module Main where

main::IO()
main = do
    fileData <- readFile "./day2inp.txt"
    print (checkValidTotal (interpritFile fileData))
    print (remove [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 9)

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
        go l valid upOrDown
          | valid == 0 = 0
          | null (drop 1 l) = valid
          | checkValidPair l upOrDown = 0
          | otherwise = go (tail l) 1 upOrDown

checkValidPair :: [Int] -> Int -> Bool
checkValidPair l upOrDown =
    let dif = (l !! 1) - head l
    in (dif * upOrDown) > 3 || (dif * upOrDown) < 1

checkValidWithRemoval :: [Int] -> Int
checkValidWithRemoval level = go level 1 (getDirection level)
    where
        go :: [Int] -> Int -> Int -> Int
        go l valid upOrDown
          | valid == 0 = 0
          | null (drop 1 l) = valid
          | checkValidPair l upOrDown = tryRemoval l level
          | otherwise = go (tail l) 1 upOrDown

tryRemoval :: [Int] -> [Int] -> Int
tryRemoval l level
  | checkValid (remove level (length level - length l)) == 1 = 1
  | checkValid (remove level (length level - length l + 1)) == 1 = 1
  | otherwise = 0


remove :: [Int] -> Int -> [Int]
remove l idx =
    take idx l ++ drop (idx + 1) l

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