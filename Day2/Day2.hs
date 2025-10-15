module Main where

main::IO()
main = do
    fileData <- readFile "./day2inp.txt"
    print (checkValidTotal (interpritFile fileData))

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
        go (x : y : xs) valid upOrDown
          | valid == 0 = 0
          | checkValidPair (x : y : xs) upOrDown = 0
          | otherwise = go (y : xs) 1 upOrDown
        go _ valid _ = valid

checkValidPair :: [Int] -> Int -> Bool
checkValidPair (x : y : _) upOrDown =
    let dif = y - x
    in (dif * upOrDown) > 3 || (dif * upOrDown) < 1
checkValidPair _ _ = True

checkValidWithRemoval :: [Int] -> Int
checkValidWithRemoval level = go level 1 (getDirection level)
    where
        go :: [Int] -> Int -> Int -> Int
        go (x : y : xs) valid upOrDown
          | valid == 0 = 0
          | checkValidPair (x : y : xs) upOrDown = tryRemoval (x : y : xs) level
          | otherwise = go (y : xs) 1 upOrDown
        go _ valid _ = valid

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
        go (x : y : xs) prev
            | getDirectionPair x y == 1 = go (y : xs) (fst prev, snd prev + 1)
            | getDirectionPair x y == -1 = go (y : xs) (fst prev + 1, snd prev)
        go _ prev = prev

getDirectionPair :: Int -> Int -> Int
getDirectionPair one two =
    if (one - two) < 0
        then 1
    else -1