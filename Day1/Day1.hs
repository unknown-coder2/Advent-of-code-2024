module Main where
import Data.List
import Data.Bifunctor

main::IO()
main = do
    fileData <- readFile "./day1inp.txt"
    print (getTotalDiff (sortTup (interpritFile fileData)))

stripFileTokens :: String -> [[String]]
stripFileTokens contents = map words (lines contents)

interpritFile :: String -> ([Int], [Int])
interpritFile contents =
    let tokens = stripFileTokens contents
    in
        unzip (map (\ l -> (read (head l), read (last l))) tokens)

getDiff2 :: (Int, Int) -> Int
getDiff2 tup = abs (uncurry (-) tup)

sortTup :: ([Int], [Int]) -> ([Int], [Int])
sortTup = bimap sort sort

getTotalDiff :: ([Int], [Int]) -> Int
getTotalDiff tup = sum (map getDiff2 (uncurry zip tup))