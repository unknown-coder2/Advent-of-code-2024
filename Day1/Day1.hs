module Main where
import Data.List
import Data.Bifunctor
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)

main::IO()
main = do
    fileData <- readFile "./day1inp.txt"
    let lPair = interpritFile fileData
    print (getTotalDiff (sortTup lPair))
    print (totalSimScore (fst lPair) (getCounts (snd lPair)))


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

updateIntMap :: Int -> IntMap Int -> IntMap Int
updateIntMap key = IntMap.insertWithKey f key 1 
    where
         f :: Int -> Int -> Int -> Int
         f key nVal oVal = oVal + 1

getCounts :: [Int] -> IntMap Int
getCounts = go IntMap.empty
    where
        go :: IntMap Int -> [Int] -> IntMap Int
        go pMap l = 
            if null l
            then pMap
            else 
                let val = head l
                in go (updateIntMap val pMap) (tail l)

simScore :: IntMap Int -> Int -> Int
simScore lMap val = val * IntMap.findWithDefault 0 val lMap

totalSimScore :: [Int] -> IntMap Int -> Int
totalSimScore l lMap = sum (map (simScore lMap) l)