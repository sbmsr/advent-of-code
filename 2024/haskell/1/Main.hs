import Data.List (sort, foldl')
import Data.Maybe(fromMaybe)
import GHC.Internal.System.IO (readFile')
import qualified Data.Map as Map

process :: String -> ([Int], [Int])
process s = 
    let lns = lines s
        pairs = map words lns 
        lefts = map (read . head) pairs 
        rights = map (read . last) pairs
    in (lefts, rights)


readInputFile :: IO ([Int], [Int])
readInputFile = do
    contents <- readFile' "./input.txt"
    let result = process contents
    return result

part1 :: IO ()
part1 = do
    (left, right) <- readInputFile
    let differences = zipWith (\l r -> abs (l-r))
                              (sort left)
                              (sort right)
    print $ sum differences


createFrequencyMap :: [Int] -> Map.Map Int Int
createFrequencyMap nums = foldl' (\acc x -> Map.insertWith (+) x 1 acc) Map.empty nums

part2 :: IO ()
part2 = do 
    (left, right) <- readInputFile
    let freqMap = createFrequencyMap right
    print $ foldr (\x acc -> acc + x * fromMaybe 0 (Map.lookup x freqMap)) 0 left
    
main :: IO ()
main = do
    part1
    part2
