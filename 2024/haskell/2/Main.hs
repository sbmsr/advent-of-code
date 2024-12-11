import GHC.Internal.System.IO (readFile')
import Data.Text (splitOn)
import Data.Char (digitToInt)

readInput :: IO [[Int]]
readInput = do
    input <- readFile "./input.txt"
    let ls = lines input
    let nums = map words ls
    return (map (map read) nums)

isReportValidAscending :: [Int] -> Bool
isReportValidAscending (a:b:xs) = (a + 3 >= b && a < b) && isReportValidAscending (b:xs)
isReportValidAscending _ = True

isReportValidDescending :: [Int] -> Bool
isReportValidDescending (a:b:xs) = (a - 3 <= b && a > b) && isReportValidDescending (b:xs)
isReportValidDescending _ = True

part1 :: IO ()
part1 = do 
    reports <- readInput
    let reportValidities = map (\(l,r) -> l || r) (zip (map isReportValidAscending reports) (map isReportValidDescending reports))
    print (length (filter (== True) reportValidities))

newStuff :: [Int] -> Int -> [[Int]] -> [[Int]]
newStuff 

part2 :: IO ()
part2 = do
    reports <- readInput
    print map newStuff (take 0 reports) 0 (take 0 reports)

main :: IO ()
main = do 
    part1
    part2
