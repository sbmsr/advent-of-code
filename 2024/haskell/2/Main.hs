import GHC.Internal.System.IO (readFile')
import Data.Text (splitOn)
import Data.Char (digitToInt)

readInput :: IO [[Int]]
readInput = do
    input <- readFile "./input.txt"
    let ls = lines input
    let nums = map words ls
    return (map (map read) nums)

helper :: [Int] -> Bool
helper (a:b:xs) = (a + 3 >= b && a < b) && helper (b:xs)
helper _ = True

main :: IO ()
main = do 
    reports <- readInput
    let bools = map helper reports
    print (length (filter (== True) bools))
