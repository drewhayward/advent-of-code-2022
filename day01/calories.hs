import System.IO
import Data.List

breakChunks :: [String] -> [[String]]
breakChunks [] = []
breakChunks xs = case break (== "") xs of
                    (chunk , (_:remainder)) -> chunk : breakChunks (remainder)
                    (chunk, []) -> [chunk]


parseInput :: String -> [[Int]]
parseInput xs = let chunks = (breakChunks . lines) xs
                    numChunks = map (map read) chunks
                in numChunks

part1 :: [[Int]] -> Int
part1 xs = maximum $ map sum xs

part2 :: [[Int]] -> Int
part2 xs = let elfTotals = map sum xs
               sortedTotals = (reverse . sort) elfTotals
               top3 = take 3 sortedTotals
            in sum top3

main = do
    contents <- readFile "input.txt"
    let input = parseInput contents

    putStrLn "Part 1"
    print $ part1 input

    putStrLn "Part 2"
    print $ part2 input