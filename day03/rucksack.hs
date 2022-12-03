import qualified Data.Set as S
import Data.Char
import Data.Bifunctor

priority :: Char -> Int
priority c = case isUpper c of
             True -> (ord c) - (ord 'A') + 27
             False -> (ord c) - (ord 'a') + 1

intersectionPriority :: S.Set Char -> S.Set Char -> Int
intersectionPriority set1 set2 = let commonItems = S.toList $ set1 `S.intersection` set2
                                 in priority $ commonItems !! 0

commonThreePriority :: S.Set Char -> S.Set Char -> S.Set Char -> Int
commonThreePriority set1 set2 set3 = let commonItems = set1 `S.intersection` set2 `S.intersection` set3
                                     in priority $ (head . S.toList) commonItems
                                     
split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

part1 :: [(S.Set Char, S.Set Char)] -> Int
part1 [] = 0
part1 ((comp1, comp2):xs) = (intersectionPriority comp1 comp2) + (part1 xs)

part2 :: [S.Set Char] -> Int
part2 [] = 0
part2 xs = let (s1:s2:s3:leftover) = xs
           in (commonThreePriority s1 s2 s3) + (part2 leftover)


parseInput :: String -> [(S.Set Char, S.Set Char)]
parseInput contents = let rPairs = map split $ lines contents
                          rSets = map (bimap S.fromList S.fromList) rPairs
                      in rSets
                      
parseInput2 :: String -> [S.Set Char]
parseInput2 contents = let sacks = lines contents
                           rSets = map S.fromList sacks
                       in rSets


main = do
    testcontents <- readFile "test.txt"
    let testInput = parseInput testcontents
    putStrLn "Test"
    print $ part1 testInput

    contents <- readFile "input.txt"
    let input = parseInput contents
    putStrLn "Part 1"
    print $ part1 input

    let input2 = parseInput2 contents
    putStrLn "Part 2"
    print $ part2 input2