
data Sign = Rock | Paper | Scissors deriving (Enum, Eq, Show)
data Result = Lose | Draw | Win deriving (Enum, Eq, Show)

signBonus :: Sign -> Int
signBonus s = (fromEnum s) + 1

beats :: Sign -> Sign
beats sign = let sign_num = fromEnum sign
                 beats_num = (sign_num - 1) `mod` 3
             in toEnum beats_num

vs :: Sign -> Sign -> Result
s1 `vs` s2 = if s1 == s2
                then Draw
              else if (s2 == ( beats s1 ))
              then Win
              else Lose

readSign :: String -> Sign
readSign c = case c of
             "A" -> Rock
             "B" -> Paper
             "C" -> Scissors
             "X" -> Rock
             "Y" -> Paper
             "Z" -> Scissors

readResult :: String -> Result
readResult r = case r of
               "X" -> Lose
               "Y" -> Draw
               "Z" -> Win


score :: Sign -> Sign -> Int
score yourSign theirSign = case yourSign `vs` theirSign of
                                Win -> 6 + bonus
                                -- you win
                                Draw -> 3 + bonus
                                -- you lose
                                Lose -> bonus
                           where bonus = signBonus yourSign

toTup :: [a] -> (a, a)
toTup xs = (xs !! 0, xs !! 1)

parseInput :: String -> [(Sign, Sign)]
parseInput s = let stringRows = map words (lines s)
                   signs = map (map readSign) stringRows
                   gameSigns = map toTup signs
               in gameSigns

parseInput2 :: String -> [(Sign, Result)]
parseInput2 s = let stringRows = map words (lines s)
                    signs = map (readSign . (!!0)) stringRows
                    results = map (readResult . (!!1)) stringRows
               in zip signs results

playGame :: (Sign, Sign) -> Int
playGame (theirSign, yourSign) = score yourSign theirSign

-- What sign do I need to force the result given the opp sign?
forceGame :: Sign -> Result -> Sign
forceGame theirSign desiredResult = case desiredResult of
                                       Win -> beats $ beats theirSign
                                       Draw -> theirSign
                                       Lose -> beats theirSign

part2 :: [(Sign, Result)] -> Int
part2 [] = 0 
part2 ((s, r):xs) = let forcedSign = forceGame s r
                      in (score forcedSign s) + (part2 xs)

main = do
    testContents <- readFile "test.txt"
    let testInput = parseInput testContents
    putStrLn "Test"
    print $ sum $ map playGame testInput
    
    putStrLn "Part 1"
    contents <- readFile "input.txt"
    let input = parseInput contents
    print $ sum $ map playGame input

    putStrLn "Test 2"
    let testInput2 = parseInput2 testContents
    print $ part2 testInput2

    putStrLn "Part 2"
    let input2 = parseInput2 contents
    print $ part2 input2
