import Data.List
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, Maybe)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Functor (fmap)
import System.IO

type Stack = [Char]

boxChars :: String -> [Char]
boxChars s = [s !! i | i <- [1,5..(length s)]]

parseStacks :: [String] -> [Stack]
parseStacks xs = let boxLines = init xs
                     boxes = reverse $ map boxChars boxLines
                     stacks = map (filter (/=' ')) $ transpose boxes
                 in stacks

parseMoves :: [String] -> [[Int]]
parseMoves xs = let moveLines = tail xs
                    wordLines = map words moveLines
                    maybeNums = (map  (map readMaybe) wordLines ) :: [[Maybe Int]]
                in map catMaybes maybeNums

dropL :: Seq.Seq a -> Int -> Seq.Seq a
dropL xs i = (Seq.reverse . (Seq.drop i) . Seq.reverse) xs
            
moveStacks :: [Stack] -> [Int] -> [Stack]
moveStacks stacks (count:from:to:_) = let sequenceStack = Seq.fromList $ map Seq.fromList stacks
                                          addedStack = (Seq.index sequenceStack (to - 1))
                                          subtractedStack = (Seq.index sequenceStack (from - 1))
                                          movingSection = ((Seq.take count) . Seq.reverse) subtractedStack
                                          newStacks1 = Seq.update (to - 1) (addedStack Seq.>< movingSection) sequenceStack
                                          newStacks2 = Seq.update (from - 1) (dropL subtractedStack count) newStacks1
                                      in Foldable.toList $ fmap Foldable.toList newStacks2

moveStacks' :: [Stack] -> [Int] -> [Stack]
moveStacks' stacks (count:from:to:_) = let sequenceStack = Seq.fromList $ map Seq.fromList stacks
                                           addedStack = (Seq.index sequenceStack (to - 1))
                                           subtractedStack = (Seq.index sequenceStack (from - 1))
                                           movingSection = (Seq.reverse . (Seq.take count) . Seq.reverse) subtractedStack
                                           newStacks1 = Seq.update (to - 1) (addedStack Seq.>< movingSection) sequenceStack
                                           newStacks2 = Seq.update (from - 1) (dropL subtractedStack count) newStacks1
                                       in Foldable.toList $ fmap Foldable.toList newStacks2

part1 :: [Stack] -> [[Int]] -> [Stack]
part1 stacks moves = foldl moveStacks stacks moves

part2 :: [Stack] -> [[Int]] -> [Stack]
part2 stacks moves = foldl moveStacks' stacks moves

printStacks :: [Stack] -> IO ()
printStacks xs = mapM_ putStrLn xs

parseInput :: String -> ([Stack], [[Int]])
parseInput contents = let (stacks, moves) = break (=="") (lines contents)
                          startStacks = parseStacks stacks
                          startMoves = parseMoves moves
                      in (startStacks, startMoves)

main = do
    -- Test
    contents <- readFile "test.txt"
    let (testStacks, testMoves) = parseInput contents
    let testStack = part1 testStacks testMoves
    putStrLn $ map last testStack

    inputContents <- readFile "input.txt"
    let (inputStacks, inputMoves) = parseInput inputContents

    putStrLn "Part 1"
    let part1Stack = part1 inputStacks inputMoves
    putStrLn $ map last part1Stack

    putStrLn "Part 2"
    let part2Stack = part2 inputStacks inputMoves
    putStrLn $ map last part2Stack