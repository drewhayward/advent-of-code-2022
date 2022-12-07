import Data.Array
import qualified Data.Set as S

allUnique :: String -> Bool
allUnique s = length s == length (S.fromList s)

window :: Int -> [a] -> [[a]]
window window_size [] = []
window window_size xs
    | window_size > length xs = []
    | otherwise = take window_size xs : window window_size (tail xs)

part1 size xs = let enumWindows = zip [0..] $ window size xs
                    uniqueWindows = filter (\x -> allUnique (snd x)) enumWindows
                in ((fst . head) uniqueWindows) + size

main = do
    print $ part1 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    print $ part1 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

    putStrLn "Part 1"
    contents <- readFile "input.txt"
    print $ part1 4 contents

    putStrLn "Part 2"
    print $ part1 14 contents