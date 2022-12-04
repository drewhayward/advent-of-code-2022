
type Span = (Int, Int)

readSpan :: String -> Span
readSpan s = let (start, end) = break (=='-') s
                 end' = tail end -- drop the separator char
             in (read start, read end')


parseLine :: String -> (Span, Span)
parseLine s = let (span1, span2) = break (==',') s
                  span2' = tail span2 -- drop the separator char
              in (readSpan span1, readSpan span2')

subsumes :: Span -> Span -> Bool
subsumes (a1, a2) (b1, b2) = (a1 <= b1) && (b2 <= a2)

eitherSubsumes :: Span -> Span -> Bool
eitherSubsumes a b = (subsumes a b) || (subsumes b a)

overlaps :: Span -> Span -> Bool
overlaps (a1, a2) (b1, b2) = not $ (a2 < b1) || (b2 < a1)

part1 :: String -> Int
part1 contents = let pairs = (map parseLine) . lines $ contents
                     validPairs = filter (uncurry eitherSubsumes) pairs
                 in length validPairs

part2 :: String -> Int
part2 contents = let pairs = (map parseLine) . lines $ contents
                     validPairs = filter (uncurry overlaps) pairs
                 in length validPairs


main = do
    contents <- readFile "test.txt"
    print $ part1 contents

    putStrLn "Part 1"
    contents <- readFile "input.txt"
    print $ part1 contents

    putStrLn "Part 2"
    contents <- readFile "input.txt"
    print $ part2 contents