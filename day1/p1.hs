type IntPair = (Integer, Integer)

pairs :: [Integer] -> [IntPair]
pairs (x : y : rem) = (x, y) : pairs (y : rem)
pairs [x] = []
pairs [] = []

countIncreases' :: Integer -> IntPair -> Integer
countIncreases' acc (l, r)
  | r > l = acc + 1
  | otherwise = acc

countIncreases :: [IntPair] -> Integer
countIncreases = foldl countIncreases' 0

main :: IO ()
main = do
  userInput <- getContents
  let numbers = map read $ lines userInput :: [Integer]
  print $ countIncreases $ pairs numbers
