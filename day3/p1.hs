bin2counter :: String -> [Integer]
bin2counter = map bin2int'
  where
    bin2int' :: Char -> Integer
    bin2int' x
      | x == '1' = 1
      | x == '0' = -1
      | otherwise = 0

type CounterConv = (Integer, Integer)

gammaConv :: CounterConv
gammaConv = (1, 0)

epsilonConv :: CounterConv
epsilonConv = (0, 1)

counter2int :: CounterConv -> [Integer] -> Integer
counter2int conv = foldl shiftAndCount 0
  where
    shiftAndCount acc cnt = acc * 2 + cnt2bin cnt
    cnt2bin cnt
      | cnt > 0 = fst conv
      | cnt < 0 = snd conv
      | otherwise = error "Counter is zero"

main :: IO ()
main = do
  userInput <- getContents
  let counters = map bin2counter $ lines userInput
  let count = foldl1 (zipWith (+)) counters

  let gamma = counter2int gammaConv count
  let epsilon = counter2int epsilonConv count
  print $ gamma * epsilon
