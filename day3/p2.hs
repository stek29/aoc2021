import Data.Bits (Bits (xor))
import Data.Char ( digitToInt )

import Debug.Trace

type Counts = [Int]

type FilterX = Int -> Int -> String -> Bool

filterOxy :: FilterX
filterOxy count bit number
  | count < 0 = number !! bit == '0'
  | count > 0 = number !! bit /= '0'
  | otherwise = number !! bit /= '0'

filterCO2 :: FilterX
filterCO2 count bit number
  | count < 0 = number !! bit /= '0'
  | count > 0 = number !! bit == '0'
  | otherwise = number !! bit == '0'

filterAll' :: Int -> FilterX -> [String] -> String
filterAll' _ _ [] = error "empty results"
filterAll' _ _ [chosen] = chosen
filterAll' bit filterX numbers
  | length (head numbers) == bit = error ("cant find it" ++ show numbers ++ show bit)
  | otherwise =
    -- trace ("bit " ++ show bit ++ "\nnumbers " ++ show numbers ++ "\nfiltered " ++ show filtered)
    filterAll' (bit + 1) filterX filtered
    where
      filtered = filter (filterX count bit) numbers
      count = countAtBit bit numbers

filterAll :: FilterX -> [String] -> String
filterAll = filterAll' 0

binToCount' :: Char -> Int
binToCount' x
  | x == '1' = 1
  | x == '0' = -1
  | otherwise = error "binary with non 01 chars"

countAtBit :: Int -> [String] -> Int
countAtBit bit numbers = sum counts
  where
    counts = map (binToCount' . (!! bit)) numbers

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

main :: IO ()
main = do
  userInput <- getContents
  let numbers = lines userInput

  let oxy = binToInt $ filterAll filterOxy numbers
  let co2 = binToInt $ filterAll filterCO2 numbers

  print $ oxy * co2
