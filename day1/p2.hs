type Pair a = (a, a)

type Triple a = (a, a, a)

pairs :: [a] -> [Pair a]
pairs [] = []
pairs [x] = []
pairs (x : y : rem) = (x, y) : pairs (y : rem)

triples :: [a] -> [Triple a]
triples [] = []
triples [x] = []
triples [x, y] = []
triples (x : y : z : rem) = (x, y, z) : triples (y : z : rem)

tri :: (t -> t -> t) -> (t, t, t) -> t
tri op (x, y, z) = x `op` y `op` z

main :: IO ()
main = do
  userInput <- getContents
  let numbers = map read $ lines userInput :: [Integer]
  let sums = map (tri (+)) $ triples numbers
  let increases = map (\(l, r) -> fromEnum $ r > l) (pairs sums)
  print $ sum increases
