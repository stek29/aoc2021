import Data.Char

data Action
  = Forward Integer
  | Up Integer
  | Down Integer
  deriving (Show, Read)

capitalized :: String -> String
capitalized (head : tail) = Data.Char.toUpper head : tail
capitalized [] = []

data State = State
  { x :: Integer,
    depth :: Integer
  }
  deriving (Show, Read)

initialState :: State
initialState = State {x = 0, depth = 0}

transformState :: State -> Action -> State
transformState s (Forward d) = s {x = x s + d}
transformState s (Up d) = s {depth = depth s - d}
transformState s (Down d) = s {depth = depth s + d}

main :: IO ()
main = do
  userInput <- getContents
  let actions = map (read . capitalized) $ lines userInput :: [Action]
  let state = foldl transformState initialState actions
  print $ depth state * x state
