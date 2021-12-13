import Data.Char
import Language.Haskell.TH.PprLib (integer)

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
    depth :: Integer,
    aim :: Integer
  }
  deriving (Show, Read)

initialState :: State
initialState =
  State
    { x = 0,
      depth = 0,
      aim = 0
    }

transformState :: State -> Action -> State
transformState s (Forward d) =
  s
    { x = x s + d,
      depth = depth s + aim s * d
    }
transformState s (Up d) = s {aim = aim s - d}
transformState s (Down d) = s {aim = aim s + d}

main :: IO ()
main = do
  userInput <- getContents
  let actions = map (read . capitalized) $ lines userInput :: [Action]
  let state = foldl transformState initialState actions
  print $ depth state * x state
