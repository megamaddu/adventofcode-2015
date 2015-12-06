module Main where

import Control.Applicative ((<|>))

main :: IO ()
main = do
  input <- getLine
  print $ runCommands $ interpInput <$> input
  where
    interpInput :: Char -> ElevatorCommand
    interpInput '(' = Up
    interpInput ')' = Down
    interpInput x   = error $ "invalid command: " ++ [x]

data ElevatorCommand = Up | Down

data ElevatorResult = ElevatorResult
  { eFloor :: Integer
  , eSteps :: Integer
  , eBasementEntryStep :: Maybe Integer
  }

instance Show ElevatorResult where
  show r = "Floor: " ++ show (eFloor r) ++ " (" ++ show (eSteps r) ++ " steps total, "
        ++ maybe "never entered basement" ("basement entered on step " ++) (show <$> eBasementEntryStep r)
        ++ ")"

runCommands :: [ElevatorCommand] -> ElevatorResult
runCommands = foldl step ElevatorResult { eFloor = 0, eSteps = 0, eBasementEntryStep = Nothing }
  where
    step :: ElevatorResult -> ElevatorCommand -> ElevatorResult
    step r c = ElevatorResult
      { eFloor = nextFloor
      , eSteps = nextStepCount
      , eBasementEntryStep = nextBasementEntryStep
      }
      where
        nextFloor :: Integer
        nextFloor = interpCommand (eFloor r) c

        nextStepCount :: Integer
        nextStepCount = 1 + eSteps r

        nextBasementEntryStep :: Maybe Integer
        nextBasementEntryStep =
          eBasementEntryStep r <|>
            if nextFloor < 0 then Just nextStepCount else Nothing

    interpCommand :: Integer -> ElevatorCommand -> Integer
    interpCommand x Up      = x + 1
    interpCommand x Down    = x - 1
