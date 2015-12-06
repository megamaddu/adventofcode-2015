module Main where

import Control.Applicative ((<|>))

main :: IO ()
main = do
  input <- getLine
  putStrLn $ showResult . runCommands $ interpInput <$> input
  where
    interpInput :: Char -> ElevatorCommand
    interpInput '(' = Up
    interpInput ')' = Down
    interpInput x   = error $ "invalid command: " ++ [x]

data ElevatorCommand = Up | Down

type Floor = Integer

type Step = Integer

type BasementEntryStep = Maybe Step

type ElevatorResult = (Floor, Step, BasementEntryStep)

runCommands :: [ElevatorCommand] -> ElevatorResult
runCommands = foldl step (0, 0, Nothing)
  where
    step :: ElevatorResult -> ElevatorCommand -> ElevatorResult
    step (f, s, b) c = (nextFloor, nextStepCount, nextBasementEntryStep)
      where
        nextFloor = interpCommand f c
        nextStepCount = 1 + s
        nextBasementEntryStep = b <|> if nextFloor < 0 then Just nextStepCount else Nothing

    interpCommand :: Integer -> ElevatorCommand -> Integer
    interpCommand x Up   = x + 1
    interpCommand x Down = x - 1

showResult :: ElevatorResult -> String
showResult (f, s, b) = "Floor: " ++ show f ++ " (" ++ show s ++ " steps total, "
  ++ maybe "never entered basement" ("basement entered on step " ++) (show <$> b) ++ ")"
