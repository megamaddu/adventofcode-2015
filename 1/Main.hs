module Main where

main :: IO ()
main = do
  input <- getLine
  print $ calcFloor input
  print $ calcStepsToBasementEntry input

calcFloor :: String -> Integer
calcFloor = foldl interp 0

calcStepsToBasementEntry :: String -> Integer
calcStepsToBasementEntry = step 1 0
  where
    step :: Integer -> Integer -> String -> Integer
    step _ _ ""     = 0
    step c t (x:xs) =
      if t' < 0
        then c
        else step (c + 1) t' xs
      where t' = interp t x

interp :: Integer -> Char -> Integer
interp x '(' = x + 1
interp x ')' = x - 1
interp x _   = x
