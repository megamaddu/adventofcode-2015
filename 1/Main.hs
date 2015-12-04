module Main where

main :: IO ()
main = do
  input <- getLine
  print $ calcFloor input

calcFloor :: String -> Integer
calcFloor = foldl (\x c -> x + interp c) 0
  where
    interp '(' =  1
    interp ')' = -1
    interp _   =  0
