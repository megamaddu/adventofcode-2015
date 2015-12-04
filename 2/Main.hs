module Main where

main :: IO ()
main = do
  input <- getLine
  print input

-- getWrappingPaperSurfaceArea ::

type Len = Integer

data Box = Box Len Len Len

data Side = Side Len Len

getSides :: Box -> [Side]
getSides (Box l w h) = [ Side l w
                       , Side w h
                       , Side l h ]

class Shape a where
  area :: (Num b) => a -> b

instance Shape Box where
  area b = 2 * sum (area <$> getSides b)

instance Shape Side where
  area (Side x y) = fromIntegral (x * y)

instance Read Box where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault
