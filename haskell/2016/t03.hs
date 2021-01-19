{-# LANGUAGE QuasiQuotes                      #-}
import Text.RE.TDFA.String
import Data.List

parseContent s =
  let ls = filter (\l -> l /= "") (lines s)
  in map parseLine ls

parseLine s = 
  let strings = matches $ s *=~ [re|[0-9]+|]
      ints = map (read :: String -> Int) strings
      [a, b, c] = sort ints
  in (a, b, c)

isTriangle (a, b, c) = a + b > c

main = do 
  content <- readFile "data/t03.txt"
  print $ length $ filter isTriangle $ parseContent content
