-- stack script --resolver lts-21.25 --package split  --package containers

import Data.Function ((&))
import Data.List (group, head, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)

main :: IO ()
main = do
  content <- readFile "../data/t06.txt"
  print $ task1 $ lines content

task1 :: [String] -> String
task1 = map mostFrequent . transpose

mostFrequent :: String -> Char
mostFrequent = head . minimumBy (comparing length) . group . sort
