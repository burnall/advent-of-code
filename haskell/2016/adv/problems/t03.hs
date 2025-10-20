-- stack script --resolver lts-21.25 --package split  --package containers

import Data.Function ((&))
import Data.List (foldl', sort)
import qualified Data.Map as Map
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  content <- readFile "../data/t03.txt"
  let triangles = content & lines & parseTriangles2
  let cnt = triangles & filter isTriangle & length
  print cnt
    

parseTriangles :: [String] -> [(Int, Int, Int)]
parseTriangles = map parseLine
  where
    parseLine line = let [a, b, c] = words line in (read a, read b, read c)

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a, b, c) = let [x, y, z] = sort [a, b, c] in x + y > z

parseTriangles2 :: [String] -> [(Int, Int, Int)]
parseTriangles2 = concatMap transpose . (chunksOf 3) . map (map read . words)
  where
    transpose [[a1, b1, c1], [a2, b2, c2], [a3, b3, c3]] =
      [(a1, a2, a3), (b1, b2, b3), (c1, c2, c3)]
