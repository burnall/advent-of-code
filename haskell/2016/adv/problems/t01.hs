-- stack script --resolver lts-21.25 --package split  --package containers

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

main :: IO ()
main = do
  content <- readFile "../data/t01.txt"
  let directions = parseDirections content
  task2 directions

task1 :: [(Char, Int)] -> IO ()
task1 directions = do
  -- let (x, y) = last $ travel3 directions
  let r = directions & travel3 & last & manhattanDistance
  print r

task2 :: [(Char, Int)] -> IO ()
task2 directions = do
  let p = manhattanDistance $ head $ filterRevisited $ travelThorough directions
  print p

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

parseDirections :: String -> [(Char, Int)]
parseDirections =
  map (\(dir : distance) -> (dir, read distance :: Int)) . splitOn ", "

travel :: [(Char, Int)] -> (Int, Int)
travel = snd . foldl step ('N', (0, 0))
  where
    step (dir, (x, y)) (turn, dist) =
      let newDir = turns Map.! (dir, turn)
          (dx, dy) = shifts Map.! newDir
       in (newDir, (x + dx * dist, y + dy * dist))

travel3 :: [(Char, Int)] -> [(Int, Int)]
travel3 = go ('N', (0, 0))
  where
    go _ [] = []
    go (dir, (x, y)) ((turn, dist) : distances) =
      let newDir = turns Map.! (dir, turn)
          (dx, dy) = shifts Map.! newDir
          p = (x + dx * dist, y + dy * dist)
       in p : go (newDir, p) distances

travelThorough :: [(Char, Int)] -> [(Int, Int)]
travelThorough = go ('N', (0, 0))
  where
    go _ [] = []
    go (dir, (x, y)) ((turn, dist) : distances) =
      let newDir = turns Map.! (dir, turn)
          (dx, dy) = shifts Map.! newDir
          p = (x + dx * dist, y + dy * dist)
          route = map (\i -> (x + i * dx, y + i * dy)) [1..dist]
       in route ++ go (newDir, p) distances

filterRevisited :: [(Int, Int)] -> [(Int, Int)]
filterRevisited = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `Set.member` seen = x : go seen xs
      | otherwise = go (Set.insert x seen) xs

turns =
  Map.fromList
    [ (('N', 'R'), 'E'),
      (('N', 'L'), 'W'),
      (('E', 'R'), 'S'),
      (('E', 'L'), 'N'),
      (('S', 'R'), 'W'),
      (('S', 'L'), 'E'),
      (('W', 'R'), 'N'),
      (('W', 'L'), 'S')
    ]

shifts = Map.fromList [('N', (0, -1)), ('E', (1, 0)), ('S', (0, 1)), ('W', (-1, 0))]
