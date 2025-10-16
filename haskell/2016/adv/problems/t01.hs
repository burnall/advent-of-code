-- stack script --resolver lts-21.25 --package split  --package containers

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  content <- readFile "../data/t01.txt"
  let directions = parseDirections content
  let (x, y) = travel directions
  print (x + y)

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
