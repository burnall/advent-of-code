-- stack script --resolver lts-21.25 --package split  --package containers

import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map as Map

main :: IO ()
main = do
  content <- readFile "../data/t02.txt"
  let commands = lines content
  let codes = read_codes '5' commands
  print codes

read_codes :: Char -> [String] -> String
read_codes startPos = reverse . snd . foldl' go (startPos, [])
  where
    go (pos, acc) cmd =
      let code = read_code pos cmd
       in (code, code : acc)

read_code :: Char -> String -> Char
read_code startPos = foldl' go startPos
  where
    go pos dir = Map.findWithDefault pos (pos, dir) moves2

-- turns Map.! (dir, turn)

moves =
  Map.fromList
    [ (('1', 'R'), '2'),
      (('1', 'D'), '4'),
      (('2', 'L'), '1'),
      (('2', 'R'), '3'),
      (('2', 'D'), '5'),
      (('3', 'L'), '2'),
      (('3', 'D'), '6'),
      (('4', 'U'), '1'),
      (('4', 'R'), '5'),
      (('4', 'D'), '7'),
      (('5', 'U'), '2'),
      (('5', 'L'), '4'),
      (('5', 'R'), '6'),
      (('5', 'D'), '8'),
      (('6', 'L'), '5'),
      (('6', 'D'), '9'),
      (('6', 'U'), '3'),
      (('7', 'U'), '4'),
      (('7', 'R'), '8'),
      (('8', 'L'), '7'),
      (('8', 'R'), '9'),
      (('8', 'U'), '5'),
      (('9', 'L'), '8'),
      (('9', 'U'), '6')
    ]

{-
1 2 3
4 5 6
7 8 9
-}

moves2 =
  Map.fromList
    [ (('1', 'D'), '3'),
        (('2', 'R'), '3'),
        (('2', 'D'), '6'),
        (('3', 'L'), '2'),
        (('3', 'R'), '4'),
        (('3', 'U'), '1'),
        (('3', 'D'), '7'),
        (('4', 'L'), '3'),
        (('4', 'D'), '8'),
        (('5', 'R'), '6'),
        (('6', 'L'), '5'),
        (('6', 'R'), '7'),
        (('6', 'U'), '2'),
        (('6', 'D'), 'A'),
        (('7', 'L'), '6'),
        (('7', 'R'), '8'),
        (('7', 'U'), '3'),
        (('7', 'D'), 'B'),
        (('8', 'L'), '7'),
        (('8', 'R'), '9'),
        (('8', 'U'), '4'),
        (('8', 'D'), 'C'),
        (('9', 'L'), '8'),
        (('A', 'U'), '6'),
        (('A', 'R'), 'B'),
        (('B', 'L'), 'A'),
        (('B', 'U'), '7'),
        (('B', 'R'), 'C'),
        (('B', 'D'), 'D'),
        (('C', 'L'), 'B'),
        (('C', 'U'), '8'),
        (('D', 'U'), 'B')
        ]

{-
    1
  2 3 4
5 6 7 8 9
  A B C
    D
  -}