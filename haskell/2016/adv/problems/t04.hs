-- stack script --resolver lts-21.25 --package split  --package containers --package regex-tdfa --package multiset

import Data.Char (chr, isAlpha, ord)
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.MultiSet as MS
import Data.Ord (Down (..))
import Text.Regex.TDFA ((=~))

data Room = Room
  { name :: String,
    sectorId :: Int,
    checksum :: String
  }
  deriving (Show)

main :: IO ()
main = do
  content <- readFile "../data/t04.txt"
  let rooms = content & lines & map parseRoom
  task2 rooms

task1 :: [Room] -> Int
task1 rooms = rooms & filter isValid & map sectorId & sum
  where
    isValid r = crc r == checksum r

task2 :: [Room] -> IO ()
task2 rooms = do
  let decode room = map (rotateChar (sectorId room)) (name room)
      pair room = (decode room, sectorId room)
      decoded = map pair rooms 
  mapM_ print decoded

-- muqfedyput-ydjuhdqjyedqb-vbemuh-tulubefcudj-712[lqaik]
parseRoom :: String -> Room
parseRoom line = Room n (read sid :: Int) cs
  where pattern = "([a-z-]+)-([0-9]+)\\[([a-z]+)\\]"
        (_, _, _, [n, sid, cs]) = line =~ pattern :: (String, String, String, [String])


crc :: Room -> String
crc room = room & name & MS.fromList & MS.deleteAll '-' & MS.toOccurList & sortedDesc & map fst & take 5
  where sortedDesc = sortOn (\(l, n) -> (Down n, l))

rotateChar :: Int -> Char -> Char
rotateChar shift c
  | c == '-' = ' '
  | otherwise = toEnum $ base + ((fromEnum c - base + shift) `mod` 26)
  where
    base = fromEnum 'a'
