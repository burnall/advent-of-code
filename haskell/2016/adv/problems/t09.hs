{-# LANGUAGE RecordWildCards #-}
import Debug.Trace (trace)

-- stack script problems/t09.hs --resolver lts-22.18 --package containers --package regex-tdfa --package regex-base --package array

import Data.Array ((!))
import Data.Function ((&))
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()

data Marker = Marker
  { len :: Int,
    times :: Int,
    start :: Int,
    end :: Int
  }
  deriving (Show)

data MarkerTree = MarkerTree
  { tstart :: Int,
    tend :: Int,
    tlen :: Int,
    ttimes :: Int,
    tnested :: [MarkerTree]
  }
  deriving (Show)

main :: IO ()
main = do
  content <- readFile "../data/t09.txt"
  let --content = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" 
      ms = parseMarkers content
  print $ measure2' ms $ length content
-- print $ measureTree $ buildTree ms

parseMarkers :: String -> [Marker]
parseMarkers input =
  let matches :: [MatchText String]
      matches = input =~ "\\(([0-9]+)x([0-9]+)\\)"
   in [ let (off, len') = snd (mt ! 0)
            a = read (fst (mt ! 1))
            b = read (fst (mt ! 2))
         in Marker {start = off, end = off + len', len = a, times = b}
      | mt <- matches
      ]

measure :: [Marker] -> Int -> Int
measure markers fullLen = step markers 0
  where
    step [] pos = fullLen - pos
    step (Marker {..} : rest) pos
      | start < pos = step rest pos
      | otherwise = start - pos + len * times + step rest (end + len)

measure2 :: [Marker] -> Int -> Int
measure2 markers fullLen = step markers 0
  where
    step [] pos = fullLen - pos
    step (Marker {start = markerStart, end = markerEnd, len, times} : rest) pos
      | markerStart < pos = step rest pos
      | otherwise =
          let gap = markerStart - pos
              segmentLen = len
              segmentStart = markerEnd
              segmentEnd = markerEnd + len
              nested = [m | m <- markers, start m >= segmentStart, end m <= segmentEnd]
              expanded = measure2 nested segmentLen * times
              nextPos = segmentEnd
           in gap + expanded + step rest nextPos

buildTree :: [Marker] -> [MarkerTree]
buildTree = go
  where
    go [] = []
    go (Marker {start = s, end = e, len = l, times = t} : rest) =
      let segmentEnd = e + l
          (children, remaining) = span (\m -> start m >= e && end m <= segmentEnd) rest
          subtree = MarkerTree s e l t (go children)
       in subtree : go remaining

measureTree :: [MarkerTree] -> Int
measureTree = go 0
  where
    go pos [] = 0
    go pos (MarkerTree s e l t children : rest) =
      let gap = s - pos
          expanded = measureTree children * t
          nextPos = e + l
       in gap + expanded + go nextPos rest

measure2' :: [Marker] -> Int -> Int
measure2' markers fullLen = snd $ step markers 0 fullLen
  where
    step :: [Marker] -> Int -> Int -> ([Marker], Int)
    step [] pos parentEnd = ([], parentEnd - pos)
    step (m@Marker {..} : rest) pos parentEnd
      | start < parentEnd =
          let segmentEnd = end + len
              (remaining, nestedLen) = step rest end segmentEnd
              total = start - pos + times * nestedLen
              (finalRest, restLen) = step remaining segmentEnd parentEnd
          in (finalRest, total + restLen)
      | otherwise = (m : rest, parentEnd - pos)

measure2'debug :: [Marker] -> Int -> Int
measure2'debug markers fullLen = snd $ step markers 0 fullLen
  where
    step :: [Marker] -> Int -> Int -> ([Marker], Int)
    step [] pos parentEnd = 
        let msgs = "1st clause " ++ show pos
          in trace msgs ([], parentEnd - pos)
    step (m@Marker {..} : rest) pos parentEnd
      | start < parentEnd =
          let segmentEnd = end + len
              (remaining, nestedLen) = step rest end segmentEnd
              total = start - pos + times * nestedLen
              (finalRest, restLen) = step remaining segmentEnd parentEnd
              msgs = unlines
                [ "Processing marker:"
                , "  start = " ++ show start
                , "  end   = " ++ show end
                , "  len   = " ++ show len
                , "  times = " ++ show times
                , "  nestedLen = " ++ show nestedLen
                , "  total = " ++ show total
                , "  restLen = " ++ show restLen
                , "  pos = " ++ show pos
                ]
          in trace msgs (finalRest, total + restLen)
      | otherwise = 
        let msgs = "Otherwise " ++ show (parentEnd - pos)
        in trace msgs (m : rest, parentEnd - pos)
