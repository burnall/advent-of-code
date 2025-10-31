-- stack script problems/t08.hs --resolver lts-22.18 --package split --package containers --package regex-tdfa --package regex-base --package vector

import Data.Function ((&))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Text.Regex.Base.RegexLike (AllTextSubmatches (..))
import Text.Regex.TDFA

type Row = VU.Vector Bool

type Screen = V.Vector Row

emptyScreen :: Screen
emptyScreen = V.replicate 6 $ VU.replicate 50 False

data Command
  = Rect Int Int
  | RotateColumn Int Int
  | RotateRow Int Int
  deriving (Show, Eq)

main :: IO ()
main = do
  content <- readFile "../data/t08.txt"
  let screen = content & lines & map parseLine & foldl apply emptyScreen
  screen & countPixels & print
  screen & printScreen
  --[Rect 3 2, RotateColumn 1 1, RotateRow 0 4, RotateColumn 1 1] & foldl apply emptyScreen & printScreen

parseLine :: String -> Command
parseLine s =
  case getAllTextSubmatches (s =~ pattern :: AllTextSubmatches [] String) of
    [_, x1, y1, "", "", "", ""] -> Rect (read x1) (read y1)
    [_, "", "", x2, y2, "", ""] -> RotateColumn (read x2) (read y2)
    [_, "", "", "", "", x3, y3] -> RotateRow (read x3) (read y3)
    xs -> error $ show s ++ ", groups: " ++ show xs
  where
    pattern = "rect ([0-9]+)x([0-9]+)|rotate column x=([0-9]+) by ([0-9]+)|rotate row y=([0-9]+) by ([0-9]+)"

apply :: Screen -> Command -> Screen
apply screen (Rect w h) =
  screen V.// [(y, updateRow (screen V.! y)) | y <- [0 .. h - 1]]
  where
    updateRow row = row VU.// [(x, True) | x <- [0 .. w - 1]]
apply screen (RotateRow y d) =
  screen V.// [(y, newRow)]
  where
    row = screen V.! y
    len = VU.length row
    newRow = VU.generate len (\x -> row VU.! ((x - d) `mod` len))
apply screen (RotateColumn x d) =
  V.imap updateRow screen
  where
    len = V.length screen
    column = V.generate len (\i -> (screen V.! i) VU.! x)
    updateRow y row =
      let newVal = column V.! ((y - d) `mod` len)
       in row VU.// [(x, newVal)]

countPixels :: V.Vector (VU.Vector Bool) -> Int
countPixels = V.sum . V.map (VU.length . VU.filter id)

printScreen :: V.Vector (VU.Vector Bool) -> IO ()
printScreen screen = V.forM_ screen $ \row -> do
  putStrLn $ VU.toList row >>= (\b -> if b then "#" else ".")