-- stack script problems/t07.hs --resolver lts-22.18 --package split --package containers

import Data.Function ((&))
import Data.List.Split (splitWhen)
import qualified Data.Set as Set

main :: IO ()
main = do
  content <- readFile "../data/t07.txt"
  content & lines & task2 & length & print

task1 :: [String] -> [String]
task1 = filter isValid
  where
    isValid addr = validateParts (zip (cycle [False, True]) (splitByBrackets addr))

    validateParts :: [(Bool, String)] -> Bool
    validateParts = go False
      where
        go hadAbba [] = hadAbba
        go hadAbba ((isBracketed, part) : xs)
          | not isBracketed = go (hadAbba || hasAbba part) xs
          | hasAbba part = False
          | otherwise = go hadAbba xs

hasAbba :: String -> Bool
hasAbba (a : b : c : d : rest)
  | a /= b && a == d && b == c = True
  | otherwise = hasAbba (b : c : d : rest)
hasAbba _ = False

splitByBrackets :: String -> [String]
splitByBrackets = splitWhen (`elem` "[]")

task2 :: [String] -> [String]
task2 = filter isValid
  where
    isValid addr =
      let (outside, inside) = splitEvenOdd $ splitByBrackets addr
          abas = concatMap getAba outside
          babs = concatMap getAba inside
       in any (\aba -> toBab aba `elem` babs) abas

splitEvenOdd :: [String] -> ([String], [String])
splitEvenOdd xs =
  ( [x | (i, x) <- zip [0 ..] xs, even i],
    [x | (i, x) <- zip [0 ..] xs, odd i]
  )

getAba :: String -> [String]
getAba (a : b : c : rest)
  | a == c && a /= b = [a, b, c] : getAba (b : c : rest)
  | otherwise = getAba (b : c : rest)
getAba _ = []

toBab :: String -> String
toBab (a : b : _) = [b, a, b]
