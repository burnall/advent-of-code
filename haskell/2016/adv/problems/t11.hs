-- stack script problems/t11.hs --resolver lts-22.18 --package containers --package regex-tdfa --package regex-base --package array

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (tails)

data DeviceKind = MC | GEN
  deriving (Show, Eq, Ord)

data Elem = TH | PL | SR | PR | RT
  deriving (Show, Eq, Ord)

data Device = Device
  { kind :: DeviceKind,
    elem :: Elem
  }
  deriving (Show, Eq, Ord)

data State = State
  { floors :: [Set Device],
    elevator :: Int
  }
  deriving (Show)

initial :: State
initial =
  State
    { floors =
        [ Set.fromList [Device GEN TH, Device MC TH, Device GEN PL, Device GEN SR],
          Set.fromList [Device MC PL, Device MC SR],
          Set.fromList [Device GEN PR, Device MC PR, Device GEN RT, Device MC RT],
          Set.empty
        ],
      elevator = 0
    }

main :: IO ()
main = do
  --print initial
  --print $ combos $ (floors initial) !! 0 
  print $ combos12 $ Set.fromList [1, 2, 3] -- Set.fromList [Device MC PL, Device MC SR]

combos12 :: Ord a => Set a -> [Set a]
combos12 s = oneElem ++ twoElem
  where
    xs = Set.toList s
    oneElem = map Set.singleton xs
    twoElem = [ Set.fromList [x, y] | (x:ys) <- tails xs, y <- ys ]

combos012 :: Ord a => Set a -> Set Set a
combos012 s = Set.singleton s `Set.union` oneElem `Set.union` twoElem
  where
    xs = Set.toList s
    oneElem = Set.map Set.singleton xs
    twoElem = [ Set.fromList [x, y] | (x:ys) <- tails xs, y <- ys ]