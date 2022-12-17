module Main where

import Advent (challenge)
import Data.Bifunctor (first, second)

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type Challenge = String

type Shape = S.HashSet (Int, Int)
type Cave = S.HashSet (Int, Int)

shapes :: [Shape]
shapes =
  cycle [
    S.fromList [(0, 0), (0, 1), (0, 2), (0, 3)] -- -
  , S.fromList [(1, 0), (1, 1), (1, 2), (0, 1), (2, 1)] -- +
  , S.fromList [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)] -- _|
  , S.fromList [(0, 0), (1, 0), (2, 0), (3, 0)] -- |
  , S.fromList [(0, 0), (1, 0), (0, 1), (1, 1)] -- []
  ]

parse :: String -> Challenge
parse = cycle

shiftLeft, shiftRight, shiftDown :: Shape -> Shape
shiftLeft = S.map (second pred)
shiftRight = S.map (second succ)
shiftDown = S.map (first pred)

height :: Cave -> Int
height cave = if S.null cave then 0 else succ $ maximum $ map fst $ S.toList cave

valid :: Cave -> Shape -> Bool
valid cave shape = inBounds && notClipping
  where inBounds = all (\(r, c) -> r >= 0 && c >= 0 && c < 7) $ S.toList shape
        notClipping = S.null $ S.intersection cave shape

push :: Cave -> Shape -> String -> (Cave, String)
push cave shape (j:jets)
  | valid cave pushedShape = descend cave pushedShape jets
  | otherwise = descend cave shape jets
  where p '<' = shiftLeft shape
        p '>' = shiftRight shape
        pushedShape = p j

descend :: Cave -> Shape -> String -> (Cave, String)
descend cave shape jets
  | valid cave nextShape = push cave nextShape jets
  | otherwise = (S.union shape cave, jets)
  where nextShape = shiftDown shape

load :: Cave -> Shape -> Shape
load cave = horizontal . vertical
  where horizontal = shiftRight . shiftRight
        vertical = S.map (first (\n -> n + h + 3))
        h = height cave

step :: [Shape] -> Cave -> String -> [Cave]
step (shape:rest) cave jets = cave:uncurry (step rest) next
  where next = push cave (load cave shape) jets

part1 :: Challenge -> String
part1 = show . height . (!! 2022) . step shapes S.empty

findCycle :: [Cave] -> (Int,Int)
findCycle = go M.empty 0
  where
    go seen i (x:xs) =
      case M.lookup x seen of
        Nothing -> go (M.insert x i seen) (i+1) xs
        Just j -> (j,i)

trim :: Cave -> Cave
trim cave = S.map (first ((-) cutoff)) $ S.filter (\(r, _) -> r > cutoff) cave
  where cutoff = height cave - 113

part2 :: Challenge -> String
part2 dirs = show $ height (boards !! (end + rest)) + repeatedHeight * (count - 1)
  where boards = take 3123 $ step shapes S.empty dirs
        trimmed = map trim boards
        (start, end) = findCycle trimmed
        len = end - start
        (count, rest) = (1000000000000 - start) `divMod` len
        repeatedHeight = height (boards !! end) - height (boards !! start)


main :: IO ()
main = challenge 17 parse part1 part2