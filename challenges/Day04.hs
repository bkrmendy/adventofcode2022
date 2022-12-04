module Main where

import Advent (challenge)
import Utils (readInt, countWhere)
import Data.List.Split (splitOn)

type Interval = (Int, Int)

type Challenge = [(Interval, Interval)]

interval :: String -> Interval
interval i = case splitOn "-" i of
  [lo, hi] -> (readInt lo, readInt hi)
  _ -> error $ "Unexpected interval: " <> i

parse :: String -> Challenge
parse = map (intervals . splitOn ",") . lines
  where intervals [a, b] = (interval a, interval b)

contains :: Interval -> Interval -> Bool
contains (l1, h1) (l2, h2) = l1 <= l2 && h2 <= h1

overlaps :: Interval -> Interval -> Bool
overlaps (l1, h1) (l2, h2) = l1 <= h2 && h1 >= l2

part1 :: Challenge -> Int
part1 = countWhere (\(a, b) -> contains a b || contains b a)

part2 :: Challenge -> Int
part2 = countWhere (\(a, b) -> overlaps a b || overlaps b a)

main :: IO ()
main = challenge 04 parse part1 part2