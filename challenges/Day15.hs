{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge)
import Utils (int, parseL, manhattanDistance)

import Text.Parsec hiding (parse, count)

type Challenge = [((Int, Int), (Int, Int))]

parseOne :: Parsec String () ((Int, Int), (Int, Int))
parseOne = (,) <$> sensor <*> beacon
  where sensor = (,) <$> (string "Sensor at x=" *> int) <*> (string ", y=" *> int)
        beacon = (,) <$> (string ": closest beacon is at x=" *> int) <*> (string ", y=" *> int)

parse :: String -> Challenge
parse = parseL parseOne

part1 :: Challenge -> Int
part1 i = sum [1 | x <- [leftExtreme..rightExtreme], occupied (x, 2000000)]
  where leftExtreme = minimum $ map (\(s, b) -> fst s - manhattanDistance s b) i
        rightExtreme = maximum $ map (\(s, b) -> fst s + manhattanDistance s b) i
        occupied point = any (\(s, b) -> manhattanDistance s point <= manhattanDistance s b && b /= point) i

part2 :: Challenge -> Int
part2 = const 42069

main :: IO ()
main = challenge 15 parse part1 part2