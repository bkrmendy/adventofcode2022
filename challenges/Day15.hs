{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge, visual)
import Utils (int, parseL, manhattanDistance)

import Control.Monad (guard)
import Text.Parsec hiding (parse, count)

type Challenge = [((Int, Int), (Int, Int))]

parseOne :: Parsec String () ((Int, Int), (Int, Int))
parseOne = (,) <$> sensor <*> beacon
  where sensor = (,) <$> (string "Sensor at x=" *> int) <*> (string ", y=" *> int)
        beacon = (,) <$> (string ": closest beacon is at x=" *> int) <*> (string ", y=" *> int)

parse :: String -> Challenge
parse = parseL parseOne

count :: Int -> Int -> Challenge -> Int
count left right beacons = sum $ do
  x <- [left..right]
  guard $ occupied (x, 2000000)
  pure 1

  where
    occupied point = any (\(s, b) -> manhattanDistance s point <= manhattanDistance s b && b /= point) beacons

part1 :: Challenge -> String
part1 i = show $ count leftExtreme rightExtreme i
  where leftExtreme = minimum $ map (\(s, b) -> fst s - manhattanDistance s b) i
        rightExtreme = maximum $ map (\(s, b) -> fst s + manhattanDistance s b) i

valid :: Challenge -> (Int, Int) -> Bool
valid i point = all (\(s, b) -> manhattanDistance s point > manhattanDistance s b) i

search
  :: ((Int, Int) -> Bool)
  -> ((Int, Int), (Int, Int))
  -> [(Int, Int)]
search validFn ((x, y), b) = do
  dy <- [0..4000000]
  nx <- [x - dist - 1 .. min x 4000000]
  guard $ nx >= 0
  dz <- [-dy, dy]
  guard $ 0 <= y + dz
  guard $ y + dz <= 4000000 && validFn (nx, y + dz)
  pure (nx, y + dz)

  where
    dist = manhattanDistance (x, y) b

headOrNull :: [a] -> [a]
headOrNull (h:_) = [h]
headOrNull [] = []

findB :: Challenge -> (Int, Int)
findB c = head $ concatMap (headOrNull . search (valid c)) c

tuning :: (Int, Int) -> Int
tuning (x, y) = x * 4000000 + y

part2 :: Challenge -> String
part2 = show . tuning . findB

main :: IO ()
main = visual 15 parse part1 part2