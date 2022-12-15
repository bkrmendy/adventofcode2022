{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge)
import Utils (int, parseL, manhattanDistance)

import Text.Parsec hiding (parse, count)

data Pair = MkPair { _sensor :: (Int, Int), _beacon :: (Int, Int)}

radius :: Pair -> Int
radius (MkPair sensor beacon) = manhattanDistance sensor beacon

inRange :: Pair -> (Int, Int) -> Bool
inRange pair@(MkPair sensor _) pos = manhattanDistance sensor pos <= radius pair

type Challenge = [Pair]

parseOne :: Parsec String () Pair
parseOne = MkPair <$> sensor <*> beacon
  where sensor = (,) <$> (string "Sensor at x=" *> int) <*> (string ", y=" *> int)
        beacon = (,) <$> (string ": closest beacon is at x=" *> int) <*> (string ", y=" *> int)

parse :: String -> Challenge
parse = parseL parseOne

part1 :: Challenge -> Int
part1 i = sum [1 | x <- [leftExtreme..rightExtreme], occupied (x, 2000000)]
  where leftExtreme = minimum $ map (\(MkPair s b) -> fst s - manhattanDistance s b) i
        rightExtreme = maximum $ map (\(MkPair s b) -> fst s + manhattanDistance s b) i
        occupied point = any (\pair -> inRange pair point && _beacon pair /= point) i

-- | cribbed from Big E

data Rect = MkRect { _x :: Int, _y :: Int, _width :: Int, _height :: Int }
toRect :: Pair -> Rect
toRect pair@(MkPair (sx, sy) _) = MkRect (sx - r) (sy - r) (2 * r + 1) (2 * r + 1)
  where r = radius pair

left, right, top, bottom :: Rect -> Int
left rect   = _x rect
right rect  = _x rect + _width rect - 1
top rect    = _y rect
bottom rect = _y rect + _height rect - 1

corners :: Rect -> [(Int, Int)]
corners rect = [(left rect, top rect), (right rect, top rect), (right rect, bottom rect), (left rect, bottom rect)]

split :: Rect -> [Rect]
split rect = [
    MkRect (left rect) (top rect) w0 h0,
    MkRect (left rect + w0) (top rect) w1 h0,
    MkRect (left rect) (top rect + h0) w0 h1,
    MkRect (left rect + w0) (top rect + h0) w1 h1
  ]
  where w0 = _width rect `div` 2
        w1 = _width rect - w0
        h0 = _height rect `div` 2
        h1 = _height rect - h0

uncoveredAreas :: Rect -> [Pair] ->  [Rect]
uncoveredAreas rect pairs
  | _width rect == 0 || _height rect == 0 = []
  | any (\pair -> all (inRange pair) (corners rect)) pairs = []
  | _width rect == 1 && _height rect == 1 = [rect]
  | otherwise = do
    subRect <- split rect
    uncoveredAreas subRect pairs

tuning :: Rect -> Int
tuning rect = _x rect * 4000000 + _y rect

part2 :: Challenge -> Int
part2 = tuning . head . uncoveredAreas (MkRect 0 0 4000001 4000001)

main :: IO ()
main = challenge 15 parse part1 part2