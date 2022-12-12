module Main where

import Advent (challenge)
import Utils (spfa)

import Control.Monad (guard)
import Data.Maybe (fromJust, catMaybes)
import Data.List (find)
import Data.Char (ord)
import qualified Data.Array as A

type ElevationMap = A.Array (Int, Int) Char
type Challenge = ElevationMap

parse :: String -> Challenge
parse i = A.array ((1, 1), (width, height)) $ do
  (iRow, row) <- zip [1..] rows
  (iCol, cell) <- zip [1..] row
  pure ((iCol, iRow), cell)
  where rows = lines i
        width = length (head rows)
        height = length rows

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height c = ord c - ord 'a'

type NeighborsFn = (Int, ((Int, Int), Char)) -> [(Int, ((Int, Int), Char))]

neighbors :: ElevationMap -> NeighborsFn
neighbors elevationMap (cost, ((row, col), elevation)) = do
  neighbor <- [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
  guard $ A.inRange (A.bounds elevationMap) neighbor
  let cell = elevationMap A.! neighbor
      diff = height cell - height elevation
  guard $ diff < 2
  pure (cost + 1, (neighbor, cell))

search :: NeighborsFn -> ((Int, Int), Char) -> ((Int, Int), Char) -> Maybe Int
search neighborsFn end start = fst <$> spfa neighborsFn end (0, start) 

part1 :: Challenge -> Int
part1 elevationMap = fromJust $ search (neighbors elevationMap) end start
  where start = fromJust $ find (\(_, e) -> e == 'S') $ A.assocs elevationMap
        end = fromJust $ find (\(_, e) -> e == 'E') $ A.assocs elevationMap

part2 :: Challenge -> Int
part2 elevationMap = minimum $ catMaybes $ do
  start <- filter (\(_, e) -> e == 'a') $ A.assocs elevationMap
  pure $ search (neighbors elevationMap) end start
  where end = fromJust $ find (\(_, e) -> e == 'E') $ A.assocs elevationMap

main :: IO ()
main = challenge 12 parse part1 part2