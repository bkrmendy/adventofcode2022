{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge)
import Utils (runShortestPathsFrom)

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Char (ord)
import qualified Data.Array as A

type ElevationMap = A.Array (Int, Int) Char
type Challenge = [(Int, Char)]
type Cell = ((Int, Int), Char)

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height c = ord c

neighbors :: ElevationMap -> (Int, Cell) -> [(Int, Cell)]
neighbors elevationMap (cost, ((row, col), elevation)) = do
  neighbor <- [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
  guard $ A.inRange (A.bounds elevationMap) neighbor
  let cell = elevationMap A.! neighbor
      diff = height elevation - height cell
  guard $ diff < 2
  pure (cost + 1, (neighbor, cell))

search :: ElevationMap -> ((Int, Int), Char) -> [(Int, Char)]
search elevationMap start = map (\(d, (_, c)) -> (d, c)) $ runShortestPathsFrom (neighbors elevationMap) (0, start)

parseElevationMap :: String -> ElevationMap
parseElevationMap i = A.array ((1, 1), (width, height)) $ do
  (iRow, row) <- zip [1..] rows
  (iCol, cell) <- zip [1..] row
  pure ((iCol, iRow), cell)
  where rows = lines i
        width = length (head rows)
        height = length rows

parse :: String -> Challenge
parse i = search elevationMap start
  where elevationMap = parseElevationMap i
        start = fromJust $ find (\(_, e) -> e == 'E') $ A.assocs elevationMap


part1 :: Challenge -> Int
part1 = fst . head . filter ((== 'S') . snd)

part2 :: Challenge -> Int
part2 = minimum . map fst . filter ((\c -> height c == height 'a') . snd)

main :: IO ()
main = challenge 12 parse part1 part2