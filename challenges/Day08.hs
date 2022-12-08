module Main where

import Advent (challenge)
import Utils ()

import Data.Char (digitToInt)
import qualified Data.Array.Unboxed as A

type Forest = A.Array (Int, Int) Int

type Challenge = Forest

parse :: String -> Challenge
parse input = A.array ((1, 1), (width, height)) $ do
  (row, line) <- zip [1..] (lines input)
  (column, tree) <- zip [1..] (map digitToInt line)
  pure ((column, row), tree) 
  
  where height = length (lines input)
        width = length $ head $ lines input

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (c, r) (dc, dr) = (c + dc, r + dr)

tallestInDirection :: Forest -> (Int, Int) -> Int -> (Int, Int)  -> Bool 
tallestInDirection forest (col, row) height delta
  | not (A.inRange (A.bounds forest) (col, row)) = True
  | forest A.! (col, row) >= height = False
  | otherwise = tallestInDirection forest (step (col, row) delta) height delta
  
treesInDirection :: Forest -> (Int, Int) -> Int -> (Int, Int)  -> Int 
treesInDirection forest (col, row) height delta
  | not (A.inRange (A.bounds forest) (col, row)) = 0
  | forest A.! (col, row) >= height = 1
  | otherwise = 1 + treesInDirection forest (step (col, row) delta) height delta

directions :: [(Int, Int)]
directions = [(-1, 0), (1, 0), (0, 1), (0, -1)]

visible :: Forest -> (Int, Int) -> Int -> Bool
visible forest coord height = any (\d -> tallestInDirection forest (step coord d) height d) directions

trees :: Forest -> (Int, Int) -> Int -> Int
trees forest coord height = product $ map (\d -> treesInDirection forest (step coord d) height d) directions

part1 :: Challenge -> Int
part1 forest = length $ filter (uncurry (visible forest)) $ A.assocs forest  

part2 :: Challenge -> Int
part2 forest = maximum $ map (uncurry (trees forest)) $ A.assocs forest

main :: IO ()
main = challenge 08 parse part1 part2