module Main where

import Advent (challenge)
import Utils ()
import Data.List (nub)

type Challenge = String

parse :: String -> Challenge
parse = id

window :: Int -> Int -> String -> Int
window n l cs =
  if length (nub (take l cs)) == l
  then n + l
  else window (n + 1) l (tail cs)

part1 :: Challenge -> Int
part1 = window 0 4

part2 :: Challenge -> Int
part2 = window 0 14

main :: IO ()
main = challenge 06 parse part1 part2