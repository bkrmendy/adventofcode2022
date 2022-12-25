module Main where

import Advent (challenge)
import Utils ()

import Data.List (foldl')
type Challenge = [String]

parse :: String -> Challenge
parse = lines

d :: Char -> Int
d '2' = 2
d '1' = 1
d '0' = 0
d '-' = -1
d '=' = -2

r :: Int -> Char
r 2 = '2'
r 1 = '1'
r 0 = '0'
r (-1) = '-'
r (-2) = '='

snafu :: String -> Int
snafu = foldl' (\acc v -> 5 * acc + d v) 0

ufans :: Int -> String
ufans i | i < 3 = [r i]
ufans i = ufans d ++ [r (m - 2)]
  where (d, m) = (i + 2) `divMod` 5

part1 :: Challenge -> String
part1 = ufans . sum . map snafu

part2 :: Challenge -> String
part2 = const "⭐"

main :: IO ()
main = challenge 25 parse part1 part2