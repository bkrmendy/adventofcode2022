module Main where

import Advent (challenge)
import Utils (readInt)
import Data.List (sort)
import Data.List.Split (splitOn)

type Challenge = [[Int]]

parse :: String -> Challenge
parse = map parseElf . splitOn "\n\n"
  where parseElf = map readInt . lines

part1 :: Challenge -> Int
part1 = maximum . map sum

part2 :: Challenge -> Int
part2 = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = challenge 01 parse part1 part2