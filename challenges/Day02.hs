module Main where

import Prelude hiding (round)
import Advent (challenge)
import Utils ()

type Challenge = [(Move, Move)]

data Move = Rock | Paper | Scissor deriving (Eq, Ord, Enum)

move :: String -> Move
move i | i == "A" || i == "X" = Rock
move i | i == "B" || i == "Y" = Paper
move i | i == "C" || i == "Z" = Scissor
move i = error $ "Unexpected " <> i

shape :: Move -> Int
shape Rock = 1
shape Paper = 2
shape Scissor = 3

beats :: Move -> Move
beats m = toEnum (succ (fromEnum m) `mod` 3) 

loses :: Move -> Move
loses m = toEnum (pred (fromEnum m) `mod` 3)

outcome :: Move -> Move -> Int
outcome a b | a == b    = shape b + 3
outcome a b | beats a == b = shape b + 6
            | otherwise = shape b + 0

parse :: String -> Challenge
parse = map (p . words) . lines
  where p [a, b] = (move a, move b)

correct :: (Move, Move) -> (Move, Move)
correct (left, right) = case right of
  Rock -> (left, loses left)
  Paper -> (left, left)
  Scissor -> (left, beats left)

part1 :: Challenge -> Int
part1 = sum . map (uncurry outcome)

part2 :: Challenge -> Int
part2 = sum . map (uncurry outcome . correct)

main :: IO ()
main = challenge 02 parse part1 part2