module Main where

import Prelude hiding (round)
import Advent (challenge)
import Utils ()

import qualified Data.Map.Strict as M

type Challenge = [(Move, Move)]

data Move = Rock | Paper | Scissor deriving (Eq, Ord)

move :: String -> Move
move i | i == "A" || i == "X" = Rock
move i | i == "B" || i == "Y" = Paper
move i | i == "C" || i == "Z" = Scissor
move i = error $ "Unexpected " <> i

shape :: Move -> Int
shape Rock = 1
shape Paper = 2
shape Scissor = 3

beats :: M.Map Move Move
beats = M.fromList [(Rock, Paper), (Paper, Scissor), (Scissor, Rock)]

loses :: M.Map Move Move
loses = M.fromList $ map (\(a, b) -> (b, a)) $ M.assocs beats

outcome :: Move -> Move -> Int
outcome a b | a == b    = shape b + 3
outcome a b | beats M.! a == b = shape b + 6
            | otherwise = shape b + 0

parse :: String -> Challenge
parse = map (p . words) . lines
  where p [a, b] = (move a, move b)

correct :: (Move, Move) -> (Move, Move)
correct (left, right) = case right of
  Rock -> (left, loses M.! left)
  Paper -> (left, left)
  Scissor -> (left, beats M.! left)

part1 :: Challenge -> Int
part1 = sum . map (uncurry outcome)

part2 :: Challenge -> Int
part2 = sum . map (uncurry outcome . correct)

main :: IO ()
main = challenge 02 parse part1 part2