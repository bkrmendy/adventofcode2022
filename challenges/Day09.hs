module Main where

import Prelude hiding (head, tail)
import Advent (challenge)
import Utils (readInt)
import qualified Data.Set as S

type Challenge = [(String, Int)]

parse :: String -> Challenge
parse = map (p . words) . lines
  where p [d, a] = (d, readInt a)

adjacent :: (Int, Int) -> (Int, Int) -> Bool
adjacent (hr, hc) (tr, tc) = abs (hr - tr) <= 1 && abs (hc - tc) <= 1

moveTowards :: String -> (Int, Int) -> (Int, Int)
moveTowards "R" (r, c) = (r, c - 1)
moveTowards "L" (r, c) = (r, c + 1)
moveTowards "D" (r, c) = (r - 1, c)
moveTowards "U" (r, c) = (r + 1, c)

moveBehind :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveBehind (hr, hc) (tr, tc) = (tr + dr, tc + dc)
  where dr = signum (hr - tr)
        dc = signum (hc - tc)

cascade :: [(Int, Int)] -> String -> ([(Int, Int)], (Int, Int))
cascade knots = go knots []
  where
        go :: [(Int, Int)] -> [(Int, Int)] -> String -> ([(Int, Int)], (Int, Int))
        go [tail] acc _ = (reverse (tail:acc), tail)
        go (head:tail:rest) acc d =
          let
            nt = if not (adjacent head tail) then moveBehind head tail else tail
         in go (nt:rest) (head:acc) d

move :: [(Int, Int)] -> S.Set (Int, Int) -> [(String, Int)] -> S.Set (Int, Int)
move _ seen [] = seen
move segments seen ((_, 0):rest) = move segments seen rest
move (head:segments) seen ((d, n):rest) = move newSegments (S.insert newTail seen) ((d, n-1):rest)
  where newHead = moveTowards d head
        (newSegments, newTail) = cascade (newHead:segments) d

part1 :: Challenge -> Int
part1 = S.size . move (replicate 2 (0, 0)) S.empty

part2 :: Challenge -> Int
part2 = S.size . move (replicate 10 (0, 0)) S.empty

main :: IO ()
main = challenge 9 parse part1 part2