module Main where

import Advent (visual)
import Utils (readInt)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

type Challenge = [(String, Int)]

parse :: String -> Challenge
parse = map (p . words) . lines
  where p [i, o] = (i, readInt o)
        p [i] = (i, 0)
  
run :: Int -> [(String, Int)] -> [Int]
run _ [] = []
run x (("noop", _):rest) = x:run x rest
run x (("addx", i):rest) = x:run x (("addx_end", i):rest)
run x (("addx_end", i):rest) = x:run (x + i) rest

pick :: [Int] -> M.Map Int Int -> [Int]
pick is from = map (\i -> i * from M.! i) is

part1 :: Challenge -> Int
part1 = sum . pick [20, 60, 100, 140, 180, 220] . M.fromList . zip [1..] . run 1

draw :: [Int] -> String
draw = unlines . map (zipWith (curry d) [0..]) . chunksOf 40
  where d (c, s) = if abs (c - s) < 2 then '▊' else ' '

part2 :: Challenge -> String
part2 = draw . run 1

main :: IO ()
main = visual 10 parse (show . part1) part2