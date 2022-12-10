module Main where

import Advent (visual)
import Utils (readInt)
import Data.List (foldl')
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

type Screen = M.Map (Int, Int) Bool

update :: Screen -> Int -> Int -> Screen
update screen clock x = M.insert (row, col) hit screen
  where (row, col) = (clock - 1) `divMod` 40
        hit = abs (x - col) < 2
        
        
draw :: Screen -> String
draw screen = unlines [[ d (screen M.! (r, c)) | c <- [0..39]] | r <- [0..5]]
  where d p = if p then '#' else ' '

part2 :: Challenge -> String
part2 = draw . foldl' (\m (c, s) -> update m c s) M.empty . zip [1..] . run 1

main :: IO ()
main = visual 10 parse (show . part1) part2