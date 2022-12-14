{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge)
import Utils (readInt, countWhere, runBFS)

import Control.Monad (guard)
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as M

type Scan = M.HashMap (Int, Int) Char
type Challenge = (Int, Scan)

wall :: [(Int, Int)] -> [(Int, Int)]
wall [] = []
wall [_] = []
wall (f:t:rest) = [(fr + rr, fc + cc) | cc <- [0 .. tc - fc], rr <- [0..tr - fr]] ++ wall (t:rest)
  where (fc, tc) = (min (fst f) (fst t), max (fst f) (fst t))
        (fr, tr) = (min (snd f) (snd t), max (snd f) (snd t))

parse :: String -> Challenge
parse = (\cs -> (maxRow cs, scan cs)) . concatMap line . lines
  where line = wall . map joint . splitOn " -> "
        joint = (\([row, col]) -> (readInt col, readInt row)) . splitOn ","
        scan = M.fromList . zipWith (flip (,)) (repeat '#')
        maxRow = maximum . map snd

free :: Scan -> (Int, Int) -> Bool
free scan (c, r) = case M.findWithDefault '.' (c, r) scan of
  '.' -> True
  _ -> False

type StepFn = Int -> Scan -> (Int, Int) -> Maybe Scan

stepPt1 :: StepFn
stepPt1 maxR scan (c, r)
  | r >= maxR = Nothing
  | free scan (c, r + 1) = stepPt1 maxR scan (c, r + 1)
  | free scan (c - 1, r + 1) = stepPt1 maxR scan (c - 1, r + 1)
  | free scan (c + 1, r + 1) = stepPt1 maxR scan (c + 1, r + 1)
  | otherwise = Just $ M.insert (c, r) 'o' scan

pour :: StepFn -> Int -> Scan -> Scan
pour step maxRow scan = case step maxRow scan (500, 0) of
  Nothing -> scan
  Just next -> pour step maxRow next

flood :: Challenge -> Int
flood (maxR, scan) = length $ runBFS neighbor [(500, 0)]
  where
    neighbor (c, r) = do
      guard $ r <= maxR
      cell <- [(c, r + 1), (c - 1, r + 1), (c + 1, r + 1)]
      guard $ M.findWithDefault '.' cell scan == '.'
      pure cell

result :: Scan -> Int
result = countWhere (== 'o') . M.elems

part1 :: Challenge -> Int
part1 = result . (uncurry (pour stepPt1))

part2 :: Challenge -> Int
part2 = flood

main :: IO ()
main = challenge 14 parse part1 part2