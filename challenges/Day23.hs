module Main where

import Advent (challenge)

import Data.List (foldl')
import Control.Monad (guard)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

type Position = (Int, Int)
type Grove = S.HashSet Position
type Challenge = Grove

parse :: String -> Challenge
parse i = S.fromList $ do
  (nRow, row) <- zip [0..] (lines i)
  (nCol, col) <- zip [0..] row
  guard $ col == '#'
  pure (nRow, nCol)

n, ne, nw, s, se, sw, e, w :: Position -> Position
s (r, c) = (r + 1, c)
n (r, c) = (r - 1, c)
w (r, c) = (r, c - 1)
e (r, c) = (r, c + 1)
ne = n . e
nw = n . w
se = s . e
sw = s . w

free :: Grove -> [Position] -> Bool
free grove ps = all (\p -> not $ S.member p grove) ps

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]

type Dir = Grove -> Position -> [Position]

nDir, sDir, wDir, eDir :: Dir
nDir grove pos = [n pos | free grove [n pos, ne pos, nw pos]]
sDir grove pos = [s pos | free grove [s pos, se pos, sw pos]]
wDir grove pos = [w pos | free grove [w pos, nw pos, sw pos]]
eDir grove pos = [e pos | free grove [e pos, ne pos, se pos]]

rotating :: [a] -> [[a]]
rotating xs = xs:rotating (rotate xs)

dirs :: [[Dir]]
dirs = rotating [nDir, sDir, wDir, eDir]

propose :: [Dir] -> Grove -> M.HashMap Position [Position]
propose dir grove = foldl' go M.empty (S.toList grove)
  where
    go plans pos = M.insertWith (++) next [pos] plans
      where
        stay = [pos | free grove [n pos, s pos, w pos, e pos, ne pos, nw pos, se pos, sw pos]]
        next = head $ stay ++ concat [d grove pos | d <- dir] ++ [pos]

move :: Grove -> M.HashMap Position [Position] -> Grove
move grove proposals = foldl' go grove (M.toList proposals)
  where
    go next (target, [applicant]) = S.insert target $ S.delete applicant next
    go next _                     = next

step :: [[Dir]] -> Grove -> [Grove]
step (dir:rest) grove = next : step rest next
  where next = move grove $ propose dir grove

result :: Grove -> Int
result grove = (maxCol - minCol + 1) * (maxRow - minRow + 1) - S.size grove
  where rows = map fst $ S.toList grove
        cols = map snd $ S.toList grove
        (minRow, maxRow) = (minimum rows, maximum rows)
        (minCol, maxCol) = (minimum cols, maximum cols)

part1 :: Challenge -> String
part1 = show . result . (!! 9) . step dirs

takeUntilNoChange :: (Eq a) => [a] -> [a]
takeUntilNoChange (a:b:_) | a == b  = [a, b]
takeUntilNoChange (a:b:rest)        = a : takeUntilNoChange (b:rest)

part2 :: Challenge -> String
part2 = show . length . takeUntilNoChange . step dirs

main :: IO ()
main = challenge 23 parse part1 part2