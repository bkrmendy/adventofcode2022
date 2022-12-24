{-# LANGUAGE TupleSections #-}

module Main where

import Advent (challenge)
import Utils (runBFS, thd)
import Debug.Trace

import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Position = (Int, Int, Int) -- row, col, time

type Valley = (Int, Int, M.Map Position (S.Set Char)) -- width, height
type Challenge = Valley

parse :: String -> Challenge
parse i = (!! 1000) $ extrapolate cells (width, height, M.empty)
  where
    ls = lines i
    height = length ls - 2
    width = length (head ls) - 2
    cells = M.fromList $ do
      (iRow, row) <- zip [0..] ls
      (iCol, cell) <- zip [0..] row
      guard $ cell /= '#'
      pure $ ((iRow, iCol, 0), mk cell)
      where mk '.' = S.singleton '.'
            mk cel = S.fromList ['.', cel]

blow :: Int -> Int -> Char -> Position -> Position
blow w _ '>' (r, c, t) = if c == w then (r, 1, t + 1) else (r, c + 1, t + 1)
blow _ h 'v' (r, c, t) = if r == h then (1, c, t + 1) else (r + 1, c, t + 1)
blow w _ '<' (r, c, t) = if c == 1 then (r, w, t + 1) else (r, c - 1, t + 1)
blow _ h '^' (r, c, t) = if r == 1 then (h, c, t + 1) else (r - 1, c, t + 1)
blow _ _ '.' (r, c, t) = (r, c, t + 1)

extrapolate :: M.Map Position (S.Set Char) -> Valley -> [Valley]
extrapolate frontier (w, h, cells) = (w, h, M.union frontier cells) : extrapolate nextGeneration (w, h, M.union frontier cells)
  where
    nextGeneration = foldl' go M.empty (M.assocs frontier)
    go :: M.Map Position (S.Set Char) -> (Position, (S.Set Char)) -> M.Map Position (S.Set Char)
    go next (pos, things) = foldl' update next [(blow w h cel pos, S.singleton cel) | cel <- S.elems things]
      where update n (p, t) = M.insertWith S.union p t n

type NeighborsFn a = Valley -> a -> [a]

neighborsPt1 :: NeighborsFn Position
neighborsPt1 (_, _, valley) (r, c, t) = [p | p <- ns (r, c), ok p]
  where
    ns (rr, cc) = [(rr + 1, cc, t + 1), (rr - 1, cc, t + 1), (rr, cc + 1, t + 1), (rr, cc - 1, t + 1), (rr, cc, t + 1)]
    ok p = case M.lookup p valley of
        Nothing -> False
        Just s -> s == S.singleton '.'

type EndFn a = (Int, Int) -> a -> Maybe Int
endPt1 :: EndFn Position
endPt1 (re, ce) (r, c, t) = if re == r && c == ce then Just t else Nothing

walk
  :: (Ord a)
  => NeighborsFn a
  -> EndFn a
  -> (Int -> Int -> a)
  -> Valley -> Int
walk neighbors end start valley  = minimum
                                  $ mapMaybe (end (re, ce))
                                  $ runBFS (neighbors valley) [start rs cs]
  where (rs, cs, _) = fst $ M.findMin (thd valley)
        (re, ce, _) = fst $ M.findMax (thd valley)

part1 :: Challenge -> String
part1 = show . walk neighborsPt1 endPt1 (\rs cs -> (rs, cs, 0))

data Stage = There | Back | BackAgain deriving (Eq, Ord, Show)

endPt2 :: EndFn (Int, Int, Int, Stage)
endPt2 (re, ce) (r, c, t, s) = if re == r && c == ce && s == BackAgain then Just t else Nothing

neighborsPt2 :: NeighborsFn (Int, Int, Int, Stage)
neighborsPt2 (_, h, valley) (r, c, t, stage)
  | r > h && stage == BackAgain = []
  | r > h && stage == There     = [(r - 1, c, t + 1, Back)]
  | r < 1 && stage == Back      = [(r + 1, c, t + 1, BackAgain)]
  | otherwise = [p | p <- ns (r, c), ok p]
  where
    ns (rr, cc) = [ (rr + 1, cc, t + 1, stage)
                  , (rr - 1, cc, t + 1, stage)
                  , (rr, cc + 1, t + 1, stage)
                  , (rr, cc - 1, t + 1, stage)
                  , (rr, cc, t + 1, stage)
                  ]
    ok (rr, cc, tt, _) = case M.lookup (rr, cc, tt) valley of
        Nothing -> False
        Just s -> s == S.singleton '.'

part2 :: Challenge -> String
part2 = show . walk neighborsPt2 endPt2 (\rs cs -> (rs, cs, 0, There))

main :: IO ()
main = challenge 24 parse part1 part2