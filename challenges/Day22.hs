module Main where

import Advent (challenge)
import Utils (int, parseLines)
import Debug.Trace

import Control.Monad (guard)
import Data.List.Split (splitOn)
import Text.Parsec hiding (parse)
import qualified Data.Map.Strict as M

type Board = M.Map (Int, Int) Char
data Dir = L | R deriving (Show)
data Instr = M Int | T Dir deriving (Show)

type Challenge = (Board, [Instr])

dir :: Parsec String () Dir
dir = (char 'L' >> pure L) <|> (char 'R' >> pure R)

instr :: Parsec String () Instr
instr = (T <$> dir) <|> (M <$> int)

parse :: String -> Challenge
parse t = (b, i)
  where
    [board, instrs] = splitOn "\n\n" t
    i = parseLines (many instr) instrs
    b = M.fromList $ do
      (nRow, row) <- zip [1..] (lines board)
      (nCol, col) <- zip [1..] row
      guard $ col /= ' '
      pure ((nRow, nCol), col)

type Position = ((Int, Int), (Int, Int))

result :: Position -> Int
result ((r, c), f) = 1000 * r + 4 * c + facing f
  where facing (0,  1) = 0
        facing (1,  0) = 1
        facing (0, -1) = 2
        facing (-1, 0) = 3

turn :: Dir -> (Int, Int) -> (Int, Int)
turn L (dr, dc) = (-dc, dr)
turn R (dr, dc) = (dc, -dr)

type WrapF = Board -> Position -> Position 

jump :: WrapF
jump board ((r, c), (dr, dc)) =
  if dr == 0
  then ((picker dc) (rows r board), (dr, dc))
  else ((picker dr) (cols c board), (dr, dc))
  where rows row = M.filterWithKey (\(rr, _) _ -> rr == row)
        cols col = M.filterWithKey (\(_, cc) _ -> cc == col)
        picker i = fst . if i < 0 then M.findMax else M.findMin

-- side lengths: 4 in example, 50 in real
cube :: WrapF
cube board ((r, c), (dr, dc)) = undefined

move :: WrapF -> Board -> Position -> Position
move wrap board ((r, c), (dr, dc)) = case M.lookup nextPos board of
  Just '.'  -> (nextPos, (dr, dc))
  Just '#'  -> ((r, c), (dr, dc))
  Nothing   -> wrap board ((r, c), (dr, dc))
  where nextPos = (r + dr, c + dc)

walk :: WrapF -> Position -> Board -> [Instr] -> Position
walk _    pos    _     []          = pos
walk wrap (p, f) board (T d:rest)  = walk wrap (p, turn d f) board rest
walk wrap pos    board (M 0:rest)  = walk wrap pos board rest
walk wrap pos    board (M n:rest)  = walk wrap (move wrap board pos) board (M (n-1):rest)

start :: Board -> (Int, Int)
start = fst . M.findMin . M.filterWithKey (\(r, _) _ -> r == 1)

part1 :: Challenge -> String
part1 (board, is) = show $ result $ walk jump (start board, (0, 1)) board is

part2 :: Challenge -> String
part2 (board, is) = "" -- show $ result $ walk cube (start board, (0, 1)) board is

main :: IO ()
main = challenge 22 parse part1 part2