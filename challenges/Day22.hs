module Main where

import Advent (challenge)
import Utils (int, parseLines)

import Control.Monad (guard)
import Data.List.Split (splitOn)
import Text.Parsec hiding (parse, lookAhead)
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

type LookAheadFn = Board -> Position -> (Position, Char)

jump :: LookAheadFn
jump board ((r, c), f@(dr, dc)) = case M.lookup nextPos board of
  Just cel -> ((nextPos, f), cel)
  Nothing -> (wrap, board M.! (fst wrap))
  where
    nextPos = (r + dr, c + dc)
    wrap =
      if dr == 0
        then ((picker dc) (rows r board), (dr, dc))
        else ((picker dr) (cols c board), (dr, dc))
        where rows row = M.filterWithKey (\(rr, _) _ -> rr == row)
              cols col = M.filterWithKey (\(_, cc) _ -> cc == col)
              picker i = fst . if i < 0 then M.findMax else M.findMin

inRange :: (Int, Int) -> Int -> Bool
inRange (lo, hi) v = lo <= v && v <= hi

-- ☠ ☠️☠ ☠️☠ ☠️☠
cubeWrap :: Position -> Position
cubeWrap ((r, 150), (_, 1)) = ((151 - r, 100), (0, -1))
cubeWrap ((r, 100), (_, 1)) | inRange (51, 100) r = ((50, 100 + (r - 50)), (-1, 0))
cubeWrap ((r, 100), (_, 1)) | inRange (101, 150) r = ((51 - (r - 100), 150), (0, -1))
cubeWrap ((r, 50), (_, 1))  = ((150, 50 + (r - 150)), (-1, 0))

cubeWrap ((r, 51), (_, -1)) | inRange (1, 50) r  = ((151 - r, 1), (0, 1))
cubeWrap ((r, 51), (_, -1)) | inRange (51, 100) r  = ((101, r - 50), (1, 0))
cubeWrap ((r, 1), (_, -1)) | inRange (101, 150) r  = ((1 + (150 - r), 51), (0, 1))
cubeWrap ((r, 1), (_, -1)) | inRange (151, 200) r  = ((1, r - 150 + 50), (1, 0))

cubeWrap ((50, c), (1, _)) = ((c - 50, 100), (0, -1))
cubeWrap ((150, c), (1, _)) = ((c + 100, 50), (0, -1))
cubeWrap ((200, c), (1, _)) = ((1, c + 100), (1, 0))

cubeWrap ((1, c), (-1, _)) | inRange (51, 100) c = ((c + 100, 1), (0, 1))
cubeWrap ((1, c), (-1, _)) | inRange (101, 150) c = ((200, c - 100), (-1, 0))
cubeWrap ((101, c), (-1, _)) = ((c + 50, 51), (0, 1))
cubeWrap a = error $ "cube wrap direction not implemented: " <> show a

cube :: LookAheadFn
cube board ((r, c), f@(dr, dc)) = case M.lookup nextPos board of
  Just cel -> ((nextPos, f), cel)
  Nothing -> (cw, board M.! (fst cw))
  where
    nextPos = (r + dr, c + dc)
    cw = cubeWrap ((r, c), (dr, dc))
    
move :: LookAheadFn -> Board -> Position -> Position
move lookAhead board pos = case lookAhead board pos of
  (nextPos, '.')  -> nextPos
  (_, _)          -> pos

walk :: LookAheadFn -> Position -> Board -> [Instr] -> Position
walk _    pos    _     []          = pos
walk lookAhead (p, f) board (T d:rest)  = walk lookAhead (p, turn d f) board rest
walk lookAhead pos    board (M 0:rest)  = walk lookAhead pos board rest
walk lookAhead pos    board (M n:rest)  = walk lookAhead (move lookAhead board pos) board (M (n-1):rest)

start :: Board -> (Int, Int)
start = fst . M.findMin . M.filterWithKey (\(r, _) _ -> r == 1)

part1 :: Challenge -> String
part1 (board, is) = show $ result $ walk jump (start board, (0, 1)) board is

part2 :: Challenge -> String
part2 (board, is) = show $ result $ walk cube (start board, (0, 1)) board is

main :: IO ()
main = challenge 22 parse part1 part2