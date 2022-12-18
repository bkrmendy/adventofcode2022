module Main where

import Prelude hiding (fst, snd)
import Advent (challenge)
import Utils (readInt, fst, snd, thd, runBFS)

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type Coord = (Int, Int, Int)
type Scan = S.HashSet Coord
type Neighbors = M.HashMap Coord [Coord]
type BBox = (Coord, Coord)

contains :: BBox -> Coord -> Bool
contains ((minx, miny, minz), (maxx, maxy, maxz)) (x, y, z) =
  between (minx, maxx) x &&
  between (miny, maxy) y &&
  between (minz, maxz) z
  where between (l, h) n = l <= n && n <= h

cubes :: BBox -> [Coord]
cubes ((minx, miny, minz), (maxx, maxy, maxz)) = [(x, y, z) | x <- [minx..maxx], y <- [miny..maxy], z <- [minz..maxz]]

type Challenge = Scan

parse :: String -> Challenge
parse = S.fromList . map (p . splitOn ",") . lines
  where p [a, b, c] = (readInt a, readInt b, readInt c)

neighbors :: Coord -> [Coord]
neighbors (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z - 1), (x, y, z + 1)]

collectNeighbors :: Scan -> Neighbors -> Coord -> Neighbors
collectNeighbors scan faces coord = foldl' insert faces $ filter (flip S.member scan) (neighbors coord)
  where insert acc neighbor = M.insertWith (++) coord [neighbor] acc

surfaceArea :: Scan -> Int
surfaceArea scan = S.size scan * 6 - coveredSides
   where neighbors = foldl' (collectNeighbors scan) M.empty (S.toList scan)
         coveredSides = sum $ map length (M.elems neighbors)

part1 :: Challenge -> Int
part1 = surfaceArea

bounds :: Scan -> BBox
bounds scan = ((minimum xs - 1, minimum ys - 1, minimum zs - 1), (maximum xs + 1, maximum ys + 1, maximum zs + 1))
  where elems = S.toList scan
        (xs, ys, zs) = (map fst elems, map snd elems, map thd elems)

part2 :: Challenge -> Int
part2 scan = surfaceArea (S.union pockets scan)
  where
    bbox@(corner, _) = bounds scan
    neighbor = filter (\n -> contains bbox n && (not $ S.member n scan)) . neighbors
    air = S.fromList $ runBFS neighbor [corner]
    pockets = S.fromList $ filter (\c -> (not $ S.member c air || S.member c scan)) (cubes bbox)

main :: IO ()
main = challenge 18 parse part1 part2