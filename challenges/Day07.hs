module Main where

import Advent (challenge)
import Utils (readInt)
import Debug.Trace (traceShow)

import Data.List (foldl', sort)
import qualified Data.Map.Strict as M

type Path = [String]
type Subtree = M.Map String INode
data INode = File Int | Dir Subtree deriving (Show)

type Challenge = [[String]]

parse :: String -> Challenge
parse = map words . lines

put :: Path -> Subtree -> (String, INode) -> Subtree
put [] node (name, new) = M.insert name new node
put (dir:rest) node new = M.insert dir (Dir $ put rest parent new) node
  where
    parent = case node M.! dir of
      File _ -> error "Expected dir"
      Dir contents -> contents

command :: Path -> Subtree -> [String] -> (Path, Subtree)
command (_:parent) tree ["$", "cd", ".."] = (parent, tree)
command _ tree ["$", "cd", "/"] = (["/"], tree)
command path tree ["$", "cd", dir] = (dir:path, tree)
command path tree ["$", "ls"] = (path, tree)
command path tree ["dir", name] = (path, put (reverse path) tree (name, Dir M.empty))
command path tree [size, filename] = (path, put (reverse path) tree (filename, File (readInt size)))
command _ _ c = error $ "Unrecongized command: " <> show c

dirSize :: Subtree -> Int
dirSize = foldl' go 0 . M.elems
  where go acc (File s) = acc + s
        go acc (Dir contents) = acc + dirSize contents  

sizes :: Subtree -> [Int]
sizes dir = dirSize dir : concatMap size (M.elems dir) 
  where size :: INode -> [Int]
        size (File _) = []
        size (Dir contents) = sizes contents

root :: Subtree
root = M.fromList [("/", Dir M.empty)]

part1 :: Challenge -> Int
part1 = sum . filter (<= 100000) . sizes . snd . foldl' (uncurry command) ([], root)

part2 :: Challenge -> Int
part2 commands = head $ dropWhile (\a -> (unused + a) < 30000000) $ sort $ sizes tree
  where (_, tree) = foldl' (uncurry command) ([], root) commands
        total = dirSize tree
        unused = 70000000 - total

main :: IO ()
main = challenge 07 parse part1 part2