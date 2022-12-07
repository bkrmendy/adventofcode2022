module Main where

import Advent (challenge)
import Utils (readInt)

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
command _ _ c = error $ "Unrecognized command: " <> show c

dirSize :: Subtree -> Int
dirSize = foldl' go 0 . M.elems
  where go acc (File s) = acc + s
        go acc (Dir contents) = acc + dirSize contents  

sizes :: Subtree -> [Int]
sizes dir = dirSize dir : concatMap go (M.elems dir) 
  where go (File _) = []
        go (Dir contents) = sizes contents

root :: Subtree
root = M.fromList [("/", Dir M.empty)]

buildFs :: [[String]] -> Subtree
buildFs = snd . foldl' (uncurry command) ([], root)

part1 :: Challenge -> Int
part1 = sum . filter (<= 100000) . sizes . buildFs

part2 :: Challenge -> Int
part2 commands = head $ dropWhile (\a -> (total - a) > 40000000) $ sort $ sizes tree
  where tree = buildFs commands
        total = dirSize tree

main :: IO ()
main = challenge 07 parse part1 part2