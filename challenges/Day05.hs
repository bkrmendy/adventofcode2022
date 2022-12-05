module Main where

import Advent (challenge)
import Utils (readInt)

import Data.List.Split (splitOn)
import Data.List (foldl', transpose)
import Data.Function ((&))
import Data.Char (isUpper)
import Data.Bifunctor (first)

import qualified Data.Map.Strict as M

type Stacks = M.Map Int [Char]
type Challenge = (Stacks, [(Int, Int, Int)])

move :: String -> (Int, Int, Int)
move i = (readInt n, readInt from, readInt to)
  where [_, n, _, from, _, to] = words i 

stacks :: String -> Stacks
stacks = M.fromList . zip [1..] . filter (not . null) . map (filter isUpper) . transpose . lines

parse :: String -> Challenge
parse i = (stacks starting, map move $ lines script)
  where [starting, script] = splitOn "\n\n" i

popAt :: Int -> Int -> Stacks -> ([Char], Stacks)
popAt n p stacks = (pp, newStacks)
  where stack = stacks M.! n
        (pp, remaining) = splitAt p stack
        newStacks = M.insert n remaining stacks

pushAt :: Int -> [Char] -> Stacks -> Stacks
pushAt n c stacks = M.insert n (c ++ stacks M.! n) stacks

type Crane = [Char] -> [Char]

step :: Crane -> (Int, Int, Int) -> Stacks -> Stacks
step crane (n, from, to) s = popAt from n s
                           & first crane
                           & uncurry (pushAt to)

run :: Crane -> Challenge -> String
run crane = map (head . snd) . M.assocs . uncurry (foldl' (flip (step crane)))

main :: IO ()
main = challenge 05 parse (run reverse) (run id)