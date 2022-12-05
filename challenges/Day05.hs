module Main where

import Advent (challenge)
import Utils (readInt)

import Data.List.Split (splitOn)
import Data.List (foldl', transpose)
import Data.Function ((&))
import Data.Char (isUpper)
import Data.Bifunctor (first)

type Challenge = ([[Char]], [(Int, Int, Int)])

move :: String -> (Int, Int, Int)
move i = (readInt n, readInt from, readInt to)
  where [_, n, _, from, _, to] = words i 

stacks :: String -> [[Char]]
stacks = filter (not . null) . map (filter isUpper) . transpose . lines

parse :: String -> Challenge
parse i = (stacks starting, map move $ lines script)
  where [starting, script] = splitOn "\n\n" i
  
popAt :: Int -> Int -> [[Char]] -> ([Char], [[Char]])
popAt 1 p (h:rest)= let (pp, hh) = splitAt p h in (pp, hh:rest)
popAt n p (h:rest) = let (c, hh) = popAt (n - 1) p rest in (c, h:hh)

pushAt :: Int -> [Char] -> [[Char]] -> [[Char]]
pushAt 1 c (h:rest) = (c ++ h):rest
pushAt n c (h:rest) = h : pushAt (n - 1) c rest

type Crane = [Char] -> [Char]

step :: Crane -> (Int, Int, Int) -> [[Char]] -> [[Char]]
step crane (n, from, to) s = popAt from n s
                           & first crane
                           & uncurry (pushAt to)

run :: Crane -> Challenge -> String
run crane = map head . uncurry (foldl' (flip (step crane)))  

main :: IO ()
main = challenge 05 parse (run reverse) (run id)