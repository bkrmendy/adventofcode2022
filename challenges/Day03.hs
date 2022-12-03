module Main where

import Advent (challenge)
import qualified Data.Set as S
import Data.Char (ord, isLower, isUpper)
import Data.List.Split (chunksOf)
import Data.List (foldl')

type Compartment = S.Set Char

type Challenge = [String]

halve :: [a] -> ([a], [a]) 
halve xs = splitAt s xs
    where s = length xs `div` 2
        
multisection :: (Ord a) => [S.Set a] -> S.Set a
multisection [] = S.empty
multisection (first:rest) = foldl' S.intersection first rest

priority :: Char -> Int
priority c | isLower c = ord c - ord 'a' + 1
priority c | isUpper c = ord c - ord 'A' + 27
priority _ = 0

compartments :: String -> [Compartment]
compartments l = (\(a, b) -> [S.fromList a, S.fromList b]) $ halve l
  
part1 :: Challenge -> Int
part1 = sum . concatMap (map priority . S.elems . multisection . compartments)

part2 :: Challenge -> Int
part2 = sum . concatMap (map priority . S.elems . multisection . map S.fromList) . chunksOf 3

main :: IO ()
main = challenge 03 lines part1 part2