module Main where

import Advent (challenge)
import qualified Data.Set as S
import Data.Char (ord, isLower, isUpper)
import Data.Bifunctor (bimap)
import Data.List (foldl')

type Compartment = S.Set Char

type Challenge = [String]

halve :: [a] -> ([a], [a]) 
halve xs = splitAt s xs
    where s = length xs `div` 2
    
threes :: [a] -> [[a]]
threes [] = []
threes (a:b:c:rest) = [a, b, c] : threes rest
threes end = [end]
    
threentersection :: (Ord a) => [S.Set a] -> S.Set a
threentersection [] = S.empty
threentersection (first:rest) = foldl' S.intersection first rest

priority :: Char -> Int
priority c | isLower c = ord c - ord 'a' + 1
priority c | isUpper c = ord c - ord 'A' + 27
priority _ = 0

compartments :: String -> (Compartment, Compartment)
compartments l = bimap S.fromList S.fromList $ halve l
  
part1 :: Challenge -> Int
part1 = sum . concatMap (map priority . S.elems . uncurry S.intersection . compartments)

part2 :: Challenge -> Int
part2 = sum . concatMap (map priority . S.elems . threentersection . map S.fromList) . threes

main :: IO ()
main = challenge 03 lines part1 part2