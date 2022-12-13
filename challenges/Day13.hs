module Main where

import Advent (challenge)
import Utils (int, parseLines)

import Data.Maybe (fromJust)
import Data.List (sort, findIndex)
import Data.List.Split (splitOn)
import Text.Parsec hiding (parse)

data Packet = Single Int | List [Packet] deriving (Show, Eq)

single :: Parsec String () Packet
single = Single <$> int

list :: Parsec String () Packet
list = List <$> between (char '[') (char ']') (sepBy packet (char ','))

packet :: Parsec String () Packet
packet = single <|> list

type Challenge = [(Packet, Packet)]

parse :: String -> [(Packet, Packet)]
parse = map (p . lines) . splitOn ("\n\n")
  where p [left, right] = (parseLines packet left, parseLines packet right)
  
instance Ord Packet where
  compare (Single left) (Single right) = left `compare` right
  compare (List left) (List right) = left `compare` right
  compare (List left) (Single right) = compare (List left) (List [Single right])
  compare (Single left) (List right) = compare (List [Single left]) (List right)

part1 :: Challenge -> Int
part1 = sum . map fst . filter ((== LT) . snd) . zip [1..] . map (uncurry compare)

part2 :: Challenge -> Int
part2 = (\ps -> find (divider 2) ps * find (divider 6) ps) . sort . ([divider 2, divider 6] ++) . concatMap (\(l, r) -> [l, r])
  where find packet = succ . fromJust . findIndex (== packet)
        divider i = List [List [Single i]]

main :: IO ()
main = challenge 13 parse part1 part2