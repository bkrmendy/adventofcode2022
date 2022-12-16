module Main where

import Advent (challenge)
import Utils (int, parseL)

import Text.Parsec hiding (parse, State)
import Data.List (tails)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

type Challenge = M.HashMap String (Int, [String])

parseOne :: Parsec String () (String, Int, [String])
parseOne = (,,) <$> (string "Valve " *> valve)
                <*> (string " has flow rate=" *> int)
                <*> ((try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve ") *> valves)
  where valve = many upper
        valves = sepBy1 valve (string ", ") <|> (do { v <- valve; return [v] })

parse :: String -> Challenge
parse = M.fromList . map (\(v, r, vs) -> (v, (r, vs))) . parseL parseOne

neighbors :: Challenge -> String -> [String]
neighbors i = snd . (i M.!)

rate :: Challenge -> String -> Int
rate i = fst . (i M.!)

-- | cribbed from glguy
solve :: Int -> Challenge -> M.HashMap (S.HashSet String) Int
solve timer i = go [(("AA", S.empty), 0)] timer
  where
    simplify = M.toList . M.fromListWith max 
    go states 0 = M.fromListWith max [(open, n) | ((_, open), n) <- states]
    go states t = go (simplify (concatMap step states)) (t-1)
      where
        step ((here, open), n) =
          [((next, open), n) | next <- neighbors i here] ++
          [((here, S.insert here open), n + (t-1) * amt)
              | not $ S.member here open
              , let amt = rate i here, amt /= 0 ]

part1 :: Challenge -> Int
part1 = maximum . solve 30

part2 :: Challenge -> Int
part2 i = maximum [v1+v2 | (open1,v1) : elephants <- tails (M.toList routes),
                           (open2,v2) <- elephants,
                           S.null (S.intersection open1 open2)]
  where routes = solve 26 i

main :: IO ()
main = challenge 16 parse part1 part2