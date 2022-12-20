module Main where

import Advent (challenge)
import Utils (readInt)

import qualified Data.Sequence as S

type File = S.Seq Int

parse :: String -> [Int]
parse = map readInt . lines

result :: File -> Int
result file = sum [i | idx <- [1000, 2000, 3000], let i = S.index file ((start + idx) `mod` S.length file)]
  where Just start = S.elemIndexL 0 file

mix :: Int -> [Int] -> File
mix times ns = go (S.fromList $ zip [1..] ns) (concat $ replicate times [1..length ns])
  where go s [] = snd <$> s
        go s (idx:idxs) = go (S.insertAt d (idx, v) (b <> a)) idxs
          where Just i = S.findIndexL ((== idx) . fst) s
                (a, (_, v) S.:<| b) = S.splitAt i s
                d = v `mod` (S.length s - 1)

part1 :: [Int] -> Int
part1 = result . mix 1

part2 :: [Int] -> Int
part2 = result . mix 10 . map (* 811589153)

main :: IO ()
main = challenge 20 parse part1 part2