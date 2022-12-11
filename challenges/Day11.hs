{-# LANGUAGE BangPatterns #-}

module Main where

import Advent (challenge)
import Utils ()

import Prelude hiding (round)
import Data.List (sortBy)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

data Monkey  = MkMonkey { _starting :: ![Int], _op :: Int -> Int, _test :: Int -> Int }

divisible :: Int -> Int -> Int -> (Int -> Int)
divisible by thenBranch elseBranch = \i -> if i `mod` by == 0 then thenBranch else elseBranch

type Monkeys = M.Map Int Monkey
type Business = M.Map Int Int
type Challenge = Monkeys

monkeys :: Monkeys
monkeys = M.fromList . zip [0..] $ [
  MkMonkey [84, 66, 62, 69, 88, 91, 91] (* 11) (divisible 2 4 7),
  MkMonkey [98, 50, 76, 99]             (\o -> o * o) (divisible 7 3 6),
  MkMonkey [72, 56, 94]                 (+ 1) (divisible 13 4 0),
  MkMonkey [55, 88, 90, 77, 60, 67]     (+ 2) (divisible 3 6 5),
  MkMonkey [69, 72, 63, 60, 72, 52, 63, 78] (* 13) (divisible 19 1 7),
  MkMonkey [89, 73]                     (+ 5) (divisible 17 2 0),
  MkMonkey [78, 68, 98, 88, 66]         (+ 6) (divisible 11 2 5),
  MkMonkey [70]                         (+ 7) (divisible 5 1 3) ]

theMagicTrick :: Int
theMagicTrick = product [2, 7, 13, 3, 19, 17, 11, 5]

data MS = MkMS { _business :: !Business, _worryFn :: Int -> Int, _monkeys :: !Monkeys }

throw :: Int -> Int -> State MS ()
throw target payload = do
  monkey <- gets ((M.! target) . _monkeys)
  modify' $ \s -> s { _monkeys = M.insert target (monkey { _starting = _starting monkey ++ [payload] }) (_monkeys s)}

turn :: Int -> State MS ()
turn i = do
  (MkMonkey items worry test) <- gets ((M.! i) . _monkeys)
  worryFn <- gets _worryFn
  modify' $ \s -> s { _monkeys = M.insert i (MkMonkey [] worry test) (_monkeys s) }
  modify' $ \s -> s { _business = M.insertWith (+) i (length items) (_business s) }
  forM_ items $ \item -> do
    let !w = worryFn (worry item)
        !target = test w
    throw target w

run :: Int -> State MS Business
run rounds = do
  forM_ [1..rounds] $ \_ ->
    forM_ [0..7] turn
  gets _business
  
monkeyBusiness :: Business -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare) . M.elems

parse :: String -> Challenge
parse = const monkeys

part1 :: Challenge -> Int
part1 = monkeyBusiness . evalState (run 20) . MkMS M.empty (`div` 3)

part2 :: Challenge -> Int
part2 = monkeyBusiness . evalState (run 10000) . MkMS M.empty (`mod` theMagicTrick)

main :: IO ()
main = challenge 11 parse part1 part2