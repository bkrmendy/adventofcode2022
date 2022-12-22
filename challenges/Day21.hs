{-# LANGUAGE LambdaCase #-}
module Main where

import Advent (challenge)
import Utils (readInt)

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Control.Monad.State.Strict

import qualified Data.Map.Strict as M

data Expr = N Int | E [String] deriving (Show)

type Challenge = M.Map String Expr

monkey :: String -> State Challenge Expr
monkey m = gets $ (M.! m)

parse :: String -> Challenge
parse = M.fromList . map entry . lines
  where entry = (\[m, e] -> expr m e) . splitOn ": "
        expr m e = if all isDigit e then (m, N (readInt e)) else (m, E (words e))

exec :: String -> Int -> Int -> Int
exec "+" = (+)
exec "-" = (-)
exec "*" = (*)
exec "/" = div

eval :: String -> State Challenge Int
eval m = do
  result <- (monkey m) >>= \case
    N n -> return n
    E [left, op, right] -> (exec op) <$> eval left <*> eval right
  modify' $ M.insert m (N result)
  return result

path :: String -> State Challenge (Maybe [(Int, String, String)])
path "humn" = return $ Just []
path m = do
  (monkey m) >>= \case
    N _ -> return $ Nothing
    E [left, op, right] -> do
      path left >>= \case
        Just s -> do
          v <- eval right
          return $ Just ((v, left, op):s)
        Nothing -> do
          path right >>= \case
            Nothing -> return Nothing
            Just s -> do
              v <- eval left
              return $ Just ((v, right, op):s)

humanValue :: (Int, String, String) -> (Int, String, String) -> State Challenge (Int, String, String)
humanValue (target, current, _) (v, next, op) = do
  (monkey current) >>= \case
    E [left, _, _] -> do
      nextTarget <- return $ case op of
        "+" -> target - v
        "*" -> target `div` v
        "/" -> target * v
        "-" -> if left == next then target + v else v - target
      return $ (nextTarget, next, op)

part1 :: Challenge -> Int
part1 = evalState (eval "root")

part2 :: Challenge -> Int
part2 m = flip evalState m $ do
  p <- fromJust <$> path "root"
  (\(r, _, _) -> r) <$> foldM humanValue (head p) (tail p)

main :: IO ()
main = challenge 21 parse part1 part2