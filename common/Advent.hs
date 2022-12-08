{-# LANGUAGE CPP #-}
module Advent where

import Text.Printf (printf)
import System.FilePath (takeDirectory, (</>))
import qualified Test.BenchPress as B

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

readInput :: Int -> IO String
readInput day = readFile $ baseDir </> "input" </> printf "%02d" day <> ".txt"

challenge
    :: (Show output)
    => Int
    -> (String -> input)
    -> (input -> output)
    -> (input -> output)
    -> IO ()
challenge day parse part1 part2 = do
    inputText <- readInput day
    let structured = parse inputText
    print $ part1 structured
    print $ part2 structured

bench
  :: (Show output)
  => Int
  -> (String -> input)
  -> (input -> output)
  -> (input -> output)
  -> IO ()
bench day parse part1 part2 = do
  inputText <- readInput day
  let structured = parse inputText
  B.bench 100 $ do
    print $ part1 structured
  B.bench 100 $ do
    print $ part2 structured

visual
  ::  Int
  -> (String -> input)
  -> (input -> String)
  -> (input -> String)
  -> IO ()
visual day parse part1 part2 = do
    inputText <- readInput day
    let structured = parse inputText
    putStrLn $ part1 structured
    putStrLn $ part2 structured