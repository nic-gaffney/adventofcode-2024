module Main where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.List

tuplify :: [a] -> (a, a)
tuplify (x : xx : _) = (x, xx)

getInputLine :: String -> (Int, Int)
getInputLine str = tuplify $ map read $ words str

getInput :: String -> [(Int, Int)]
getInput str = map getInputLine $ lines str

parseInput :: String -> ([Int], [Int])
parseInput = foldr (\x -> bimap (fst x :) (snd x :)) ([], []) . getInput

difference :: ([Int], [Int]) -> [Int]
difference (x : xs, y : ys) = abs (x - y) : difference (xs, ys)
difference ([], []) = []

partOne :: ([Int], [Int]) -> Int
partOne = sum . difference . join bimap sort

similarity :: Int -> [Int] -> Int
similarity x ys = x * length (filter (== x) ys)

partTwo :: ([Int], [Int]) -> Int
partTwo (x, y) = sum $ map (`similarity` y) x

main :: IO ()
main = do
  contents <- getContents
  putStrLn "Advent Of Code Day 1"
  putStrLn "Reading input.txt..."
  putStr "Part 1: "
  (print . partOne . parseInput) contents
  putStr "Part 2: "
  (print . partTwo . parseInput) contents
