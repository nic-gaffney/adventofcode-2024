{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Ix (range)
import Data.List (sort)

parseInput :: String -> [Int]
parseInput = map read . words

verify :: [Int] -> [Int]
verify (x : xx : xs) = (x - xx) : verify (xx : xs)
verify [x] = []
verify [] = []

getMaxDiff :: [Int] -> Int
getMaxDiff = maximum . map abs . verify

verifyHelp :: [Int] -> Bool
verifyHelp xs = elem (length xs) $ map (length . (`filter` xs)) [(> 0), (< 0)]

safe :: [Int] -> Bool
safe x = getMaxDiff x < 4 && (verifyHelp . verify) x

withoutIndex :: [Int] -> Int -> [Int]
withoutIndex xs x = take (x - 1) xs ++ drop x xs

safeWithRemoval :: [Int] -> Bool
safeWithRemoval xs = safe xs || any (safe . withoutIndex xs) (range (1, length xs))

partOne :: [[Int]] -> Int
partOne = length . filter safe

partTwo :: [[Int]] -> Int
partTwo = length . filter safeWithRemoval

main :: IO ()
main = do
  content <- getContents
  let input = (map parseInput . lines) content
  putStrLn "Advent of Code day 2"
  putStrLn "Reading input.txt..."
  putStr "Day 1: "
  (print . partOne) input
  putStr "Day 2: "
  (print . partTwo) input
