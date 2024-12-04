{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Arrow ((***))
-- import Control.Monad (join)
-- import Data.Bifunctor (bimap)
import Data.List ({- intersect, intersectBy, -} transpose)
import Data.Text (count, pack)
import Data.Universe.Helpers (diagonals)

rotateL :: String -> String
rotateL (_ : str) = str ++ [' ']
rotateL [] = []

rotateR :: String -> String
rotateR = reverse . rotateL . reverse

getOccurance :: String -> [String] -> [(Int, String)]
getOccurance haystack = map go
  where
    packed = pack haystack
    go needle = (count (pack needle) packed, needle)

getHorizontalOccurance :: [String] -> Int
getHorizontalOccurance = sum . map ((sum . map fst) . (`getOccurance` ["XMAS", "SAMX"]))

getVerticalOccurance :: [String] -> Int
getVerticalOccurance = getHorizontalOccurance . transpose

getAllLineOccurance :: [String] -> Int
getAllLineOccurance str = getHorizontalOccurance str + getVerticalOccurance str

partOne :: [String] -> Int
partOne dat = getAllLineOccurance dat + (getHorizontalOccurance . diagonals) dat + (getHorizontalOccurance . diagonals . map reverse) dat

substring :: String -> String -> Bool
substring (_ : _) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_ : _) [] = False
prefix (x : xs) (y : ys) = x == y && prefix xs ys

-- getSigIndex n arrlen = n - 2 + arrlen

-- partTwo dat = (diagonals dat, (diagonals . reverse) dat)
--   where
--     sorted = join (***) (filter (\x -> substring "MAS" x || substring "SAM" x)) (diagonals dat, (diagonals . reverse) dat)
--     indices = uncurry (intersectBy (\x y -> length x == length y)) sorted

main :: IO ()
main = do
  contents <- getContents
  let dat = lines contents
  print $ partOne dat

-- print $ partTwo dat

-- print $ (diagonals . map reverse) dat
