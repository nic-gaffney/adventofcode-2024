module Main where

import Text.Parsec

parseSection :: Monad m => ParsecT String s m (Int, Int)
parseSection =
  do
    num1 <- manyTill anyChar (string "mul(") *> many digit
    num2 <- char ',' *> many digit <* char ')'
    return (read num1, read num2)

dummy :: Monad m => ParsecT String s m (Int, Int)
dummy = do
  _ <- manyTill anyChar anyChar
  return (0, 0)

run :: Monad m => ParsecT String s m [(Int, Int)]
run = many (try parseSection <|> dummy)

parseAll :: String -> Int
parseAll str = do
  let result = parse run "(source)" str
  case result of
    Right v -> sum . map (uncurry (*)) $ v
    Left _ -> -1

-- Part 1: just run with cabal
-- Part 2: run `vim -s vimParse input.txt` then run with cabal
main :: IO ()
main = do
  content <- getContents
  print $ parseAll content
