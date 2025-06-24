-- A simple command-line interface for your regex matcher.
-- It allows the user to enter a regex and a string, then prints whether it matches.

module Main where

import RegexMatcher

main :: IO ()
main = do
    putStrLn "Enter regex:"
    regex <- fmap stripQuotes getLine
    putStrLn "Enter input string:"
    input <- getLine
    let result = matchRegex regex input
    putStrLn $ "Result: " ++ show result

stripQuotes :: String -> String
stripQuotes s = case s of
  ('"':xs) -> reverse (dropWhile (== '"') (reverse xs))
  _        -> s

