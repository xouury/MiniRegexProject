
-- This file defines the core abstract syntax tree for regular expressions.
-- The tree consists of variants for empty strings, literals, concatenation,
-- alternation (|), and Kleene star (*).
-- This module serves as the central representation that other modules work with.

module RegexTree (Regex(..)) where

data Regex
    = Empty 
    | Literal Char
    | Concat Regex Regex
    | Alt Regex Regex
    | Star Regex
    deriving (Show, Eq)