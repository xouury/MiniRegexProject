-- This file implements a recursive descent parser which converts input strings
-- into RegexTree tree. Supports literals, concatenation, alternation (|), and Kleene star (*).
-- We implement instances of Functor, Applicative, Monad, and Alternative to enable sequencing parsers
-- mapping functions over results, combining parsers with choice (<|>).

module RegexParser (parseRegex) where

import Control.Applicative ( Alternative((<|>), empty, some) ) 
import RegexTree ( Regex(..) )

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (result, rest) <- p input
        runParser (f result) rest

instance Functor Parser where
    fmap f p = Parser $ \input -> do
        (result, rest) <- runParser p input
        return (f result, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    pf <*> px = Parser $ \input -> do
        (f, rest1) <- runParser pf input
        (x, rest2) <- runParser px rest1
        return (f x, rest2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \input ->
        runParser p1 input <|> runParser p2 input

-- Basic primitive parsers, they simplify the implementation of the main parser.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs) | p x = Just (x, xs)
             | otherwise    = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

eof :: Parser ()
eof = Parser $ \input -> case input of
    [] -> Just ((), [])
    _  -> Nothing

-- Entry point for parsing full regular expressions.

parseRegex :: String -> Maybe Regex
parseRegex inp = case runParser (regex <* eof) inp of
    Just (r, "") -> Just r
    _            -> Nothing

regex :: Parser Regex
regex = do
    t <- term
    (do _ <- char '|'
        Alt t <$> regex) <|> return t

term :: Parser Regex
term = do
    fs <- some factor <|> pure []
    case fs of
        [] -> pure Empty
        (f:rest) -> pure (foldl Concat f rest)

factor :: Parser Regex
factor = do
    b <- base
    (do _ <- char '*'
        return (Star b)) <|> return b

base :: Parser Regex
base =
    (do _ <- char '('
        r <- regex
        _ <- char ')'
        return r)
    <|> (Literal <$> satisfy isLiteral)

isLiteral :: Char -> Bool
isLiteral c = c `notElem` "*|()"


