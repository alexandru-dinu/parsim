module Parser where

import Data.Char
import Control.Applicative

import Types


data Parser a = Parser { parse :: String -> [(a, String)] }


instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f a, rest) | (a, rest) <- p s]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser p1) <*> (Parser p2) = Parser $ \s -> [(f x, r2) | (f, r1) <- p1 s, (x, r2) <- p2 r1]

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s -> concatMap (\(x, r) -> parse (f x) r) $ parse p s 

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = Parser $ \s ->
        case parse p1 s of 
        []  -> parse p2 s
        res -> res

runParser :: Parser a -> String -> a
runParser = undefined -- TODO

-- representation of a _failure_ parser
-- not that this is different from pure []
failure :: Parser a
failure = Parser $ \s -> []

-- consume a char (move _cursor_ one pos the right)
move :: Parser Char
move = Parser $ \s ->
    case s of
    []     -> []
    (c:cs) -> [(c, cs)]

-- construct a char parser from a predicate
fromPredicate :: (Char -> Bool) -> Parser Char
fromPredicate pred = move >>= (\c -> if pred c then return c else failure)

-- char parser - match a char c
charp :: Char -> Parser Char
charp c = fromPredicate (== c)

-- string parser - match a given string
stringp :: String -> Parser String
stringp ""  = Parser $ \s -> [("", s)]
stringp (n:ns) = (charp n) >>= (\c -> (stringp ns) >>= (\r -> return (c:r)))

-- (+) quantifier
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p >>= (\v -> (zeroOrMore p) >>= (\v' -> return (v:v')))

-- (*) quantifier
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- consume the given char from a string
-- don't store it, since we won't need it
consume :: Char -> Parser String
consume c = (oneOrMore $ charp c) >>= (\_ -> return "") 

-- whitespace = [ \t\n]
rmWhitespace :: Parser String
rmWhitespace = (zeroOrMore wsp) >>= (\_ -> return "") where wsp = (charp ' ' <|> charp '\n')

-- apply parser p to a trimmed string
trim :: Parser a -> Parser a
trim p = rmWhitespace >>= (\_ -> p >>= (\r -> rmWhitespace >>= (\_ -> return r)))


-- numeric parser
numericp = trim $ oneOrMore $ fromPredicate isDigit

-- token parser
tokenp = trim $ oneOrMore $ fromPredicate isAlpha