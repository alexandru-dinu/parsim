module Parser where

import Types

import Data.Char
import Control.Applicative


data Parser a = Parser { parse :: String -> [(a, String)] }

failure :: Parser a
failure = Parser (\s -> [])

instance Functor Parser where
    fmap f (Parser p) = Parser (\s -> [(f a, rest) | (a, rest) <- p s])

instance Applicative Parser where
    pure x = Parser (\s -> [(x, s)])
    (Parser p1) <*> (Parser p2) = Parser (\s -> [(f x, r2) | (f, r1) <- p1 s, (x, r2) <- p2 r1])

instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    p >>= f = Parser $ \s -> concatMap (\(x, r) -> parse (f x) r) $ parse p s 



runParser :: Parser a -> String -> a
runParser = undefined -- TODO


move :: Parser Char
move = Parser $ \s ->
    case s of
    []     -> []
    (c:cs) -> [(c, cs)]


sat :: (Char -> Bool) -> Parser Char
sat pred = move >>= (\c -> if pred c then return c else failure)


charp :: Char -> Parser Char
charp c = sat (== c)