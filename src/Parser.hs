{-# LANGUAGE LambdaCase #-}

module Parser where

import Data.Char
import Control.Applicative

import Types


data Parser a = Parser { parseWith :: String -> [(a, String)] }


instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f a, rest) | (a, rest) <- p s]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser p1) <*> (Parser p2) = Parser $ \s -> [(f x, r2) | (f, r1) <- p1 s, (x, r2) <- p2 r1]

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s -> concatMap (\(x, r) -> parseWith (f x) r) $ parseWith p s

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = Parser $ \s ->
        case parseWith p1 s of
        []  -> parseWith p2 s
        res -> res


-- representation of a _failure_ parser
-- note that this is different from pure []
failure :: Parser a
failure = Parser $ \s -> []

-- consume a char (move _cursor_ one pos the right)
move :: Parser Char
move = Parser $ \case
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

-- apply charp once
once :: Char -> Parser String
once c = (charp c) >>= (\_ -> return "")

-- apply charp twice
twice :: Char -> Parser String
twice c = (once c) >>= (\_ -> once c) >>= (\_ -> return "")

-- numeric parser
numericp = trim $ oneOrMore $ fromPredicate isDigit

-- token parser
tokenp = trim $ oneOrMore $ fromPredicate isAlpha



-- CONSTRUCT INTERNAL ADT REPRESENTATION OF PROGRAMS


-- Parse Expression
exprp :: Parser Expr
exprp = equalp <|> smallerp <|> arithmeticp


-- Arithmetic parser
arithmeticp :: Parser Expr
arithmeticp = multp <|> addp <|> subp <|> valuep <|> symbolp

-- Value parser
valuep :: Parser Expr
valuep = numericp >>= (\v -> return (Value (read v :: Int)))

-- Symbol parser
symbolp :: Parser Expr
symbolp = tokenp >>= (\v -> return (Symbol v))

-- Mult parser
multp :: Parser Expr
multp = do
    v1 <- valuep <|> symbolp
    _ <- once '*'
    v2 <- valuep <|> symbolp
    return $ Mult v1 v2

-- Add parser
addp :: Parser Expr
addp = do
    v1 <- valuep <|> symbolp
    _ <- once '+'
    v2 <- valuep <|> symbolp
    return $ Add v1 v2

-- Sub parser
subp :: Parser Expr
subp = do
    v1 <- valuep <|> symbolp
    _ <- once '-'
    v2 <- valuep <|> symbolp
    return $ Sub v1 v2

-- Equal parser
equalp :: Parser Expr
equalp = do
    v1 <- arithmeticp
    _ <- twice '='
    v2 <- arithmeticp
    return $ Equal v1 v2

-- Smaller parser
smallerp :: Parser Expr
smallerp = do
    v1 <- arithmeticp
    _ <- once '<'
    v2 <- arithmeticp
    return $ Smaller v1 v2


-- Parse Programs
progp :: Parser Prog
progp = assgnp <|> ifp <|> whilep <|> retp


-- Sequence parser
seqp :: Parser Prog
seqp = do
    p1 <- progp
    p2 <- seqp <|> progp
    return $ Seq p1 p2

-- parse assignment
assgnp :: Parser Prog
assgnp = do
    t <- tokenp
    _ <- once '='
    e <- exprp
    _ <- trim $ once ';'
    return $ Eq t e

-- If parser
ifp :: Parser Prog
ifp = do
    _ <- trim $ stringp "if "
    _ <- once '('
    c <- exprp -- condition
    _ <- trim $ stringp ") then {"
    pt <- seqp <|> progp
    _ <- trim $ stringp "} else {"
    pe <- seqp <|> progp
    _ <- trim $ stringp "}"
    return $ If c pt pe

-- While parser
whilep :: Parser Prog
whilep = do
    _ <- trim $ stringp "while "
    _ <- once '('
    c <- exprp -- condition
    _ <- trim $ stringp ") {"
    p <- seqp <|> progp
    _ <- trim $ stringp "}"
    return $ While c p

-- Return parser
retp :: Parser Prog
retp = do
    _ <- trim $ stringp "return "
    e <- exprp -- returned expression
    _ <- trim $ once ';'
    return $ Return e
