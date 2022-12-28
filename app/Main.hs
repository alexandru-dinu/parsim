module Main where

import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )

import Types ( Prog )
import Parser ( progp, seqp, Parser(parseWith) )
import Interpreter ( evalProg, getFrom, Possibly )

-- parser main
runParser :: String -> Possibly Prog
runParser s = case parseWith (seqp <|> progp) s of
    [(prog, "")] -> pure prog
    _            -> fail "Syntax error(s)"

-- eval main
runEval :: Prog -> Possibly Int
runEval p = do
    c' <- evalProg p []
    getFrom c' "$"


-- raw (parse -> eval)
runRaw :: String -> Possibly Int
runRaw s = do
    p <- runParser s
    runEval p


main :: IO ()
main = do
    a <- getArgs
    p <- readFile $ head a
    print $ runRaw p
