module Main where

import Control.Applicative
import System.Environment (getArgs)

import Types
import Parser
import Interpreter

-- parser main
runParser :: String -> Possibly Prog
runParser s = case parseWith (seqp <|> progp) s of 
    [(prog, "")] -> pure prog
    _            -> fail "Syntax error(s)"

-- eval main
runEval :: Prog -> Possibly Int
runEval p = do
    c' <- evalProg p []
    res <- getFrom c' "$"
    return res


-- raw (parse -> eval)
runRaw :: String -> Possibly Int
runRaw s = do
    p <- runParser s
    r <- runEval p
    return r


main :: IO ()
main = do
    a <- getArgs
    p <- readFile $ head a
    putStrLn $ show $ runRaw p