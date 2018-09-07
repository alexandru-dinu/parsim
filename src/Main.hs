module Main where

import Control.Applicative
import Types
import Parser

-- parser main
runParser :: String -> Maybe Prog
runParser s = case parseWith (seqp <|> progp) s of 
    [(prog, "")] -> Just prog
    _            -> Nothing

path = "../tests/raw_input/test5-5.unvar.in"
main :: IO ()
main = do
    p <- readFile path
    putStrLn $ show $ runParser p